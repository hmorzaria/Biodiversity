#Hem Nalini Morzaria Luna
#March 2015
#organize data frome excel spreadsheets
#clean up the space

rm(list=ls())

#use these lines if packages are not installed
#install.packages("sp","gdata","XLConnect","maptools",PBSmapping","rgdal","fields","raster","rasterVis")
#install.packages("spocc") # this package had a dependency issue, check later to see if fixed

require(gdata)
require(maptools)   # used here to read in the spatial data
require(PBSmapping) # for GIS-like geoprocessing operations
require(rgdal)  
require(fields)
require(spocc)
require(rgbif)
require(raster)
require(rasterVis)
require(sp)
require(sperich)
require(dplyr)
library(spatstat)


# USER BLOCK: CHECK AND CHANGE OPTIONS HERE  
#_________________________________________________________________________
#set working directories
#this should match the path where your files directories are stored
#note the "/" go in the opposite direction than in Windows explorer

filepath="E:/Archivos/1Archivos/Articulos/En preparacion/Spatial management/Analysis/Ocurrencia" #put path
analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Spatial management/Analysis" #put path
savepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation"

#set directory path
setwd(savepath)

#read in Gulf of California shapefile
goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")#this shapefile does not use wetlands, only marine areas
#I had originally used the polygon Golfo_california_wetland_polygon2.shp 
#but it took too much of terrestrial area
#read in grid shapefile
grid.shape <- readOGR(".", "grid_9km_gc_lmb")

# #read in Northern Gulf of California shapefile
# goc.shape <- readOGR(".", "Atlantis12_AggregatePolygons")
# 
# 
# projections
#Lambert
crs.geo.lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat")
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

########################User can ignore everything below

#read in species occurrance
#this data was obtained and cleaned in Data_biodiversity.R
#files were then organized in Organize_biodiversity.R

setwd(analysispath)
biodiversity.all  = read.csv("biodiver_species_goc.csv",header=T, fill=TRUE)
#select species, lat and lon and rename
biodiversity = biodiversity.all[,c(4,5,2)]
names(biodiversity) = c('long','lat','speciesID')

# Use package sperich to create biodiversity model

  #calculate grid from data 
  dimension = getDimension(biodiversity,resolution=0.1)
  shift = getShift(biodiversity)
  
  #create null masks
  landwatermask = createLandwatermask(NULL,dimension,shift,resolution=0.1)
  heightwatermask = createHeightmask(NULL,dimension,shift,resolution=0.1)
  
  #map species ocurrence
  noninterpolatedgrid = createNonInterpolatedGrid(biodiversity,dimension,shift,resolution=0.1, all.species=-1)
  
  #calculate inverse-distance weighted richness
  
  species.richness.weighted = species.richness(biodiversity,
                                               landwatermask=landwatermask,
                                               distances=1:10,
                                               weight=0.5,
                                               dimension=dimension,
                                               shift=shift,
                                               resolution=0.1,
                                               upperbound=3000,
                                               all.species=-1,
                                               silent=FALSE,
                                               do.parallel=FALSE)
  #adjust the result for sampling effort
  #set clusterlimit to prepare clusterlist
  clusterlist=searchClusters(species.richness.weighted,
                             dimension,
                             shift,
                             resolution=0.1,
                             clusterlimit = 100)
  
  #adjust inverse-weighted species
  #richness grid for sampling effort
  
  species.richness.adjusted = adjustment(species.richness.weighted,
                                         noninterpolatedgrid,
                                         clusterlist)
  
  #save clusters with minimal size of 20 pixels
  min.size =  20
  
  for(i in 1:length(clusterlist))
  {
    if(length(clusterlist[[i]])>= min.size)
    {
      cluster=matrix(0,
                     dimension[1], dimension[2])
      cluster[clusterlist[[i]]]=
        rep(1,length(clusterlist[[i]]))
      exportAsGDAL(cluster, shift,
                   resolution=0.1, directory=getwd(),
                   filename=paste("cluster",i,".tif",sep=""),
                   drivename="GTiff")
    }
  }
  
  
}
