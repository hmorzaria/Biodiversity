
#Hem Nalini Morzaria Luna
#March 2015
#organize data frome excel spreadsheets
#obtain data from repositories

#clean up the space

rm(list=ls())

#use these lines if packages are not installed
#install.packages(c("sp","gdata","XLConnect","maptools","PBSmapping","rgdal","fields","raster","rasterVis","rgbif","sperich","dplyr"))
#install.packages("spocc") # this package had an error, check later to see if fixed

#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_45')
require(gdata)
library(readxl)
require(maptools)   # used here to read in the spatial data
require(PBSmapping) # for GIS-like geoprocessing operations
require(rgdal)  
require(fields)
require(data.table)
#require(spocc)
require(rgbif)
require(raster)
require(rasterVis)
require(sp)
require(sperich)
require(dplyr)
require(ecoengine)
library(rvertnet)
library("httr")
#library spocc can be used to get data from GBIF and other repositories USGS's BISON, iNaturalist, Berkeley Ecoinformatics Engine
#eBird, AntWeb/ However there some error with the package and the syntax from their manual does not work.

#spocc.data <- occ(from = 'gbif',geometry=this.polygon, limit=50)
# USER BLOCK: CHECK AND CHANGE OPTIONS HERE  
#_________________________________________________________________________
#set working directories
#this should match the path where your files directories are stored
#note the "/" go in the opposite direction than in Windows explorer

datafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Spatial management/Datos/Ocurrencia_especies"
vertnetfiles="E:/Archivos/1Archivos/Articulos/En preparacion/Spatial management/Datos/Vert_net_datos" #put path
#put path
analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Spatial management/Analysis/Ocurrencia" #put path
#put path
datapath="E:/Archivos/1Archivos/Articulos/En preparacion/Spatial management/Analysis" #put path

savepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation"

setwd(savepath)
#read in Gulf of California shapefile
goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")
#read in grid shapefile
#grid.shape <- readOGR(".", "grid_9km_gc_lmb")

# projections
#Lambert
crs.geo.lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat")
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84



########################User can ignore everything below



biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

#obtain GBIF data from bounding box
#bounding box created http://boundingbox.klokantech.com/
#This obtains data for part of the Gulf

#set bounding polygons
polygons=c('POLYGON((-113.8001 29.3949, -109.8193 29.3949, -109.8193 27.0713, -113.8001 27.0713, -113.8001 29.3949))',
           'POLYGON((-111.9818 27.1113, -107.9132 27.1113, -107.9132 24.8989, -111.9818 24.8989, -111.9818 27.1113))',
           'POLYGON((-110.7843 24.9346, -106.0236 24.9346, -106.0236 22.8799, -110.7843 22.8799, -110.7843 24.9346))',
           'POLYGON((-110.7843 22.9466, -105.1172 22.9466, -105.1172 20.3884, -110.7843 20.3884, -110.7843 22.9466))',
           'POLYGON((-114.9042 31.8153, -112.3627 31.8153, -112.3627 29.3462, -114.9042 29.3462, -114.9042 31.8153))')




#loop to read data and output points for the Gulf of California
                   
for(eachpolygon in 1:length(polygons)){

  this.polygon = polygons[eachpolygon]#subset bounding box
#call GBIF API, max records are 200000
gbif.goc <- occ_search(geometry=this.polygon,return='data',fields=c("name","decimalLatitude","decimalLongitude"),limit=200000)
names(gbif.goc) = c("Especie","Longitud","Latitud")#rename
gbif.goc$Fuente = "GBIF"
#add source
fuente = "GBIF"

gbif.goc2 = gbif.goc[complete.cases(gbif.goc),]#eliminate rows with NA

gbif.goc2$Fuente= as.factor(gbif.goc2$Fuente)#make source a factor

#remove duplicates based on the combination of latitude, longitude and species

biodiversity.clean = gbif.goc2[!duplicated(gbif.goc2[,c('Especie', 'Latitud', 'Longitud')]),]

#section the file so it can be subset for the Gulf of California

biodiv.rows = nrow(biodiversity.clean)

iterations = round(biodiv.rows/1000,0)

if(biodiv.rows>1000)
{
  last.row = 1001
  first.row = 1
}

if(biodiv.rows<1000)
{
  last.row = biodiv.rows
  first.row = 1
}


for (each.iteration in 1:iterations)

{
  new.last.row = ((last.row-1)*each.iteration)
  
  section.biodiv = biodiversity.clean[first.row:new.last.row,]
  section.biodiv = na.omit(section.biodiv)
  coordinates(section.biodiv) <- c("Longitud", "Latitud")  # set spatial coordinates
  proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
  print(summary(section.biodiv)) # print summary
  # subset ocurrence points within GOC
  stations_subset <- section.biodiv[goc.shape, ]
  #get table from shapefile
  biodiversity.goc <- as(stations_subset, "data.frame")
  
biodiversity = rbind(biodiversity,biodiversity.goc)
biodiversity = biodiversity[!duplicated(biodiversity[,c('Especie', 'Latitud', 'Longitud')]),]

  print(paste("biodiversity has ",nrow(biodiversity)," records"))
  
  first.row = new.last.row+1
}
  

}



biodiversity$Longitud = as.numeric(biodiversity$Longitud)

setwd(analysispath)
#write table
write.csv(biodiversity, file="GBIF_biodiver_species_goc.csv")

biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

#set bounding polygons
polygons=c('-114.0198,29.9937,-112.7307,31.7191',
           '-115.0305,29.9937,-113.9612,31.7938',
           '-114.6241,29.9937,-112.0386,28.9892',
           '-113.6902,28.0145,-110.9949,28.9892',
           '-112.8113,28.0145,-109.7095,26.9713',
           '-112.24,25.9884,-109.0283,26.9713',
           '-111.4929,25.9884,-107.8967,24.9863',
           '-110.9656,23.9669,-106.831,24.9863',
           '-110.0757,23.9669,-105.9851,22.989',
           '-109.9988,21.5867,-105.1611,22.989',
           '-109.9988,21.5867,-105.1611,20.3346')


#extract data from Berkley ecoengine

for(eachpolygon in 1:length(polygons)){
  
  this.polygon = polygons[eachpolygon]#subset bounding box
  #call ecoengine API
  ee.data = ee_observations(page_size=10000,country="Mexico", georeferenced = TRUE,bbox = this.polygon)
  ee.data.frame = as.data.frame(ee.data$data)
  ee.data.goc = ee.data.frame[,c(12,1,2)]
  names(ee.data.goc) = c("Especie","Longitud","Latitud")#rename
  ee.data.goc$Fuente = "Ecoengine"
  #add source
  fuente = "Ecoengine"
  
  ee.data.goc2 = ee.data.goc[complete.cases(ee.data.goc),]#eliminate rows with NA
  
  ee.data.goc2$Fuente= as.factor(ee.data.goc2$Fuente)#make source a factor
  
  #remove duplicates based on the combination of latitude, longitude and species
  
  biodiversity.clean = ee.data.goc2[!duplicated(ee.data.goc2[,c('Especie', 'Latitud', 'Longitud')]),]
  
  #section the file so it can be subset for the Gulf of California
  
  biodiv.rows = nrow(biodiversity.clean)
  
  iterations = round(biodiv.rows/1000,0)
  
  if(biodiv.rows>1000)
  {
    last.row = 1001
    first.row = 1
  }
  
  if(biodiv.rows<1000)
  {
    last.row = biodiv.rows
    first.row = 1
  }
  
  
  for (each.iteration in 1:iterations)
    
  {
    new.last.row = ((last.row-1)*each.iteration)
    
    section.biodiv = biodiversity.clean[first.row:new.last.row,]
    section.biodiv = na.omit(section.biodiv)
    coordinates(section.biodiv) <- c("Longitud", "Latitud")  # set spatial coordinates
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    
    biodiversity = rbind(biodiversity,biodiversity.goc)
    biodiversity = biodiversity[!duplicated(biodiversity[,c('Especie', 'Latitud', 'Longitud')]),]
    print(paste("biodiversity has ",nrow(biodiversity)," records"))
    
    first.row = new.last.row+1
  }
  
  
  
}

#write table
write.csv(biodiversity, file="Ecoengine_biodiver_species_goc.csv")

setwd(datafiles)#switch directory
biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

csv.files <- list.files(pattern = "\\.csv$")#list csv files

#loop to read in data and obtain points in Gulf of California
for(each.file in 1:length(csv.files)){
    
  this.file = csv.files[each.file]#file name
  
Biom  = fread(this.file,header=T, sep=",",select=c(1:11))#read in one file

Biom2= as.data.frame(Biom)

Biom3 = subset(Biom2, select = c("sname","latitude","longitude")) #subset needed variables
names(Biom3)=c('Especie', 'Latitud', 'Longitud') # rename columns
Biom3$Fuente = "OBIS" # set source
Fuente = "OBIS"  

Biom4 = Biom3[complete.cases(Biom3),]#eliminate rows with NA
  
Biom4$Fuente= as.factor(Biom4$Fuente)#make source a factor
  
#remove duplicates based on the combination of latitude, longitude and species
  
biodiversity.clean = Biom4[!duplicated(Biom4[,c('Especie', 'Latitud', 'Longitud')]),]
  # remove negative latitude values
biodiversity.clean2 = biodiversity.clean[biodiversity.clean$Latitud > 0,]
  
#section the file so it can be subset for the Gulf of California

biodiv.rows = nrow(biodiversity.clean2)

iterations = round(biodiv.rows/1000,0)

if(biodiv.rows>1000)
{
  last.row = 1001
  first.row = 1
}

if(biodiv.rows<1000)
{
  last.row = biodiv.rows
  first.row = 1
}

for (each.iteration in 1:iterations)
  
{
  new.last.row = ((last.row-1)*each.iteration)
  
  section.biodiv = biodiversity.clean2[first.row:new.last.row,]
  
  section.biodiv = na.omit(section.biodiv)
  
  coordinates(section.biodiv) <- c("Longitud", "Latitud")  # set spatial coordinates
  proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
  print(summary(section.biodiv)) # print summary
  # subset ocurrence points within GOC
  stations_subset <- section.biodiv[goc.shape, ]
  #get table from shapefile
  biodiversity.goc <- as(stations_subset, "data.frame")
  
  #names(biodiversity.goc) = c("Especie","Fuente","Longitud","Latitud")

biodiversity = rbind(biodiversity,biodiversity.goc)
biodiversity = biodiversity[!duplicated(biodiversity[,c('Especie', 'Latitud', 'Longitud')]),]

  print(paste("biodiversity has ",nrow(biodiversity)," records"))
  
first.row = new.last.row+1
}

 setwd(datafiles)#switch directory
      
  }
setwd(analysispath)
#write table
write.csv(biodiversity, file="OBIS_biodiver_species_goc.csv")


biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))


# for excel files from UABCS
setwd(datafiles)
xls.files <- list.files(pattern = "\\.xlsx$")# list files

#loop to read in data and obtain GOC data
for(each.file in 1:length(xls.files))
  {
  
  this.file = xls.files[each.file]#select file
  print(paste("Analyzing"," file",each.file,"_",this.file))
  df = read_excel(this.file, sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0)
  indx.sp= grep("Especie|Nombre|especie|nombre",colnames(df))
  indx.fuen= grep('Fuente|fuente|informacion|Base',colnames(df))
  indx.lon= grep('Longitud|longitud',colnames(df))
  indx.lat= grep('Latitud|latitud|Latutud',colnames(df))
  
  df2 = df[,c(indx.sp,indx.fuen,indx.lon,indx.lat)]
  names(df2) = c('Especie','Fuente','Longitud','Latitud')
  
  df2$Longitud = as.numeric(df2$Longitud) #make sure lon and lat are numeric
  df2$Latitud = as.numeric(df2$Latitud)
  
  df3 = df2[complete.cases(df2),]#eliminate rows with NA
  
  df3$Fuente= as.factor(df3$Fuente)#make source a factor
  
  #remove duplicates based on the combination of latitude, longitude and species
  
  biodiversity.clean = df3[!duplicated(df3[,c('Especie', 'Latitud', 'Longitud')]),]
  # remove negative latitude values
  biodiversity.clean2 = biodiversity.clean[biodiversity.clean$Latitud > 0,]
   
  #section the file so it can be subset for the Gulf of California
  
  biodiv.rows = nrow(biodiversity.clean2)
  
  iterations = round(biodiv.rows/1000,0)
  
  if(biodiv.rows>1000)
  {
    last.row = 1001
    first.row = 1
  }
  
  if(biodiv.rows<1000)
  {
    last.row = biodiv.rows
    first.row = 1
  }
  
  for (each.iteration in 1:iterations)
    
  {
    new.last.row = ((last.row-1)*each.iteration)
    
    section.biodiv = biodiversity.clean2[first.row:new.last.row,]
    section.biodiv = na.omit(section.biodiv)
    section.biodiv = data.frame(section.biodiv) # set spatial coordinates
    
    coordinates(section.biodiv) <- c("Longitud", "Latitud") 
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    
biodiversity = rbind(biodiversity,biodiversity.goc)
biodiversity = biodiversity[!duplicated(biodiversity[,c('Especie', 'Latitud', 'Longitud')]),]

    print(paste("biodiversity has ",nrow(biodiversity)," records"))
    
first.row = new.last.row+1
  }


 setwd(datafiles)

  
}

setwd(analysispath)

#write table
write.csv(biodiversity, file="COBI_biodiver_species_goc.csv")

#retrieve vertnet files

setwd(datapath)


biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

points.goc = fread("spatial_point_grid.csv",header = T)

#loop to read in data and obtain points in Gulf of California

for(each.point in 1:nrow(points.goc))
  {
  
  this.point = points.goc[each.point]#point
  point.lat = this.point$latitude
  point.lon = this.point$longitude
  
  res <- spatialsearch(lat = point.lat, lon = point.lon, radius = 10000, limit = 1000, config=verbose()) #radius in meters
  
  test.res = is.null(res)
  if(test.res==FALSE)
  {
  
  Biom = as.data.frame(res$data)
  
  indx.sp= grep("scientificname",colnames(Biom))
  
  test.sp.name = !length(indx.sp)
  
  if ( !length(indx.sp)==FALSE )
  {
  Biom = Biom[,c("scientificname","decimallatitude","decimallongitude")]
  names(Biom)=c('Especie', 'Latitud', 'Longitud') # rename columns
  Biom$Fuente = "VertNet" # set source
  Fuente = "VertNet"  
  Biom$Longitud = as.numeric(Biom$Longitud) #make sure lon and lat are numeric
  Biom$Latitud = as.numeric(Biom$Latitud)
  
  
  Biom4 = Biom[complete.cases(Biom),]#eliminate rows with NA
  
  Biom4$Fuente= as.factor(Biom4$Fuente)#make source a factor
  
  #remove duplicates based on the combination of latitude, longitude and species
  
  biodiversity.clean = Biom4[!duplicated(Biom4[,c('Especie', 'Latitud', 'Longitud')]),]
  # select only positive latitude values
  biodiversity.clean2 = biodiversity.clean[biodiversity.clean$Latitud > 0,]
  
  # select only negative longitude values
  biodiversity.clean2 = biodiversity.clean2[biodiversity.clean2$Longitud < 0,]
  
  #section the file so it can be subset for the Gulf of California
  
  biodiv.rows = nrow(biodiversity.clean2)
  
  iterations = round(biodiv.rows/500,0)
  
  if(biodiv.rows>500)
  {
    last.row = 501
    first.row = 1
  }
  
  if(biodiv.rows<500)
  {
    last.row = biodiv.rows
    first.row = 1
  }
  
  for (each.iteration in 1:iterations)
    
  {
    new.last.row = ((last.row-1)*each.iteration)
    
    section.biodiv = biodiversity.clean2[first.row:new.last.row,]
    
    section.biodiv = na.omit(section.biodiv)
    
    coordinates(section.biodiv) <- c("Longitud", "Latitud")  # set spatial coordinates
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    #names(biodiversity.goc) = c("Especie","Fuente","Longitud","Latitud")
    
    biodiversity = rbind(biodiversity,biodiversity.goc)
    biodiversity = biodiversity[!duplicated(biodiversity[,c('Especie', 'Latitud', 'Longitud')]),]
    
    print(paste("biodiversity has ",nrow(biodiversity)," records"))
    
    first.row = new.last.row+1
  }
  }
  }
  setwd(datafiles)#switch directory
  
}
setwd(analysispath)
#write table
write.csv(biodiversity, file="VertNet_biodiver_species_goc.csv")


