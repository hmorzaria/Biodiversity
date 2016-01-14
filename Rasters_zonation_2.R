#Hem Nalini Morzaria Luna
#July 2015
#take in rasters for Zonation, cut to corridor extent and add border


require(gdata)
require(XLConnect)
require(maptools)   # used here to read in the spatial data
require(PBSmapping) # for GIS-like geoprocessing operations
require(rgdal)  
require(fields)
library(rgeos)
library(rgdal)
require(rgbif)
library(raster)
library(rasterVis)
require(dplyr)

rm(list=ls())

# USER BLOCK: CHECK AND CHANGE OPTIONS HERE  
#_________________________________________________________________________

datafiles="E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation" #put path

rasterfiles="E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation/Rasters_biodiv" #put path

corridor.polygon = "propuesta_corredor_costero"
####

setwd(datafiles)


#read in corridor polygon

goc.shape <- readOGR(".", corridor.polygon)
goc.shape$sorting_id <- sapply(slot(goc.shape, "polygons"), function(x) slot(x, "ID"))
goc.shape$sorting_id = as.numeric(goc.shape$sorting_id)

orig.table <- as(goc.shape, "data.frame")
new.table = orig.table
new.table$LOC = "corridor"
new.table.ordered <- new.table[order(new.table$sorting_id), ]
row.names(new.table.ordered) <- row.names(orig.table)
shape.new = goc.shape
shape.new@data = new.table.ordered
ob <- SpatialPolygons(shape.new@polygons,proj4string=shape.new@proj4string)

#buffer corridor
naamp.10km <- gBuffer(goc.shape, width=9000) # remember units is m
X11() # open a window
plot(naamp.10km) # plot polygon


#check projection
crs.geo.lcc = proj4string(goc.shape)


#make empty raster to extent of buffer 
e.shape <- extent(naamp.10km)
mask.raster = raster(e.shape)
res(mask.raster) = 1000 # The log books used maps of resolution 2700 m
#Marcia used resolution of 1km2 (1000 x 1000 m)
#set background values to 0
mask.raster[is.na(mask.raster)] <- 0
proj4string(mask.raster) = crs.geo.lcc

value = -9999

#rasterize buffer to extent of buffer
buffer.r <- rasterize(naamp.10km,mask.raster)

buffer.r[!is.na(buffer.r)] <- value
buffer.r[is.na(buffer.r)] <- 0
plot(buffer.r)

#go to raster file directory
setwd(rasterfiles)


# list all raster files 

raster.list = list.files(getwd(),  pattern=".tif$", full.names=FALSE)

for(eachraster in 1:length(raster.list))
  {
  this.raster.name = raster.list[eachraster] # subset one raster
  raster.name= unlist(strsplit(this.raster.name,"[.]"))[1] # extract name without extension
    this.raster = raster(this.raster.name) # read raster
  #Crop raster data by extent of corridor
  cropped.raster =  crop(this.raster, extent(goc.shape))
  #merge corridor and buffer
  merged.raster = merge(cropped.raster, buffer.r, overlap=FALSE)
  
  writeRaster(merged.raster, filename=paste(raster.name,"_corridor"), format="GTiff", overwrite=TRUE)  
  
  }





raster.species=stack(out.r,buffer.r)
#FIX DIFFERENT EXTENT. USE THE EXTENT OF THE BUFFER WHICH IS LARGER

raster.species[is.na(raster.species)] <- 0



#make empty raster to extent of Corridor 
e.shape <- extent(goc.shape)
mask.raster = raster(e.shape)
res(mask.raster) = 1000 # The log books used maps of resolution 2700 m
#Marcia used resolution of 1km2 (1000 x 1000 m)
#set background values to 0
mask.raster[is.na(mask.raster)] <- 0
proj4string(mask.raster) = crs.geo.lcc

value = 1

#rasterize corridor
out.r <- rasterize(ob,mask.raster)

out.r[!is.na(out.r)] <- value
out.r[is.na(out.r)] <- 0
plot(out.r)

