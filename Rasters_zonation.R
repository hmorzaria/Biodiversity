#Hem Nalini Morzaria Luna
#March 2015
#organize data frome excel spreadsheets
#clean up the space

rm(list=ls())


require(gdata)
require(XLConnect)
require(maptools)   # used here to read in the spatial data
require(PBSmapping) # for GIS-like geoprocessing operations
require(rgdal)  
require(fields)
require(spocc)
require(rgbif)

# USER BLOCK: CHECK AND CHANGE OPTIONS HERE  
#_________________________________________________________________________

datafiles="E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation" #put path

corridor.polygon = "propuesta_corredor_3"
####

setwd(datafiles)


#read in corridor polygon

goc.shape <- readOGR(".", corridor.polygon)


#buffer polygon 9 km

naamp.10km <- gBuffer(corridor.polygon, width=9000) # remember units is m

mask.raster = raster(naamp.10km)

#set background values as 
mask.raster[is.na(mask.raster)] <- 0


