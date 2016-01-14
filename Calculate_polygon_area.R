# Hem Nalini Mozaria Luna                                                                                                            # Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
# July 30, 2014
# Extract biomass values for specific cells

library(gdata)
library(ggplot2)
library(reshape)
library(plyr)
library(RColorBrewer)
library(classInt)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
library(rasterVis)

rm(list=ls())

graphics.off()

setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management/")

pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management"

#this is the empty shapefile with for the Northern Gulf

base.file.name = "Zona_exclusion_Vaquita_lm"
#define projection of data
crs.geo <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

all.rg <- readOGR(".", base.file.name)

X11()
plot(all.rg)
poly.area = gArea(all.rg)

#This function can be used to obtain the area of multiple polygons
# sapply(data@polygons, function(x) x@Polygons[[1]]@area)

print(paste("Area",poly.area, sep=" "))



