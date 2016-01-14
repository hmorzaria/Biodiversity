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

setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Vulnerabilidad_Pacifico/")

pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Vulnerabilidad_Pacifico"

#this is the empty shapefile with for the Northern Gulf

base.file.name = "nacional_lam"
#define projection of data
crs.geo <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

all.rg <- readOGR(".", base.file.name)

X11()
plot(all.rg)
poly.area = gArea(all.rg) #Calculate area
print(paste("Area",poly.area, sep=" "))


naamp.10km <- gBuffer(all.rg, width=10000) # remember units is m

writeOGR(naamp.10km, ".", Mexico_lm_buffer_10km,driver="ESRI Shapefile",overwrite_layer=TRUE)






