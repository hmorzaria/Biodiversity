                                                                                                            # Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
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

setwd("E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management/Bitacoras_original")

mainpath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management/Bitacoras_original"

pathToSaveShapes = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Spatial management/Bitacoras_revisado"

#this is the empty shapefile with for the Northern Gulf

base.file.name = "Atlantis12_AggregatePolygons"

#List of names for standarization across locations
names_bitacoras = read.csv("nombres_bitacoras.csv", header=TRUE)

#define projection of data
crs.geo <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

location.folders <- list.dirs(full.names = TRUE,recursive=FALSE)

  for (i in 1:length(location.folders)) 
    
    {
    
    this.folder = location.folders[i]
    
    name.location = unlist(strsplit(this.folder,"/"))[2]
    
    path.this.folder = paste(mainpath,name.location,sep="/")
    
    setwd(path.this.folder)
    
    species.folders <- list.dirs(full.names = TRUE, recursive=FALSE)
    
    for( i in 1:length(species.folders))
 { 
  
this.species.folder = species.folders[i]

name.species = unlist(strsplit(this.species.folder,"/"))[2]

path.this.species.folder = paste(mainpath,name.location,name.species,sep="/")

setwd(path.this.species.folder)

files <- list.files(pattern = "\\.shp$")

  for(i in 1:length(files)){
    
    this.shape.file = files[i]
      
      shape.file.name = unlist(strsplit(this.shape.file,"[.]"))[1]
    
    sp.name.string = unlist(strsplit(shape.file.name,"-"))
    
    if(length(sp.name.string) ==3){
      
      sp.name = unlist(strsplit(sp.name.string,"-"))[3]
    }
    
    if(length(sp.name.string) ==4){
      
      sp.name = unlist(strsplit(sp.name.string,"-"))[4]
    }
    
    ogrInfo(".", shape.file.name)
    shape.rg <- readOGR(".", shape.file.name)
    shape.rg$sorting_id <- sapply(slot(shape.rg, "polygons"), function(x) slot(x, "ID"))
    shape.rg$sorting_id = as.numeric(shape.rg$sorting_id)
    
    orig.table <- as(shape.rg, "data.frame")
    new.table = orig.table
    
    #check if CODSPP column is present
    # if so remove it
    my.names = colnames(new.table)
    check.names = grepl("CODSPP",my.names)
    
    result.test = is.element('TRUE', check.names)
    
    if(result.test == TRUE)
    {
      new.table=subset(new.table, select = -CODSPP)
    }
    
    #check if CODIGO column is present
    # if so remove it
    
    check.names = grepl("CODIGO",my.names)
    
    result.test = is.element('TRUE', check.names)
    
    if(result.test == TRUE)
    {
      new.table=subset(new.table, select = -CODIGO)
    }
    
    #check if CODE column is present
    check.names = grepl("CODE",my.names)
    
    result.test = is.element('TRUE', check.names)
    
    ##CONTINUE HERE MAKE SURE ALL FILES HAVE SAME COLUMNS
    if(result.test == FALSE)
    {
      new.table$CODE="NA"
    }
    
    common.name = names_bitacoras$Nombre[names_bitacoras$Nombre_folder==name.species]
    common.name = as.character(drop.levels(common.name))
    
    
    
    new.table$LOC = name.location
    new.table$NOM_ESPECIE= common.name
    new.table$CODIGO = shape.file.name
    new.table$SPPCODE = sp.name  
        
    
    table.col = ncol(new.table)
    cat(paste("Shapefile has",table.col,"columns",sep=" "))
    
    
    if(table.col!=13)
    {
      
      if(table.col==12){
        
        print("*******************THIS FILE HAS TOO FEW FIELDS***************")
        new.table$EXTRA="NA"
      }
      
      if(table.col==14){
        
        print("*******************THIS FILE HAS TOO MANY FIELDS***************")
        break
      }
    }
#     
        new.table.ordered <- new.table[order(new.table$sorting_id), ]
    row.names(new.table.ordered) <- row.names(orig.table)
    shape.new = shape.rg
    shape.new@data = new.table.ordered
    
    
    pathToSaveShapes2 = paste(pathToSaveShapes,common.name,sep="/")
      
      dir.create(paste(pathToSaveShapes,common.name,sep="/"))
    
    setwd(pathToSaveShapes2)
    writeOGR(shape.new, ".", shape.file.name,driver="ESRI Shapefile",overwrite_layer=TRUE)
    
    setwd(path.this.species.folder)    
    
    
    
  }


}

  }


## Now merge all polygon layers
##Based on code by Zia Ahmed zua3 at cornell.edu 
###############################################################################

# Get list of all shape files from directory
#------------------------------------------------------------

setwd(pathToSaveShapes)


species.folders <- list.dirs(full.names = TRUE,recursive=FALSE)

for (eachfolder in 1:length(species.folders)) 
  
{
  
  this.folder = species.folders[eachfolder]
  
  name.sp.folder = unlist(strsplit(this.folder,"/"))[2]
  
  print(name.sp.folder)
paththisSpecies = paste(pathToSaveShapes,name.sp.folder,sep="/")

  setwd(paththisSpecies)

  files = list.files(pattern=".shp$", recursive=FALSE)

  uid=1

# Get polygons from first file 
#-------------------------------------

  first.shape= unlist(strsplit(files[1],"[.]"))[1]
  
poly.data<- readOGR(".",first.shape)

n <- length(slot(poly.data, "polygons"))
poly.data <- spChFIDs(poly.data, as.character(uid:(uid+n-1)))
uid <- uid + n

# mapunit polygon: combin remaining  polygons with first polygon
#-----------------------------------------------------------------
  
  
  if(length(files)>1){
   
    for (eachfile in 2:length(files)) {
    
    this.shape= unlist(strsplit(files[eachfile],"[.]"))[1]
    
    temp.data <- readOGR(".",this.shape)
    n <- length(slot(temp.data, "polygons"))
    temp.data <- spChFIDs(temp.data, as.character(uid:(uid+n-1)))
    uid <- uid + n
    poly.data <- spRbind(poly.data,temp.data)
  }
  }
  

names(poly.data)
proj4string(poly.data) = crs.geo

setwd(pathToSaveShapes)
writeOGR(poly.data, ".", paste(name.sp.folder,"_merge",sep=""),driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  }



#now make merged file into raster
#-----------------------------------------------------------------

setwd(pathToSaveShapes)

#make empty raster to extent of Northern Gulf
shape.rg <- readOGR(".", base.file.name)

e.shape <- extent(shape.rg)
mask.raster = raster(e.shape)
res(mask.raster) = 1000 # The log books used maps of resolution 2700 m
#Marcia used resolution of 1km2 (1000 x 1000 m)
#set background values to 0
mask.raster[is.na(mask.raster)] <- 0
proj4string(mask.raster) = crs.geo

merged.species=list.files(getwd(),  pattern="_merge.shp$", full.names=FALSE)

for(eachmergedfile in 1:length(merged.species))
  
{
  this.merged.species = merged.species[eachmergedfile]
  merged.file.name = unlist(strsplit(this.merged.species,"[.]"))[1]
  ogrInfo(".", merged.file.name)
  merge.rg <- readOGR(".", merged.file.name)
  
   
  value = 1
  
  out.r <- rasterize(merge.rg, field="LOC", mask.raster)
  
  # set the cells associated with the shapfile to the specified value
  out.r[!is.na(out.r)] <- value
  out.r[is.na(out.r)] <- 0
  
  # export to the working directory as a tif file
  
  writeRaster(out.r, filename=merged.file.name, format="GTiff", overwrite=TRUE)  
  
}



# Make a raster stack
#-----------------------------------------------------------------

#Go back to main directory
setwd(pathToSaveShapes)

# list all raster files and make stack

raster.species=stack(list.files(getwd(),  pattern=".tif$", full.names=FALSE))

raster.species[is.na(raster.species)] <- 0
#sum to get fishing intensity

fishing.intensity= sum(raster.species)



writeRaster(fishing.intensity, filename="fishing_intensity", format="GTiff", overwrite=TRUE)  

coast.rg <- readOGR(".", "GOC_Clip_polygon_Project")

X11()
plot(fishing.intensity, main="Frequencia pesquerias en base a bitacoras")
plot(coast.rg, add=TRUE)


#NOW merge all species together
##Based on code by Zia Ahmed zua3 at cornell.edu 
###############################################################################

# Get list of all shape files from directory
#------------------------------------------------------------

setwd(pathToSaveShapes)


files = list.files(pattern="_merge.shp$", recursive=FALSE)
  
  uid=1
  
  # Get polygons from first file 
  #-------------------------------------
  
  first.shape= unlist(strsplit(files[1],"[.]"))[1]
  
  poly.data<- readOGR(".",first.shape)
  
  n <- length(slot(poly.data, "polygons"))
  poly.data <- spChFIDs(poly.data, as.character(uid:(uid+n-1)))
  uid <- uid + n
  
  # mapunit polygon: combin remaining  polygons with first polygon
  #-----------------------------------------------------------------
  
    
    for (eachfile in 2:length(files)) {
      
      this.shape= unlist(strsplit(files[eachfile],"[.]"))[1]
      
      temp.data <- readOGR(".",this.shape)
      n <- length(slot(temp.data, "polygons"))
      temp.data <- spChFIDs(temp.data, as.character(uid:(uid+n-1)))
      uid <- uid + n
      poly.data <- spRbind(poly.data,temp.data)
    }
  
  
  
  names(poly.data)
  proj4string(poly.data) = crs.geo
  
  setwd(pathToSaveShapes)
  writeOGR(poly.data, ".", "All_species_bitacoras",driver="ESRI Shapefile",overwrite_layer=TRUE)
  

#Calculate fishing frequency just for species in the corridor

all.rg <- readOGR(".", "All_species_bitacoras")


corridor.com <- all.rg[all.rg$LOC == "PLO" | all.rg$LOC == "PPE" | all.rg$LOC =="SJO",]

sp.levels = levels(corridor.com$NOM_ESP)

for (eachspecies in 1:length(sp.levels))
{
  this.species = as.character(sp.levels[eachspecies])
  
  #test the species is still present
  
  test.sp = corridor.com$NOM_ESP == this.species
  
  if(TRUE %in% test.sp==TRUE) {
    sp.corridor.com <- corridor.com[corridor.com$NOM_ESP == this.species,]
    
    writeOGR(sp.corridor.com, ".", paste(this.species,"_corridor",sep=""),driver="ESRI Shapefile",overwrite_layer=TRUE)
    
    value = 1
    
    out.r <- rasterize(sp.corridor.com, field="LOC", mask.raster)
    
    # set the cells associated with the shapfile to the specified value
    out.r[!is.na(out.r)] <- value
    out.r[is.na(out.r)] <- 0
    
    # export to the working directory as a tif file
    
    writeRaster(out.r, filename=paste(this.species,"_corridor",sep=""), format="GTiff", overwrite=TRUE)  
    
  }
  
}



# list all raster files and make stack

raster.species=stack(list.files(getwd(),  pattern="corridor.tif$", full.names=FALSE))

raster.species[is.na(raster.species)] <- 0
#sum to get fishing intensity

fishing.intensity.com= sum(raster.species)



writeRaster(fishing.intensity.com, filename="fishing_intensity_corridor", format="GTiff", overwrite=TRUE)  

coast.rg <- readOGR(".", "GOC_Clip_polygon_Project")

X11()
plot(fishing.intensity.com, main="Frequencia pesquerias en base a bitacoras")
plot(coast.rg, add=TRUE)