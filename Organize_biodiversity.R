#Hem Nalini Morzaria Luna
#July 2015
#Take data organized and obtained from repositories (Data_biodiversity.R)
#Create one common file, remove duplicates and fix taxonomy

#clean up the space

rm(list=ls())

#use these lines if packages are not installed
#install.packages(c("gdata","rgdal","dplyr"))
#install.packages("taxize")
require(gdata)
require(rgdal)  
require(dplyr)
library("taxize")
library(data.table) 

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
# #read in Northern Gulf of California shapefile
# goc.shape <- readOGR(".", "Atlantis12_AggregatePolygons")
# 
# 
# projections
#Lambert
crs.geo.lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat")
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

########################User can ignore everything below
corridor.shape <- readOGR(".", "propuesta_corredor_costero")
corridor.wgs = spTransform(corridor.shape, crs.geo.wgs)


setwd(filepath)

biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))


# get list of files

csv.files <- list.files(pattern = "\\.csv$")#list csv files

for(each.file in 1:length(csv.files))
{
  this.file = csv.files[each.file]#file name
  
  biodiversity.goc  = fread(this.file,header=T, fill=TRUE) %>% 
    dplyr::select(starts_with('name'),contains('long'),contains('lati'),source_data)
  
  names(spocc.data.source) = c("Species","Long","Lat","Source")
  
  #read each file
  biodiversity.goc2 = biodiversity.goc[,c("Species","Long","Lat","Source")]
  biodiversity = rbind(biodiversity,biodiversity.goc2) #bind it to a empty frame
  biodiversity = biodiversity[!duplicated(biodiversity[,c("Species","Long","Lat")]),] #eliminate duplicates
  
  print(paste("biodiversity has ",nrow(biodiversity)," records")) #print records
}

#write species table to helpclean up misspellings
biodiv.species = levels(biodiversity$Especie)
biodiv.species.clean = biodiv.species[!duplicated(biodiv.species)]#eliminate duplicates
biodiv.species.clean= cbind(biodiv.species.clean,biodiv.species.clean)
names(biodiv.species.clean) = c("Especie","NewSpecies")

#Output species table
setwd(analysispath)
#this table has to be read manually to fix the species misspellings
write.csv(biodiv.species.clean, file="biodiver_species_list.csv")


UNCOMMENT this section if needed to check taxonomy again

#we have eliminated records not identified to species
#see biodiver_species_list.xlsx
rev.species.list = read.csv("species_list.csv", header=T,na.strings="NA")

names(rev.species.list) = c("Especie","NewSpecies")

species.list = levels(rev.species.list$NewSpecies) 


species.taxonomy = as.data.frame(matrix(0,nrow=0,ncol=4))

for(each.species in 1:length(species.list))
{
  this.species= species.list[each.species]
  print(paste("Analyzing species ",each.species,"_", this.species,"of ",length(species.list)))
  tryCatch(taxo <- gnr_resolve(names = this.species), error = function(cond)"skip") 
  
  sub_name = as.data.frame(taxo$results)
  current.name= sub_name$matched_name[1]
  indx.sp= grep(this.species,current.name)
  
  test.sp.name = !length(indx.sp)
  
  if ( !length(indx.sp)==TRUE )
  {
  
    this.row = sub_name[1,]
    species.taxonomy =  rbind(species.taxonomy,this.row)
}
}

#this table only saves taxonomic ambiguities that can the be checked manually
#fix species_list accordingly
names(species.taxonomy)= c("Especie","Sp_aceptada","data_source","score")
write.csv(species.taxonomy, file="master_taxonomy_list.csv")

#read table again
rev.species.list = read.csv("species_list.csv", header=T,na.strings="NA")


#use dplyr to match using the new names
new.biodiv.clean = biodiversity %>% left_join(rev.species.list, by = "Especie")
print(dim(new.biodiv.clean))
new.biodiversity = new.biodiv.clean[complete.cases(new.biodiv.clean),]#eliminate rows with NA
print(dim(new.biodiversity))
#remove duplicates based on the combination of latitude, longitude and newspecies
new.biodiversity.goc.clean = new.biodiversity[!duplicated(new.biodiversity[,c('NewSpecies', 'Latitud', 'Longitud')]),]
new.biodiversity.goc.clean = new.biodiversity.goc.clean[,c('NewSpecies','Fuente','Longitud','Latitud')]
print(dim(new.biodiversity.goc.clean))


setwd(analysispath)
#write table
write.csv(new.biodiversity.goc.clean, file="biodiver_species_goc.csv")

print(paste("old biodiversity has ",nrow(biodiversity)," records"))
print(paste("new biodiversity has ",nrow(new.biodiversity.goc.clean)," records"))

new.biodiversity.goc= new.biodiversity.goc.clean
#set directory path
setwd(savepath)
#Save as shapefile
coordinates(new.biodiversity.goc) <- c("Longitud", "Latitud")  # set spatial coordinates
proj4string(new.biodiversity.goc) <- crs.geo.wgs
writeOGR(new.biodiversity.goc, ".", "biodiversity_goc",driver="ESRI Shapefile",overwrite_layer=TRUE)


#cut point not in corridor



biodiv.rows = nrow(new.biodiversity.goc.clean)

iterations = round(biodiv.rows/1000,0)

last.row = 1001
first.row = 1

biodiversity.corridor = as.data.frame(matrix(0,nrow=0,ncol=4))


for (each.iteration in 1:iterations)
  
{
  new.last.row = ((last.row-1)*each.iteration)
  
  section.biodiv = new.biodiversity.goc.clean[first.row:new.last.row,]
  section.biodiv = na.omit(section.biodiv)
  coordinates(section.biodiv) <- c("Longitud", "Latitud")  # set spatial coordinates
  proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
  print(summary(section.biodiv)) # print summary
  # subset ocurrence points within GOC
  stations_subset <- section.biodiv[corridor.wgs, ]
  #get table from shapefile
  biodiversity.goc <- as(stations_subset, "data.frame")
  
  biodiversity.corridor = rbind(biodiversity.corridor,biodiversity.goc)
  
  print(paste("biodiversity has ",nrow(biodiversity.corridor)," records"))
  
  first.row = new.last.row+1
}

setwd(analysispath)
#write table
write.csv(biodiversity.corridor, file="biodiver_species_corridor.csv")

#set directory path
setwd(savepath)
#Save as shapefile
coordinates(biodiversity.corridor) <- c("Longitud", "Latitud")  # set spatial coordinates
proj4string(biodiversity.corridor) <- crs.geo.wgs
plot(biodiversity.corridor)
writeOGR(biodiversity.corridor, ".", "biodiversity_corridor",driver="ESRI Shapefile",overwrite_layer=TRUE)
