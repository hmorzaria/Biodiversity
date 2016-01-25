# This is our external R script called biodiversity_analysis_chunks.R
# chunks:
# Hem Nalini Morzaria Luna hmorzarialuna@gmail.com
# August 2015

# setpreferences: library prefs
#getbiodiversity: retrieve and clean up biodiversity files
#organizebiodiversity: Merge all biodiversity files, check taxonomy
#checktaxonomy: check using taxize
#updatetaxonomy: update taxonomy
#recbiodiversity: update biodiversity file
#mapbiodiversity: Reads in the species records and maps them
#richnessmodel: Calculates a richness model using sperich
#corridorpoints: Cut out points in corridor and map
#richnessmodelcorridor: model in corridor


## @knitr setpreferences

if(!require(dismo)){install.packages("dismo"); library(dismo)}
if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(XML)){install.packages("XML"); library(XML)}
if(!require(jsonlite)){install.packages("jsonlite"); library(jsonlite)}
if(!require(graphics)){install.packages("graphics"); library(graphics)}
if(!require(maps)){install.packages("maps"); library(maps)}
if(!require(maptools)){install.packages("maptools"); library(maptools)}
if(!require(rgeos)){install.packages("rgeos"); library(rgeos)}
if(!require(rgdal)){install.packages("rgdal"); library(rgdal)}
if(!require(magrittr)){install.packages("magrittr"); library(magrittr)}
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(Hmisc)){install.packages("Hmisc"); library(Hmisc)}
if(!require(spocc)){install.packages("spocc"); library(spocc)}
if(!require(rgdal)){install.packages("rgdal"); library(rgdal)}
if(!require(ridigbio)){install.packages("ridigbio"); library(ridigbio)}
if(!require(rvertnet)){install.packages("rvertnet"); library(rvertnet)}
if(!require(ecoengine)){install.packages("ecoengine"); library(ecoengine)}
if(!require(rbison)){install.packages("rbison"); library(rbison)}
if(!require(rgbif)){install.packages("rgbif"); library(rgbif)}
if(!require(rebird)){install.packages("rebird"); library(rebird)}
if(!require(readxl)){install.packages("readxl"); library(readxl)}
if(!require(knitr)){install.packages("knitr"); library(knitr)}
if(!require(knitcitations)){install.packages("knitcitations"); library(knitcitations)}
if(!require(taxize)){install.packages("taxize"); library(taxize)}
if(!require(sperich)){install.packages("sperich"); library(sperich)}
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if(!require(ggmap)){install.packages("ggmap"); library(ggmap)}
if(!require(cowplot)){install.packages("cowplot"); library(cowplot)}
if(!require(rmarkdown)){install.packages("rmarkdown"); library(rmarkdown)}
if(!require(xtable)){install.packages("xtable"); library(xtable)}


cleanbib()
options("citation_format" = "pandoc")

# read chunk (does not run code)
read_chunk('biodiversity_analysis_chunks.R')
#bib <- read.bibtex("Bibrefs.bib") #bibliography already set in YAML

## @knitr getbiodiversity


    
## @knitr organizebiodiversity
rm(list=ls())    
savepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis"
    
setwd(savepath)
# read in file with all records for the goc 

biodiv.species = biodiversity.all %>% 
  .$species %>% unique 

#' this table has to be read manually 
#' use it to delete genera with no species and higher taxa
#' eliminate authorities

write.csv(biodiv.species, file="biodiver_species_list.csv")


## @knitr checktaxonomy    
    rm(list=ls())
#' this table is read after we have eliminated records not identified to species
#' auhtorities and combined with previously assessed taxonomy
#' see biodiver_species_list.xlsx

rev.species.list = read.csv("species_list.csv", header=T,na.strings="NA")
    
biodiv.species.names = rev.species.list %>% 
      setnames(c("Species","NewSpecies")) %>% 
      .$NewSpecies %>% 
      unique %>% as.character
    
tryCatch(taxo <- gnr_resolve(names = biodiv.species.names, best_match_only = TRUE, preferred_data_sources = c(3,155,4,9,1,12,167,163,175,173,174,165,163)), error = function(cond)"skip") 
  
  accepted_name = taxo %>% 
    .$results %>% 
    as.data.frame %>% 
    tbl_df
  
  write.csv(accepted_name, file="master_taxonomy_list.csv")
    
## @knitr updatetaxonomy
  rm(list=ls())
#read table again
savepath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
shapepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/SIG_Biodiversity"

setwd(savepath)
  
rev.species.list = fread("species_list.csv", header=T, na.strings="NA")
    
new.biodiv.clean = fread("goc_biodiversity.csv", header=T, na.strings="NA") %>% 
      left_join(rev.species.list, by = "name") #match new names
    
print(dim(new.biodiv.clean))
new.biodiversity = new.biodiv.clean[complete.cases(new.biodiv.clean),]#eliminate rows with NA
print(dim(new.biodiversity))
#remove duplicates based on the combination of latitude, longitude and newspecies
new.biodiversity.goc.clean = new.biodiversity[!duplicated(new.biodiversity[,c('NewSpecies', 'lat', 'long')]),]
    
new.biodiversity.goc.clean2 = new.biodiversity.goc.clean %>% 
    select(NewSpecies,source,lat,lon)
    
print(dim(new.biodiversity.goc.clean2))
    
#write table
write.csv(new.biodiversity.goc.clean2, file="biodiver_species_goc.csv")
    
#' print(paste("old biodiversity has ",nrow(biodiversity)," records"))
print(paste("new biodiversity has ",nrow(new.biodiversity.goc.clean2)," records"))
    
  new.biodiversity.goc = new.biodiversity.goc.clean %>% 
      data.frame
#' set directory path
setwd(shapepath)
#' Save as shapefile
coordinates(new.biodiversity.goc) <- c("long", "lat")  # set spatial coordinates
proj4string(new.biodiversity.goc) <- crs.geo.wgs
writeOGR(new.biodiversity.goc, ".", "biodiversity_goc",driver="ESRI Shapefile",overwrite_layer=TRUE)
    

## @knitr mapbiodiversity
    rm(list=ls())
#read in species occurrance
#this data was obtained and cleaned in Data_biodiversity.R
#files were then organized in Organize_biodiversity.R

savepath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" 
shapepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/SIG_Biodiversity"

setwd(savepath)

biodiversity  = fread("biodiver_species_goc.csv",header=T) %>% 
  dplyr::select(long, lat, NewSpecies) %>% #select species, lat and lon
  data.frame %>% 
setnames(c('long','lat','speciesID')) # and rename

geo.biodiv = biodiversity
coordinates(geo.biodiv) = c("long", "lat")

mapbiodiv <- get_map(location = c(lon = mean(geo.biodiv$long), lat = mean(geo.biodiv$lat)), zoom = 6,
                     source='stamen',maptype = "terrain", scale = 2) 

map.plot = ggmap(mapbiodiv) +
  geom_point(data = biodiversity, aes(x = long, y = lat, alpha = 0.8), colour = 'darkred', fill= 'darkred', size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  labs(x = 'Longitude', y = 'Latitude')+
  theme(axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))


setwd(shapepath)
#read in Gulf of California shapefile
#goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")
goc.shape <- readOGR(".", "Golfo_california_wetland_poly_WGS84")#with wetlands


setwd(savepath)

resolution= 0.08
all.species=-1

dimension = biodiversity %>%  getDimension(resolution)
shift = biodiversity %>%  getShift


#create null masks
landwatermask = createLandwatermask(NULL,dimension,shift,resolution)
heightwatermask = createHeightmask(NULL,dimension,shift,resolution)

#map species ocurrence
noninterpolatedgrid = createNonInterpolatedGrid(biodiversity,dimension,shift,resolution, all.species)

#calculate inverse-distance weighted richness

species.richness.weighted = species.richness(biodiversity,
                                             landwatermask=landwatermask,
                                             distances=1:10,
                                             weight=0.5,
                                             dimension=dimension,
                                             shift=shift,
                                             resolution,
                                             upperbound=3000,
                                             all.species,
                                             silent=FALSE,
                                             do.parallel=FALSE)
#adjust the result for sampling effort
#set clusterlimit to prepare clusterlist
clusterlist=searchClusters(species.richness.weighted,
                           dimension,
                           shift,
                           resolution,
                           clusterlimit = 100)

#adjust inverse-weighted species
#richness grid for sampling effort

species.richness.adjusted = adjustment(species.richness.weighted,
                                       noninterpolatedgrid,
                                       clusterlist)

#cross validate
species.richness.cv = species.richness.cv(biodiversity, 
                                          landwatermask, fold=5, loocv.limit=10, distances=2:5, 
                                          weight=0.5, dimension, shift, resolution, upperbound=5, 
                                          all.species=-1)
#calculate robustness
robust = species.richness.cv/species.richness.adjusted
robust[which(is.na(robust)==TRUE)] <- 0
#export as grid

exportAsGDAL(species.richness.adjusted, shift, resolution, 
             directory=getwd(), filename="species_richness_goc.tif", drivername="GTiff")


exportAsGDAL(robust, shift, resolution, 
             directory=getwd(), filename="robust_goc.tif", drivername="GTiff")

exportAsGDAL(noninterpolatedgrid, shift, resolution, 
             directory=getwd(), filename="noninterpolated_goc.tif", drivername="GTiff")

createImage(grid=species.richness.adjusted,landwatermask,
            image.title = "GOC richness",
            directory=getwd(),
            filename="mapgoc.png",
            shift,
            part=15,
            resolution)

createImage(grid=noninterpolatedgrid,landwatermask,
            image.title = "GOC richness",
            directory=getwd(),
            filename="non_interpolated_mapgoc.png",
            shift,
            part=15,
            resolution)

#reload saved tiff rasters and change colors

non.data = raster("noninterpolated_goc.tif") %>% 
  reclassify(cbind(0, NA)) %>% 
  rasterToPoints %>% 
  data.frame %>% 
  setnames(c('Longitude', 'Latitude', 'Richness'))


sp.data = raster("species_richness_goc.tif") %>% 
  mask(goc.shape) %>% 
  reclassify(cbind(0, NA)) 

sp.data = (sp.data - cellStats(sp.data,"min")) / (cellStats(sp.data,"max")-cellStats(sp.data,"min"))

sp.data2 = sp.data %>% 
  rasterToPoints %>% 
  data.frame %>% 
  setnames(c('Longitude', 'Latitude', 'Richness'))

robust.data = raster("robust_goc.tif") %>% 
  mask(goc.shape) %>% 
  reclassify(cbind(0, NA)) 

robust.data = (robust.data - cellStats(robust.data,"min")) / (cellStats(robust.data,"max")-cellStats(robust.data,"min"))

robust.data2 = robust.data %>% 
  rasterToPoints %>% 
  data.frame %>% 
  setnames(c('Longitude', 'Latitude', 'Robustness'))
#create color palette

pal <- wes_palette(10, name = "FantasticFox", type = "continuous")

#plot richness model and robustness
non.plot = ggplot(data=non.data, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Richness)) +
  theme_bw() +
  coord_equal() + 
  scale_fill_gradientn(colours = pal)+
   theme(axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))

pal <- wes_palette(15, name = "GrandBudapest", type = "continuous")

robust.plot = ggplot(data=robust.data2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Robustness)) +
  theme_bw() +
  coord_equal() + 
  scale_fill_gradientn(colours = pal) +
theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=10),
      legend.title = element_text(size=12))

sp.plot = ggplot(data=sp.data2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Richness)) +
  theme_bw() +
  coord_equal() + 
  scale_fill_gradientn(colours = pal) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))

#export as png
png("goc_richness_results.png")
plot_grid(robust.plot,sp.plot, align='h',labels = c('A','B'),
          label_size = 16,hjust = -1,vjust = 3)
dev.off()

png("goc_richness_non.png")
plot_grid(map.plot,non.plot,align='h',nrow=1,ncol=2, labels = c('A','B'),
          label_size = 16,hjust = -1,vjust = 6,
          rel_widths = c(1, 1.3), scale = 0.95)
dev.off()


## @knitr corridorpoints
#cut point not in corridor

savepath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
shapepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/SIG_Biodiversity"
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(shapepath)
corridor.shape <- readOGR(".", "propuesta_corredor_costero_wetland_WGS84")
corridor.wgs = spTransform(corridor.shape, crs.geo.wgs)

setwd(savepath)
biodiversity  = fread("biodiver_species_goc.csv",header=T) %>% 
  select(long, lat,NewSpecies) %>% #select species, lat and lon
  as.data.frame

biodiv.rows = nrow(biodiversity)

iterations = round(biodiv.rows/1000,0)

last.row = 1001
first.row = 1

biodiversity.corridor = as.data.frame(matrix(0,nrow=0,ncol=4))


for (eachiteration in 1:iterations)
{
  new.last.row = ((last.row-1)*eachiteration)
  
  section.biodiv = biodiversity[first.row:new.last.row,] %>% 
    na.omit
  
  coordinates(section.biodiv) <- c("long", "lat") # set spatial coordinates
  proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
  print(summary(section.biodiv)) # print summary
  # subset ocurrence points within GOC
  biodiversity.goc = section.biodiv[corridor.wgs, ] %>% 
    as("data.frame")
  
  biodiversity.corridor = rbind(biodiversity.corridor,biodiversity.goc)
  
  print(paste("biodiversity has ",nrow(biodiversity.corridor)," records"))
  
  first.row = new.last.row+1
}


setwd(savepath)
#write table
write.csv(biodiversity.corridor, file="biodiver_species_corridor.csv")

biodiversity.shape = biodiversity.corridor
#set directory path
setwd(shapepath)
#Save as shapefile
coordinates(biodiversity.shape) <- c("long", "lat")  # set spatial coordinates
proj4string(biodiversity.shape) <- crs.geo.wgs
plot(biodiversity.shape)
writeOGR(biodiversity.shape, ".", "biodiversity_corridor",driver="ESRI Shapefile",overwrite_layer=TRUE)

mapbiodiv <- get_map(location = c(lon = mean(biodiversity.corridor$long), lat = mean(biodiversity.corridor$lat)), zoom = 8,
                                         source="stamen",maptype = "terrain", scale = 2) 

corridor = fortify(corridor.wgs)

ggmap(mapbiodiv, legend='none') +
  geom_polygon(aes(x=long, y=lat, group=group), fill='dodgerblue4', size=0.75, color='dodgerblue4', data=corridor, alpha=0) +
  geom_point(data = biodiversity.corridor, aes(x = long, y = lat, alpha = 0.8), colour = 'darkred', fill= 'darkred', size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  labs(x = 'Longitude', y = 'Latitude') +
  scale_alpha(guide='none')

setwd(savepath)
ggsave ("map_goc_points_corridor.png", dpi = 300)

## @knitr richnessmodelcorridor

shapepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/SIG_Biodiversity"
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(shapepath)
corridor.shape <- readOGR(".", "propuesta_corredor_costero_wetland_WGS84")
corridor.wgs = spTransform(corridor.shape, crs.geo.wgs)

savepath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
setwd(savepath)

biodiversity  = fread("biodiver_species_corridor.csv",header=T) %>% 
  select(long, lat, NewSpecies) %>% #select species, lat and lon
  as.data.frame

names(biodiversity) = c('long','lat','speciesID') # and rename

resolution= 0.014
all.species=-1

dimension = biodiversity %>%  getDimension(resolution)
shift = biodiversity %>%  getShift


#create null masks
landwatermask = createLandwatermask(NULL,dimension,shift,resolution)
heightwatermask = createHeightmask(NULL,dimension,shift,resolution)

#map species ocurrence
noninterpolatedgrid = createNonInterpolatedGrid(biodiversity,dimension,shift,resolution, all.species)

#calculate inverse-distance weighted richness

species.richness.weighted = species.richness(biodiversity,
                                             landwatermask=landwatermask,
                                             distances=1:10,
                                             weight=0.5,
                                             dimension=dimension,
                                             shift=shift,
                                             resolution,
                                             upperbound=3000,
                                             all.species,
                                             silent=FALSE,
                                             do.parallel=FALSE)
#adjust the result for sampling effort
#set clusterlimit to prepare clusterlist
clusterlist=searchClusters(species.richness.weighted,
                           dimension,
                           shift,
                           resolution,
                           clusterlimit = 100)

#adjust inverse-weighted species
#richness grid for sampling effort

species.richness.adjusted = adjustment(species.richness.weighted,
                                       noninterpolatedgrid,
                                       clusterlist)

#cross validate
species.richness.cv = species.richness.cv(biodiversity, 
                                          landwatermask, fold=5, loocv.limit=10, distances=2:5, 
                                          weight=0.5, dimension, shift, resolution, upperbound=5, 
                                          all.species=-1)
#calculate robustness
robust = species.richness.cv/species.richness.adjusted
robust[which(is.na(robust)==TRUE)] <- 0
#export as grid

exportAsGDAL(species.richness.adjusted, shift, resolution, 
             directory=getwd(), filename="species_richness_corridor.tif", drivername="GTiff")


exportAsGDAL(robust, shift, resolution, 
             directory=getwd(), filename="robust_corridor.tif", drivername="GTiff")


createImage(grid=species.richness.adjusted,landwatermask,
            image.title = "GOC richness",
            directory=getwd(),
            filename="mapgoc_corridor_adjusted.png",
            shift,
            part=15,
            resolution)

createImage(grid=noninterpolatedgrid,landwatermask,
            image.title = "GOC richness",
            directory=getwd(),
            filename="non_interpolated_mapgoc_corridor.png",
            shift,
            part=15,
            resolution)

## @knitr conflictindex

shapepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/SIG_Biodiversity"
#specify coordinate system
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

#get shapefile for corridor
setwd(shapepath)
#' this data set includes wetlands
coast.wgs = readOGR(".", "propuesta_corredor_costero_wetland_WGS84") %>% 
  spTransform(crs.geo.wgs)# transform to WGS

#' no wetlands
#' coast.wgs = readOGR(".", "propuesta_corredor_costero") %>% 
#' spTransform(crs.geo.wgs)# transform to WGS

savepath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
setwd(savepath)

#get richness model for corridor
sp.data = raster("species_richness_corridor.tif") %>% 
    reclassify(cbind(0, NA)) %>% 
  mask(coast.wgs)

#get robustness data
robust.data = raster("robust_corridor.tif") %>% 
  mask(coast.wgs) 

#read fisheries importance model

fishing.data = raster("ie_index_pangas.tif") %>% 
  projectRaster(crs=crs.geo.wgs) %>% 
  reclassify(cbind(0, NA)) %>% 
  mask(coast.wgs)
  

# create an empty output raster that spans the extent input
# rasters, and uses the same coordinate reference system

bounding.raster = sp.data %>% 
  extent %>% 
  raster(crs=crs.geo.wgs)

res(bounding.raster) <- res(sp.data)

#align grids for both raster sets by resampling

target.raster = crop(bounding.raster, fishing.data)
resampled.fish = raster::resample(fishing.data, target.raster, method="bilinear")
resampled.fish = resampled.fish %>% 
  mask(coast.wgs)
norm.fish = (resampled.fish - cellStats(resampled.fish,"min")) / (cellStats(resampled.fish,"max")-cellStats(resampled.fish,"min"))


target.raster = crop(bounding.raster, sp.data)
resampled.sp = raster::resample(sp.data, target.raster, method="bilinear")
resampled.sp.sp = resampled.sp %>% 
  mask(coast.wgs)
norm.sp = (resampled.sp - cellStats(resampled.sp,"min")) / (cellStats(resampled.sp,"max")-cellStats(resampled.sp,"min"))

rs1 <- norm.sp - norm.fish  
rs2 = (rs1 - cellStats(rs1,"min")) / (cellStats(rs1,"max")-cellStats(rs1,"min"))

#convert overlay index and importance to point set for ggplot
#normalize
sp.data = (sp.data - cellStats(sp.data,"min")) / (cellStats(sp.data,"max")-cellStats(sp.data,"min"))

sp.data2 =  sp.data %>% 
  mask(coast.wgs) %>% 
  rasterToPoints %>% 
  data.frame %>% 
  setnames(c('Longitude', 'Latitude', 'Index'))

rs2 =  rs2 %>% 
  mask(coast.wgs) %>% 
  rasterToPoints %>% 
  data.frame %>% 
  setnames(c('Longitude', 'Latitude', 'Index'))

norm.fish2 =  norm.fish %>% 
  rasterToPoints %>% 
  data.frame %>% 
  setnames(c('Longitude', 'Latitude', 'Index'))

#normalize 

robust.data2 = robust.data %>% 
  rasterToPoints %>% 
  data.frame %>% 
  setnames(c('Longitude', 'Latitude', 'Robustness'))

#polygon to dataframe
coast.pol = fortify(coast.wgs)


#create color palette
pal <- wes_palette(10, name = "GrandBudapest", type = "continuous")

#plot data
overlap.plot = ggplot(data=rs2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Index)) +
  theme_bw() +
  coord_equal() + 
  scale_fill_gradientn(colours = pal,name = "Overlap") +
  geom_polygon(data = coast.pol, aes(x = long, y = lat, group = group),
               fill = NA, color = "gray20", size = 0.25) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))

pal <- wes_palette(10, name = "FantasticFox", type = "continuous")

fish.plot = ggplot(data=norm.fish2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Index)) +
  theme_bw() +
  coord_equal() + 
  scale_fill_gradientn(colours = pal,name = "Economic\nvalue") +
  geom_polygon(data = coast.pol, aes(x = long, y = lat, group = group),
               fill = NA, color = "gray20", size = 0.25) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))

richness.plot = ggplot(data=sp.data2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Index)) +
  theme_bw() +
  coord_equal() + 
  scale_fill_gradientn(colours = pal,name = "Richness") +
  geom_polygon(data = coast.pol, aes(x = long, y = lat, group = group),
               fill = NA, color = "gray20", size = 0.25) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))

robust.plot = ggplot(data=robust.data2, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Robustness)) +
  theme_bw() +
  coord_equal() + 
  scale_fill_gradientn(colours = pal,name = "Robustness") +
  geom_polygon(data = coast.pol, aes(x = long, y = lat, group = group),
               fill = NA, color = "gray20", size = 0.25) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))

#export as png
png("overlap_corridor.png")
plot_grid(robust.plot,richness.plot,fish.plot,overlap.plot, align='h',
          labels = c('A','B','C','D'),
          label_size = 16,hjust = -1,vjust = 1,
        rel_widths = c(1.2,1.2,1.2,1.2), rel_heights = c(1.2,1.2,1.2,1.2),
        scale = 1)
dev.off()


