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

x = c("Hmisc","readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
      "PBSmapping", "rgdal", "fields","data.table","rgbif","raster", "rasterVis",
      "sp","sperich","spocc","dplyr","SDMTools","ggplot2","ggmap", "ecoengine", 
      "rvertnet", "httr","wesanderson","tidyr","cowplot","rbison","rebird","taxize")
lapply(x, require, character.only = TRUE)
cleanbib()
options("citation_format" = "pandoc")

# read chunk (does not run code)
read_chunk('biodiversity_analysis_chunks.R')
#bib <- read.bibtex("Bibrefs.bib") #bibliography already set in YAML

## @knitr getbiodiversity

datafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Datos/Ocurrencia_especies"
vertnetfiles="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Datos/Vert_net_datos" #put path
ulloafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Datos/Ulloa_datos" #put path
analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path

datapath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/richness" #put path
datapath2="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/Raw_data" #put path
shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Biodiversity"

setwd(shapepath)
#read in Gulf of California shapefile
#goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")#without wetlands
goc.shape <- readOGR(".", "Golfo_california_wetland_poly_WGS84")#with wetlands

# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84


#first extract data using spocc from bison

#set bounding polygons
polygons=c('POLYGON((-115.16 31.49, -113.17 31.49, -113.17 31.97, -115.16 31.97, -115.16 31.49))',
           'POLYGON((-115.16 31.49, -113.17 31.49, -113.17 31.15, -115.16 31.15, -115.16 31.49))',
           'POLYGON((-115.16 31.34, -112.95 31.34, -112.95 31.0, -115.16 31.0, -115.16 31.34))',
           'POLYGON((-115.16 31.19, -112.86 31.19, -112.86 30.81, -115.16 30.81, -115.16 31.19))',
           'POLYGON((-115.12 31.0, -112.82 31.0, -112.82 30.62, -115.12 30.62, -115.12 31.0))',
           'POLYGON((-115.12 30.85, -112.82 30.85, -112.82 30.47, -115.12 30.47, -115.12 30.85))',
           'POLYGON((-115.1 30.75, -112.8 30.75, -112.8 30.37, -115.1 30.37, -115.1 30.75))',
           'POLYGON((-115.07 30.66, -112.77 30.66, -112.77 30.28, -115.07 30.28, -115.07 30.66))',
           'POLYGON((-114.96 30.6, -112.66 30.6, -112.66 30.22, -114.96 30.22, -114.96 30.6))',
           'POLYGON((-114.94 30.47, -112.64 30.47, -112.64 30.09, -114.94 30.09, -114.94 30.47))',
           'POLYGON((-114.94 30.35, -112.64 30.35, -112.64 29.98, -114.94 29.98, -114.94 30.35))',
           'POLYGON((-114.7229 30.2337, -112.5989 30.2337, -112.5989 30.0096, -114.7229 30.0096, -114.7229 30.2337))',
           'POLYGON((-114.635 29.9956, -112.511 29.9956, -112.511 29.8102, -114.635 29.8102, -114.635 29.9956))',
           'POLYGON((-114.5252 29.8717, -112.4011 29.8717, -112.4011 29.6863, -114.5252 29.6863, -114.5252 29.8717))',
           'POLYGON((-114.4593 29.7858, -112.3352 29.7858, -112.3352 29.6005, -114.4593 29.6005, -114.4593 29.7858))',
           'POLYGON((-114.4043 29.7094, -112.2803 29.7094, -112.2803 29.5241, -114.4043 29.5241, -114.4043 29.7094))',
           'POLYGON((-114.2395 29.6426, -112.2803 29.6426, -112.2803 29.4572, -114.2395 29.4572, -114.2395 29.6426))',
           'POLYGON((-114.1736 29.5565, -112.2143 29.5565, -112.2143 29.3712, -114.1736 29.3712, -114.1736 29.5565))',
           'POLYGON((-114.1297 29.4991, -112.1704 29.4991, -112.1704 29.3138, -114.1297 29.3138, -114.1297 29.4991))',
           'POLYGON((-114.1297 29.4991, -112.1704 29.4991, -112.1704 29.1892, -114.1297 29.1892, -114.1297 29.4991))',
           'POLYGON((-113.8001 28.866, -111.9946 28.866, -111.9946 29.1892, -113.8001 29.1892, -113.8001 28.866))',
           'POLYGON((-113.5804 28.866, -111.4673 28.866, -111.4673 28.419, -113.5804 28.419, -113.5804 28.866))',
           'POLYGON((-112.9981 27.948, -110.8411 27.948, -110.8411 28.419, -112.9981 28.419, -112.9981 27.948))',
           'POLYGON((-112.9981 27.948, -110.3467 27.948, -110.3467 27.3508, -112.9981 27.3508, -112.9981 27.948))',
           'POLYGON((-112.4048 26.7576, -109.6875 26.7576, -109.6875 27.3508, -112.4048 27.3508, -112.4048 26.7576))',
           'POLYGON((-112.0423 26.7576, -108.8415 26.7576, -108.8415 26.1737, -112.0423 26.1737, -112.0423 26.7576))',
           'POLYGON((-111.6028 25.5347, -108.6877 25.5347, -108.6877 26.1737, -111.6028 26.1737, -111.6028 25.5347))',
           'POLYGON((-111.2293 25.5347, -107.7319 25.5347, -107.7319 24.6256, -111.2293 24.6256, -111.2293 25.5347))',
           'POLYGON((-110.9656 24.4394, -106.5344 24.4394, -106.5344 24.6256, -110.9656 24.6256, -110.9656 24.4394))',
           'POLYGON((-110.9656 24.3694, -106.5344 24.3694, -106.5344 24.5556, -110.9656 24.5556, -110.9656 24.3694))',
           'POLYGON((-110.9546 24.2893, -107.0288 24.2893, -107.0288 24.5056, -110.9546 24.5056, -110.9546 24.2893))',
           'POLYGON((-110.9546 24.2293, -107.0288 24.2293, -107.0288 24.4456, -110.9546 24.4456, -110.9546 24.2293))',
           'POLYGON((-110.9436 24.1692, -107.0178 24.1692, -107.0178 24.3855, -110.9436 24.3855, -110.9436 24.1692))',
           'POLYGON((-110.9436 24.1191, -107.0178 24.1191, -107.0178 24.3354, -110.9436 24.3354, -110.9436 24.1191))',
           'POLYGON((-110.7569 24.049, -106.831 24.049, -106.831 24.2653, -110.7569 24.2653, -110.7569 24.049))',
           'POLYGON((-110.2515 23.9888, -106.7761 23.9888, -106.7761 24.175, -110.2515 24.175, -110.2515 23.9888))',
           'POLYGON((-110.2185 23.9287, -106.7432 23.9287, -106.7432 24.1148, -110.2185 24.1148, -110.2185 23.9287))',
           'POLYGON((-110.2185 23.8382, -106.7432 23.8382, -106.7432 24.0446, -110.2185 24.0446, -110.2185 23.8382))',
           'POLYGON((-110.1197 23.7177, -106.6443 23.7177, -106.6443 23.9241, -110.1197 23.9241, -110.1197 23.7177))',
           'POLYGON((-110.1087 23.6674, -106.6333 23.6674, -106.6333 23.8738, -110.1087 23.8738, -110.1087 23.6674))',
           'POLYGON((-110.0977 23.5366, -106.5015 23.5366, -106.5015 23.7632, -110.0977 23.7632, -110.0977 23.5366))',
           'POLYGON((-109.7791 23.4158, -106.4136 23.4158, -106.4136 23.6524, -109.7791 23.6524, -109.7791 23.4158))',
           'POLYGON((-109.7791 23.315, -106.4136 23.315, -106.4136 23.5516, -109.7791 23.5516, -109.7791 23.315))',
           'POLYGON((-109.7351 23.1738, -106.2158 23.1738, -106.2158 23.4609, -109.7351 23.4609, -109.7351 23.1738))',
           'POLYGON((-110.0318 23.1233, -106.062 23.1233, -106.062 22.8953, -110.0318 22.8953, -110.0318 23.1233))',
           'POLYGON((-109.9659 22.9209, -105.4687 22.9209, -105.4687 22.6828, -109.9659 22.6828, -109.9659 22.9209))',
           'POLYGON((-109.9659 22.7792, -105.227 22.7792, -105.227 22.2157, -109.9659 22.2157, -109.9659 22.7792))',
           'POLYGON((-109.9659 21.7829, -105.1611 21.7829, -105.1611 22.2259, -109.9659 22.2259, -109.9659 21.7829))',
           'POLYGON((-109.9659 21.7829, -105.0403 21.7829, -105.0403 21.2871, -109.9659 21.2871, -109.9659 21.7829))',
           'POLYGON((-109.9659 20.8002, -105.0403 20.8002, -105.0403 21.2871, -109.9659 21.2871, -109.9659 20.8002))',
           'POLYGON((-109.9659 20.8002, -105.0403 20.8002, -105.0403 20.2703, -109.9659 20.2703, -109.9659 20.8002))')
           
this.source = 'bison'
biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

  for(eachpolygon in 1:length(polygons)){
    
    this.polygon = polygons[eachpolygon]#subset bounding box
    spocc.data = bison(aoi=this.polygon,count=10000)
    
    spocc.data.source = spocc.data$points %>% 
      data.frame %>% 
      tbl_df 
    
    if(nrow(spocc.data.source)!=0) {
      
     spocc.data.source = spocc.data$points %>%  
      tbl_df 
    
     
    spocc.data.source$source_data = this.source
    
    setwd(datapath2)
    #write table
    write.csv(spocc.data.source, file=paste(this.source,"_",eachpolygon,"_biodiver_full.csv",sep=""))
    
    
    spocc.data.source2 = spocc.data.source %>% 
      dplyr::select(name,decimalLongitude,decimalLatitude,source_data)
    
    
    spocc.data.source3 = spocc.data.source2[complete.cases(spocc.data.source2),]  #eliminate rows with NA
    # only needed columns
    
    setnames(spocc.data.source3, c("name","long","lat","source_data"))
    
    
    biodiversity.clean = spocc.data.source3[!duplicated(spocc.data.source3[,c('name', 'long', 'lat')]),]
    
    #section the file so it can be subset for the Gulf of California
    
    biodiv.rows = nrow(biodiversity.clean)
    
    iterations = round(biodiv.rows/1000,0)
    
    if(biodiv.rows>1000)
    {
      last.row = 1001
      first.row = 1
      
      for (each.iteration in 1:iterations)
        
      {
        new.last.row = ((last.row-1)*each.iteration)
        
        section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
          na.omit %>% 
          mutate_each(funs(as.numeric),lat,long) %>% 
          as.data.frame
        
        coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
        proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
        print(summary(section.biodiv)) # print summary
        # subset ocurrence points within GOC
        stations_subset <- section.biodiv[goc.shape, ]
        #get table from shapefile
        biodiversity.goc <- as(stations_subset, "data.frame")
        
        test.bio = nrow(biodiversity.goc)==0
        if (test.bio==FALSE)
        {
          biodiversity = rbind(biodiversity,biodiversity.goc)
          biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
          print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
          
        }
        
        first.row = new.last.row+1
      }
    }
    
    if(biodiv.rows<1000)
    {
      last.row = biodiv.rows
      first.row = 1
      section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
        na.omit %>% 
        mutate_each(funs(as.numeric),lat,long) %>% 
        as.data.frame
      
      coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
      proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
      print(summary(section.biodiv)) # print summary
      # subset ocurrence points within GOC
      stations_subset <- section.biodiv[goc.shape, ]
      #get table from shapefile
      biodiversity.goc <- as(stations_subset, "data.frame")
      
      test.bio = nrow(biodiversity.goc)==0
      if (test.bio==FALSE)
      {
        biodiversity = rbind(biodiversity,biodiversity.goc)
        biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
        print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
        
      }
    }
    }
    
  } #end polygon
  


setwd(datapath)
write.csv(biodiversity, file="bison_biodiver_full.csv")

#extract data gbif using their API

#uses same polygons as spocc
biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

for(eachpolygon in 1:length(polygons)){
  
  this.polygon = polygons[eachpolygon]#subset bounding box
  #call GBIF API, max records are 200000
  gbif.goc = occ_search(geometry=this.polygon,return='data',fields=c("name","decimalLatitude","decimalLongitude","datasetName","collectionCode"),limit=200000, callopts=verbose()) %>% 
    tbl_df()
  gbif.goc$source_data = "gbif"
  #add source
  this.source = "gbif"
  
  setwd(datapath2)
  write.csv(gbif.goc, file=paste(this.source,"_",eachpolygon,"_biodiver_full.csv",sep=""))
  
    
  gbif.goc = gbif.goc %>% 
    select(name, decimalLongitude, decimalLatitude,source_data) %>% 
    setnames(c("name","long","lat","source_data"))
  
  gbif.goc2 =  gbif.goc[complete.cases(gbif.goc),]#eliminate rows with NA
  
  
  #remove duplicates based on the combination of latitude, longitude and species
  
  biodiversity.clean = gbif.goc2[!duplicated(gbif.goc2[,c('name', 'lat', 'long')]),]
  
  #section the file so it can be subset for the Gulf of California
  
  biodiv.rows = nrow(biodiversity.clean)
  
  iterations = round(biodiv.rows/1000,0)
  
  if(biodiv.rows>1000)
  {
    last.row = 1001
    first.row = 1
    
    for (each.iteration in 1:iterations)
      
    {
      new.last.row = ((last.row-1)*each.iteration)
      
      section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
        na.omit %>% 
        mutate_each(funs(as.numeric),lat,long) %>% 
        as.data.frame
      
      coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
      proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
      print(summary(section.biodiv)) # print summary
      # subset ocurrence points within GOC
      stations_subset <- section.biodiv[goc.shape, ]
      #get table from shapefile
      biodiversity.goc <- as(stations_subset, "data.frame")
      
      test.bio = nrow(biodiversity.goc)==0
      if (test.bio==FALSE)
      {
        biodiversity = rbind(biodiversity,biodiversity.goc)
        biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
        print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
        
      }
      
      first.row = new.last.row+1
    }
  }
  
  if(biodiv.rows<1000)
  {
    last.row = biodiv.rows
    first.row = 1
    section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
      na.omit %>% 
      mutate_each(funs(as.numeric),lat,long) %>% 
      as.data.frame
    
    coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    test.bio = nrow(biodiversity.goc)==0
    if (test.bio==FALSE)
    {
      biodiversity = rbind(biodiversity,biodiversity.goc)
      biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
      print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
      
    }
  }
  
  
} #end file

setwd(datapath)
#write table
write.csv(biodiversity, file="gbif_biodiver_full.csv")


biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

#set bounding polygons
polygons=c('-115.17,30.81,-112.95,31.85',
           '-114.87,30.83,-112.58,30.23',
           '-114.8,29.59,-112.29,30.23',
           '-114.8,29.59,-112.29,30.23',
           '-113.83,28.67,-111.59,29.12',
           '-113.5,28.67,-111.21,28.27',
           '-113.61,28.77,-111.32,28.37',
           '-113.55,28.73,-111.26,28.16',
           '-113.04,27.74,-110.4,28.14',
           '-112.98,27.61,-110.34,28.0',
           '-112.87,27.1,-110.01,27.83',
           '-112.36,27.1,-109.22,26.54',
           '-111.94,25.92,-108.84,26.54',
           '-111.57,25.92,-108.05,25.37',
           '-111.24,24.79,-107.57,25.37',
           '-111.24,24.79,-107.19,24.54',
           '-111.17,24.67,-107.13,24.22',
           '-110.89,24.45,-106.84,23.99',
           '-110.69,24.25,-106.64,23.79',
           '-110.08,23.24,-106.2,23.79',
           '-110.08,23.24,-105.41,22.54',
           '-110.01,23.14,-105.35,22.44',
           '-110.01,22.76,-105.04,21.95',
           '-110.01,22.43,-105.04,21.95',
           '-110.01,22.27,-105.04,21.79',
           '-109.94,22.19,-104.97,21.48',
           '-109.94,20.84,-104.97,21.48',
           '-109.94,20.84,-104.97,20.13',
           '-109.94,19.77,-104.97,20.13')

#extract data from Berkley ecoengine

#add source
this.source = "ecoengine"

for(eachpolygon in 1:length(polygons)){
  
  this.polygon = polygons[eachpolygon]#subset bounding box
  #call ecoengine API
  ee.data.frame = ee_observations(page_size=10000,country="Mexico", georeferenced = TRUE,bbox = this.polygon) %>% 
  .$data %>% tbl_df()
  
  setwd(datapath2)
  
  write.csv(ee.data.frame, file=paste(this.source,"_",eachpolygon,"_biodiver_full.csv",sep=""))
  
  ee.data.goc = ee.data.frame %>% 
    select(scientific_name, longitude, latitude) %>% 
    setnames(c("name","long","lat"))
  
  ee.data.goc$source_data = "ecoengine"
  
  
  ee.data.goc2 = ee.data.goc[complete.cases(ee.data.goc),]#eliminate rows with NA
  
  #remove duplicates based on the combination of latitude, longitude and species
  
  biodiversity.clean = ee.data.goc2[!duplicated(ee.data.goc2[,c('name', 'lat', 'lon')]),]
  
  #section the file so it can be subset for the Gulf of California
  
  biodiv.rows = nrow(biodiversity.clean)
  
  iterations = round(biodiv.rows/1000,0)
  
  if(biodiv.rows>1000)
  {
    last.row = 1001
    first.row = 1
    
    for (each.iteration in 1:iterations)
      
    {
      new.last.row = ((last.row-1)*each.iteration)
      
      section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
        na.omit %>% 
        mutate_each(funs(as.numeric),lat,long) %>% 
        as.data.frame
      
      coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
      proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
      print(summary(section.biodiv)) # print summary
      # subset ocurrence points within GOC
      stations_subset <- section.biodiv[goc.shape, ]
      #get table from shapefile
      biodiversity.goc <- as(stations_subset, "data.frame")
      
      test.bio = nrow(biodiversity.goc)==0
      if (test.bio==FALSE)
      {
        biodiversity = rbind(biodiversity,biodiversity.goc)
        biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
        print(paste(this.source, "biodiversity has ",nrow(biodiversity)," records"))
        
      }
      
      first.row = new.last.row+1
    }
  }
  
  if(biodiv.rows<1000)
  {
    last.row = biodiv.rows
    first.row = 1
    section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
      na.omit %>% 
      mutate_each(funs(as.numeric),lat,long) %>% 
      as.data.frame
    
    coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    test.bio = nrow(biodiversity.goc)==0
    if (test.bio==FALSE)
    {
      biodiversity = rbind(biodiversity,biodiversity.goc)
      biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
      print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
      
    }
  }
  
} # end polygon

setwd(datapath)
#write table
write.csv(biodiversity, file="ecoengine__biodiver_full.csv")

#get OBIS, downloaded from website

setwd(datafiles)#switch directory
biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

csv.files <- list.files(pattern = "\\.csv$")#list csv files

this.source= "obis"
#loop to read in data and obtain points in Gulf of California
for(eachfile in 1:length(csv.files)){
  
  Biom  = fread(csv.files[eachfile],header=T, sep=",",select=c(1:11)) %>%
    tbl_df %>% 
  select(sname, longitude, latitude) %>% #subset needed variables
    setnames(c("name","long","lat"))
 
    Biom$source_data = "obis" # set source
    
  
  Biom2 = Biom[complete.cases(Biom),]#eliminate rows with NA
  

  #remove duplicates based on the combination of latitude, longitude and species
  
  Biom3 = Biom2[!duplicated(Biom2[,c('name', 'lat', 'lon')]),]
  # remove negative latitude values
  biodiversity.clean = Biom3[Biom3$lat > 0,]
  
  #section the file so it can be subset for the Gulf of California
  
  biodiv.rows = nrow(biodiversity.clean)
  
  iterations = round(biodiv.rows/1000,0)
  
  if(biodiv.rows>1000)
  {
    last.row = 1001
    first.row = 1
    
    for (each.iteration in 1:iterations)
      
    {
      new.last.row = ((last.row-1)*each.iteration)
      
      section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
        na.omit %>% 
        mutate_each(funs(as.numeric),lat,long) %>% 
        as.data.frame
      
      coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
      proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
      print(summary(section.biodiv)) # print summary
      # subset ocurrence points within GOC
      stations_subset <- section.biodiv[goc.shape, ]
      #get table from shapefile
      biodiversity.goc <- as(stations_subset, "data.frame")
      
      test.bio = nrow(biodiversity.goc)==0
      if (test.bio==FALSE)
      {
        biodiversity = rbind(biodiversity,biodiversity.goc)
        biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
        print(paste(this.source,nrow(biodiversity)," records"))
        
      }
      
      first.row = new.last.row+1
    }
  }
  
  if(biodiv.rows<1000)
  {
    last.row = biodiv.rows
    first.row = 1
    section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
      na.omit %>% 
      mutate_each(funs(as.numeric),lat,long) %>% 
      as.data.frame
    
    coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    test.bio = nrow(biodiversity.goc)==0
    if (test.bio==FALSE)
    {
      biodiversity = rbind(biodiversity,biodiversity.goc)
      biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
      print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
      
    }
  }
  
} # end polygon

setwd(datapath)
#write table
write.csv(biodiversity, file="obis_biodiver_full.csv")

#get idigiobio, downloaded from website

setwd(datafiles)#switch directory
biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

csv.files <- list.files(pattern = "\\GC.csv$")#list csv files

this.source= "idigiobio"
#loop to read in data and obtain points in Gulf of California
for(eachfile in 1:length(csv.files)){
  
  Biom  = fread(csv.files[eachfile], header=T) %>%
    tbl_df %>% 
    dplyr::select(ScientificName, Lon, Lat) %>% #subset needed variables
    setnames(c("name","long","lat"))
  
  cap.name = capitalize(Biom$name) %>% data.frame %>% tbl_df %>% setnames("name")

  Biom = Biom %>% dplyr::select(long,lat) %>% cbind(cap.name) %>% 
    tbl_df %>%  dplyr::select(name, long, lat)
    
  Biom$source_data = this.source # set source
  
  
  Biom2 = Biom[complete.cases(Biom),]#eliminate rows with NA
  
  
  #remove duplicates based on the combination of latitude, longitude and species
  
  Biom3 = Biom2[!duplicated(Biom2[,c('name', 'lat', 'lon')]),]
  # remove negative latitude values
  biodiversity.clean = Biom3[Biom3$lat > 0,]
  
  #section the file so it can be subset for the Gulf of California
  
  biodiv.rows = nrow(biodiversity.clean)
  
  iterations = round(biodiv.rows/1000,0)
  
  if(biodiv.rows>1000)
  {
    last.row = 1001
    first.row = 1
    
    for (each.iteration in 1:iterations)
      
    {
      new.last.row = ((last.row-1)*each.iteration)
      
      section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
        na.omit %>% 
        mutate_each(funs(as.numeric),lat,long) %>% 
        as.data.frame
      
      coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
      proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
      print(summary(section.biodiv)) # print summary
      # subset ocurrence points within GOC
      stations_subset <- section.biodiv[goc.shape, ]
      #get table from shapefile
      biodiversity.goc <- as(stations_subset, "data.frame")
      
      test.bio = nrow(biodiversity.goc)==0
      if (test.bio==FALSE)
      {
        biodiversity = rbind(biodiversity,biodiversity.goc)
        biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
        print(paste(this.source,nrow(biodiversity)," records"))
        
      }
      
      first.row = new.last.row+1
    }
  }
  
  if(biodiv.rows<1000)
  {
    last.row = biodiv.rows
    first.row = 1
    section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
      na.omit %>% 
      mutate_each(funs(as.numeric),lat,long) %>% 
      as.data.frame
    
    coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    test.bio = nrow(biodiversity.goc)==0
    if (test.bio==FALSE)
    {
      biodiversity = rbind(biodiversity,biodiversity.goc)
      biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
      print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
      
    }
  }
  
} 

setwd(datapath)
#write table
write.csv(biodiversity, file="idigbios_biodiver_full.csv")

biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))


# for excel files from UABCS
setwd(datafiles)
xls.files <- list.files(pattern = "\\.xlsx$")# list files
this.source="cobi_uabcs"
#loop to read in data and obtain GOC data
for(eachfile in 1:length(xls.files))
{
  
  print(paste("Analyzing"," file",eachfile,"_",xls.files[eachfile]))

    df = read_excel(xls.files[eachfile], sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0)
  indx.sp= grep("Especie|Nombre|especie|nombre",colnames(df))
  indx.fuen= grep('Fuente|fuente|informacion|Base',colnames(df))
  indx.lon= grep('Longitud|longitud',colnames(df))
  indx.lat= grep('Latitud|latitud|Latutud',colnames(df))
  
  df2 = df[,c(indx.sp,indx.lon,indx.lat,indx.fuen)]
  
  setnames(df2, c("name","long","lat","source_data"))
  
  
  df2$long = as.numeric(df2$long) #make sure lon and lat are numeric
  df2$lat = as.numeric(df2$lat)
  
  df3 = df2[complete.cases(df2),]#eliminate rows with NA
  
  #remove duplicates based on the combination of latitude, longitude and species
  
  df4 = df3[df3$lat > 0,]
  
  biodiversity.clean = df4[!duplicated(df4[,c('name', 'lat', 'long')]),]
  # remove negative latitude values
  
  #section the file so it can be subset for the Gulf of California
  
  biodiv.rows = nrow(biodiversity.clean)
  
  iterations = round(biodiv.rows/1000,0)
  
  if(biodiv.rows>1000)
  {
    last.row = 1001
    first.row = 1
    
    for (each.iteration in 1:iterations)
      
    {
      new.last.row = ((last.row-1)*each.iteration)
      
      section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
        na.omit %>% 
        mutate_each(funs(as.numeric),lat,long) %>% 
        as.data.frame
      
      coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
      proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
      print(summary(section.biodiv)) # print summary
      # subset ocurrence points within GOC
      stations_subset <- section.biodiv[goc.shape, ]
      #get table from shapefile
      biodiversity.goc <- as(stations_subset, "data.frame")
      
      test.bio = nrow(biodiversity.goc)==0
      if (test.bio==FALSE)
      {
        biodiversity = rbind(biodiversity,biodiversity.goc)
        biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
        print(paste(this.source,nrow(biodiversity)," records"))
        
      }
      
      first.row = new.last.row+1
    }
  }
  
  if(biodiv.rows<1000)
  {
    last.row = biodiv.rows
    first.row = 1
    section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
      na.omit %>% 
      mutate_each(funs(as.numeric),lat,long) %>% 
      as.data.frame
    
    coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
    proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
    print(summary(section.biodiv)) # print summary
    # subset ocurrence points within GOC
    stations_subset <- section.biodiv[goc.shape, ]
    #get table from shapefile
    biodiversity.goc <- as(stations_subset, "data.frame")
    
    test.bio = nrow(biodiversity.goc)==0
    if (test.bio==FALSE)
    {
      biodiversity = rbind(biodiversity,biodiversity.goc)
      biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
      print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
      
    }
  }
  
} # end file

setwd(datapath)
#write table
write.csv(biodiversity, file="cobi_biodiver_full.csv")


#retrieve vertnet files

setwd(analysispath)


biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))

points.goc = fread("spatial_point_grid.csv",header = T)

#loop to read in data and obtain points in Gulf of California

this.source = "vertnet"

for(eachpoint in 1:nrow(points.goc))
{
  
  this.point = points.goc[eachpoint]#point
  point.lat = this.point$latitude
  point.lon = this.point$longitude
  
  res <- spatialsearch(lat = point.lat, lon = point.lon, radius = 15000, limit = 1000, config=verbose()) #radius in meters
  
  test.res = is.null(res)
  if(test.res==FALSE)
  {
    
    Biom = as.data.frame(res$data)
    
    setwd(datapath2)
    #write table
    write.csv(Biom, file=paste(this.source,"_",eachpoint,"_biodiver_full.csv",sep=""))
    
    
    indx.sp= grep("scientificname",colnames(Biom))
    
    test.sp.name = !length(indx.sp)
    
    if ( !length(indx.sp)==FALSE )
    {
      Biom = Biom[,c("scientificname","decimallongitude","decimallatitude")]
      
      setnames(Biom, c('name', 'long', 'lat')) # rename columns
      Biom$source_data = this.source # set source
      
      Biom$long = as.numeric(Biom$long) #make sure lon and lat are numeric
      Biom$lat = as.numeric(Biom$lat)
      Biom2 = Biom[complete.cases(Biom),]#eliminate rows with NA
      
      #remove duplicates based on the combination of latitude, longitude and species
      # select only positive latitude values
      Biom3 = Biom2[Biom2$lat > 0,]
      # select only negative longitude values
      Biom4 = Biom3[Biom3$long < 0,]
      
      
      biodiversity.clean = Biom4[!duplicated(Biom4[,c('name', 'lat', 'long')]),]
      
      
      #section the file so it can be subset for the Gulf of California
      
      biodiv.rows = nrow(biodiversity.clean)
      
      iterations = round(biodiv.rows/1000,0)
      
      if(biodiv.rows>1000)
      {
        last.row = 1001
        first.row = 1
        
        for (each.iteration in 1:iterations)
          
        {
          new.last.row = ((last.row-1)*each.iteration)
          
          section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
            na.omit %>% 
            mutate_each(funs(as.numeric),lat,long) %>% 
            as.data.frame
          
          coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
          proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
          print(summary(section.biodiv)) # print summary
          # subset ocurrence points within GOC
          stations_subset <- section.biodiv[goc.shape, ]
          #get table from shapefile
          biodiversity.goc <- as(stations_subset, "data.frame")
          
          test.bio = nrow(biodiversity.goc)==0
          if (test.bio==FALSE)
          {
            biodiversity = rbind(biodiversity,biodiversity.goc)
            biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
            print(paste(this.source,nrow(biodiversity)," records"))
            
          }
          
          first.row = new.last.row+1
        }
      }
      
      if(biodiv.rows<1000)
      {
        last.row = biodiv.rows
        first.row = 1
        section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
          na.omit %>% 
          mutate_each(funs(as.numeric),lat,long) %>% 
          as.data.frame
        
        coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
        proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
        print(summary(section.biodiv)) # print summary
        # subset ocurrence points within GOC
        stations_subset <- section.biodiv[goc.shape, ]
        #get table from shapefile
        biodiversity.goc <- as(stations_subset, "data.frame")
        
        test.bio = nrow(biodiversity.goc)==0
        if (test.bio==FALSE)
        {
          biodiversity = rbind(biodiversity,biodiversity.goc)
          biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
          print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
          
        }
      }
    }
  }
      
    } # end points
    
    setwd(datapath)
    #write table
    write.csv(biodiversity, file="vertnet_biodiver_full.csv",row.names=FALSE)
  
    
    #get records from ebird
    setwd(analysispath)
    
    
    biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))
     
    #loop to read in data and obtain points in Gulf of California
    
    this.source = "ebird"
    regions.ebird = c('MX-SON','MX-BCN','MX-SIN','MX-BCS','MX-NAY','MX-JAL')
    for(eachregion in 1:length(regions.ebird))
    {
      
      this.region = regions.ebird[eachregion]#region
     
      res <- ebirdregion(this.region, max=10000)
      
        if(nrow(res)!=0)
      {
        
        setwd(datapath2)
        #write table
        write.csv(res, file=paste(this.source,"_",this.region,"_biodiver_full.csv",sep=""))
        
        Biom = res %>% 
          select(sciName,lng,lat) %>% 
          setnames(c('name', 'long', 'lat')) %>% 
          mutate(source_data = this.source) %>% 
          mutate_each(funs(as.numeric),long:lat)#make sure lon and lat are numeric
          
        Biom2 = Biom[complete.cases(Biom),]#eliminate rows with NA
          
          #remove duplicates based on the combination of latitude, longitude and species
          # select only positive latitude values
          Biom3 = Biom2[Biom2$lat > 0,]
          # select only negative longitude values
          Biom4 = Biom3[Biom3$long < 0,]
          
          
          biodiversity.clean = Biom4[!duplicated(Biom4[,c('name', 'lat', 'long')]),]
          
          
          #section the file so it can be subset for the Gulf of California
          
          biodiv.rows = nrow(biodiversity.clean)
          
          iterations = round(biodiv.rows/1000,0)
          
          if(biodiv.rows>1000)
          {
            last.row = 1001
            first.row = 1
            
            for (each.iteration in 1:iterations)
              
            {
              new.last.row = ((last.row-1)*each.iteration)
              
              section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
                na.omit %>% 
                mutate_each(funs(as.numeric),lat,long) %>% 
                as.data.frame
              
              coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
              proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
              print(summary(section.biodiv)) # print summary
              # subset ocurrence points within GOC
              stations_subset <- section.biodiv[goc.shape, ]
              #get table from shapefile
              biodiversity.goc <- as(stations_subset, "data.frame")
              
              test.bio = nrow(biodiversity.goc)==0
              if (test.bio==FALSE)
              {
                biodiversity = rbind(biodiversity,biodiversity.goc)
                biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
                print(paste(this.source,nrow(biodiversity)," records"))
                
              }
              
              first.row = new.last.row+1
            }
          }
          
          if(biodiv.rows<1000)
          {
            last.row = biodiv.rows
            first.row = 1
            section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
              na.omit %>% 
              mutate_each(funs(as.numeric),lat,long) %>% 
              as.data.frame
            
            coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
            proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
            print(summary(section.biodiv)) # print summary
            # subset ocurrence points within GOC
            stations_subset <- section.biodiv[goc.shape, ]
            #get table from shapefile
            biodiversity.goc <- as(stations_subset, "data.frame")
            
            test.bio = nrow(biodiversity.goc)==0
            if (test.bio==FALSE)
            {
              biodiversity = rbind(biodiversity,biodiversity.goc)
              biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
              print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
              
            }
          }
        }
      }
      
  
    
    setwd(datapath)
    #write table
    write.csv(biodiversity, file="ebird_biodiver_full.csv",row.names=FALSE)
    
    #get shark ad seagrass data files from Ulloa et al. 2006
    #these were the only groups with species-level data
    
    setwd(ulloafiles)
    csv.files <- list.files(pattern = "\\.csv$")# list files
    biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4))
    
    #loop to read in data and obtain points in Gulf of California
    
    this.source = "ulloa_et_al_2006"
    
    #loop to read in data and obtain GOC data
    for(eachfile in 1:length(csv.files))
    {
      
      print(paste("Analyzing"," file",eachfile,"_",csv.files[eachfile]))
      
      df2 = fread(csv.files[eachfile], header=TRUE) %>% 
        tbl_df %>% 
        select(NOM_CIEN, LONGITUD, LATITUD) %>% 
        mutate(source_data=this.source) %>% 
      setnames(c("name","long","lat","source_data")) %>% 
        mutate_each(funs(as.numeric),long:lat) %>% 
        data.frame
      
      df3 = df2[complete.cases(df2),]#eliminate rows with NA
      
      #remove duplicates based on the combination of latitude, longitude and species
      
      df4 = df3[df3$lat > 0,]
      
      biodiversity.clean = df4[!duplicated(df4[,c('name', 'lat', 'long')]),]
      # remove negative latitude values
      
      #section the file so it can be subset for the Gulf of California
      
      biodiv.rows = nrow(biodiversity.clean)
      
      iterations = round(biodiv.rows/1000,0)
      
      if(biodiv.rows>1000)
      {
        last.row = 1001
        first.row = 1
        
        for (each.iteration in 1:iterations)
          
        {
          new.last.row = ((last.row-1)*each.iteration)
          
          section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
            na.omit %>% 
            mutate_each(funs(as.numeric),lat,long) %>% 
            as.data.frame
          
          coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
          proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
          print(summary(section.biodiv)) # print summary
          # subset ocurrence points within GOC
          stations_subset <- section.biodiv[goc.shape, ]
          #get table from shapefile
          biodiversity.goc <- as(stations_subset, "data.frame")
          
          test.bio = nrow(biodiversity.goc)==0
          if (test.bio==FALSE)
          {
            biodiversity = rbind(biodiversity,biodiversity.goc)
            biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
            print(paste(this.source,nrow(biodiversity)," records"))
            
          }
          
          first.row = new.last.row+1
        }
      }
      
      if(biodiv.rows<1000)
      {
        last.row = biodiv.rows
        first.row = 1
        section.biodiv = biodiversity.clean[first.row:last.row,] %>% 
          na.omit %>% 
          mutate_each(funs(as.numeric),lat,long) %>% 
          as.data.frame
        
        coordinates(section.biodiv) <- c("long", "lat")  # set spatial coordinates
        proj4string(section.biodiv) <- crs.geo.wgs  # define projection system of our data
        print(summary(section.biodiv)) # print summary
        # subset ocurrence points within GOC
        stations_subset <- section.biodiv[goc.shape, ]
        #get table from shapefile
        biodiversity.goc <- as(stations_subset, "data.frame")
        
        test.bio = nrow(biodiversity.goc)==0
        if (test.bio==FALSE)
        {
          biodiversity = rbind(biodiversity,biodiversity.goc)
          biodiversity = biodiversity[!duplicated(biodiversity[,c('name', 'long', 'lat')]),]
          print(paste(this.source, " biodiversity has ",nrow(biodiversity)," records"))
          
        }
      }
      
    } # end file
    setwd(datapath)
    #write table
    write.csv(biodiversity, file="ulloa_biodiver_full.csv",row.names=FALSE)
    
    
## @knitr organizebiodiversity
    rm(list=ls())    
    filepath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/Ocurrencia" #put path
    analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
    datapath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/richness" #put path
    shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Biodiversity"
    
    setwd(shapepath)
    #read in Gulf of California shapefile
    #goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")#without wetlands
    goc.shape <- readOGR(".", "Golfo_california_wetland_poly_WGS84")#with wetlands
    
    # projections
    crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
    
   
    setwd(datapath)
    
    biodiversity = as.data.frame(matrix(0,nrow=0,ncol=4)) %>% 
      tbl_df %>% 
      setnames(c("name","long","lat","source"))
    
    # get list of files
    
    csv.files <- list.files(pattern = "\\_full.csv$")#list csv files
    
    for(eachfile in 1:length(csv.files))
    {
      
      biodiversity.goc  = fread(csv.files[eachfile],header=T) %>% 
        tbl_df %>% 
        dplyr::select(starts_with('name'),contains('lon'),contains('lat'),source_data) %>% 
        setnames(c("name","long","lat","source_data"))
      
      
      #read each file
      biodiversity = rbind(biodiversity, biodiversity.goc) #bind it to a empty frame
      biodiversity = biodiversity[!duplicated(biodiversity[,c("name","long","lat")]),] #eliminate duplicates
      
      print(paste("biodiversity has ",nrow(biodiversity)," records")) #print records
    }
    
    setwd(analysispath)
    
    write.csv(biodiversity,"biodiversity_all_raw.csv")
    
    #write species table to helpclean up misspellings
    biodiv.species = unique(biodiversity$name)
    biodiv.species.clean = biodiv.species[!duplicated(biodiv.species)]#eliminate duplicates
    biodiv.species.clean = tbl_df(data.frame(biodiv.species.clean))
    biodiv.species.clean2= cbind(biodiv.species.clean,biodiv.species.clean)
    setnames(biodiv.species.clean2, c("Species","NewSpecies"))
    
    #Output species table
    setwd(analysispath)
    #this table has to be read manually to fix the species misspellings
    write.csv(biodiv.species.clean2, file="biodiver_species_list.csv")
    
## @knitr checktaxonomy    
    rm(list=ls())
  #we have eliminated records not identified to species
  #see biodiver_species_list.xlsx
    rev.species.list = read.csv("species_list.csv", header=T,na.strings="NA")
    
    species.list = rev.species.list %>% 
      setnames(c("Species","NewSpecies")) %>% 
      .$NewSpecies %>% 
      unique
    
    species.taxonomy = as.data.frame(matrix(0,nrow=0,ncol=4))
    
    for(eachspecies in 1:length(species.list))
    {
      this.species = species.list[eachspecies] %>% 
        drop.levels %>% 
        as.character
      
      print(paste("Analyzing species ",eachspecies,"_", this.species,"of ",length(species.list)))
      tryCatch(taxo <- gnr_resolve(names = this.species), error = function(cond)"skip") 
      
        sub_name = taxo %>% 
          .$results %>% 
          as.data.frame
        
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
    names(species.taxonomy)= c("Species","Sp_accepted","data_source","score")
    write.csv(species.taxonomy, file="master_taxonomy_list3.csv")
    
  ## @knitr updatetaxonomy
    rm(list=ls())
    #read table again
    analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
    shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Biodiversity"
    crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
    
    setwd(analysispath)
  
    rev.species.list = fread("species_list.csv", header=T, na.strings="NA")
    
    new.biodiv.clean = fread("biodiversity_all_raw.csv", header=T, na.strings="NA") %>% 
      left_join(rev.species.list, by = "name") #match new names
    
    print(dim(new.biodiv.clean))
    new.biodiversity = new.biodiv.clean[complete.cases(new.biodiv.clean),]#eliminate rows with NA
    print(dim(new.biodiversity))
    #remove duplicates based on the combination of latitude, longitude and newspecies
    new.biodiversity.goc.clean = new.biodiversity[!duplicated(new.biodiversity[,c('NewSpecies', 'lat', 'long')]),]
    
    new.biodiversity.goc.clean2 = new.biodiversity.goc.clean %>% 
      select(NewSpecies,source_data,long,lat)
    
    print(dim(new.biodiversity.goc.clean2))
    
    
    setwd(analysispath)
    #write table
    write.csv(new.biodiversity.goc.clean2, file="biodiver_species_goc.csv")
    
    
    
    #print(paste("old biodiversity has ",nrow(biodiversity)," records"))
    print(paste("new biodiversity has ",nrow(new.biodiversity.goc.clean2)," records"))
    
    new.biodiversity.goc = new.biodiversity.goc.clean %>% 
      data.frame
    #set directory path
    setwd(shapepath)
    #Save as shapefile
    coordinates(new.biodiversity.goc) <- c("long", "lat")  # set spatial coordinates
    proj4string(new.biodiversity.goc) <- crs.geo.wgs
    writeOGR(new.biodiversity.goc, ".", "biodiversity_goc",driver="ESRI Shapefile",overwrite_layer=TRUE)
    
    ## @knitr recbiodiversity
    
    #read in species occurrance
    #this data was obtained and cleaned in Data_biodiversity.R
    #files were then organized in Organize_biodiversity.R
    rm(list=ls())
    analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" 
    
    setwd(analysispath)
    
    biodiversity  = fread("biodiver_species_goc.csv",header=T) %>% 
      select(long, lat, NewSpecies) %>% #select species, lat and lon
      as.data.frame
    
    biodiversity.records = nrow(biodiversity)
    unique.species = biodiversity %>% 
      .$NewSpecies %>% 
      unique %>% 
      length    
    
## @knitr mapbiodiversity
    rm(list=ls())
#read in species occurrance
#this data was obtained and cleaned in Data_biodiversity.R
#files were then organized in Organize_biodiversity.R

analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" 
shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Biodiversity"

setwd(analysispath)

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


setwd(analysispath)

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

analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Biodiversity"
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(shapepath)
corridor.shape <- readOGR(".", "propuesta_corredor_costero_wetland_WGS84")
corridor.wgs = spTransform(corridor.shape, crs.geo.wgs)

setwd(analysispath)
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


setwd(analysispath)
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

setwd(analysispath)
ggsave ("map_goc_points_corridor.png", dpi = 300)

## @knitr richnessmodelcorridor

shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Biodiversity"
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(shapepath)
corridor.shape <- readOGR(".", "propuesta_corredor_costero_wetland_WGS84")
corridor.wgs = spTransform(corridor.shape, crs.geo.wgs)

analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
setwd(analysispath)

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

shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Biodiversity"
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

analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
setwd(analysispath)

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


