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

x = c("readxl","knitcitations","knitr","xtable","devtools","gdata","maptools", 
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
shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation"

setwd(shapepath)
#read in Gulf of California shapefile
goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")

# projections
#Lambert
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84


#first extract data using spocc from bison

#set bounding polygons
polygons=c('POLYGON((-115.05 31.9, -112.99 31.9, -112.99 30.99, -115.05 30.99, -115.05 31.9))',
           'POLYGON((-115.05 30.28, -112.82 30.28, -112.82 30.99, -115.05 30.99, -115.05 30.28))',
           'POLYGON((-114.74 30.28, -112.49 30.28, -112.49 29.53, -114.74 29.53, -114.74 30.28))',
           'POLYGON((-113.98 28.83, -111.94 28.83, -111.94 29.53, -113.98 29.53, -113.98 28.83))',
           'POLYGON((-113.49 28.83, -111.32 28.83, -111.32 28.21, -113.49 28.21, -113.49 28.83))',
           'POLYGON((-113.03 27.59, -110.42 27.59, -110.42 28.34, -113.03 28.34, -113.03 27.59))',
           'POLYGON((-112.55 27.59, -109.74 27.59, -109.74 26.94, -112.55 26.94, -112.55 27.59))',
           'POLYGON((-112.0752 26.3164, -109.1382 26.3164, -109.1382 26.9392, -112.0752 26.9392, -112.0752 26.3164))',
           'POLYGON((-111.46 26.3164, -109.0723 26.3164, -109.0723 25.738, -111.46 25.738, -111.46 26.3164))',
           'POLYGON((-111.3172 25.1287, -108.6163 25.1287, -108.6163 25.738, -111.3172 25.738, -111.3172 25.1287))',
           'POLYGON((-110.9601 25.1287, -107.8802 25.1287, -107.8802 24.7044, -110.9601 24.7044, -110.9601 25.1287))',
           'POLYGON((-110.7843 24.2603, -107.4188 24.2603, -107.4188 24.7044, -110.7843 24.7044, -110.7843 24.2603))',
           'POLYGON((-110.6388 24.2603, -106.842 24.2603, -106.842 23.7677, -110.6388 23.7677, -110.6388 24.2603))',
           'POLYGON((-109.7928 23.2901, -106.4026 23.2901, -106.4026 23.7677, -109.7928 23.7677, -109.7928 23.2901))',
           'POLYGON((-109.7928 23.2901, -106.095 23.2901, -106.095 22.9811, -109.7928 22.9811, -109.7928 23.2901))',
           'POLYGON((-110.1059 22.7542, -105.5511 22.7542, -105.5511 22.9811, -110.1059 22.9811, -110.1059 22.7542))',
           'POLYGON((-110.1059 22.7542, -105.5511 22.7542, -105.5511 22.5049, -110.1059 22.5049, -110.1059 22.7542))',
           'POLYGON((-109.9906 22.6374, -105.5237 22.6374, -105.5237 22.3172, -109.9906 22.3172, -109.9906 22.6374))',
           'POLYGON((-109.9796 22.5511, -105.5127 22.5511, -105.5127 22.2309, -109.9796 22.2309, -109.9796 22.5511))',
           'POLYGON((-109.9906 22.4343, -105.5237 22.4343, -105.5237 22.114, -109.9906 22.114, -109.9906 22.4343))',
           'POLYGON((-109.9741 22.2817, -105.293 22.2817, -105.293 21.9615, -109.9741 21.9615, -109.9741 22.2817))',
           'POLYGON((-109.9796 22.1493, -105.2985 22.1493, -105.2985 21.8291, -109.9796 21.8291, -109.9796 22.1493))',
           'POLYGON((-109.9906 22.0321, -105.3094 22.0321, -105.3094 21.7119, -109.9906 21.7119, -109.9906 22.0321))',
           'POLYGON((-109.9906 21.4044, -105.1556 21.4044, -105.1556 21.7119, -109.9906 21.7119, -109.9906 21.4044))',
           'POLYGON((-109.9906 21.4044, -105.1556 21.4044, -105.1556 21.1392, -109.9906 21.1392, -109.9906 21.4044))',
           'POLYGON((-109.9906 20.9229, -105.1556 20.9229, -105.1556 21.1392, -109.9906 21.1392, -109.9906 20.9229))',
           'POLYGON((-109.9906 20.9229, -105.1556 20.9229, -105.1556 20.6876, -109.9906 20.6876, -109.9906 20.9229))',
           'POLYGON((-109.9906 20.2441, -105.1556 20.2441, -105.1556 20.6876, -109.9906 20.6876, -109.9906 20.2441))')

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
  
  res <- spatialsearch(lat = point.lat, lon = point.lon, radius = 10000, limit = 1000, config=verbose()) #radius in meters
  
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
    
    filepath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/Ocurrencia" #put path
    analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
    savepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation"
    datapath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/richness" #put path
    
    #set directory path
    setwd(savepath)
    
    #read in Gulf of California shapefile
    goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")#this shapefile does not use wetlands, only marine areas
    #I had originally used the polygon Golfo_california_wetland_polygon2.shp 
    #but it took too much of terrestrial area
     
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
    write.csv(species.taxonomy, file="master_taxonomy_list2.csv")
    
  ## @knitr updatetaxonomy
    #read table again
    analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
    shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation"
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
    
    
    
    print(paste("old biodiversity has ",nrow(biodiversity)," records"))
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

#read in species occurrance
#this data was obtained and cleaned in Data_biodiversity.R
#files were then organized in Organize_biodiversity.R

analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" 

setwd(analysispath)

biodiversity  = fread("biodiver_species_goc.csv",header=T) %>% 
  select(long, lat, NewSpecies) %>% #select species, lat and lon
  data.frame %>% 
setnames(c('long','lat','speciesID')) # and rename

geo.biodiv = biodiversity
coordinates(geo.biodiv) = c("long", "lat")

mapbiodiv <- get_map(location = c(lon = mean(geo.biodiv$long), lat = mean(geo.biodiv$lat)), zoom = 6,
                     source='stamen',maptype = "terrain", scale = 2) 

ggmap(mapbiodiv) +
  geom_point(data = biodiversity, aes(x = long, y = lat, alpha = 0.8), colour = 'darkred', fill= 'darkred', size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  labs(x = 'Longitude', y = 'Latitude')

ggsave ("map_goc_points.png", dpi = 300)

## @knitr richnessmodel
shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation"

setwd(shapepath)
#read in Gulf of California shapefile
goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")

analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
setwd(analysispath)

biodiversity  = fread("biodiver_species_goc.csv",header=T) %>% 
  select(long, lat,NewSpecies) %>% #select species, lat and lon
  as.data.frame

names(biodiversity) = c('long','lat','speciesID') # and rename

resolution= 0.008
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
  reclassify(cbind(0, NA)) %>% 
  rasterToPoints %>% 
  data.frame %>% 
  setnames(c('Longitude', 'Latitude', 'Richness'))


robust.data = raster("robust_goc.tif") %>% 
  mask(goc.shape) %>%
  reclassify(cbind(0, NA)) %>% 
rasterToPoints %>% 
  data.frame %>% 
  setnames(c('Longitude', 'Latitude', 'Robustness')) %>% 
  left_join(sp.data,by=c('Longitude', 'Latitude')) 

#create color palette
pal <- wes_palette(10, name = "FantasticFox", type = "continuous")

#plot richness model and robustness
non.plot = ggplot(data=non.data, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Richness)) +
  theme_bw() +
  coord_equal() + 
  scale_fill_gradientn(colours = pal)+
   theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12, angle=90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))

pal <- wes_palette(15, name = "Zissou", type = "continuous")

robust.plot = ggplot(data=robust.data, aes(y=Latitude, x=Longitude)) +
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

sp.plot = ggplot(data=sp.data, aes(y=Latitude, x=Longitude)) +
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
plot_grid(sp.plot, robust.plot, align='h')
dev.off()

png("goc_richness_non.png")
non.plot
dev.off()
## @knitr corridorpoints
#cut point not in corridor

analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation"
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(shapepath)
corridor.shape <- readOGR(".", "propuesta_corredor_costero")
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

shapepath = "E:/Archivos/SIG/Proyectos/ArcGis/Datos ordenados/Shapes y layers/Archivos_articulos/Zonation"
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84

setwd(shapepath)
corridor.shape <- readOGR(".", "propuesta_corredor_costero")
corridor.wgs = spTransform(corridor.shape, crs.geo.wgs)

analysispath="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis" #put path
setwd(analysispath)

biodiversity  = fread("biodiver_species_corridor.csv",header=T) %>% 
  select(long, lat, NewSpecies) %>% #select species, lat and lon
  as.data.frame

names(biodiversity) = c('long','lat','speciesID') # and rename

resolution= 0.008
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

#reload saved tiff rasters and change colors

#reload saved tiff rasters and change colors

sp.data = raster("species_richness_corridor.tif") %>% 
  mask(corridor.wgs) %>% 
  reclassify(cbind(0, NA)) %>% 
  rasterToPoints %>% 
  data.frame %>% 
  setnames(c('Longitude', 'Latitude', 'Richness'))


robust.data = raster("robust_corridor.tif") %>% 
  mask(corridor.wgs) %>%
  reclassify(cbind(0, NA)) %>% 
  rasterToPoints %>% 
  data.frame %>% 
  setnames(c('Longitude', 'Latitude', 'Robustness')) %>% 
  left_join(sp.data,by=c('Longitude', 'Latitude')) 

#create color palette
pal <- wes_palette(12, name = "GrandBudapest", type = "continuous")

#plot richness model and robustness
robust.plot = ggplot(data=robust.data, aes(y=Latitude, x=Longitude)) +
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

sp.plot = ggplot(data=sp.data, aes(y=Latitude, x=Longitude)) +
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
        legend.text  = element_text(size=10),
        legend.title = element_text(size=12))

#export as png
png("goc_richness_corridor_results.png")
plot_grid(sp.plot, robust.plot, align='h')#could add labels
dev.off()