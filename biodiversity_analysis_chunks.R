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
rm(list=ls())  
workpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/RCode"
shapepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/SIG_Biodiversity"
savepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis"
ulloafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Datos/Ulloa_datos" #put path
datafiles="E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Datos/Ocurrencia_especies"

# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
crs.lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

setwd(shapepath)
#read in Gulf of California shapefile
#goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")#without wetlands
goc.shape <- readOGR(".", "Golfo_california_wetland_poly_WGS84")#with wetlands

setwd(workpath)

#'create bounding polygon for the Gulf of California
#'upper.left = c(32.139900, -115.142516)
#'lower.left = c(20.164036, -115.142516)
#'lower.right = c(20.164036, -104.95342)
#'upper.right = c(32.139900, -104.95342)

lat = c(32.139900,20.164036,20.164036,32.139900)
lon = c(-115.142516, -115.142516, -104.95342, -104.95342)

#' points to polygons
goc.points = data.frame(lon,lat) 
coordinates(goc.points) <- c("lon", "lat")
goc.points = goc.points %>% rbind(.,goc.points[1,]) 
proj4string(goc.points) <- crs.geo.wgs # define projection

goc.pol = SpatialPolygons(list(Polygons(list(Polygon(goc.points)), 1)))
proj4string(goc.pol) <- crs.geo.wgs # define projection

#' create point grid for the Gulf of California
#' this grid will be used as vertices to search for species records
#' keep in geographic
#' this returns a spatial point object
#' otherwise couldn't find difference from makegrid
#' goc.sample.points = spsample(goc.pol, n = 4000, "regular")
goc.point.grid = makegrid(goc.pol, n = 4000, pretty=FALSE)
#' create list of WKT polygons
#' 
boxes = 59*70 # rows of lat (-1) vs rows of lon (-1)#29*24 for grid of 1000 points
wkt.data = matrix(1:boxes, ncol=1)
boxes.data = matrix(1:boxes, ncol=1)
#' create new counter
counter=0

print("Generating point grids")

for (i in 1:boxes)
{
  corner1 = goc.point.grid[(1*counter)+1,] %>% paste(collapse=" ")
  corner2 = goc.point.grid[(1*counter)+2,] %>% paste(collapse=" ")
  corner3 = goc.point.grid[(1*counter)+62,] %>% paste(collapse=" ")
  corner4 = goc.point.grid[(1*counter)+61,] %>% paste(collapse=" ")
  
  coords = paste(corner1, corner2,corner3, corner4, corner1,sep=" ,")
  wkt.data[i]=paste("POLYGON((",coords,"))", sep="")
  
  counter=counter+1
  print(counter)
  #' bounding boxes for ecoengine
  #' order is min Longitude , min Latitude , max Longitude , max Latitude. 
  boxes.data[i] = paste(goc.point.grid[(1*counter)+1,1],goc.point.grid[(1*counter)+1,2],goc.point.grid[(1*counter)+62,1],goc.point.grid[(1*counter)+62,2],sep=",")
  
}
poly.data = as.vector(wkt.data)
bbox.data = as.vector(boxes.data)

###############
#' Record retrieval
#' First retrieve from data bases that don't take bounding boxes
#' this is FishBase and iDigBio
#' retrieve all records for an ecosystem
#' Gulf of California = 165
#' 
site = 165 # 144 Gulf of Mexico, 132 California Current, 145 Caribbean
print("Now querying Fishbase")
url <- paste("http://www.fishbase.org/map/EcosystemOccurrencesList.php?e_code=",
             site,sep="")

fishbase.data <- try(readHTMLTable(url),silent=TRUE)

fishbase.data.1 = fishbase.data[[3]] %>% data.frame
fishbase.coordinates <- data.frame(fishbase.data.1[,2], fishbase.data.1[,5],fishbase.data.1[,6])

#' define helper function to replace empty values
empty_as_na <- function(x) ifelse(x=="", NA,x)

#' clean up fishbase records and convert to main database
biodiversity.sp = fishbase.coordinates %>% tbl_df %>% 
  mutate(source = "fishbase") %>% 
  setNames(c("species","lat","lon","source")) %>% 
  mutate_each(funs(as.character),lat:lon)%>% 
  mutate_each(funs(as.numeric),lat:lon) 
#' eliminate empty species names and duplicate records
biodiversity.sp[biodiversity.sp==""]  <- NA
biodiversity.sp = na.omit(biodiversity.sp)
biodiversity.sp = biodiversity.sp[!duplicated(biodiversity.sp[,c('species', 'lat', 'lon')]),]

#' print records
print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))

setwd(savepath)
write.csv(biodiversity.sp,file="record_queries1.csv")

#' retrieve iDigBio
#' can only retrieve records for all Mexico
biodiversity.sp = matrix(0,nrow=0,ncol=3) %>% as.data.frame %>% 
  setNames(c("scientificname","geopoint.lon","geopoint.lat")) 

print("Now querying iDigBio")
#vary the offset, the record where retrieval starts
for(eachnumber in 1:1000){
  print(paste("Analyzing record",eachnumber))
  thisoffset = 5000*eachnumber
  
  df1 <- try(idig_search_records(rq=list(country="mexico", geopoint=list(type="exists")), fields=c("scientificname", "geopoint"), limit=5000,offset=thisoffset))
  
  if(!inherits(df1, "try-error")){
    
    biodiversity.sp = rbind(biodiversity.sp,df1)
    biodiversity.sp = biodiversity.sp[!duplicated(biodiversity.sp[,c('scientificname', 'geopoint.lon', 'geopoint.lat')]),]
    print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))
    
  }
}

#modify data frame names and capitalization
biodiversity.sp = biodiversity.sp %>% tbl_df %>% 
  select(scientificname,geopoint.lat,geopoint.lon) %>% 
  mutate(source = "idigbio") %>% 
  setNames(c("species","lat","lon","source"))%>% 
  mutate(species = capitalize(species))

#use this to save the complete data file
setwd(savepath)
write.csv(biodiversity.sp,file="record_queries2.csv")

#' print records
print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))

#' use rbison, rgbif, ecoengine packages
#' tried spocc again but still buggy
#create new matrix for output of spatial queries
biodiversity.sp = matrix(0,nrow=0,ncol=4) %>% as.data.frame %>% 
  setNames(c("species","lat","lon","source")) 

print("Now querying spatial data rbison, rgbif, ecoengine")
for(eachpolygon in 1:length(poly.data)){
  
  print(paste("Records retrieved from polygons ",nrow(biodiversity.sp),sep = " "))
  #select polygons or bounding box    
  this.polygon = poly.data[eachpolygon]#subset bounding box
  this.bb = bbox.data[eachpolygon]#subset bounding box
  print(paste("Bison: Analyzing polygon ",eachpolygon,sep = " "))
  
  #ERROR HANDLING
  # obtain bison data
  bison.data <- try(bison(aoi=this.polygon,count=10000))
  
  if(!inherits(bison.data, "try-error")){
    
    bison.data = bison(aoi=this.polygon,count=10000)%>% 
      .$points %>% 
      data.frame %>% 
      tbl_df
    
    #' add source and reorded and rename columns only if records are present
    #' otherwise create empty frame with required col names
    if(nrow(bison.data)!=0) {
      bison.data$source = "bison"
      bison.data = bison.data %>% 
        select(name,decimalLatitude,decimalLongitude,source) %>% 
        setNames(c("species","lat","lon","source")) 
    } else {
      bison.data = matrix(0,nrow=0,ncol=4) %>% data.frame %>% tbl_df %>% 
        setNames(c("species","lat","lon","source"))
    }
    
    biodiversity.sp = rbind(biodiversity.sp, bison.data)
    biodiversity.sp = biodiversity.sp[!duplicated(biodiversity.sp[,c('species', 'lat', 'lon')]),]
    
  }
}
setwd(savepath)  
write.csv(biodiversity.sp,file="record_queries3.csv")

print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))

biodiversity.sp = matrix(0,nrow=0,ncol=4) %>% as.data.frame %>% 
  setNames(c("species","lat","lon","source")) 

for(eachpolygon in 1:length(poly.data)){
  
  print(paste("Records retrieved from polygons ",nrow(biodiversity.sp),sep = " "))
  this.polygon = poly.data[eachpolygon]#subset bounding box
  this.bb = bbox.data[eachpolygon]#subset bounding box
  print(paste("GBIF: Analyzing polygon ",eachpolygon,sep = " "))
  
  #ERROR HANDLING
  # get gbif records
  gbif.data <- try(occ_search(geometry=this.polygon,return='data',fields=c("name","decimalLatitude","decimalLongitude","datasetName","collectionCode"),limit=200000))
  
  if(!inherits(gbif.data, "try-error")){
    #' arrange data frame only if records exist
    #' otherwise returns empty frame
    if(gbif.data[1]!="no data found, try a different search"){
      
      gbif.data = gbif.data %>% tbl_df() %>% mutate(source = "gbif") %>% 
        select(name, decimalLatitude,decimalLongitude, source) %>% 
        setNames(c("species","lat","lon","source"))
    }else {
      gbif.data = matrix(0,nrow=0,ncol=4) %>% data.frame %>% tbl_df() %>% 
        setNames(c("species","lat","lon","source"))
    } 
    
    biodiversity.sp = rbind(biodiversity.sp, gbif.data)
    biodiversity.sp = biodiversity.sp[!duplicated(biodiversity.sp[,c('species', 'lat', 'lon')]),]
  } #end if no error in try catch
}
setwd(savepath)  
write.csv(biodiversity.sp,file="record_queries4.csv")
print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))

#get ecoengine
#create new matrix for output of spatial queries
biodiversity.sp = matrix(0,nrow=0,ncol=4) %>% as.data.frame %>% 
  setNames(c("species","lat","lon","source")) 

print("Now querying ecoengine")

for(eachpolygon in 1:length(poly.data)){
  
  print(paste("Records retrieved from polygons ",nrow(biodiversity.sp),sep = " "))
  this.polygon = poly.data[eachpolygon]#subset bounding box
  this.bb = bbox.data[eachpolygon]#subset bounding box
  print(paste("Ecoengine: Analyzing polygon ",eachpolygon,sep = " "))
  
  #ERROR HANDLING
  # obtain ecoengine data
  ee.data.frame <- try(ee_observations(page_size=10000, georeferenced = TRUE,bbox = this.bb))
  
  if(!inherits(ee.data.frame, "try-error")){
    #' arrange data frame only if records exist
    #' otherwise returns empty frame
    if(ee.data.frame!="No records"){
      ee.data = ee.data.frame %>% .$data %>% tbl_df() %>% 
        select(scientific_name, latitude, longitude) %>% 
        setNames(c("species","lat","lon")) %>% 
        mutate(source = "ecoengine")
    }else {
      ee.data = matrix(0,nrow=0,ncol=4) %>% data.frame %>% tbl_df %>% 
        setNames(c("species","lat","lon","source"))
    }
    
    biodiversity.sp = rbind(biodiversity.sp, ee.data)
    biodiversity.sp = biodiversity.sp[!duplicated(biodiversity.sp[,c('species', 'lat', 'lon')]),]
    
  }
}

#use this to save the complete data file
setwd(savepath)
write.csv(biodiversity.sp,file="record_queries5.csv")
print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))

#create new matrix for output of spatial queries
biodiversity.sp = matrix(0,nrow=0,ncol=4) %>% as.data.frame %>% 
  setNames(c("species","lat","lon","source")) 

print("Now querying vertnet")
# query Vertnet using spatial points  

for(eachpoint in 1:nrow(goc.point.grid)) {
  
  this.point = goc.point.grid[eachpoint,]#point
  print(paste("Analyzing grid point ",eachpoint,sep = " "))
  point.lat = this.point[,2]
  point.lon = this.point[,1]
  #ERROR HANDLING
  vertnet.data <- try(spatialsearch(lat = point.lat, lon = point.lon, radius = 15000, limit = 1000, verbose= TRUE))
  if(!inherits(vertnet.data, "try-error")){
    
    vertnet.data = vertnet.data %>% .$data %>% data.frame #radius in meters
    #' only modify dataframe if records are available
    test.res = is.null(vertnet.data)
    if(test.res==FALSE)
    {
      if (any(grepl("specificepithet",colnames(vertnet.data)))==TRUE){
        #create scientificname column when species and genus are separate 
        vertnet.data = vertnet.data %>% tbl_df %>% 
          mutate(scientificname = paste(genus,specificepithet, sep= " "))  %>%  
          select(scientificname, decimallatitude,decimallongitude) %>% 
          mutate(source = "vertnet") %>% 
          setNames(c('species', 'lat', 'lon',"source")) %>% # rename columns
          mutate_each(funs(as.character),lat:lon)%>% 
          mutate_each(funs(as.numeric),lat:lon)
        vert.names = unique(vertnet.data$scientificname)
        
      }else if (any(grepl("scientificname",colnames(vertnet.data)))==TRUE){
        # eliminate records with no scientific name
        vertnet.data = as.data.frame(vertnet.data$data)
        vert.names = unique(vertnet.data$scientificname)
      }
      if (!length(vert.names)==0){
        
        vertnet.data = vertnet.data %>% tbl_df %>% select(scientificname, decimallongitude, decimallatitude) %>% 
          mutate(source = "vertnet") %>% 
          setNames(c('species', 'lat', 'lon',"source")) %>% # rename columns
          mutate_each(funs(as.character),lat:lon)%>% 
          mutate_each(funs(as.numeric),lat:lon)
        
      }} else {
        
        vertnet.data = matrix(0,nrow=0,ncol=4) %>% data.frame %>% tbl_df %>% 
          setNames(c("species","lat","lon","source"))
      }
    biodiversity.sp = rbind(biodiversity.sp, vertnet.data)
    biodiversity.sp = biodiversity.sp[!duplicated(biodiversity.sp[,c('species', 'lat', 'lon')]),]
  }
}

setwd(savepath)
write.csv(biodiversity.sp,file="record_queries6.csv")
print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))

# get ebird records
print("Now querying ebird")
biodiversity.sp = matrix(0,nrow=0,ncol=4) %>% as.data.frame %>% 
  setNames(c("species","lat","lon","source")) 

regions.ebird = c('MX-SON','MX-BCN','MX-SIN','MX-BCS','MX-NAY','MX-JAL')
for(eachregion in 1:length(regions.ebird))
{
  this.region = regions.ebird[eachregion]#region
  ebird.data <- ebirdregion(this.region, max=10000)
  
  if(nrow(ebird.data)!=0)  {
    ebird.data = ebird.data %>% 
      select(sciName,lat,lng) %>% 
      setNames(c('species', 'lat', 'lon')) %>% 
      mutate(source = "ebird") %>% 
      mutate_each(funs(as.character),lat:lon)%>% 
      mutate_each(funs(as.numeric),lat:lon)#make sure lon and lat are numeric
  } else {
    ebird.data = matrix(0,nrow=0,ncol=4) %>% data.frame %>% tbl_df %>% 
      setNames(c("species","lat","lon","source"))
    
  }
  biodiversity.sp = rbind(biodiversity.sp, ebird.data)
  
}

#' bind spatial records and eliminate duplicates
biodiversity.sp = biodiversity.sp %>% 
  mutate_each(funs(as.character),lat:lon) %>% 
  mutate_each(funs(as.numeric),lat:lon)

biodiversity.sp = biodiversity.sp[!duplicated(biodiversity.sp[,c('species', 'lat', 'lon')]),]


#' print records
print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))

setwd(savepath)
write.csv(biodiversity.sp,"record_queries7.csv")
print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))

biodiversity.sp = matrix(0,nrow=0,ncol=4) %>% as.data.frame %>% 
  setNames(c("species","lat","lon","source")) 


print("Now combining pre-existing data")
print("Reading Ulloa et al. 2006 files")

# now retreive records from other databases
setwd(ulloafiles)
csv.files <- list.files(pattern = "\\.csv$")# list files

for(eachfile in 1:length(csv.files))
{
  print(paste("Analyzing"," file",eachfile,"_",csv.files[eachfile]))
  
  ulloa.data = fread(csv.files[eachfile], header=TRUE) %>% 
    tbl_df %>% 
    select(NOM_CIEN, LATITUD, LONGITUD) %>% 
    mutate(source = "ulloa") %>% 
    setNames(c("species","lat","lon","source")) %>% 
    mutate_each(funs(as.character),lat:lon)%>% 
    mutate_each(funs(as.numeric),lat:lon) 
  
  biodiversity.sp = rbind(biodiversity.sp,ulloa.data)
}

setwd(savepath)
write.csv(biodiversity.sp,"record_queries8.csv")
print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))

print("Reading OBIS files")

biodiversity.sp = matrix(0,nrow=0,ncol=4) %>% as.data.frame %>% 
  setNames(c("species","lat","lon","source"))

#' combine obis data downloaded from their website
setwd(datafiles)#switch directory
csv.files <- list.files(pattern = "\\.csv$")#list csv files

#loop to read in data and obtain points in Gulf of California
for(eachfile in 1:length(csv.files)){
  
  obis.data  = fread(csv.files[eachfile],header=T, sep=",",select=c(1:11)) %>%
    tbl_df %>% 
    select(sname, latitude,longitude) %>% #subset needed variables
    setnames(c("species","lat","lon")) %>% 
    mutate(source = "obis") # set source
  print(paste("Analyzing file ",csv.files[eachfile],sep=""))
  biodiversity.sp = rbind(biodiversity.sp,obis.data)
}

setwd(savepath)
write.csv(biodiversity.sp,"record_queries9.csv")
print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))

print("Reading xls files")
biodiversity.sp = matrix(0,nrow=0,ncol=5) %>% as.data.frame %>% 
  setNames(c("species","lat","lon","source","file"))

#' read in xls files collated by Reef ecology lab
#' 
setwd(datafiles)
xls.files <- list.files(pattern = "\\.xlsx$")# list files
#loop to read in data and obtain GOC data
for(eachfile in 1:length(xls.files))
{
  
  print(paste("Analyzing"," file",eachfile,"_",xls.files[eachfile]))
  
  df = read_excel(xls.files[eachfile], sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0)
  indx.sp= grep("Species|Especie|Nombre|especie|nombre",colnames(df))
  indx.fuen= grep('Source|Fuente|fuente|informacion|Base',colnames(df))
  indx.lon= grep('Longitude|Longitud|longitud|Lon',colnames(df))
  indx.lat= grep('Latitud|latitud|Latutud|Lat',colnames(df))
  
  df2 = df[,c(indx.sp,indx.lat,indx.lon,indx.fuen)]
  
  uabcs.data = df2 %>% setNames(c("species","lat","lon","source")) %>% 
    mutate_each(funs(as.character),lat:lon)%>% 
    mutate_each(funs(as.numeric),lat:lon) %>% 
    mutate(file = xls.files[eachfile])
  
  uabcs.data = uabcs.data[complete.cases(uabcs.data),]#eliminate rows with NA
  
  print(uabcs.data[1,])
  biodiversity.sp = rbind(biodiversity.sp,uabcs.data)
  
}

setwd(savepath)
write.csv(biodiversity.sp,"record_queries10.csv")
print(paste("Records retreived:", nrow(biodiversity.sp),sep= " "))

#' subset only data in the Gulf
#' iterates on sets of data otherwise insufficient memory
#' create new data frame for clean records
setwd(savepath)
record.files <- list.files(pattern = "record_queries*")# list files

biodiversity.all = matrix(0,nrow=0,ncol=4) %>% data.frame %>% 
  tbl_df %>% 
  setnames(c("species","lat","lon","source"))

for(eachfile in 1:length(record.files))
{
  print(paste("Analyzing"," file",eachfile,"_",record.files[eachfile]))
  
  biodiversity = fread(record.files[eachfile], header=TRUE, select=c('species', 'lat', 'lon','source')) 
  
  biodiversity = biodiversity[!duplicated(biodiversity[,c('species', 'lat', 'lon')]),]
  #' eliminate rows with NA
  biodiversity = biodiversity[complete.cases(biodiversity),]#eliminate rows with NA
  
  biodiversity.all = rbind(biodiversity.all,biodiversity)
}

setwd(savepath)
write.csv(biodiversity.all,"record_queries_all.csv")
print(paste("Records retreived:", nrow(biodiversity.all),sep= " "))

biodiv.rows = nrow(biodiversity.all)
iterations = round(biodiv.rows/1000,0)

biodiversity.goc = matrix(0,nrow=0,ncol=4) %>% data.frame %>% 
  tbl_df %>% 
  setnames(c("species","lat","lon","source"))

last.row = 1001
first.row = 1

#for(eachrow in 1654759:biodiv.rows){

for (each.iteration in 2034:iterations)
  
{
  new.last.row = ((last.row-1)*each.iteration)
  
  
  ##
  print(paste("Analyzing row ",first.row," to ",new.last.row," out of ",biodiv.rows, sep=""))
  x = biodiversity.all[first.row:new.last.row,] %>% 
    na.omit %>% 
    mutate_each(funs(as.numeric),lat,lon) %>% 
    as.data.frame
  
  coordinates(x) <- c("lon", "lat")  # set spatial coordinates
  proj4string(x) <- crs.geo.wgs  # define projection system of our data
  
  # subset ocurrence points within GOC
  #get table from shapefile
  biodiversity.sec <- x[goc.shape, ] %>% as("data.frame")
  test.bio = nrow(biodiversity.sec)==0
  
  if (test.bio==FALSE)
  {
    biodiversity.goc = rbind(biodiversity.goc,biodiversity.sec)
    biodiversity.goc = biodiversity.goc[!duplicated(biodiversity.goc[,c('species', 'lon', 'lat')]),]
    print(paste("Gulf of California database has ",nrow(biodiversity.goc)," species records"))
  }
  
  first.row = new.last.row+1
}


setwd(savepath)
biodiversity.goc = biodiversity.goc[!duplicated(biodiversity.goc[,c('species', 'lon', 'lat')]),]
biodiversity.goc = biodiversity.goc[complete.cases(biodiversity.goc),]#eliminate rows with NA

write.csv(biodiversity.goc,"goc_species_ocurrence.csv")
print(paste("GOC records retreived:", nrow(biodiversity.goc),sep= " "))



    
## @knitr organizebiodiversity
rm(list=ls())    
savepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis"
    
setwd(savepath)


# read in file with all records for the goc 

fread("goc_species_ocurrence.csv") %>%  
  .$species %>% unique %>% 
  write.csv(file="biodiver_species_list.csv")

#' this table has to be read manually 
#' use it to delete genera with no species and higher taxa
#' eliminate authorities


## @knitr checktaxonomy    
    rm(list=ls())
#' this table is read after we have eliminated records not identified to species
#' auhtorities and combined with previously assessed taxonomy
#' see biodiver_species_list.xlsx

savepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis"
    
setwd(savepath)
    
rev.species.list = read.csv("missing_species_list.csv", header=T,na.strings="NA")
    
biodiv.species.names = rev.species.list %>% 
      setnames(c("Species")) %>% 
      .$Species %>% 
      unique %>% data.frame
  
names.goc = matrix(0,nrow=0,ncol=2) %>% data.frame %>% 
  tbl_df %>% 
  setnames(c("newname","oldname"))


for(eachname in 1:nrow(biodiv.species.names))
{
  oldname = biodiv.species.names[eachname,1] %>% as.character
  tryCatch(newname <- gnr_resolve(names = oldname, best_match_only = TRUE, preferred_data_sources = c(3,155,4,9,1,12,167,163,175,173,174,165,163)), error = function(cond) "skip") 
  
  #if(nrow(newname)!=0){
  acceptedname = newname$results  %>% tbl_df %>% select(matched_name) %>% data.frame %>% setnames("newname")
  acceptedname$oldname = oldname
  names.goc = rbind(names.goc,acceptedname)
  #}
}

 
write.csv(names.goc, file="master_taxonomy_list4.csv")
    
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


