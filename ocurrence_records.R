#'Hem Nalini Morzaria Luna
#'hmorzarialuna@gmail.com
#'Based on R script by Miguel Gandra || m3gandra@gmail.com || April 2015 
#' 
#'clean up the space
rm(list=ls())

#' Automatically install required libraries  

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
biodiversity.coordinates = fishbase.coordinates %>% tbl_df %>% 
  mutate(source = "fishbase") %>% 
  setNames(c("species","lat","lon","source")) %>% 
  mutate(lat=as.character(lat)) %>%  mutate(lat=as.numeric(lat)) %>% 
  mutate(lon=as.character(lon)) %>%  mutate(lon=as.numeric(lon)) 
#' eliminate empty species names and duplicate records
biodiversity.coordinates[biodiversity.coordinates==""]  <- NA
biodiversity.coordinates = na.omit(biodiversity.coordinates)
biodiversity.coordinates = biodiversity.coordinates[!duplicated(biodiversity.coordinates[,c('species', 'lat', 'lon')]),]

#' print records
print(paste("Records retreived:", nrow(biodiversity.coordinates),sep= " "))

#' retrieve iDigBio
#' can only retrieve records for all Mexico
biodiversity = matrix(0,nrow=0,ncol=3) %>% as.data.frame %>% 
setNames(c("scientificname","geopoint.lon","geopoint.lat")) 

print("Now querying iDigBio")
#vary the offset, the record where retrieval starts
for(eachnumber in 1:1000){
 print(paste("Analyzing record",eachnumber))
   thisoffset = 5000*eachnumber
  df1 <- idig_search_records(rq=list(country="mexico", geopoint=list(type="exists")),
                             fields=c("scientificname", "geopoint"), limit=5000,offset=thisoffset)
  biodiversity = rbind(biodiversity,df1)
  biodiversity = biodiversity[!duplicated(biodiversity[,c('scientificname', 'geopoint.lon', 'geopoint.lat')]),]
  }

setwd(savepath)
#use this to save the complete data file
write.csv(biodiversity,file="idigbio_Mexico.csv")

#modify data frame names and capitalization
biodiversity = biodiversity %>% tbl_df %>% 
select(scientificname,geopoint.lat,geopoint.lon) %>% 
  mutate(source = "idigbio") %>% 
           setNames(c("species","lat","lon","source"))%>% 
  mutate(species = capitalize(species))

#bind with fishbase results and eliminate duplicates
biodiversity.coordinates2 = rbind(biodiversity.coordinates, biodiversity)
biodiversity.coordinates2 = biodiversity.coordinates2[!duplicated(biodiversity.coordinates2[,c('species', 'lat', 'lon')]),]

#' print records
print(paste("Records retreived:", nrow(biodiversity.coordinates2),sep= " "))

#create new matrix for output of spatial queries
biodiversity.sp = matrix(0,nrow=0,ncol=4) %>% as.data.frame %>% 
  setNames(c("species","lat","lon","source")) 

#' use rbison, rgbif, ecoengine packages
#' tried spocc again but still buggy


print("Now querying spatial data rbison, rgbif, ecoengine")
for(eachpolygon in 1:length(poly.data)){
  
#select polygons or bounding box    
  this.polygon = poly.data[eachpolygon]#subset bounding box
  this.bb = bbox.data[eachpolygon]#subset bounding box
print(paste("Analyzing polygon ",eachpolygon,sep = " "))
  # obtain bison data  
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

# get gbif records
gbif.data = occ_search(geometry=this.polygon,return='data',fields=c("name","decimalLatitude","decimalLongitude","datasetName","collectionCode"),limit=200000) 
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
  
#' get ecoengine data
 ee.data.frame = tryCatch(ee_observations(page_size=10000, georeferenced = TRUE,bbox = this.bb),error = function(cond)"No records")
 #' arrange data frame only if records exist
 #' otherwise returns empty frame
if(ee.data.frame!="No records"){
 ee.data = ee.data.frame %>% .$data %>% tbl_df() %>% 
    select(scientific_name, latitude, longitude) %>% 
    setNames(c("name","lat","lon")) %>% 
    mutate(source = "ecoengine")
}else {
  ee.data = matrix(0,nrow=0,ncol=4) %>% data.frame %>% tbl_df %>% 
    setNames(c("species","lat","lon","source"))
}

#' bind gbif, bison, and ee data together
spatial.data = rbind(gbif.data,bison.data,ee.data)
spatial.data = spatial.data[!duplicated(spatial.data[,c('species', 'lat', 'lon')]),]

biodiversity.sp = rbind(biodiversity.sp, spatial.data)
biodiversity.sp = biodiversity.sp[!duplicated(biodiversity.sp[,c('species', 'lat', 'lon')]),]

  }

print("Now querying vertnet")
  # query Vertnet using spatial points  
  
  for(eachpoint in 1:nrow(goc.point.grid)) {
    this.point = goc.point.grid[eachpoint,]#point
    print(paste("Analyzing grid point ",eachpoint,sep = " "))
        point.lat = this.point[,2]
    point.lon = this.point[,1]
    
    vertnet.data <- spatialsearch(lat = point.lat, lon = point.lon, radius = 15000, limit = 1000, verbose= TRUE) #radius in meters
    #' only modify dataframe if records are available
    test.res = is.null(vertnet.data)
    if(test.res==FALSE)
    {
      # eliminate records with no scientific name
      vertnet.data = as.data.frame(vertnet.data$data)
      vert.names = unique(vertnet.data$scientificname)
      
      if (!length(vert.names)==0){
        
        vertnet.data = vertnet.data %>% tbl_df %>% select(scientificname, decimallongitude, decimallatitude) %>% 
          mutate(source = "vertnet") %>% 
          setNames(c('species', 'lat', 'lon',"source")) %>% # rename columns
          mutate(lat=as.character(lat)) %>%  mutate(lat=as.numeric(lat)) %>% 
          mutate(lon=as.character(lon)) %>%  mutate(lon=as.numeric(lon)) 
      }} else {
         
        vertnet.data = matrix(0,nrow=0,ncol=4) %>% data.frame %>% tbl_df %>% 
          setNames(c("species","lat","lon","source"))
               }
    biodiversity.sp = rbind(biodiversity.sp, vertnet.data)
    biodiversity.sp = biodiversity.sp[!duplicated(biodiversity.sp[,c('species', 'lat', 'lon')]),]
    
    }
  

# get ebird records
print("Now querying ebird")

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
      mutate_each(funs(as.numeric),lat:lon)#make sure lon and lat are numeric
 } else {
   ebird.data = matrix(0,nrow=0,ncol=4) %>% data.frame %>% tbl_df %>% 
     setNames(c("species","lat","lon","source"))
   
 }
 biodiversity.sp = rbind(biodiversity.sp, ebird.data)
 biodiversity.sp = biodiversity.sp[!duplicated(biodiversity.sp[,c('species', 'lat', 'lon')]),]
 
}

#' bind spatial records and eliminate duplicates
biodiversity.coordinates3 = rbind(biodiversity.coordinates2, biodiversity.sp)
#' eliminate duplicates
biodiversity.coordinates3 = biodiversity.coordinates3[!duplicated(biodiversity.coordinates3[,c('species', 'lat', 'lon')]),]
#' eliminate rows with NA
biodiversity.coordinates3 = biodiversity.coordinates3[complete.cases(biodiversity.coordinates3),]#eliminate rows with NA

#' print records
print(paste("Records retreived:", nrow(biodiversity.coordinates3),sep= " "))

print("Now combining pre-existing data")
print("Reading Ulloa et al. 2006 files")

# now retreive records from other databases
setwd(ulloafiles)
csv.files <- list.files(pattern = "\\.csv$")# list files
retrieved.sp = matrix(0,nrow=0,ncol=4) %>% data.frame %>% 
  setNames(c("species","lat","lon","source"))

for(eachfile in 1:length(csv.files))
{
  print(paste("Analyzing"," file",eachfile,"_",csv.files[eachfile]))
  
  ulloa.data = fread(csv.files[eachfile], header=TRUE) %>% 
    tbl_df %>% 
    select(NOM_CIEN, LATITUD, LONGITUD) %>% 
    mutate(source = "ulloa") %>% 
    setNames(c("species","lat","lon","source")) %>% 
    mutate_each(funs(as.numeric),lat:lon) 
  
  retrieved.sp = rbind(retrieved.sp,ulloa.data)
}

print("Reading OBIS files")

#' combine obis data downloaded from their website
setwd(datafiles)#switch directory
csv.files <- list.files(pattern = "\\.csv$")#list csv files

#loop to read in data and obtain points in Gulf of California
for(eachfile in 1:length(csv.files)){
  
  obis.data  = fread(csv.files[eachfile],header=T, sep=",",select=c(1:11)) %>%
    tbl_df %>% 
    select(sname, latitude,longitude) %>% #subset needed variables
    setnames(c("name","lat","lon")) %>% 
mutate(source = "obis") # set source
  print(paste("Analyzing file ",csv.files[eachfile],sep=""))
  retrieved.sp = rbind(retrieved.sp,obis.data)
}

print("Reading UABCS files")

#' read in xls files collated by Reef ecology lab
#' 
xls.files <- list.files(pattern = "\\.xlsx$")# list files
this.source="uabcs"
#loop to read in data and obtain GOC data
for(eachfile in 1:length(xls.files))
{
  
  print(paste("Analyzing"," file",eachfile,"_",xls.files[eachfile]))
  
  df = read_excel(xls.files[eachfile], sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0)
  indx.sp= grep("Especie|Nombre|especie|nombre",colnames(df))
  indx.fuen= grep('Fuente|fuente|informacion|Base',colnames(df))
  indx.lon= grep('Longitud|longitud',colnames(df))
  indx.lat= grep('Latitud|latitud|Latutud',colnames(df))
  
  df2 = df[,c(indx.sp,indx.lat,indx.lon,indx.fuen)]
  
  uabcs.data = df2 %>% setNames(c("species","lat","lon","source")) %>% 
    mutate_each(funs(as.numeric),lat:lon) 

  retrieved.sp = rbind(retrieved.sp,uabcs.data)
  
}

#' bind spatial records and eliminate duplicates
biodiversity.coordinates4 = rbind(biodiversity.coordinates3, retrieved.sp)
#' eliminate duplicates
biodiversity.coordinates4 = biodiversity.coordinates4[!duplicated(biodiversity.coordinates4[,c('species', 'lat', 'lon')]),]
#' eliminate rows with NA
biodiversity.coordinates4 = biodiversity.coordinates4[complete.cases(biodiversity.coordinates4),]#eliminate rows with NA

#' print records
print(paste("Records retreived:", nrow(biodiversity.coordinates4),sep= " "))

#' subset only data in the Gulf
#' iterates on sets of data otherwise insufficient memory
#' create new data frame for clean records

biodiversity = matrix(0,nrow=0,ncol=4) %>% data.frame %>% 
  tbl_df %>% 
  setnames(c("species","lat","lon","source"))

biodiv.rows = nrow(biodiversity.coordinates4)
iterations = round(biodiv.rows/1000,0)

biodiversity.clean = biodiversity.coordinates4

  last.row = 1001
  first.row = 1
  
  for (eachiteration in 1:iterations)  {
    new.last.row = ((last.row-1)*eachiteration)
    section.biodiv = biodiversity.clean[first.row:new.last.row,] %>% 
      na.omit %>% 
      mutate_each(funs(as.numeric),lat,lon) %>% 
      as.data.frame
    
    coordinates(section.biodiv) <- c("lon", "lat")  # set spatial coordinates
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
      biodiversity = biodiversity[!duplicated(biodiversity[,c('species', 'lon', 'lat')]),]
      print(paste("Gulf of California database has ",nrow(biodiversity)," species records"))
      }
    
    first.row = new.last.row+1
  }

setwd(savepath)
write.csv(biodiversity,file="goc_biodiversity.csv")