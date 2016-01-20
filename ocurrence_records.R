#'Hem Nalini Morzaria Luna
#'hmorzarialuna@gmail.com
#'Based on R script by Miguel Gandra || m3gandra@gmail.com || April 2015 
#' 
#'clean up the space
rm(list=ls())

#' Automatically install required libraries  

if(!require(dismo)){install.packages("dismo"); library(dismo)}
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



workpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/RCode"
setwd(workpath)
shapepath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/SIG_Biodiversity"

# projections
crs.geo.wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # geographical, datum WGS84
crs.lcc <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=0 +lon_0=-102 +x_0=2000000 +y_0=0 +datum=NAD27 +units=m +no_defs")

setwd(shapepath)
#read in Gulf of California shapefile
#goc.shape <- readOGR(".", "Golfo_california_polygon_WSG84")#without wetlands
goc.shape <- readOGR(".", "Golfo_california_wetland_poly_WGS84")#with wetlands


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
#' goc.sample.points = spsample(goc.pol, n = 1000, "regular")
goc.point.grid = makegrid(goc.pol, n = 1000, pretty=FALSE)
#' create list of WKT polygons
#' 
boxes = 29*34 # rows of lat vs rows of lon
wkt.data = matrix(1:boxes, ncol=1)
#' create new counter
counter=0

for (i in 1:boxes)
{
 corner1 = goc.point.grid[(1*counter)+1,] %>% paste(collapse=" ")
 corner2 = goc.point.grid[(1*counter)+2,] %>% paste(collapse=" ")
 corner3 = goc.point.grid[(1*counter)+32,] %>% paste(collapse=" ")
 corner4 = goc.point.grid[(1*counter)+31,] %>% paste(collapse=" ")
 
  coords = paste(corner1, corner2,corner3, corner4, corner1,sep=" ,")
  wkt.data[i]=paste("POLYGON((",coords,"))", sep="")
  counter=counter+1
  print(counter)
  
}
poly.data = as.vector(wkt.data)

###############
#' Record retrieval
#' First retrieve from data bases that don't take bounding boxes
#' this is FishBase and iDigBio
#' retrieve all records for an ecosystem
#' Gulf of California = 165
#' 
site = 165 # 144 Gulf of Mexico, 132 California Current, 145 Caribbean

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

for(eachnumber in 51:100){
  thisoffset = 5000*eachnumber
  df1 <- idig_search_records(rq=list(country="mexico", geopoint=list(type="exists")),
                             fields=c("scientificname", "geopoint"), limit=5000,offset=thisoffset)
  biodiversity = rbind(biodiversity,df1)
}

biodiversity = biodiversity[!duplicated(biodiversity[,c('scientificname', 'geopoint.lon', 'geopoint.lat')]),]


gbif.res = occ(geometry = eachpolygon, from = c("vertnet","gbif","bison","ebird","ecoengine"), limit=50)



#subset only data in the Gulf
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