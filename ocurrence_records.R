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


workpath = "E:/Archivos/1Archivos/Articulos/En preparacion/Biodiversity_model/Analysis/RCode"
setwd(workpath)
#' retrieve all records for an ecosystem
#' Gulf of California = 165
#' 
site = 165 # 144 Gulf of Mexico, 132 California Current, 145 Caribbean

url <- paste("http://www.fishbase.org/map/EcosystemOccurrencesList.php?e_code=",
             site,sep="")

fishbase.data <- try(readHTMLTable(url),silent=TRUE)

fishbase.data.1 = fishbase.data[[3]] %>% data.frame
fishbase.coordinates <- data.frame(fishbase.data.1[,2], fishbase.data.1[,5],fishbase.data.1[,6])

#' define a helper function to replace empty values
empty_as_na <- function(x) ifelse(x=="", NA, x)

fishbase.coordinates.1 = fishbase.coordinates %>% setNames(c("Species","Lat","Long")) %>% 
  mutate(Lat=as.character(Lat)) %>%  mutate(Lat=as.numeric(Lat)) %>% 
  mutate(Long=as.character(Long)) %>%  mutate(Long=as.numeric(Long)) %>% 
  tbl_df %>% mutate_each(funs(empty_as_na)) %>% na.omit

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



gbif.res = occ(geometry = eachpolygon, from = c("vertnet","gbif","bison","ebird","ecoengine"), limit=50)

biodiversity = matrix(0,nrow=0,ncol=3) %>% as.data.frame %>% 
setNames(c("scientificname","geopoint.lon","geopoint.lat"))

for(eachnumber in 1:20){
  thisoffset = 5000*eachnumber
  df1 <- idig_search_records(rq=list(country="mexico", geopoint=list(type="exists")),
                             fields=c("scientificname", "geopoint"), limit=5000,offset=thisoffset)
  biodiversity = rbind(biodiversity,df1)
  }

write.csv(biodiversity,file="idigbio_Mexico.csv")

