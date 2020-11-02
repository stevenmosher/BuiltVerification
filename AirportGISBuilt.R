 
library(tidyverse)
library(readr)
library(sf)
library(rgdal)
library(raster)
source("createExtent.R")
source("extentToSF.R")
source("st_bboxToExent.R")
source("extentToPoly.R")

source('D:/BuiltVerification/Filenames.R')


 
ESH<-"GIS"
ESHP <-read_sf(dsn=GISDIR,layer=ESH)
 
AIR<-tbl_df(read.csv(CHINA_BUILT30M,stringsAsFactors = F)) %>%
     dplyr::select(-X)
LL<-extentToSF(Inventory=AIR)

 
#LL <- st_transform(LL,crs=crs(ESHP)) 
 
Files<-st_join(LL,ESHP, join=st_within)
nrow(LL)
nrow(Files)
### note the duplicates in the overlap zones
Files<-Files %>%  dplyr::filter(!is.na(Filename))%>%
          dplyr::select(ID,Filename)
nrow(Files)
sum(duplicated(Files$ID))
 
Files<-Files %>% mutate(AreaBuiltGIS=NA)

for(i in 1:nrow(Files)){
  print(i)
  R<-raster(Files$Filename[i])
  e<-st_bboxToExent(st_bbox(Files$geometry[i]))
  R<-crop(R,e)
  R<-Which(R==2)
  A<-area(R)*R
  Files$AreaBuiltGIS[i]<- cellStats(A,stat="sum") 
  
}

dex<-duplicated(Files$ID)
dupID<- Files$ID[dex]
UniqueID <-setdiff(Files$ID,dupID)
DUPS<-Files%>% dplyr::filter(ID %in% dupID)
D<-DUPS %>% group_by(ID) %>% summarise(AreaBuiltGIS=max(AreaBuiltGIS,na.rm=T))
D<-D%>% st_drop_geometry()
Files<-Files %>% dplyr::filter(ID %in% UniqueID)%>%
       st_drop_geometry()%>% dplyr::select(ID,AreaBuiltGIS)
Files<-bind_rows(Files,D)

AIR <-left_join(AIR,Files, by="ID")
 
 

write.csv(AIR,"AirportBuiltGIS.csv")
