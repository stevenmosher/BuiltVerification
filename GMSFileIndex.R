library(tidyverse)
library(readr)
library(sf)
library(rgdal)
library(raster)
source("createExtent.R") 

 
Files<- list.files(GMSDIR,pattern="tif",full.names = T)
DF<-tbl_df(data.frame(Filename=Files,LonMin=NA,LonMax=NA,LatMin=NA,LatMax=NA,
                      Grid=str_sub(basename(Files),1,3),geometry=NA ))

M<-list()
for(i in 1 :nrow(DF)){
  print(i)
  R<-raster(DF$Filename[i])
  DF$LonMin[i]<-xmin(R)
  DF$LonMax[i]<-xmax(R)
  DF$LatMin[i]<-ymin(R)
  DF$LatMax[i]<-ymax(R)
  m<-rbind(c(xmin(R),ymax(R)),c(xmax(R),ymax(R)),
           c(xmax(R),ymin(R)),c(xmin(R),ymin(R)),
           c(xmin(R),ymax(R)))
  m<-st_polygon(list(m)) 
  DF$geometry[i]<-st_sfc(m,crs=4326)
  
}

XF<-st_sf(DF,crs=4326) 
 
st_write(XF,dsn=GMSDIR,driver="ESRI Shapefile", layer="GMIS.shp")

DF<-DF %>% dplyr::select(-geometry)

write.csv(DF,"GMISIndex.csv")
