library(tidyverse)
library(readr)
library(sf)
library(rgdal)
library(raster)
source("createExtent.R")
source("extentToPoly.R")

Files<- list.files(GISDIR,pattern="tif",full.names = T)
DF<-tbl_df(data.frame(Filename=Files,LonMin=NA,LonMax=NA,LatMin=NA,LatMax=NA,
                      Grid=str_replace(basename(Files),".tif",""),geometry=NA ))

 
for(i in 1 :nrow(DF)){
  print(i)
  R<-raster(DF$Filename[i])
  DF$LonMin[i]<-xmin(R)
  DF$LonMax[i]<-xmax(R)
  DF$LatMin[i]<-ymin(R)
  DF$LatMax[i]<-ymax(R)
  if(DF$LonMin[i]< -180)DF$LonMin[i]<- -180
  if(DF$LonMax[i]>  180)DF$LonMax[i]<-  180
  m<-rbind(c(xmin(R),ymax(R)),c(xmax(R),ymax(R)),
           c(xmax(R),ymin(R)),c(xmin(R),ymin(R)),
           c(xmin(R),ymax(R)))
  m<-st_polygon(list(m)) 
  DF$geometry[i]<-st_sfc(m,crs=4326)
  
}

XF<-st_sf(DF,crs=4326) 

st_write(XF,dsn=GISDIR,driver="ESRI Shapefile", layer="GIS.shp")

DF<-DF %>% dplyr::select(-geometry)

write.csv(DF, file.path(GISDIR,"GISIndex.csv"))



 
write.csv(DF,"GISIndex.csv")
