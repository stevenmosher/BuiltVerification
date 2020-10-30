library(tidyverse)
library(readr)
library(sf)
library(rgdal)
library(raster)
source("createExtent.R")

##  airports
##  slected air
##  land class


LC <- raster(LANDCLASS)
USLC<- raster(NLCD)
AIR<- read_csv(SELECTEDAIR)

AIR<-AIR %>% mutate(LonMin=NA,LonMax=NA,LatMin=NA,LatMax=NA)

for(i in 1:nrow(AIR)){
  print(i)
  e <-createExtent(lon=AIR$Longitude[i],lat=AIR$Latitude[i],radius=1500)
  AIR$LonMin[i]<- xmin(e)
  AIR$LonMax[i]<- xmax(e)
  AIR$LatMin[i]<- ymin(e)
  AIR$LatMax[i]<- ymax(e)
  
}

# 
AIR<-AIR %>% mutate(SiteCode=NA,BuiltArea300m=NA)
AIR$SiteCode<- raster::extract(LC,y=cbind(AIR$Longitude,AIR$Latitude))

for(i in 1:nrow(AIR)){
  print(i)
  e<-extent(AIR$LonMin[i],AIR$LonMax[i],AIR$LatMin[i],AIR$LatMax[i])
  R<-crop(LC,e)
  R<-Which(R==190)
  R<-disaggregate(R,fact=10)
  A<-area(R)*R
  AIR$BuiltArea300m[i]<-cellStats(A,stat="sum")
  
}

## next up need to do the US airports
##  select those with ISO = US
##  clip to Conus
US <- AIR %>% dplyr::filter(ISO=="US" & Latitude < 55 & Longitude > -150 & Longitude< -40)
US <- st_as_sf(US,coords = c("Longitude","Latitude"),crs=4326)
US <- st_transform(US,crs=crs(USLC))

US <- US %>% mutate(BuiltNLCD=NA)
for(i in 1:nrow(US)){
  print(i)
  latlon<-cbind(st_coordinates(US[i,])[1],st_coordinates(US[i,])[2])
  e<-extent(st_coordinates(US[i,])[1]-1500, st_coordinates(US[i,])[1]+1500,
            st_coordinates(US[i,])[2]-1500, st_coordinates(US[i,])[2]+1500)
  B<- crop(USLC,e)
  B<- B/100
  B<-B*900
  US$BuiltNLCD[i]<-cellStats(B,stat="sum")/1000000 
  
}

US <- US %>% st_drop_geometry() %>% dplyr::select(ID,BuiltNLCD)
AIR<- left_join(AIR,US, by="ID")

write.csv(AIR,"AirportLC_NLCD.csv")
# drop geometry and join and outpout
 