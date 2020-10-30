library(tidyverse)
library(readr)
library(sf)
library(rgdal)
library(raster)
source("createExtent.R") 

# GHSDIR

ES<-"D:\\GlobalHistoricalClimateNetwork\\Rasters\\GHSBuilt\\V2-0"
ESP<- "GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30_V2_0"


 
ESHP <-read_sf(dsn=ES,layer=ESP)

 

AIR<-tbl_df(read.csv(AIRPORTLC_NLCD,stringsAsFactors = F)) %>%
      dplyr::select(-X)
AIR <- st_as_sf(AIR,coords = c("Longitude","Latitude"),crs=4326)
AIR <- st_transform(AIR,crs=crs(ESHP))
 
 
Files<-st_join(AIR,ESHP) %>% rename(Center=location)
(nrow(Files)==nrow(AIR))

 
UL<- Files %>% st_drop_geometry() %>% rename(Longitude=LonMin,Latitude=LatMax) %>% dplyr::select(-Center)
UL <- st_as_sf(UL,coords = c("Longitude","Latitude"),crs=4326)
UL <- st_transform(UL,crs=crs(Files)) 
UR<- Files %>% st_drop_geometry() %>% rename(Longitude=LonMax,Latitude=LatMax)%>% dplyr::select(-Center)
UR <- st_as_sf(UR,coords = c("Longitude","Latitude"),crs=4326)
UR <- st_transform(UR,crs=crs(Files))
LR<- Files %>% st_drop_geometry() %>% rename(Longitude=LonMax,Latitude=LatMin)%>% dplyr::select(-Center)
LR <- st_as_sf(LR,coords = c("Longitude","Latitude"),crs=4326) 
LR <- st_transform(LR,crs=crs(Files))
LL<- Files %>% st_drop_geometry() %>% rename(Longitude=LonMin,Latitude=LatMin)%>% dplyr::select(-Center)
LL <- st_as_sf(LL,coords = c("Longitude","Latitude"),crs=4326)
LL <- st_transform(LL,crs=crs(Files))
UL<-st_join(UL,ESHP) %>% rename(UL=location)
UR<-st_join(UR,ESHP) %>% rename(UR=location)
LR<-st_join(LR,ESHP) %>% rename(LR=location)
LL<-st_join(LL,ESHP) %>% rename(LL=location)
UL<-UL %>% st_drop_geometry() %>% dplyr::select(ID,UL)
UR<-UR %>% st_drop_geometry() %>% dplyr::select(ID,UR)
LR<-LR %>% st_drop_geometry() %>% dplyr::select(ID,LR)
LL<-LL %>% st_drop_geometry() %>% dplyr::select(ID,LL)
GEOM <-Files %>% dplyr::select(ID) 
Files<-Files %>% st_drop_geometry()
Files <- left_join(Files,UL, by="ID")
Files <- left_join(Files,UR, by="ID")
Files <- left_join(Files,LR, by="ID")
Files <- left_join(Files,LL, by="ID")

identical(Files$ID,GEOM$ID)

FC<- st_coordinates(GEOM)
Files <- Files %>% mutate(X=FC[,1],Y=FC[,2])

NC <-Files %>% group_by(ID) %>% 
  summarise(NaCount= is.na(UL)+is.na(UR)+is.na(LR)+is.na(LL),
            UniqueCount = length(unique(c(Center,UL,UR,LR,LL))))

IB <- NC %>% dplyr::filter(UniqueCount==1)
Files <- Files %>% dplyr::filter(ID %in% IB$ID)
Files <- Files %>% dplyr::select(ID,X,Y,Center)

########################################

Files <- Files %>% mutate(BuiltAreaGHS=NA)

#0  Missing 
#1 Water 
#2 Unbuilt 
#3 B2014  
# 4 B2000 
# 5 B1990 
# 6 B1975 

for(i in 1:nrow(Files)){
  print(i)
  if(!is.na(Files$Center[i])){
  e<- extent(Files$X[i]-1500,Files$X[i]+1500,Files$Y[i]-1500,Files$Y[i]+1500)
  R<- raster(file.path(GHSDIR,Files$Center[i]))
  R<-crop(R,e)
  R<- Which(R>2)
  A<-area(R)*R
  Files$BuiltAreaGHS[i] <- cellStats(A,stat="sum")/1000000
  }
  
}
 

AIR<-left_join(AIR,Files,by="ID") %>%
     dplyr::select(-X,-Y,-Center)

A <-tbl_df(read.csv(AIRPORTLC_NLCD,stringsAsFactors = F)) %>%
  dplyr::select(ID,Longitude,Latitude)

A<-left_join(A,AIR,by="ID")

A<-A %>% dplyr::select(-geometry )

write.csv(A,"AirportBuiltGHS.csv")