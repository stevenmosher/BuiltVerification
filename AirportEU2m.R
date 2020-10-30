source("SpatialConstants.R")
source("Libraries.R")
source('D:/GlobalHistoricalClimateNetwork/getInv_GHCN.R')
source('D:/GlobalHistoricalClimateNetwork/readInventory_GHCN.R')
source('D:/GlobalHistoricalClimateNetwork/filedate_GHCN.R')
source('D:/GlobalHistoricalClimateNetwork/getShapefile.R')
source('D:/GlobalHistoricalClimateNetwork/airportName.R')
source('D:/GlobalHistoricalClimateNetwork/newExtent.R')

ES<-"D:\\GlobalHistoricalClimateNetwork\\Rasters\\ESM_BUILT_VHR2015_EUROPE_R2019_3035_02_V1_0"
ESH<-"index"
ESHP <-read_sf(dsn=ES,layer=ESH)
 
AIR<-tbl_df(read.csv(AIRPORT_GHS)) %>%
     dplyr::select(-X)



EU <- AIR %>% dplyr::filter(Latitude < 72 & Longitude > -32.7 & Longitude<  45.42 & Latitude>26)
 

EU <- st_as_sf(EU,coords = c("Longitude","Latitude"),crs=4326)
EU <- st_transform(EU,crs=crs(ESHP)) 
ESHP<-st_transform(ESHP,crs=crs(EU))

Files<-st_join(EU,ESHP)
nrow(Files)
Files<- Files %>% dplyr::filter(!is.na(location)) %>% rename(Center=location)
nrow(Files)

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

Files <- Files %>% mutate(BuiltAreaSpot=NA)

 

for(i in 1:nrow(Files)){
  print(i)
   
    e<- extent(Files$X[i]-1500,Files$X[i]+1500,Files$Y[i]-1500,Files$Y[i]+1500)
    R<- raster(file.path(SPOTDIR,Files$Center[i]))
    R<-crop(R,e)
    R<- Which(R==255)
    A<-area(R)*R
    Files$BuiltAreaSpot[i] <- cellStats(A,stat="sum")/1000000
   
  
}

Files<-Files %>% dplyr::select(ID,BuiltAreaSpot)
AIR<-left_join(AIR,Files,by="ID") 

write.csv(AIR,"AirportBuiltSpot.csv")
