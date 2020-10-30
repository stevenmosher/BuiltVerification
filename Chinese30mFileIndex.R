source("SpatialConstants.R")
source("Libraries.R")
source('D:/GlobalHistoricalClimateNetwork/getInv_GHCN.R')
source('D:/GlobalHistoricalClimateNetwork/readInventory_GHCN.R')
source('D:/GlobalHistoricalClimateNetwork/filedate_GHCN.R')
source('D:/GlobalHistoricalClimateNetwork/getShapefile.R')
source('D:/GlobalHistoricalClimateNetwork/airportName.R')
source('D:/GlobalHistoricalClimateNetwork/newExtent.R')

ES<-"D:\\GlobalHistoricalClimateNetwork\\Rasters\\landcover30\\globemapsheet"
ESH<-"GlobeMapSheet"
ESHP <-read_sf(dsn=ES,layer=ESH)
CH_DIR1 <-"D:\\GlobalHistoricalClimateNetwork\\Rasters\\landcover30\\GlobeLand30_ATS2010_1"
CH_DIR2 <-"D:\\GlobalHistoricalClimateNetwork\\Rasters\\landcover30\\GlobeLand30_ATS2010_2"
CH_DIR3 <-"D:\\GlobalHistoricalClimateNetwork\\Rasters\\landcover30\\GlobeLand30_ATS2010_3"

F1<- list.files(CH_DIR1,pattern=".TIF",recursive = T ,full.names = T)
F2<- list.files(CH_DIR2,pattern=".TIF",recursive = T ,full.names = T)
F3<- list.files(CH_DIR3,pattern=".TIF",recursive = T ,full.names = T)

DF<-tbl_df(data.frame(Filename=c(F1,F2,F3),stringsAsFactors = F))

DF$Grid<-str_sub(basename(DF$Filename),1,6)
DF$Grid<-str_replace(DF$Grid,pattern="^S0","S")
DF$Grid<-str_replace(DF$Grid,pattern="^N0","N")
DF$Grid<-str_replace(DF$Grid,pattern="00$","0")
DF$Grid<-str_replace(DF$Grid,pattern="05$","5")
 
 

write.csv(DF,"Chinese30mIndex.csv")
