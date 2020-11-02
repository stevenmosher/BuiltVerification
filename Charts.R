 
library(tidyverse)
library(readr)
library(sf)
library(rgdal)
library(raster)
library(ggplot2)
source("createExtent.R")
source("extentToSF.R")
source("st_bboxToExent.R")
source("extentToPoly.R")
source('D:/BuiltVerification/Filenames.R')

 

DATA<-tbl_df(read.csv(AIRPORT_GIS,stringsAsFactors = F)) %>%
  dplyr::select(-X) %>%
  dplyr::select(ID,AirportType,BuiltAreaSpot,BuiltAreaGHS,BuiltArea300m,
                BuiltAreaCH30m,AreaBuiltGIS,BuiltNLCD,BuiltAreaSpot) %>%
  rename(BuiltAreaGIS=AreaBuiltGIS)

S<- gather(DATA,key=Metric,value=Area,-ID,-AirportType) %>%
    dplyr::filter(!is.na(Area))

ggplot(S) +geom_histogram(aes(Area)) + facet_grid(AirportType~Metric,scales="free_y")
ggplot(S) +geom_boxplot(aes(x=AirportType,y=Area)) + facet_wrap(~Metric)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(S) + 
  geom_bar(mapping=aes(x=Metric, y=Area, fill = as.factor(Metric)), 
           position = "dodge",stat="summary")+
  facet_wrap(~AirportType)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


### form deltas
SPOT <- DATA %>% mutate(SPOT_CH=BuiltAreaSpot-BuiltAreaCH30m,
                        SPOT_GIS =BuiltAreaSpot-BuiltAreaGIS,
                        SPOT_GHS= BuiltAreaSpot-BuiltAreaGHS,
                        SPOT_ESA= BuiltAreaSpot-BuiltArea300m)

M300 <- DATA %>% mutate(M300_CH=BuiltArea300m-BuiltAreaCH30m,
                        M300_GIS =BuiltArea300m-BuiltAreaGIS,
                        M300_GHS= BuiltArea300m-BuiltAreaGHS,
                        M300_NLCD = BuiltArea300m-BuiltNLCD)

GIS <- DATA %>% mutate(GIS_CH=BuiltAreaGIS-BuiltAreaCH30m,
                        GIS_GHS= BuiltAreaGIS-BuiltAreaGHS,
                        GIS_NLCD = BuiltAreaGIS-BuiltNLCD)
