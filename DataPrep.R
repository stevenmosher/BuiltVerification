library(tidyverse)
library(readr)
library(data.table)
library(ggplot2)

####  create a file recording the features used
Features<-read_delim(FEATURECODE,delim="\t",col_names = c("Code","Name",'Description'))
##  select features using regex
Codes <-c("AGRC","DEVH","GRAZ","INDS", "RESF", "SALT","MALL", "PKLT","RNCH","SQR","DSRT")
          
Kept <-vector()
for(i in Codes){Kept<-c(Kept,grep(i,Features$Code ))}
 
Features<-Features[Kept,]
write_csv(Features,"SelectedFeatureDescription.csv")

#######################################  read in all features
Data<- fread(FEATURES,quote = "")
Data<-Data[,c(5,6,7,8)]
Data<-Data[Data$V7 %in% c("L","R","S","T","V"),]
Data <- Data %>% rename(Latitude=V5,Longitude=V6,Code=V8)%>%
                 dplyr::select(-V7)
 
Data<-Data %>% dplyr::filter(Code %in% Codes) %>%
               dplyr::filter(Latitude<80 & Latitude > - 55)

Data<- Data %>% mutate(Built=case_when(Code %in% c("DEVH","INDS","SQR",
                                               "MALL","PKLT")~TRUE,
                                       TRUE~FALSE))
                                               
                                   
#400K data points
write_csv(Data,"GeonamesData.csv")
#####################################

AIR <- tbl_df(read_csv(AIRPORTS))
AIR <- AIR %>% rename(Latitude=latitude_deg,Longitude=longitude_deg)
AIR <- AIR %>% rename(AirportType=type,AirportName=name,ISO=iso_country,ID=id) %>%
               dplyr::select(ID,AirportName,AirportType,Longitude,Latitude,ISO) %>%
               dplyr::filter(AirportType %in% c("large_airport","medium_airport"))%>%
               dplyr::filter(Latitude<80 & Latitude > - 55)

write_csv(AIR,"AirportSelected.csv")

#########################
           
ggplot(AIR) + geom_bar(mapping=aes(x=AirportType,fill=AirportType)) +
  ggtitle("Airport counts")

ggplot(AIR) +geom_point(mapping=aes(x=Longitude,y=Latitude,color=AirportType))+
  ggtitle("Airport counts")                      

ggplot(Data) + geom_bar(mapping=aes(x=Code,fill=Code)) +
   
  ggtitle("Code counts")
         
         

 
          
          
    