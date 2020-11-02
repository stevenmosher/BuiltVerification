extentToSF<- function(Inventory){
  
  Inventory<-Inventory %>% group_by(ID)%>%
             mutate(geometry=extentToPoly(lonmin=LonMin,lonmax=LonMax,latmin=LatMin,latmax=LatMax)) %>%
             ungroup()
  Inventory<- st_sf(Inventory,crs=4326) %>%
             dplyr::select(ID,Longitude,Latitude)
  return(Inventory)
  
}