
extentToPoly <- function(lonmin,lonmax,latmin,latmax){
  m<-rbind(c(lonmin,latmax),c(lonmax,latmax),
           c(lonmax,latmin),c(lonmin,latmin),
           c(lonmin,latmax))
  m<-st_polygon(list(m)) 
  m<-st_sfc(m,crs=4326)
  return(m)
  
}
  