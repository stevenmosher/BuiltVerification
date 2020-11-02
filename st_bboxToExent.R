st_bboxToExent<-function(bb){
  
  e<- extent(bb$xmin,bb$xmax,bb$ymin,bb$ymax)
  return(e)
}