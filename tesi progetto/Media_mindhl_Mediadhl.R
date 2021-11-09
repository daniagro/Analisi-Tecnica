#calcolo la media tra min_dist_hi_lo e media_dist_hi_lo 
# al periodo t

Media_mindhl_Mediadhl<-function(ticker){
  min<-min_dist_hi_lo (ticker)
  media<-media_dist_hi_lo (ticker)
  MediaminMedia<-(min+media)/2
  return(MediaminMedia)
}