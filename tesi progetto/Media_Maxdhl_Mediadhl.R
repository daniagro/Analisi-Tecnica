#calcolo la media tra Max_dist_hi_lo e media_dist_hi_lo
# al periodo t
Media_Maxdhl_Mediadhl<-function(ticker){
  Max<-Max_dist_hi_lo(ticker)
  media<-media_dist_hi_lo(ticker)
  MediaMaxMedia<-(Max+media)/2
  return(MediaMaxMedia)
}