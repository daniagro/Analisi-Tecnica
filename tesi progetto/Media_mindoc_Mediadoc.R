#calcolo la media tra min_dist_op_cl e media_dist_op_cl
# al periodo t

Media_mindoc_Mediadoc<-function(ticker){
  min<-min_dist_op_cl(ticker)
  media<-media_dist_op_cl(ticker)
  MediaminMedia<-(min+media)/2
  return(MediaminMedia)
}