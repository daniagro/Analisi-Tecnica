#calcolo la media tra Max_dist_op_cl e media_dist_op_cl
# al periodo t
Media_Maxdoc_Mediadoc<-function(ticker){
  Max<-Max_dist_op_cl(ticker)
  media<-media_dist_op_cl(ticker)
  MediaMaxMedia<-(Max+media)/2
  return(MediaMaxMedia)
}