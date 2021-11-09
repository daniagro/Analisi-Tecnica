Shooting_star_Inverted_hammer<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:length(ticker[,1])) {
    if((ticker$fr[i]<= 0.30 ) &(ticker$SR[i]== 2 ) & (ticker$COR[i]==4 | ticker$COR[i]==3 | ticker$COR[i]==5) ){
      posizione[i]<-1
    }
  }
  return(posizione)
}