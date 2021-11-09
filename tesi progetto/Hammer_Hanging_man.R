Hammer_Hanging_man<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:length(ticker[,1])) {
    if((ticker$fr[i]<= 0.30 ) &(ticker$SR[i]== -2 ) & (ticker$COR[i]==4 | ticker$COR[i]==3 | ticker$COR[i]==5) ){
      posizione[i]<-1
    }
  }
  return(posizione)
}

#for (i in 1:length(ticker[,1])) {
 # if((ticker$CBR[i]==1 | ticker$CBR[i]==2) &(ticker$SR[i]== -2 | ticker$SR[i]==2) & (ticker$COR[i]==1 | ticker$COR[i]==2) ){
  #  posizione[i]<-1
  #}
#}
#return(posizione)
#}