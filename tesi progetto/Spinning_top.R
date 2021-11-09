Spinning_top<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:length(ticker[,1])) {
    if(  (ticker$CBR[i]==2 | ticker$CBR[i]==1)&(ticker$COR[i]==3 | ticker$COR[i]==4 | ticker$COR[i]==5)&(ticker$SR[i]==0 | ticker$SR[i]== -1 | ticker$SR[i]==1) ){
      posizione[i]<-1
    }
  }
  return(posizione)
  
}