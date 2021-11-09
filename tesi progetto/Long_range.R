Long_range<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:length(ticker[,1])) {
    if(  ticker$CBR[i]==4 | ticker$CBR[i]==5 ){
      posizione[i]<-1
    }
  }
  return(posizione)
  
}