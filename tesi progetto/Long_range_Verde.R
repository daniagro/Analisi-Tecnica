Long_range_Verde<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:length(ticker[,1])) {
    if(  (ticker$CBR[i]==4 | ticker$CBR[i]==5)& (ticker$op[i]<ticker$cl[i]) ){
      posizione[i]<-1
    }
  }
  return(posizione)
  
}