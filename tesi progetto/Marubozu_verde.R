Marubozu_verde<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:length(ticker[,1])) {
    if(  (ticker$CBR[i]==5)&(ticker$op[i]==ticker$lo[i])&ticker$cl[i]==ticker$hi[i] ){
      posizione[i]<-1
    }
  }
  return(posizione)
  
}