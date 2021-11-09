Graveston_Doji<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:length(ticker[,1])) {
    if(  ( abs(ticker$op[i]-ticker$cl[i])<0.02)&(ticker$COR[i]==3 | ticker$COR[i]==4 | ticker$COR[i]==5)&(ticker$SR[i]==2) ){
      posizione[i]<-1
    }
  }
  return(posizione)
  
}