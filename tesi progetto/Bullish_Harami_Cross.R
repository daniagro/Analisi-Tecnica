Bullish_Harami_Cross<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:(length(ticker[,1])-1)) {
    
    if(coredata((ticker$op[i])>coredata(ticker$cl[i]))&
       (coredata(ticker$baricentro[i+1])>=(coredata(ticker$cl[i])+0.01))& 
       (abs(coredata(ticker$cl[i+1])-coredata(ticker$op[i+1])))<=0.02)
      
    {
      posizione[i]<-1
    }
  }
  return(posizione)
}