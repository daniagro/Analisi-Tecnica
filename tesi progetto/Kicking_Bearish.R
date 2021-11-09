Kicking_Bearish<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:(length(ticker[,1])-1)) {
    
    if(coredata((ticker$op[i])<coredata(ticker$cl[i]))&
       (coredata(ticker$op[i])==coredata(ticker$lo[i]))&
       (coredata(ticker$cl[i])==coredata(ticker$hi[i]))&
       (coredata(ticker$op[i])>coredata(ticker$op[i+1]))&
       (coredata(ticker$op[i+1])==coredata(ticker$hi[i+1]))&
       (coredata(ticker$cl[i+1])==coredata(ticker$lo[i+1]))&
       (coredata(ticker$op[i+1])>coredata(ticker$cl[i+1]))) 
      
    {
      posizione[i]<-1
    }
  }
  return(posizione)
}