Squeeze_Allert_Bearish<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:(length(ticker[,1])-2)) {
    
    if( (coredata(ticker$op[i])<coredata(ticker$cl[i])  )&              
        (coredata(ticker$CBR[i])>=2 )&
        (max(coredata(ticker$op[i+1]),coredata(ticker$cl[i+1]))<coredata(ticker$op[i]))&
        (min(coredata(ticker$op[i+1]),coredata(ticker$cl[i+1]))>coredata(ticker$cl[i]))&
        (max(coredata(ticker$op[i+2]),coredata(ticker$cl[i+2]))<max(coredata(ticker$op[i+1]),coredata(ticker$cl[i+1])))&
        (min(coredata(ticker$op[i+2]),coredata(ticker$cl[i+2]))>min(coredata(ticker$op[i+1]),coredata(ticker$cl[i+1])))
        
        
    )
      
    {
      posizione[i]<-1
    }
  }
  return(posizione)
  
  
}