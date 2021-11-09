Morning_Star<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:(length(ticker[,1])-2)) {

    if( (coredata(ticker$op[i])>coredata(ticker$cl[i])  )&
        (coredata(ticker$Longrange[i])==1 )&
        (coredata(ticker$Shortrange[i+1])==1 | coredata(ticker$Doji[i+1])==1 )&
        (max(coredata(ticker$op[i+1]),coredata(ticker$cl[i+1]))<coredata(ticker$cl[i]))&
        (coredata(ticker$op[i+2])<coredata(ticker$cl[i+2]))&
        (max(coredata(ticker$op[i+1]),coredata(ticker$cl[i+1]))<coredata(ticker$op[i+2]))
        
        
        
        )
       
    {
      posizione[i]<-1
    }
  }
  return(posizione)
  
  
}