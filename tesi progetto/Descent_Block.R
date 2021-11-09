Descent_Block<-function(ticker){
  posizione<-rep(0,length(ticker[,1]))
  
  for (i in 1:(length(ticker[,1])-2)) {
    
    if( (coredata(ticker$op[i])>coredata(ticker$cl[i]))&     
        (coredata(ticker$Longrange[i])==1)&
        (coredata(ticker$op[i+1])>coredata(ticker$cl[i+1]))&
        (coredata(ticker$CBR[i+1])<=3)&
        (coredata(ticker$cl[i+2])<coredata(ticker$op[i+2]))&
        (coredata(ticker$CBR[i+2])<=2)&
        
        (coredata(ticker$op[i])>coredata(ticker$op[i+1]))&
        (coredata(ticker$op[i+1])>coredata(ticker$op[i+2]))&
        
        (coredata(ticker$cl[i])<coredata(ticker$op[i+1]))&
        (coredata(ticker$cl[i+1])<coredata(ticker$op[i+2]))&
        
        (coredata(ticker$cl[i+1])<coredata(ticker$cl[i]))&
        (coredata(ticker$cl[i+2])<coredata(ticker$cl[i+1]))
        )
    {
      posizione[i]<-1
    }
  }
  return(posizione)
}