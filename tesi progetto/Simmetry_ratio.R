#CALCOLO DEL FATTORE DI SIMMETRIA SR=SIMMETRY RATIO



source("Ombre.R")



Simmetry_ratio<-function(strumento){
  ticker=strumento
  ombre<-Ombre(ticker)
  ticker<-cbind(ticker,ombre)
  
  SR<-numeric(length = length(ticker[,1]))
  
  for(i in 1:length(ticker[,1])){
    if(ticker$ts[i]>0 | ticker$bs[i]>0){
      sr<-(ticker$ts[i]-ticker$bs[i])/(ticker$ts[i]+ticker$bs[i])
      if(0.6<sr & sr<=1){SR[i]=2}#high top asimmetry
      if(0.2<sr & sr<=0.6){SR[i]=1}#top asimmetry
      if(-0.2<=sr & sr<=0.2){SR[i]=0}#simmetry
      if(-0.6<=sr & sr< -0.2){SR[i]=-1}#bottom asimmetry
      if(-1<=sr & sr< -0.6){SR[i]=-2}#high bottom asimmetry
    }else{
      SR[i]=0
    }
    
  }
  return(SR)
}
