#FUNZIONE PER DISTANZA TRA OPEN E CLOSE DELLA CANDELA
Dist_hi_lo<-function(ticker){
  
  dhl<-numeric(length = length(ticker[,1]))
  
  for(i in 1:length(dhl)){
    dhl[i]<-abs(ticker[i,2]-ticker[i,3])
  }
  return(dhl)
}