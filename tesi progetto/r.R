#rapporto tra distanza della open dalla close 
#e distanza tra high e low


#FATTORE DI RIEMPIMENTO DELLA CANDELA
r<-function(ticker){
  fattoreRiempimento<-numeric(length = length(ticker[,1]))
  
  for (i in 1:length(fattoreRiempimento)) {
    fattoreRiempimento[i]<-abs(ticker[i,1]-ticker[i,4])/abs(ticker[i,2]-ticker[i,3])
    
  }
  return(fattoreRiempimento)
}