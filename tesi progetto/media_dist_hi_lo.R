#calcolo la media delle distanze tra la high e la low 
# nella finestra temporale t-i,t con i=20 al periodo t

media_dist_hi_lo<-function(ticker){
  #i va da 20 fino a length(MSFT)
  mediadhl<-numeric(length = length(ticker[,1]))
  dhl<-ticker[,7]#colonna sette � data dalla distanza tra high e low
  for (i in 20:length(mediadhl)) {
    
    mediadhl[i]<-mean(dhl[(i-19):i])
  }
  return(mediadhl)
}