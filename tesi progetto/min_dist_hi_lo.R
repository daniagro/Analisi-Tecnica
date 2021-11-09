#calcolo il minimo della distanza tra la high e la 
#low nella finestra temporale t-i,t con i=20 al periodo t


min_dist_hi_lo<-function(ticker){
  #i va da 20 fino a length(MSFT)
  mindhl<-numeric(length = length(ticker[,1]))
  dhl<-ticker[,7]#colonna sette è data dalla distanza tra high e low
  for (i in 20:length(mindhl)) {
    
    mindhl[i]<-min(dhl[(i-19):i])
  }
  return(mindhl)
}