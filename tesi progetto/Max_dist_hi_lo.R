#calcolo il massimo della distanza tra la open e la 
#close nella finestra temporale t-i,t con i=20 al periodo t
Max_dist_hi_lo<-function(ticker){
  #i va da 20 fino a length(MSFT)
  Maxdhl<-numeric(length = length(ticker[,1]))
  dhl<-ticker[,7]#colonna sette è data dalla distanza tra high e low
  for (i in 20:length(Maxdhl)) {
    
    Maxdhl[i]<-max(dhl[(i-19):i])
  }
  return(Maxdhl)
}