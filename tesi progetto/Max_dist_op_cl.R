#calcolo il massimo della distanza tra la open e la 
#close nella finestra temporale t-i,t con i=20 al periodo t
Max_dist_op_cl<-function(ticker){
  #i va da 20 fino a length(MSFT)
  Maxdoc<-numeric(length = length(ticker[,1]))
  doc<-ticker[,7]#colonna sette è data dalla distanza tra open e close
  for (i in 20:length(Maxdoc)) {
    
    Maxdoc[i]<-max(doc[(i-19):i])
  }
  return(Maxdoc)
}