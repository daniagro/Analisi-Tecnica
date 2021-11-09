#calcolo la media delle distanze tra la open e la close 
# nella finestra temporale t-i,t con i=20 al periodo t

media_dist_op_cl<-function(ticker){
  #i va da 20 fino a length(MSFT)
  mediadoc<-numeric(length = length(ticker[,1]))
  doc<-ticker[,7]#colonna sette è data dalla distanza tra open e close
  for (i in 20:length(mediadoc)) {
    
    mediadoc[i]<-mean(doc[(i-19):i])
  }
  return(mediadoc)
}
