#FUNZIONE PER DISTANZA TRA OPEN E CLOSE DELLA CANDELA
Dist_op_cl<-function(ticker){
  
  doc<-numeric(length = length(ticker[,1]))
  
  for(i in 1:length(doc)){
    doc[i]<-abs(ticker[i,1]-ticker[i,4])
  }
  return(doc)
}
