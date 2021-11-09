library(quantmod)
rm(list = ls())



MSFT<-getSymbols("MSFT",from="2020-01-01",to="2020-12-31",auto.assign = FALSE,src = "yahoo")
MSFT<-MSFT[,-6]
colnames(MSFT)<-c("op","hi","lo","cl","vol")




#FATTORE DI RIEMPIMENTO DELLA CANDELA
r<-function(ticker){
  fattoreRiempimento<-numeric(length = length(ticker[,1]))
  
  for (i in 1:length(fattoreRiempimento)) {
    fattoreRiempimento[i]<-abs(ticker[i,1]-ticker[i,4])/abs(ticker[i,2]-ticker[i,3])

  }
  return(fattoreRiempimento)
}
#FUNZIONE PER DISTANZA TRA OPEN E CLOSE DELLA CANDELA
Dist_op_cl<-function(ticker){
  
  doc<-numeric(length = length(ticker[,1]))
 
  for(i in 1:length(doc)){
    doc[i]<-abs(ticker[i,1]-ticker[i,4])
    }
  
  return(doc)
  
}


#PARAMETRI PER CALCOLO CBR=CANDEL BODY RANGE

#calcolo il massimo della distanza tra la open e la 
#close nella finestra temporale i=20
Max_dist_op_cl<-function(ticker){
  #i va da 20 fino a length(MSFT)
  Maxdoc<-numeric(length = length(ticker[,1]))
  doc<-ticker[,7]#colonna sette è data dalla distanza tra open e close
  for (i in 20:length(Maxdoc)) {
    
    Maxdoc[i]<-max(doc[i-19:i])
  }
  return(Maxdoc)
}
















Marubozu<-function(ticker){

}