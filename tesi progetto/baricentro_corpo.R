baricentro_corpo<-function(ticker){
  strumento<-ticker
  baricentro<-numeric(length = length(strumento[,1]))
  for(i in 1:length(baricentro)){
    baricentro[i]<- (abs(strumento$op[i]+strumento$cl[i]))/2
  }
  return(baricentro)
}