
#per questa parte bisogna far girare il file prova.R
rm(list = ls())
library(quantmod)
MSFTprova<-getSymbols("MSFT", from = "2000-01-1" , to = "2015-1-1",src = "yahoo",auto.assign = FALSE)
par(mfrow=c(2,1))
candleChart(MSFTprova,theme='white')
plot(RSI(MSFTprova$cl,14))
length(RSI(MSFTprova$cl,14))
#periodicity = "weekly"
MSFT<-getSymbols("MSFT", from = "2000-01-1" , to = "2015-1-1",src = "yahoo",auto.assign = FALSE)
candleChart(MSFT['2000-01-1/2000-02-1'],theme='white')
#primo passo costruire una nuova colonna con i valori dell'RSI
MSFTprova<-cbind(MSFTprova,RSI(MSFTprova$cl,14))
#MSFTprova<-MSFTprova[,-54]

MSFTprova[is.na(MSFTprova)]<- -1
#CONTROLLO LA PRESENZA DEI SINGOLI PATTERN E RSI per 2 candele

#PATTERN INVERSIONE RIBASSISTA E RSI>70
#BearishEngulfing
#BearishHarami
#BearishHaramiCross
#DarkCloudCover
#DescendingHawk
#OneReadCrow
for (i in 1:length(MSFTprova[,1])) {
  
  if(MSFTprova$HammerInverted[i]==1 & MSFTprova$rsi[i]>=70 ){
    print(MSFTprova$HammerInverted[i])
    
    
    if(MSFTprova$op[i+2]<MSFTprova$cl[i+2]){
      print("prima candela verde")
      print(MSFTprova$CBR[i+2])}
    
    if(MSFTprova$op[i+2]>MSFTprova$cl[i+2]){
      print("prima candela rossa")
      print(MSFTprova$CBR[i+2])}
    
    if(MSFTprova$op[i+2]==MSFTprova$cl[i+2]){
      print("prima candela body assente")
      print(MSFTprova$CBR[i+2])}
    
    if(MSFTprova$op[i+3]<MSFTprova$cl[i+3]){
      print("seconda candela verde")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+3]>MSFTprova$cl[i+3]){
      print("seconda candela rossa")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+3]== MSFTprova$cl[i+3]){
      print("seconda candela body assente")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+4]<MSFTprova$cl[i+4]){
      print("terza candela verde")
      print(MSFTprova$CBR[i+4])}
    
    if(MSFTprova$op[i+4]>MSFTprova$cl[i+4]){
      print("terza candela rossa")
      print(MSFTprova$CBR[i+4])}
    
    if(MSFTprova$op[i+4]==MSFTprova$cl[i+4]){
      print("terza candela body assente")
      print(MSFTprova$CBR[i+4])}
    
    
    
  }
}
#PATTERN INVERSIONE RIALZISTA E RSI<30
#BullishEngulfing
#BullishHarami
#BullishHaramiCross
#PiercingLine
#HomingPigeon
#OneWhiteSoldier
for (i in 1:length(MSFTprova[,1])) {
  
  if(MSFTprova$OneWhiteSoldier[i]==1 & MSFTprova$rsi[i]>=0 & MSFTprova$rsi[i]<=30){
    print(MSFTprova$OneWhiteSoldier[i])
   
    
    if(MSFTprova$op[i+2]<MSFTprova$cl[i+2]){
      print("prima candela verde")
      print(MSFTprova$CBR[i+2])}
    
    if(MSFTprova$op[i+2]>MSFTprova$cl[i+2]){
      print("prima candela rossa")
      print(MSFTprova$CBR[i+2])}
    
    if(MSFTprova$op[i+2]==MSFTprova$cl[i+2]){
      print("prima candela body assente")
      print(MSFTprova$CBR[i+2])}
    
    if(MSFTprova$op[i+3]<MSFTprova$cl[i+3]){
      print("seconda candela verde")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+3]>MSFTprova$cl[i+3]){
      print("seconda candela rossa")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+3]== MSFTprova$cl[i+3]){
      print("seconda candela body assente")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+4]<MSFTprova$cl[i+4]){
      print("terza candela verde")
      print(MSFTprova$CBR[i+4])}
    
    if(MSFTprova$op[i+4]>MSFTprova$cl[i+4]){
      print("terza candela rossa")
      print(MSFTprova$CBR[i+4])}
    
    if(MSFTprova$op[i+4]==MSFTprova$cl[i+4]){
      print("terza candela body assente")
      print(MSFTprova$CBR[i+4])}
    
    
    
  }
}





#CONTROLLO LA PRESENZA DEI SINGOLI PATTERN E RSI per 3 candele

#PATTERN INVERSIONE RIBASSISTA E RSI>70
for (i in 1:length(MSFTprova[,1])) {
  
  if(MSFTprova$SqueezeAllertBearish[i]==1 & MSFTprova$rsi[i]>=70 ){
    print(MSFTprova$SqueezeAllertBearish[i])
   
    
    if(MSFTprova$op[i+3]<MSFTprova$cl[i+3]){
      print("prima candela verde")
      print(MSFTprova$CBR[i+2])}
    
    if(MSFTprova$op[i+3]>MSFTprova$cl[i+3]){
      print("prima candela rossa")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+3]==MSFTprova$cl[i+3]){
      print("prima candela body assente")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+4]<MSFTprova$cl[i+4]){
      print("seconda candela verde")
      print(MSFTprova$CBR[i+4])}
    
    if(MSFTprova$op[i+4]>MSFTprova$cl[i+4]){
      print("seconda candela rossa")
      print(MSFTprova$CBR[i+4])}
    
    if(MSFTprova$op[i+4]== MSFTprova$cl[i+4]){
      print("seconda candela body assente")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+5]<MSFTprova$cl[i+5]){
      print("terza candela verde")
      print(MSFTprova$CBR[i+5])}
    
    if(MSFTprova$op[i+5]>MSFTprova$cl[5]){
      print("terza candela rossa")
      print(MSFTprova$CBR[i+5])}
    
    if(MSFTprova$op[i+5]==MSFTprova$cl[i+5]){
      print("terza candela body assente")
      print(MSFTprova$CBR[i+5])}
    
    
    
  }
}
#PATTERN INVERSIONE RIALZISTA E RSI<30
for (i in 1:length(MSFTprova[,1])) {
  
  if(MSFTprova$EveningStar[i]==1 & MSFTprova$rsi[i]>=0 & MSFTprova$rsi[i]<=30){
    print(MSFTprova$EveningStar[i])
   
    
    if(MSFTprova$op[i+3]<MSFTprova$cl[i+3]){
      print("prima candela verde")
      print(MSFTprova$CBR[i+2])}
    
    if(MSFTprova$op[i+3]>MSFTprova$cl[i+3]){
      print("prima candela rossa")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+3]==MSFTprova$cl[i+3]){
      print("prima candela body assente")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+4]<MSFTprova$cl[i+4]){
      print("seconda candela verde")
      print(MSFTprova$CBR[i+4])}
    
    if(MSFTprova$op[i+4]>MSFTprova$cl[i+4]){
      print("seconda candela rossa")
      print(MSFTprova$CBR[i+4])}
    
    if(MSFTprova$op[i+4]== MSFTprova$cl[i+4]){
      print("seconda candela body assente")
      print(MSFTprova$CBR[i+3])}
    
    if(MSFTprova$op[i+5]<MSFTprova$cl[i+5]){
      print("terza candela verde")
      print(MSFTprova$CBR[i+5])}
    
    if(MSFTprova$op[i+5]>MSFTprova$cl[5]){
      print("terza candela rossa")
      print(MSFTprova$CBR[i+5])}
    
    if(MSFTprova$op[i+5]==MSFTprova$cl[i+5]){
      print("terza candela body assente")
      print(MSFTprova$CBR[i+5])}
    
    
  }
}



################## strategia lungo termine
#PATTERN INVERSIONE RIALZISTA E RSI<30
#BullishEngulfing     2 win 0 lose
#BullishHarami      3 win    0 lose
#BullishHaramiCross   1 win 0 lose
#PiercingLine          1 win 0 lose
#HomingPigeon           6 win 0 lose
#OneWhiteSoldier         12 win 0 lose

#25 win 0 lose

for (i in 1:(length(MSFTprova[,1])-1)) {
  
  if(MSFTprova$OneWhiteSoldier[i]==1 &coredata(MSFTprova$rsi[i+1])>=0 & coredata(MSFTprova$rsi[i+1])<=30 ){
    apertura<-MSFTprova$cl[i+1]
    print("apertura posizione")
    print(apertura)
    chiusura<-100
    j<-1
    
    repeat{
      if(MSFTprova$rsi[i+1+j]>30){
        
        chiusura<-MSFTprova$cl[i+1+j]
        print("chiusura posizione")
        print(chiusura)
        break
      }else{
        j<-j+1
      }
    }
    print("inizio investimento")
    print(start(apertura))
    print("fine investimento")
    print(end(chiusura))
    a<-as.numeric(apertura)
    b<-as.numeric(chiusura)
    win<-a-b
    print("abbiamo vinto")
    print(win)
    
    
    
  }
  
}
#cbind(MSFTprova['2005-02-01/2005-03-5']$BullishEngulfing,MSFTprova['2005-02-01/2005-03-5']$rsi,MSFTprova['2005-02-01/2005-03-5']$cl)

par(mfrow=c(2,1))

candleChart(MSFT['2005-02-01/2005-03-5'],theme='white')

plot(RSI(MSFTprova$cl,14)['2005-02-01/2005-03-5'])


#PATTERN INVERSIONE RIBASSISTA E RSI>70
#BearishEngulfing 5win 1 lose
#BearishHarami     2win  4 lose
#BearishHaramiCross  2 win  0 lose
#DarkCloudCover     2 win    1 lose
#DescendingHawk      2 win    1 lose
#OneReadCrow           1 win   1 lose

#46 win 12 lose
for (i in 1:(length(MSFTprova[,1])-1)) {
  
  if(MSFTprova$BearishHarami[i]==1 &  coredata(MSFTprova$rsi[i+1])>70 ){
    apertura<-MSFTprova$op[i+2]
    print("apertura posizione")
    print(apertura)
    chiusura<-100
    j<-1
    
    repeat{
      if(MSFTprova$rsi[i+1+j]<70){
       
        chiusura<-MSFTprova$cl[i+1+j]
        print("chiusura posizione")
        print(chiusura)
        break
      }else{
        j<-j+1
      }
    }
    
   print("inizio investimento")
   print(start(apertura))
   print("fine investimento")
   print(end(chiusura))
   a<-as.numeric(apertura)
   b<-as.numeric(chiusura)
   win<-a-b
    print("abbiamo vinto")
    print(win)
    
    
    
    }
      
    }
    
cbind(MSFTprova['2006-11-1/2006-11-30']$BearishHarami,MSFTprova['2006-11-1/2006-11-30']$rsi,MSFTprova['2006-11-1/2006-11-30']$cl)

par(mfrow=c(2,1))
candleChart(MSFT['2006-10-1/2006-11-30'],theme='white')
addBBands(20)
plot(RSI(MSFTprova$cl,14)['2005-02-01/2005-03-5'])
bbands.close <- BBands( MSFT$MSFT.Close )






