########cambio
#PATTERN INVERSIONE RIALZISTA OTTIMIZZATI
#BullishEngulfing     
#BullishHarami      
#BullishHaramiCross  
#PiercingLine          
#HomingPigeon           
#OneWhiteSoldier         

#bande per operazioni
bbands.close <- BBands( MSFTprova$cl )
bbands.close[is.na(bbands.close)]<- -10
#bande per entrata
bbandEntry <- BBands( MSFTprova$cl,sd=1 )
bbandEntry[is.na(bbandEntry)]<- -10



#################################
pendenza<-rep(0,length(bbands.close$mavg))
#calcolo pendenza
for(i in 25:length(bbands.close$mavg)){
  inizio<-as.numeric(bbands.close$mavg[i])
  fine<-as.numeric(bbands.close$mavg[(i-5)])
  pendenza[i]<-(inizio-fine)/5
}


#########codice per bullish engalfing
vittorie<-0   #treining 3773  13         validation 5284 9              tot 5284  22
sconfitte<-0   #               7                      7                        14
guadagno<-0    #            6.104997              16.52002                   22.62502
perdita<-0      #           4.108744             10.75001                    14.85875
for (i in 25:5284) {
  
  if(MSFTprova$BullishEngulfing[i]==1 & coredata(bbandEntry$up[i+2])<coredata(MSFTprova$cl[i+2]) & (pendenza[i+2]>0.1)   ){
    
    j<-1
    
    print("inizio operazione")
    print("pendenza")
    print(pendenza[i+2])
    print(MSFTprova$cl[i+2])#da vedere se fare la op i+3
    StopLoss<-as.numeric(min(MSFTprova$lo[i],MSFTprova$lo[i+1]))
    #StopLoss<-as.numeric(bbands.close$mavg[i+2])
    OpenPosizione<-as.numeric(MSFTprova$cl[i+2])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+2+j]>0.8){
        print(bbands.close$pctB[i+2+j])
        print("take profit raggiunto")
        win<-as.numeric(MSFTprova$cl[i+2+j])-OpenPosizione
        if(win>=0){
          print(" you win")
          print(win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          vittorie<-vittorie+1
          guadagno<-guadagno+win
        }else{
          print(" you lose")
          print(-win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          sconfitte<-sconfitte+1
          perdita<-perdita-win
        }
        break
      }
      if(MSFTprova$lo[i+2+j]<=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-OpenPosizione-StopLoss
        print(lose)
        print("fine operazione")
        print(MSFTprova$lo[i+2+j])
        sconfitte<-sconfitte+1
        perdita<-perdita + abs(lose)
        break
      }
      j<-j+1
    }
    
  }
  
  
}  
##########codice per    HomingPigeon
vittorie<-0      #    treining 3773 5       validation 5284 4
sconfitte<-0     #               1                  1
guadagno<-0      #             3.19               6.680008
perdita<-0       #             0.83                 0.14
for (i in 3773:5284) {
  #&  (pendenza[i+2]>0)
  if(MSFTprova$HomingPigeon[i]==1&  (pendenza[i+2]>0)  & (coredata(MSFTprova$op[i+2])<coredata(MSFTprova$cl[i+2])) & coredata(MSFTprova$CBR[i+2])>=3   ){
    
    j<-1
    
    print("inizio operazione")
    print(MSFTprova$cl[i+2])#da vedere se fare la op i+3
    
    StopLoss<-as.numeric(min(MSFTprova$lo[i],MSFTprova$lo[i+1]))
    OpenPosizione<-as.numeric(MSFTprova$cl[i+2])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+2+j]>0.8){
        print("%B")
        print(bbands.close$pctB[i+2+j])
        print("take profit raggiunto")
        win<-as.numeric(MSFTprova$cl[i+2+j])-OpenPosizione
        if(win>=0){
          print(" you win")
          print(win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          vittorie<-vittorie+1
          guadagno<-guadagno+win
        }else{
          print(" you lose")
          print(-win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          sconfitte<-sconfitte+1
          perdita<-perdita-win
        }
        break
      }
      if(MSFTprova$lo[i+2+j]<=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-OpenPosizione-StopLoss
        print(lose)
        print("fine operazione")
        print(MSFTprova$lo[i+2+j])
        sconfitte<-sconfitte+1
        perdita<-perdita+abs(lose)
        break
      }
      j<-j+1
    }
    
  }
  
  
}  

###codice per piercing line
vittorie<-0     #    treining   6       validation   3
sconfitte<-0   #                1                    2
guadagno<-0   #             2.021254              5.959991
perdita<-0   #              0.579999              2.270001
for (i in 25:5284) {
  #&  (pendenza[i+2]>0)
  if(MSFTprova$PiercingLine[i]==1&  (pendenza[i+2]>0)  & (coredata(MSFTprova$op[i+2])<coredata(MSFTprova$cl[i+2])) & coredata(MSFTprova$CBR[i+2])>=3   ){
    
    j<-1
    
    print("inizio operazione")
    print(MSFTprova$cl[i+2])#da vedere se fare la op i+3
    
    StopLoss<-as.numeric(min(MSFTprova$lo[i],MSFTprova$lo[i+1],MSFTprova$lo[i+2]))
    OpenPosizione<-as.numeric(MSFTprova$cl[i+2])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+2+j]>0.9){
        print("%B")
        print(bbands.close$pctB[i+2+j])
        print("take profit raggiunto")
        win<-as.numeric(MSFTprova$cl[i+2+j])-OpenPosizione
        if(win>=0){
          print(" you win")
          print(win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          vittorie<-vittorie+1
          guadagno<-guadagno+win
        }else{
          print(" you lose")
          print(-win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          sconfitte<-sconfitte+1
          perdita<-perdita-win
        }
        break
      }
      if(MSFTprova$lo[i+2+j]<=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-OpenPosizione-StopLoss
        print(lose)
        print("fine operazione")
        print(MSFTprova$lo[i+2+j])
        sconfitte<-sconfitte+1
        perdita<-perdita+lose
        break
      }
      j<-j+1
    }
    
  }
}  
########### codice per one white soldier

vittorie<-0     #treining   10        validation 5       
sconfitte<-0     #          8                   3             
guadagno<-0     #       8.721249                 16.43         
perdita<-0      #      7.118752                 9.410011       
for (i in 25:3773) {
  #(coredata(MSFTprova$op[i+2])<coredata(MSFTprova$cl[i+2])) 
  if(MSFTprova$OneWhiteSoldier[i]==1 &
     coredata(bbands.close$pctB[i+2])<0.8 &
     coredata(MSFTprova$CBR[i])>=4& 
     coredata(MSFTprova$CBR[i+1])>=4 ){
    
    j<-1
    
    print("inizio operazione")
    print(MSFTprova$op[i+2])#da vedere se fare la op i+3
    
    StopLoss<-as.numeric(MSFTprova$lo[i])
    OpenPosizione<-as.numeric(MSFTprova$op[i+2])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+2+j]>0.8){
        print("%B")
        print(bbands.close$pctB[i+2+j])
        print("take profit raggiunto")
        win<-as.numeric(MSFTprova$cl[i+2+j])-OpenPosizione
        if(win>=0){
          print(" you win")
          print(win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          vittorie<-vittorie+1
          guadagno<-guadagno+win
        }else{
          print(" you lose")
          print(-win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          sconfitte<-sconfitte+1
          perdita<-perdita-win
        }
        break
      }
      if(MSFTprova$lo[i+2+j]<=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-OpenPosizione-StopLoss
        print(lose)
        print("fine operazione")
        print(MSFTprova$lo[i+2+j])
        sconfitte<-sconfitte+1
        perdita<-perdita+abs(lose)
        break
      }
      j<-j+1
    }
    
  }
  
  
} 
##########codice per Bullish Harami Cross    niente
vittorie<-0     #BAC 7                      niente
sconfitte<-0     #BAC 1                      niente
guadagno<-0     #BAC   2.450006              niente
perdita<-0      #BAC   0.350001              niente
for (i in 25:5284) {
  #&  (pendenza[i+2]>0)
  if(MSFTprova$BullishHaramiCross[i]==1&  (pendenza[i+2]>0)  & (coredata(MSFTprova$op[i+2])<coredata(MSFTprova$cl[i+2])) & coredata(MSFTprova$CBR[i+2])>=3   ){
    
    j<-1
    
    print("inizio operazione")
    print(MSFTprova$cl[i+2])#da vedere se fare la op i+3
    
    StopLoss<-as.numeric(min(MSFTprova$lo[i],MSFTprova$lo[i+1]))
    OpenPosizione<-as.numeric(MSFTprova$cl[i+2])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+2+j]>0.8){
        print("%B")
        print(bbands.close$pctB[i+2+j])
        print("take profit raggiunto")
        win<-as.numeric(MSFTprova$cl[i+2+j])-OpenPosizione
        if(win>=0){
          print(" you win")
          print(win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          vittorie<-vittorie+1
          guadagno<-guadagno+win
        }else{
          print(" you lose")
          print(-win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          sconfitte<-sconfitte+1
          perdita<-perdita-win
        }
        break
      }
      if(MSFTprova$lo[i+2+j]<=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-OpenPosizione-StopLoss
        print(lose)
        print("fine operazione")
        print(MSFTprova$lo[i+2+j])
        sconfitte<-sconfitte+1
        perdita<-perdita+abs(lose)
        break
      }
      j<-j+1
    }
    
  }
  
  
} 
##########codice per Bullish Harami 
vittorie<-0      #  treining      13     validation 6    
sconfitte<-0     # BA             7                 4
guadagno<-0     # BA          8.658751            8.400006
perdita<-0      #BA             3.17              1.850009 
for (i in 25:5284) {
  #&  (pendenza[i+2]>0)
  if(MSFTprova$BullishHarami[i]==1&  (pendenza[i+2]>0)  & (coredata(MSFTprova$op[i+2])<coredata(MSFTprova$cl[i+2])) & coredata(MSFTprova$CBR[i+2])>=3   ){
    
    j<-1
    
    print("inizio operazione")
    print(MSFTprova$cl[i+2])#da vedere se fare la op i+3
    
    StopLoss<-as.numeric(min(MSFTprova$lo[i],MSFTprova$lo[i+1]))
    OpenPosizione<-as.numeric(MSFTprova$cl[i+2])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+2+j]>0.8){
        print("%B")
        print(bbands.close$pctB[i+2+j])
        print("take profit raggiunto")
        win<-as.numeric(MSFTprova$cl[i+2+j])-OpenPosizione
        if(win>=0){
          print(" you win")
          print(win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          vittorie<-vittorie+1
          guadagno<-guadagno+win
        }else{
          print(" you lose")
          print(-win)
          print("fine operazione")
          print(MSFTprova$cl[i+2+j])
          sconfitte<-sconfitte+1
          perdita<-perdita-win
        }
        break
      }
      if(MSFTprova$lo[i+2+j]<=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-OpenPosizione-StopLoss
        print(lose)
        print("fine operazione")
        print(MSFTprova$lo[i+2+j])
        sconfitte<-sconfitte+1
        perdita<-perdita+abs(lose)
        break
      }
      j<-j+1
    }
    
  }
  
  
}  

