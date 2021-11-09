#########codice per bearish engalfing              
vittorie<-0   #treining 3773  32       validation 1511 5   completo 5284 tot 37            
sconfitte<-0   #              21                       2                   23         
guadagno<-0    #          12.21219                6.270003                18.48219   
perdita<-0      #         8.565002                6.139992               14.70499                   
for (i in 3773: 5284) {
  
  if(MSFTprova$BearishEngulfing[i]==1 & coredata(bbandEntry$dn[i+2])>coredata(MSFTprova$cl[i+2]) & (pendenza[i+2]< 0)   ){
    
    j<-1
    
    print("inizio operazione")
    print("pendenza")
    print(pendenza[i+2])
    print(MSFTprova$cl[i+2])#da vedere se fare la op i+3
    #StopLoss<-as.numeric(max(MSFTprova$lo[i],MSFTprova$lo[i+1]))
    StopLoss<-as.numeric(MSFTprova$hi[i+1])
    
    #StopLoss<-as.numeric(bbands.close$mavg[i+2])
    OpenPosizione<-as.numeric(MSFTprova$op[i+3])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+2+j]<0.20){
        print(bbands.close$pctB[i+2+j])
        print("take profit raggiunto")
        win<-OpenPosizione-as.numeric(MSFTprova$cl[i+2+j])
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
      if(MSFTprova$hi[i+2+j]>=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-StopLoss-OpenPosizione
        print(lose)
        print("fine operazione")
        print(MSFTprova$hi[i+2+j])
        sconfitte<-sconfitte+1
        perdita<-perdita+abs(lose)
        break
      }
      j<-j+1
    }
    
  }
  
  
} 
########################################################
##########codice  per DescendingHawk  
#              treining 3773         validation 1511     
vittorie<-0     #   4                       nessuno 
sconfitte<-0     #  1                       nessuno
guadagno<-0     #  3.274999                 nessuno
perdita<-0      #   0.2059999               nessuno
for (i in 25:5284) {
  if(MSFTprova$DescendingHawk[i]==1 &  (pendenza[i+2]<0) & (coredata(MSFTprova$op[i+2])>coredata(MSFTprova$cl[i+2]))  & coredata(bbands.close$pctB[i+2])>0.2   ){
    
    j<-1
    
    print("inizio operazione")
    print(MSFTprova$cl[i+2])#da vedere se fare la op i+3
    
    #StopLoss<-as.numeric(max(MSFTprova$lo[i],MSFTprova$lo[i+1]))
    StopLoss<-as.numeric(bbands.close$mavg[i+2])
    OpenPosizione<-as.numeric(MSFTprova$cl[i+2])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+2+j]<0.2){
        print("%B")
        print(bbands.close$pctB[i+2+j])
        print("take profit raggiunto")
        win<-OpenPosizione-as.numeric(MSFTprova$cl[i+2+j])
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
      if(MSFTprova$hi[i+2+j]>=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-StopLoss-OpenPosizione
        if(lose<0){
          lose<- -lose
        }
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
################Bearish  Harami Cross
vittorie<-0     #  training 3773   9       validation   2       tot 11 5284
sconfitte<-0     #                  15                  1             16
guadagno<-0     #                  9.762495       4.240002          14.0025
perdita<-0      #                 3.282503             0.5           3.782503
for (i in 3773:5284) {
  if(MSFTprova$BearishHaramiCross[i]==1 &  (pendenza[i+2]<0)   & coredata(bbands.close$pctB[i+1])>0.2   ){
    
    j<-1
    
    print("inizio operazione")
    print(MSFTprova$op[i+2])#da vedere se fare la op i+3
    
    StopLoss<-as.numeric(max(MSFTprova$hi[i],MSFTprova$hi[i+1]))
    #StopLoss<-as.numeric(bbands.close$mavg[i+2])
    OpenPosizione<-as.numeric(MSFTprova$op[i+2])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+1+j]<0.2){
        print("%B")
        print(bbands.close$pctB[i+1+j])
        print("take profit raggiunto")
        win<-OpenPosizione-as.numeric(MSFTprova$cl[i+1+j])
        if(win>=0){
          print(" you win")
          print(win)
          print("fine operazione")
          print(MSFTprova$cl[i+1+j])
          vittorie<-vittorie+1
          guadagno<-guadagno+win
        }else{
          print(" you lose")
          print(-win)
          print("fine operazione")
          print(MSFTprova$cl[i+1+j])
          sconfitte<-sconfitte+1
          perdita<-perdita-win
        }
        break
      }
      if(MSFTprova$hi[i+1+j]>=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-StopLoss-OpenPosizione
        if(lose<0){
          lose<- -lose
        }
        print(lose)
        print("fine operazione")
        print(MSFTprova$hi[i+1+j])
        sconfitte<-sconfitte+1
        perdita<-perdita+lose
        break
      }
      j<-j+1
    }
    
  }
  
  
}
################Bearish  Harami 
vittorie<-0     #  training 3773    34     validation   6       tot 40 5284
sconfitte<-0     #                  30                  7           37
guadagno<-0     #                  16.13125       26.46999          42.60124
perdita<-0      #                 12.69             18.18001           30.87001
for (i in 25:5284) {
  if(MSFTprova$BearishHarami[i]==1 &  (pendenza[i+2]<0.1)   & coredata(bbands.close$pctB[i+1])>0.2   ){
    
    j<-1
    
    print("inizio operazione")
    print(MSFTprova$op[i+2])#da vedere se fare la op i+3
    
    StopLoss<-as.numeric(max(MSFTprova$hi[i],MSFTprova$hi[i+1]))
    #StopLoss<-as.numeric(bbands.close$mavg[i+2])
    OpenPosizione<-as.numeric(MSFTprova$op[i+2])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+1+j]<0.5){
        print("%B")
        print(bbands.close$pctB[i+1+j])
        print("take profit raggiunto")
        win<-OpenPosizione-as.numeric(MSFTprova$cl[i+1+j])
        if(win>=0){
          print(" you win")
          print(win)
          print("fine operazione")
          print(MSFTprova$cl[i+1+j])
          vittorie<-vittorie+1
          guadagno<-guadagno+win
        }else{
          print(" you lose")
          print(-win)
          print("fine operazione")
          print(MSFTprova$cl[i+1+j])
          sconfitte<-sconfitte+1
          perdita<-perdita-win
        }
        break
      }
      if(MSFTprova$hi[i+1+j]>=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-StopLoss-OpenPosizione
        if(lose<0){
          lose<- -lose
        }
        print(lose)
        print("fine operazione")
        print(MSFTprova$hi[i+1+j])
        sconfitte<-sconfitte+1
        perdita<-perdita+lose
        break
      }
      j<-j+1
    }
    
  }
  
  
}
#############codice Dark Cloud Cover
vittorie<-0     #  training 3773    6     validation   0       tot 6 5284
sconfitte<-0     #                  11                 4           15
guadagno<-0     #                  12.615              0          12.615
perdita<-0      #                 5.120002         3.41999      8.539992
for (i in 25:3773) {
  if(MSFTprova$DarkCloudCover[i]==1 & (pendenza[i+1]<0)   & coredata(bbands.close$pctB[i+1])>0.2   ){
    
    j<-1
    
    print("inizio operazione")
    print(MSFTprova$op[i+2])#da vedere se fare la op i+3
    
    StopLoss<-as.numeric(max(MSFTprova$hi[i],MSFTprova$hi[i+1]))
    #StopLoss<-as.numeric(bbands.close$mavg[i+2])
    OpenPosizione<-as.numeric(MSFTprova$op[i+2])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+1+j]<0){
        print("%B")
        print(bbands.close$pctB[i+1+j])
        print("take profit raggiunto")
        win<-OpenPosizione-as.numeric(MSFTprova$cl[i+1+j])
        if(win>=0){
          print(" you win")
          print(win)
          print("fine operazione")
          print(MSFTprova$cl[i+1+j])
          vittorie<-vittorie+1
          guadagno<-guadagno+win
        }else{
          print(" you lose")
          print(-win)
          print("fine operazione")
          print(MSFTprova$cl[i+1+j])
          sconfitte<-sconfitte+1
          perdita<-perdita-win
        }
        break
      }
      if(MSFTprova$hi[i+1+j]>=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-StopLoss-OpenPosizione
        if(lose<0){
          lose<- -lose
        }
        print(lose)
        print("fine operazione")
        print(MSFTprova$hi[i+1+j])
        sconfitte<-sconfitte+1
        perdita<-perdita+lose
        break
      }
      j<-j+1
    }
    
  }
  
  
}
########One Red Crow
vittorie<-0     #  training 3773    29     validation   36            tot 6 5284
sconfitte<-0     #                  23                    29              15
guadagno<-0     #                  19.27              27.55999         12.615
perdita<-0      #                 14.87375           21.60375        8.539992
for (i in 25:5284) {
  if(MSFTprova$OneReadCrow[i]==1 & (pendenza[i+1]<0)   & coredata(bbands.close$pctB[i+1])>0.2   ){
    
    j<-1
    
    print("inizio operazione")
    print(MSFTprova$op[i+2])#da vedere se fare la op i+3
    
    StopLoss<-as.numeric(max(MSFTprova$hi[i],MSFTprova$hi[i+1]))
    #StopLoss<-as.numeric(bbands.close$mavg[i+2])
    OpenPosizione<-as.numeric(MSFTprova$op[i+2])  #vedere se mettere cl o op succcessiva
    #TakeProfit<-as.numeric(bbandTakeProfit$up[i+1+j])
    repeat{
      if(bbands.close$pctB[i+1+j]<0.3){
        print("%B")
        print(bbands.close$pctB[i+1+j])
        print("take profit raggiunto")
        win<-OpenPosizione-as.numeric(MSFTprova$cl[i+1+j])
        if(win>=0){
          print(" you win")
          print(win)
          print("fine operazione")
          print(MSFTprova$cl[i+1+j])
          vittorie<-vittorie+1
          guadagno<-guadagno+win
        }else{
          print(" you lose")
          print(-win)
          print("fine operazione")
          print(MSFTprova$cl[i+1+j])
          sconfitte<-sconfitte+1
          perdita<-perdita-win
        }
        break
      }
      if(MSFTprova$hi[i+1+j]>=StopLoss){
        print("stop loss raggiunto you lose")
        lose<-StopLoss-OpenPosizione
        if(lose<0){
          lose<- -lose
        }
        print(lose)
        print("fine operazione")
        print(MSFTprova$hi[i+1+j])
        sconfitte<-sconfitte+1
        perdita<-perdita+lose
        break
      }
      j<-j+1
    }
    
  }
  
  
}






































