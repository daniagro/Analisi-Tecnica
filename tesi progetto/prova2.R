vittorie<-0     #  training 3773    34     validation   6       tot 40 5284
sconfitte<-0     #                  30                  7           37
guadagno<-0     #                  16.13125       26.46999          42.60124
perdita<-0      #                 12.69             18.18001           30.87001
for (i in 3773:5284) {
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
cont<-0
red2<-0
red3<-0
green2<-0
green3<-0
for (i in 25:5284) {
  if(MSFTprova$BearishHaramiCross[i]==1){
    cont<-cont+1
    if(MSFTprova$op[i+2]>MSFTprova$cl[i+2]){
      red2<-red2+1
      if(MSFTprova$op[i+3]>MSFTprova$cl[i+3]){
        red3<-red3+1
      }else{green3<-green3+1}
    }else{green2<-green2+1}
  }
  
}
