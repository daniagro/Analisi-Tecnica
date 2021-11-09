#install.packages("devtools", dependencies = TRUE)
#library(devtools)
#install_github("kochiuyu/CandleStickPattern")
#library(CandleStickPattern)
#library(quantmod)
#MSFT<-getSymbols("MSFT", from = "2000-01-01" , to = "2021-01-1",src = "yahoo",auto.assign = FALSE)
#MSFT<-MSFT[,-6]
#candleChart(MSFT,theme='white')










#prova codice###############
rm(list = ls())
library(quantmod)
MSFTprova<-getSymbols("MSFT", from = "2000-1-1" , to = "2015-1-1",src = "yahoo",auto.assign = FALSE)
#MSFT<-getSymbols("MSFT", from = "2000-1-1" , to = "2000-2-1",src = "yahoo",auto.assign = FALSE)
#chartSeries(MSFT["2000-1-1/2000-2-1"], theme = "white", TA=" addBBands();addZigZag()")
#chartSeries(MSFT["2000-1-1/2000-2-1"], theme = "white")

MSFTprova<-MSFTprova[,-6]#tolo adj close
colnames(MSFTprova)<-c("op","hi","lo","cl","vol")
#calcolo fattore di riempimento
source("r.R")
fr<-r(MSFTprova)
MSFTprova<-cbind(MSFTprova,fr)
#calcolo candel body range
source("Candel_body_range.R")
CBR<-Candel_body_range(MSFTprova)

#calcolo del simmetry ratio
source("Simmetry_ratio.R")
SR<-Simmetry_ratio(MSFTprova)

#calcolo il candel oscillator range
source("Candel_oscillator_range.R")
COR<-Candel_oscillator_range(MSFTprova)
#calcolo baricentri
source("baricentro_corpo.R")
baricentro<-baricentro_corpo(MSFTprova)
#costruisco le candele con tutti i parametri
MSFTprova<-cbind(MSFTprova,CBR,SR,COR,baricentro)
#######fin qui perfetto



############

#prova funzioni#################


####INIZIO PROVA PATTERN RECOGNITION

#RICOGNIZIONE HANGING MAN  HAMMER shooting star inverted hammer
source("Shooting_star_Inverted_hammer.R")
HammerInverted<-Shooting_star_Inverted_hammer(MSFTprova)
MSFTprova<-cbind(MSFTprova,HammerInverted)

for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$HammerInverted[i]==1){
    print(MSFTprova[i,])
  }
}
###########long range   
source("Long_range.R")
Longrange<-Long_range(MSFTprova)
MSFTprova<-cbind(MSFTprova,Longrange)

for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$Longrange[i]==1){
    print(MSFTprova[i,])
  }
}

###########long range    rossa 
source("Long_range_Rossa.R")
LongrangeRossa<-Long_range_Rossa(MSFTprova)
MSFTprova<-cbind(MSFTprova,LongrangeRossa)
LRS<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$LongrangeRossa[i]==1){
    #print(MSFTprova[i,])
    LRS<-LRS+1
  }
}

###########long range    verde 
source("Long_range_Verde.R")
LongrangeVerde<-Long_range_Verde(MSFTprova)
MSFTprova<-cbind(MSFTprova,LongrangeVerde)
LRV<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$LongrangeVerde[i]==1){
   # print(MSFTprova[i,])
  LRV<-LRV+1
    }
}
##################   Marubozu verde

source("Marubozu_verde.R")
Marubozuverde<-Marubozu_verde(MSFTprova)
MSFTprova<-cbind(MSFTprova,Marubozuverde)
MV<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$Marubozuverde[i]==1){
    #print(MSFTprova[i,])
    MV<-MV+1
  }
}
############ short range
source("Short_range.R")
Shortrange<-Short_range(MSFTprova)
MSFTprova<-cbind(MSFTprova,Shortrange)
SHR<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$Shortrange[i]==1){
    #print(MSFTprova[i,])
    SHR<-SHR+1
  }
}

############ spinning top
source("Spinning_top.R")
Spinningtop<-Spinning_top(MSFTprova)
MSFTprova<-cbind(MSFTprova,Spinningtop)

for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$Spinningtop[i]==1){
    print(MSFTprova[i,])
  }
}

############ Marubozu_verde_inApertura
source("Marubozu_verde_inApertura.R")
MarubozuVerdeInApertura<-Marubozu_verde_inApertura(MSFTprova)
MSFTprova<-cbind(MSFTprova,MarubozuVerdeInApertura)

for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$MarubozuVerdeInApertura[i]==1){
    print(MSFTprova[i,])
  }
}

############ Marubozu_rossa_inApertura
source("Marubozu_rossa_inApertura.R")
MarubozuRossaInApertura<-Marubozu_rossa_inApertura(MSFTprova)
MSFTprova<-cbind(MSFTprova,MarubozuRossaInApertura)

for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$MarubozuRossaInApertura[i]==1){
    print(MSFTprova[i,])
  }
}

############ Marubozu_rossa_inChiusura
source("Marubozu_rossa_inChiusura.R")
MarubozuRossaInChiusura<-Marubozu_rossa_inChiusura(MSFTprova)
MSFTprova<-cbind(MSFTprova,MarubozuRossaInChiusura)

for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$MarubozuRossaInChiusura[i]==1){
    print(MSFTprova[i,])
  }
}
############ Marubozu_verde_inChiusura
source("Marubozu_verde_inChiusura.R")
MarubozuVerdeInChiusura<-Marubozu_verde_inChiusura(MSFTprova)
MSFTprova<-cbind(MSFTprova,MarubozuVerdeInChiusura)

for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$MarubozuVerdeInChiusura[i]==1){
    print(MSFTprova[i,])
  }
}

############ Doji
source("Doji_line.R")
Doji<-Doji_line(MSFTprova)
MSFTprova<-cbind(MSFTprova,Doji)

for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$Doji[i]==1){
    print(MSFTprova[i,])
  }
}

############ Doji Long Legend
source("Doji_Long_Legend.R")
DojiLong<-Doji_Long_Legend(MSFTprova)
MSFTprova<-cbind(MSFTprova,DojiLong)

for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$DojiLong[i]==1){
    print(MSFTprova[i,])
  }
}

############ Graveston Doji 
source("Graveston_Doji.R")
GravestonDoji<-Graveston_Doji(MSFTprova)
MSFTprova<-cbind(MSFTprova,GravestonDoji)

for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$GravestonDoji[i]==1){
    print(MSFTprova[i,])
  }
}

############ Dragonfly Doji 
source("Dragonfly_Doji.R")
DragonflyDoji<-Dragonfly_Doji(MSFTprova)
MSFTprova<-cbind(MSFTprova,DragonflyDoji)

for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$DragonflyDoji[i]==1){
    print(MSFTprova[i,])
  }
}

############ Bullish Engulfing
source("Bullish_Engulfing.R")

BullishEngulfing<-Bullish_Engulfing(MSFTprova)
MSFTprova<-cbind(MSFTprova,BullishEngulfing)
BUE<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$BullishEngulfing[i]==1){
    #print(MSFTprova[i,])
    BUE<-BUE+1
  }
}

############ Bearish Engulfing
source("Bearish_Engulfing.R")

BearishEngulfing<-Bearish_Engulfing(MSFTprova)
MSFTprova<-cbind(MSFTprova,BearishEngulfing)
BEA<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$BearishEngulfing[i]==1){
    #print(MSFTprova[i,])
    BEA<-BEA+1
  }
}

############ Bullish Harami
source("Bullish_Harami.R")

BullishHarami<-Bullish_Harami(MSFTprova)
MSFTprova<-cbind(MSFTprova,BullishHarami)
BUH<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$BullishHarami[i]==1){
    #print(MSFTprova[i,])
    BUH<-BUH+1
  }
}

############ Bearish Harami
source("Bearish_Harami.R")

BearishHarami<-Bearish_Harami(MSFTprova)
MSFTprova<-cbind(MSFTprova,BearishHarami)
BEH<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$BearishHarami[i]==1){
   # print(MSFTprova[i,])
    BEH<-BEH+1
  }
}

############ Bullish Harami Cross
source("Bullish_Harami_Cross.R")

BullishHaramiCross<-Bullish_Harami_Cross(MSFTprova)
MSFTprova<-cbind(MSFTprova,BullishHaramiCross)
BUHC<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$BullishHaramiCross[i]==1){
    #print(MSFTprova[i,])
    BUHC<-BUHC+1
  }
}

############ Bearish Harami Cross
source("Bearish_Harami_Cross.R")

BearishHaramiCross<-Bearish_Harami_Cross(MSFTprova)
MSFTprova<-cbind(MSFTprova,BearishHaramiCross)
BEHC<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$BearishHaramiCross[i]==1){
    #print(MSFTprova[i,])
    BEHC<-BEHC+1
  }
}

############ Piercing Line 
source("Piercing_Line.R")

PiercingLine<-Piercing_Line(MSFTprova)
MSFTprova<-cbind(MSFTprova,PiercingLine)
PL<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$PiercingLine[i]==1){
    #print(MSFTprova[i,])
    PL<-PL+1
  }
}

############  Dark_cloud_Cover  Bullish_Meeting_Line
source("Dark_Cloud_Cover.R")

DarkCloudCover<-Dark_Cloud_Cover(MSFTprova)
MSFTprova<-cbind(MSFTprova,DarkCloudCover)
DCC<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$DarkCloudCover[i]==1){
    #print(MSFTprova[i,])
    DCC<-DCC+1
  }
}

############   Bullish_Meeting_Line
source("Bullish_Meeting_Line.R")

BullishMeetingLine<-Bullish_Meeting_Line(MSFTprova)
MSFTprova<-cbind(MSFTprova,BullishMeetingLine)
BML<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$BullishMeetingLine[i]==1){
    #print(MSFTprova[i,])
    BML<-BML+1
  }
}


############   Bearish_Meeting_Line
source("Bearish_Meeting_Line.R")

BearishMeetingLine<-Bearish_Meeting_Line(MSFTprova)
MSFTprova<-cbind(MSFTprova,BearishMeetingLine)
BEML<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$BearishMeetingLine[i]==1){
    #print(MSFTprova[i,])
    BEML<-BEML+1
  }
}
############   Homing_Pigeon
source("Homing_Pigeon.R")

HomingPigeon<-Homing_Pigeon(MSFTprova)
MSFTprova<-cbind(MSFTprova,HomingPigeon)
HP<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$HomingPigeon[i]==1){
    #print(MSFTprova[i,])
    HP<-HP+1
  }
}


############   Descending_Hawk
source("Descending_Hawk.R")

DescendingHawk<-Descending_Hawk(MSFTprova)
MSFTprova<-cbind(MSFTprova,DescendingHawk)
DH<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$DescendingHawk[i]==1){
    #print(MSFTprova[i,])
    DH<-DH+1
  }
}
############   Matching_Low
source("Matching_Low.R")

MatchingLow<-Matching_Low(MSFTprova)
MSFTprova<-cbind(MSFTprova,MatchingLow)
ML<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$MatchingLow[i]==1){
    #print(MSFTprova[i,])
    MH<-MH+1
  }
}
############   Matching_Low
source("Matching_High.R")

MatchingHigh<-Matching_High(MSFTprova)
MSFTprova<-cbind(MSFTprova,MatchingHigh)
MH<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$MatchingHigh[i]==1){
  #  print(MSFTprova[i,])
    MH<-MH+1
  }
}

############   Kicking_Bullish
source("Kicking_Bullish.R")

KickingBullish<-Kicking_Bullish(MSFTprova)
MSFTprova<-cbind(MSFTprova,KickingBullish)
KB<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$KickingBullish[i]==1){
    print(MSFTprova[i,])
 KB<-KB+1
  }
}

############   Kicking_Bullish
source("Kicking_Bearish.R")

KickingBearish<-Kicking_Bearish(MSFTprova)
MSFTprova<-cbind(MSFTprova,KickingBearish)
KBE<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$KickingBearish[i]==1){
    #òprint(MSFTprova[i,])
    KBE<-KBE+1
  }
}


############   One_White_Soldier
source("One_White_Soldier.R")

OneWhiteSoldier<-One_White_Soldier(MSFTprova)
MSFTprova<-cbind(MSFTprova,OneWhiteSoldier)
OWS<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$OneWhiteSoldier[i]==1){
    #print(MSFTprova[i,])
    OWS<-OWS+1
  }
}



############   One_Read_Crow    
source("One_Read_Crow.R")  

OneReadCrow<-One_Read_Crow(MSFTprova)
MSFTprova<-cbind(MSFTprova,OneReadCrow)
ORC<-0
 uno<-0
 due<-0
for (i in 20:length(MSFTprova[,1])) {
  if(MSFTprova$OneReadCrow[i]==1){
    #print(MSFTprova[i,])
  ORC<-ORC+1
  if(MSFTprova$fr[i]>0.6 ){uno<-uno+1}
  if( MSFTprova$fr[i+1]>0.6){due<-due+1}
  }
}
##################################################
#################################################################
############   Morning_Star
source("Morning_Star.R")

MorningStar<-Morning_Star(MSFTprova)
MSFTprova<-cbind(MSFTprova,MorningStar)
MST<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$MorningStar[i]==1){
    #print(MSFTprova[i,])
    MST<-MST+1
  }
}
############   Evening_Star
source("Evening_Star.R")

EveningStar<-Evening_Star(MSFTprova)
MSFTprova<-cbind(MSFTprova,EveningStar)
EV<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$EveningStar[i]==1){
    #print(MSFTprova[i,])
    EV<-EV+1
  }
}


############   Upside_Gap_Two_Crow
source("Upside_Gap_Two_Crow.R")

UpsideGapTwoCrow<-Upside_Gap_Two_Crow(MSFTprova)
MSFTprova<-cbind(MSFTprova,UpsideGapTwoCrow)
UGTC<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$UpsideGapTwoCrow[i]==1){
    #print(MSFTprova[i,])
    UGTC<-UGTC+1
  }
}


############   Downside_Gap_Two_Rabbits
source("Downside_Gap_Two_Rabbits.R")

DownsideGapTwoRabbits<-Downside_Gap_Two_Rabbits(MSFTprova)
MSFTprova<-cbind(MSFTprova,DownsideGapTwoRabbits)
DGTR<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$DownsideGapTwoRabbits[i]==1){
    #print(MSFTprova[i,])
    DGTR<-DGTR+1
  }
}


############   Three_Green_SDoldiers
source("Three_Green_SDoldiers.R")

ThreeGreenSDoldiers<-Three_Green_SDoldiers(MSFTprova)
MSFTprova<-cbind(MSFTprova,ThreeGreenSDoldiers)
TGS<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$ThreeGreenSDoldiers[i]==1){
   # print(MSFTprova[i,])
    TGS<-TGS+1
  }
}


############   Three_Red_Crows
source("Three_Red_Crows.R")

ThreeRedCrows<-Three_Red_Crows(MSFTprova)
MSFTprova<-cbind(MSFTprova,ThreeRedCrows)
TRC<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$ThreeRedCrows[i]==1){
    #print(MSFTprova[i,])
    TRC<-TRC+1
  }
}


############   Advance_Block
source("Advance_Block.R")

AdvanceBlock<-Advance_Block(MSFTprova)
MSFTprova<-cbind(MSFTprova,AdvanceBlock)
AB<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$AdvanceBlock[i]==1){
   # print(MSFTprova[i,])
    AB<-AB+1
  }
}



############   Descent_Block
source("Descent_Block.R")

DescentBlock<-Descent_Block(MSFTprova)
MSFTprova<-cbind(MSFTprova,DescentBlock)
DB<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$DescentBlock[i]==1){
    #print(MSFTprova[i,])
    DB<-DB+1
  }
}


############   Two_Rabbits
source("Two_Rabbits.R")

TwoRabbits<-Two_Rabbits(MSFTprova)
MSFTprova<-cbind(MSFTprova,TwoRabbits)
TWR<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$TwoRabbits[i]==1){
    #print(MSFTprova[i,])
    TWR<-TWR+1
  }
}


############   Two_Crows
source("Two_Crows.R")

TwoCrows<-Two_Crows(MSFTprova)
MSFTprova<-cbind(MSFTprova,TwoCrows)
TWC<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$TwoCrows[i]==1){
    #print(MSFTprova[i,])
    TWC<-TWC+1
  }
}

############   Squeeze_Allert_Bullish
source("Squeeze_Allert_Bullish.R")

SqueezeAllertBullish<-Squeeze_Allert_Bullish(MSFTprova)
MSFTprova<-cbind(MSFTprova,SqueezeAllertBullish)
SABU<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$SqueezeAllertBullish[i]==1){
    #print(MSFTprova[i,])
    SABU<-SABU+1
  }
}


############   Squeeze_Allert_Bearish
source("Squeeze_Allert_Bearish.R")

SqueezeAllertBearish<-Squeeze_Allert_Bearish(MSFTprova)
MSFTprova<-cbind(MSFTprova,SqueezeAllertBearish)
SAB<-0
for (i in 1:length(MSFTprova[,1])) {
  if(MSFTprova$SqueezeAllertBearish[i]==1){
    #print(MSFTprova[i,])
    SAB<-SAB+1
  }
}



