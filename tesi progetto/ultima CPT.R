#ricerca conformazioni pattern one black crow
source("One_Read_Crow.R")  

OneReadCrow<-One_Read_Crow(MSFTprova)
MSFTprova<-cbind(MSFTprova,OneReadCrow)
ORC<-0
sequenze<-matrix(0,nrow = 221,ncol = 2)
cont<-1
for (i in 20:length(MSFTprova[,1])) {
  if(MSFTprova$OneReadCrow[i]==1){
    
    ORC<-ORC+1
  
    sequenze[cont,1]<-MSFTprova$CBR[i]
    sequenze[cont,2]<-MSFTprova$CBR[i+1]
    
    cont<-cont+1
  }
}


dd<-0

  for(i in 1:221){
    if(sequenze[i,1]==5 & sequenze[i,2]==1){
      dd<-dd+1
    }
  }

##########################
t1<-0
t2<-0
t3<-0
cont<-0
for(i in 20:3773){
  if(MSFTprova$OneReadCrow[i]==1){
    if(MSFTprova$CBR[i]==2 & coredata(MSFTprova$CBR[i+1]==1)){
      cont<-cont+1
      if(coredata(MSFTprova$cl[i+2])<coredata(MSFTprova$cl[i+1])){
        t1<-t1+1
        
      }
      if(coredata(MSFTprova$cl[i+3])<coredata(MSFTprova$cl[i+1])){
        t2<-t2+1
        
      }
      if(coredata(MSFTprova$cl[i+4])<coredata(MSFTprova$cl[i+1])){
        t3<-t3+1
        
      }
    }
    
    
    
  }
  
}







