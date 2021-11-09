#CALCOLO LE OMBRE ts=top shadow bs=bottom shadow
Ombre<-function(ticker){
  ts<-numeric(length = length(ticker[,1]))
  bs<-numeric(length = length(ticker[,1]))
  for(i in 1:length(ts)){
    if(ticker$op[i]<=ticker$cl[i]){
      ts[i]=ticker$hi[i]-ticker$cl[i]
      bs[i]=ticker$op[i]-ticker$lo[i]
    }else{
      ts[i]=ticker$hi[i]-ticker$op[i]
      bs[i]=ticker$cl[i]-ticker$lo[i] 
    }
  }
  return(cbind(ts,bs))
}