#DEFINIAMO IL CBR
#ordine delle colonne dello strumento finanziario:
#"op"=open
#"hi"=high
#"lo"=low
#"cl"=close
#"vol"=volume
#"fr"=fattore di riempimento
#"d_op_cl"=distanza tra op e cl
#"Max_20_op_cl"=distanza max nella finestra temporale
#"min_20_op_cl"=distanza min nella finestra temporale
#"media_20_d_op_cl"=media tra op e cl nella finestra temporale
#"media_Max20_media20"=media tra "Max_20_op_cl" e "media_20_d_op_cl"
#"media_min20_media20"=media tra "min_20_op_cl" e "media_20_d_op_cl"
source("Dist_op_cl.R")
source("Max_dist_op_cl.R")
source("min_dist_op_cl.R")
source("media_dist_op_cl.R")
source("Media_Maxdoc_Mediadoc.R")
source("Media_mindoc_Mediadoc.R")

Candel_body_range<-function(strumento){

ticker<-strumento

d_op_cl<-Dist_op_cl(ticker)
ticker<-cbind(ticker,d_op_cl)

Max_20_op_cl<-Max_dist_op_cl(ticker)
ticker<-cbind(ticker,Max_20_op_cl)

min_20_op_cl<-min_dist_op_cl(ticker)
ticker<-cbind(ticker,min_20_op_cl)

media_20_d_op_cl<-media_dist_op_cl(ticker)
ticker<-cbind(ticker,media_20_d_op_cl)

media_Max20_media20<-Media_Maxdoc_Mediadoc(ticker)
ticker<-cbind(ticker,media_Max20_media20)

media_min20_media20<-Media_mindoc_Mediadoc(ticker)
ticker<-cbind(ticker,media_min20_media20)



  CBR<-numeric(length = length(ticker[,1]))
  for (i in 20:length(ticker[,1])) {
    range_op_cl<-min(abs(ticker$d_op_cl[i]-ticker$min_20_op_cl[i]),
                     abs(ticker$d_op_cl[i]-ticker$media_min20_media20[i]),
                     abs(ticker$d_op_cl[i]-ticker$media_20_d_op_cl[i]),
                     abs(ticker$d_op_cl[i]-ticker$media_Max20_media20[i]),
                     abs(ticker$d_op_cl[i]-ticker$Max_20_op_cl[i]))
    
    if(range_op_cl==abs(ticker$d_op_cl[i]-ticker$min_20_op_cl[i])){CBR[i]=1}
    if(range_op_cl==abs(ticker$d_op_cl[i]-ticker$media_min20_media20[i])){CBR[i]=2}
    if(range_op_cl==abs(ticker$d_op_cl[i]-ticker$media_20_d_op_cl[i])){CBR[i]=3}
    if(range_op_cl==abs(ticker$d_op_cl[i]-ticker$media_Max20_media20[i])){CBR[i]=4}
    if(range_op_cl==abs(ticker$d_op_cl[i]-ticker$Max_20_op_cl[i])){CBR[i]=5}
  }
  return(CBR)
}
