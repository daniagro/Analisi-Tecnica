#DEFINIAMO IL COR
#ordine delle colonne dello strumento finanziario:
#"op"=open
#"hi"=high
#"lo"=low
#"cl"=close
#"vol"=volume
#"fr"=fattore di riempimento
#"d_hi_lo"=distanza tra hi e lo
#"Max_20_hi_lo"=distanza max nella finestra temporale
#"min_20_hi_lo"=distanza min nella finestra temporale
#"media_20_d_hi_lo"=media tra op e cl nella finestra temporale
#"media_Max20_media20_hi_lo"=media tra "Max_20_hi_lo" e "media_20_d_hi_lo"
#"media_min20_media20_hi_lo"=media tra "min_20_hi_lo" e "media_20_d_hi_lo"
source("Dist_hi_lo.R")
source("Max_dist_hi_lo.R")
source("min_dist_hi_lo.R")
source("media_dist_hi_lo.R")
source("Media_Maxdhl_Mediadhl.R")
source("Media_mindhl_Mediadhl.R")

Candel_oscillator_range<-function(strumento){
  
  ticker<-strumento
  
  d_hi_lo<-Dist_hi_lo(ticker)
  ticker<-cbind(ticker,d_hi_lo)
  
  Max_20_hi_lo<-Max_dist_hi_lo(ticker)
  ticker<-cbind(ticker,Max_20_hi_lo)
  
  min_20_hi_lo<-min_dist_hi_lo(ticker)
  ticker<-cbind(ticker,min_20_hi_lo)
  
  media_20_d_hi_lo<-media_dist_hi_lo(ticker)
  ticker<-cbind(ticker,media_20_d_hi_lo)
  
  media_Max20_media20_hi_lo<-Media_Maxdhl_Mediadhl(ticker)
  ticker<-cbind(ticker,media_Max20_media20_hi_lo)
  
  media_min20_media20_hi_lo<-Media_mindhl_Mediadhl(ticker)
  ticker<-cbind(ticker,media_min20_media20_hi_lo)
  
  
  
  COR<-numeric(length = length(ticker[,1]))
  for (i in 20:length(ticker[,1])) {
    range_hi_lo<-min(abs(ticker$d_hi_lo[i]-ticker$min_20_hi_lo[i]),
                     abs(ticker$d_hi_lo[i]-ticker$media_min20_media20_hi_lo[i]),
                     abs(ticker$d_hi_lo[i]-ticker$media_20_d_hi_lo[i]),
                     abs(ticker$d_hi_lo[i]-ticker$media_Max20_media20_hi_lo[i]),
                     abs(ticker$d_hi_lo[i]-ticker$Max_20_hi_lo[i]))
    
    if(range_hi_lo==abs(ticker$d_hi_lo[i]-ticker$min_20_hi_lo[i])){COR[i]=1}
    if(range_hi_lo==abs(ticker$d_hi_lo[i]-ticker$media_min20_media20_hi_lo[i])){COR[i]=2}
    if(range_hi_lo==abs(ticker$d_hi_lo[i]-ticker$media_20_d_hi_lo[i])){COR[i]=3}
    if(range_hi_lo==abs(ticker$d_hi_lo[i]-ticker$media_Max20_media20_hi_lo[i])){COR[i]=4}
    if(range_hi_lo==abs(ticker$d_hi_lo[i]-ticker$Max_20_hi_lo[i])){COR[i]=5}
  }
  return(COR)
}