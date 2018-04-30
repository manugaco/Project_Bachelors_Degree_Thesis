
#Select only tweets with economic content

library("tidyverse")
library("SnowballC")


ecolistv <- ecolist$X__1

ecolistv <- wordStem(ecolistv,  language = ("spanish"))

prueba <- df_tweets %>%
  filter(str_detect(text, 'accion|acreedor|activ|adquir|ahorr|alquil|balanz|banc|benefici|   
                           bien|billet|bitcoin|bols|bon|brok|burbuj|cambi|canj|canon|      
                           capital|carter|chequ|comision|compr|condon|consum|contrat|contribu|   
                           cost|cotiz|crecimient|cuent|deficit|deflacion|demand|deposit|desarroll|  
                           descubiert|descuent|desfalc|desgrav|deud|devalu|diner|dividend|divis|      
                           econom|especul|estaf|factur|finanz|fluctuacion|fond|fraud|fusion|     
                           gir|haciend|hipotec|huelg|impuest|indic|inflacion|ingres|intercambi| 
                           interes|inversion|letr|liquidez|lucr|merc|moned|nomin|oblig|      
                           ofert|oper|organ|pag|pag|parid|pasiv|patrimoni|pobrez|     
                           pod|preci|prestam|prim|produccion|rati|recaud|regal|remes|      
                           remuner|rent|renting|retencion|riquez|salari|sald|sector|servici|    
                           solvenci|subast|subrog|subsidi|sucursal|sueld|superavit|tarif|tarjet|     
                           tas|tesor|tip|titul|trabaj|transferent|tribut|usufruct|valor|      
                           vent'))

