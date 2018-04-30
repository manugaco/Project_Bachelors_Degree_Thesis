

#Location lists

munlisthttp <- "https://15mpedia.org/w/index.php?title=Especial%3AAsk&q=%5B%5BPage+has+default+form%3A%3AMunicipio%5D%5D+%5B%5Bpa%C3%ADs%3A%3AEspa%C3%B1a%5D%5D&po=%3F%3DMunicipio%23%0D%0A%3FComarca%0D%0A%3FProvincia%0D%0A%3FComunidad+aut%C3%B3noma%3DCC.AA.%0D%0A%3FN%C3%BAmero+de+asambleas%3DAP%0D%0A%3FN%C3%BAmero+de+centros+sociales%3DCS%0D%0A&eq=yes&p%5Bformat%5D=table&sort_num=&order_num=ASC&p%5Blimit%5D=8500&p%5Boffset%5D=8500&p%5Blink%5D=all&p%5Bsort%5D=&p%5Bheaders%5D=show&p%5Bmainlabel%5D=Municipio&p%5Bintro%5D=&p%5Boutro%5D=&p%5Bsearchlabel%5D=%26hellip%3B+siguientes+resultados&p%5Bdefault%5D=&p%5Bclass%5D=sortable+wikitable+smwtable&eq=yes"

html_munlist <- read_html(munlisthttp)
list_munlist <- html_munlist %>%
  html_nodes("table")
mun_list1 <- list_munlist[[3]] %>%
  html_table(header = TRUE)

mun_list1[,c(2,6,7)] <- NULL

muni <- subset(mun_list1, select=c("Municipio")) %>%
  unlist() %>%
  tolower()
munidf <- as.data.frame(muni)
munidf <- as.data.frame(munidf[!duplicated(munidf$muni), ])
munidf <- as.data.frame(munidf[Reduce(`&`, lapply(munidf, function(x) !(is.na(x)|x==""))),])
write.xlsx(munidf, "Datasets/munidf.xlsx")

comar <- subset(mun_list1, select=c("Comarca")) %>%
  unlist() %>%
  tolower()
comardf <- as.data.frame(comar)
comardf <- as.data.frame(comardf[!duplicated(comardf$comar), ])
comardf <- as.data.frame(comardf[Reduce(`&`, lapply(comardf, function(x) !(is.na(x)|x==""))),])
write.xlsx(comardf, "Datasets/comardf.xlsx")

prov <- subset(mun_list1, select=c("Provincia")) %>%
  unlist() %>%
  tolower()
provdf <- as.data.frame(prov)
provdf <- as.data.frame(provdf[!duplicated(provdf$prov), ])
provdf <- as.data.frame(provdf[Reduce(`&`, lapply(provdf, function(x) !(is.na(x)|x==""))),])
write.xlsx(provdf, "Datasets/provdf.xlsx")

ccaa <- subset(mun_list1, select=c("CC.AA.")) %>%
  unlist() %>%
  tolower()
ccaadf <- as.data.frame(ccaa)
ccaadf <- as.data.frame(ccaadf[!duplicated(ccaadf$ccaa), ])
ccaadf <- as.data.frame(ccaadf[Reduce(`&`, lapply(ccaadf, function(x) !(is.na(x)|x==""))),])
write.xlsx(ccaadf, "Datasets/ccaadf.xlsx")


munlist1c <- as.data.frame(c(muni, comar, prov, ccaa))
colnames(munlist1c) <- c("lista")


munlist1c <- as.data.frame(munlist1c[!duplicated(munlist1c$lista), ])
munlist0 <- munlist1c[Reduce(`&`, lapply(munlist1c, function(x) !(is.na(x)|x==""))),]




colnames(munlist1cc) <- c("lista")

munlist1cols <- as.data.frame(str_split_fixed(munlist1c$lista, " ", 4))
colnames(munlist1cols) <- c("a", "b", "c", "d")

cola <- as.data.frame(munlist1cols$a)
cola <- as.data.frame(cola[!duplicated(cola), ])
colnames(cola) <- c("lista")

colb <- as.data.frame(munlist1cols$b)
colb <- as.data.frame(colb[!duplicated(colb), ])
colnames(colb) <- c("lista")

colc <- as.data.frame(munlist1cols$c)
colc <- as.data.frame(colc[!duplicated(colc), ])
colnames(colc) <- c("lista")

cold <- as.data.frame(munlist1cols$d)
cold <- as.data.frame(cold[!duplicated(cold), ])
colnames(cold) <- c("lista")

munlistDEF <- rbind(munlist1cc, cola, colb)

munlistDEF <- as.data.frame(munlistDEF[!duplicated(munlistDEF), ])

munlist0 <- munlistDEF[Reduce(`&`, lapply(munlistDEF, function(x) !(is.na(x)|x==""))),]

munlist0 <- as.vector(munlist0)

rm(muni, comar, prov, ccaa, munlisthttp, html_munlist, list_munlist)

save(munlist, file = "Objects/munlist.Rdata")

#location (didactalia)

trans <- "https://didactalia.net/comunidad/materialeducativo/recurso/tabla-de-provincias-capitales-de-provincias-y/30886faf-dec7-4a55-b881-74d74c09ab07"

html_trans <- read_html(trans)
list_trans <- html_trans %>%
  html_nodes("table")
trans_listdf <- list_trans[[1]] %>%
  html_table(header = TRUE)

prov2 <- subset(trans_listdf, select=c("Provincia")) %>%
  unlist() %>%
  tolower()
prov2df <- as.data.frame(prov2)
prov2df <- as.data.frame(prov2df[!duplicated(prov2df$prov2), ])
prov2df <- as.data.frame(prov2df[Reduce(`&`, lapply(prov2df, function(x) !(is.na(x)|x==""))),])
write.xlsx(prov2df, "Datasets/prov2df.xlsx")

cap <- subset(trans_listdf, select=c("Capital")) %>%
  unlist() %>%
  tolower()
capdf <- as.data.frame(cap)
capdf <- as.data.frame(capdf[!duplicated(capdf$cap), ])
capdf <- as.data.frame(capdf[Reduce(`&`, lapply(capdf, function(x) !(is.na(x)|x==""))),])
write.xlsx(capdf, "Datasets/capdf.xlsx")

ccaa2 <- subset(trans_listdf, select=c('Comunidad autónoma')) %>%
  unlist() %>%
  tolower()
ccaa2df <- as.data.frame(ccaa2)
ccaa2df <- as.data.frame(ccaa2df[!duplicated(ccaa2df$ccaa2), ])
ccaa2df <- as.data.frame(ccaa2df[Reduce(`&`, lapply(ccaa2df, function(x) !(is.na(x)|x==""))),])
write.xlsx(ccaa2df, "Datasets/ccaa2df.xlsx")



write.xlsx(trans_listdf, "Datasets/TRS1.xlsx")
trans_listdf2 <- read_excel("~/Desktop/TFG/Datasets/munidf.xlsx")

munlistfinal <- subset(trans_listdf2, select=c("list")) %>%
  unlist() %>%
  tolower() %>%
  unique ()

rm(html_trans, list_trans)

trans_list2 <- unique(munlistfinal, munlistfinal!="")
trans_list2 <- trans_list2 %>%
  removePunctuation %>%
  removeSpecChar()

Agglist <- c("españa", "spain", "vasc country", "catalonia", "peninsula ibérica", "vasc")

munlist2 <- c(trans_list2, Agglist)

rm(trans_list2, trans_list, trans_listdf, trans_listdf2)

munlist2 <- munlist2 %>%
  unique()

munlist2 <- as.data.frame(munlist2)

save(munlist2, file = "Objects/munlist2.RData")

save(munlistcleaner, file = "Objects/munlistcleaner.RData")






