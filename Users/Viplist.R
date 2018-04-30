
#Users list webscrap

top_pol <- "http://www.twitter-espana.com/categoria/politica"
html_top_pol <- read_html(top_pol)
list_top_pol <- html_top_pol %>%
  html_nodes("table")
influ_pol <- list_top_pol[[1]] %>%
  html_table(header = TRUE)

top_eco <- "http://www.twitter-espana.com/categoria/economia"
html_top_eco <- read_html(top_eco)
list_top_eco <- html_top_eco %>%
  html_nodes("table")
influ_eco <- list_top_eco[[1]] %>%
  html_table(header = TRUE)

#Tidying the table

top100_1 <-rbind(influ_pol, influ_eco)
top100_1$Twittero <- NULL
top100_1$`#` <- NULL
top100_1 <- top100_1[-c(51,102),]
head(top100_1)

rm(top_pol, html_top_pol, list_top_pol, influ_pol, top_eco, html_top_eco, list_top_eco, influ_eco)

#Export to Excel to clean the users screen names and names

write.xlsx(top100_1, "Datasets/top100_1.xlsx")
rm(top100_1)
top100 <- read_excel("~/Desktop/TFG/Datasets/top100_1.1.xlsx")

rm(top100_1)

#Cleaning repeated entries

top100 <-top100 %>%
  group_by(Cuenta) %>% 
  filter(row_number(Cuenta) == 1)

Viplist <- as.vector(top100$Cuenta)
Viplist

