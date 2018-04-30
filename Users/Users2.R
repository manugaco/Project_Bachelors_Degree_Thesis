

#Libraries

library("tidyverse")
library("dplyr")
library("xlsx")
library("readxl")
library("data.table")
library("rvest")
library("twitteR")
library("purrr")
library("magrittr")
library("jsonlite")

#Twitter API authenticaton, public key

setup_twitter_oauth(consumer_key = "...",
                    consumer_secret = "...", 
                    access_token = "...",
                    access_secret = "...")

#Getting the information from the user list I already have

UsersDF0 <- twListToDF(lookupUsers(Viplist)) %>%
  select(id, name, screenName, location, lang)

all <- c("id", "name", "screenName", "location", "lang")

UsersDF0 <- UsersDF0 %>%
  arrange(by=id) %>%
  unique() %>%
  select(all)

#Because API has a pin limit, I am going to split the references list 
#Weighting each user and taking arround no more than 10%

top100_3$Seguidopor <- as.numeric(gsub(",","",top100_3$Seguidopor))
sumat <- sum(top100_3$Seguidopor) #overall of followers, need to filter

top100_3 <- top100_3 %>%
  mutate(
    weights = Seguidopor/sumat
  )
top100_3$sum <- cumsum(top100_3$weights)

Viplist_1 <- Viplist[c(1)]
Viplist_2 <- Viplist[c(2)]
Viplist_3 <- Viplist[c(3)]
Viplist_4 <- Viplist[c(4,5)]
Viplist_5 <- Viplist[c(6:9)]
Viplist_6 <- Viplist[c(10:13)]
Viplist_7 <- Viplist[c(14:18)]
Viplist_8 <- Viplist[c(19:24)]
Viplist_9 <- Viplist[c(25:34)]
Viplist_10 <- Viplist[c(35:50)]
Viplist_11 <- Viplist[c(51:72)]
Viplist_11 <- Viplist[c(73:94)]

#Getting the dataset

UsersDF1 <- as.data.frame(sapply(Viplist_1, fun_uf1)) #works

UsersDF1.1 <- as.data.frame(sapply(Viplist_1, fun_uf2)) #

UsersDF2 <- as.data.frame(sapply(Viplist_2, fun_uf1)) #works

UsersDF2.2 <- as.data.frame(sapply(Viplist_2, fun_uf2))

UsersDF3 <- as.data.frame(sapply(Viplist_3, fun_uf1))

UsersDF3.3 <- as.data.frame(sapply(Viplist_3, fun_uf2))

UsersDF4 <- as.data.frame(sapply(Viplist_4, fun_uf1))

UsersDF4.4 <- as.data.frame(sapply(Viplist_4, fun_uf2))

UsersDF5 <- as.data.frame(sapply(Viplist_5, fun_uf1))

UsersDF5.5 <- as.data.frame(sapply(Viplist_5, fun_uf2))

UsersDF6 <- as.data.frame(sapply(Viplist_6, fun_uf1))

UsersDF6.6 <- as.data.frame(sapply(Viplist_6, fun_uf2))

UsersDF7 <- as.data.frame(sapply(Viplist_7, fun_uf1))

UsersDF7.7 <- as.data.frame(sapply(Viplist_7, fun_uf2))

UsersDF8 <- as.data.frame(sapply(Viplist_8, fun_uf1))

UsersDF8.8 <- as.data.frame(sapply(Viplist_8, fun_uf2))

UsersDF9 <- as.data.frame(sapply(Viplist_9, fun_uf1))

UsersDF9.9 <- as.data.frame(sapply(Viplist_9, fun_uf2))

UsersDF10 <- as.data.frame(sapply(Viplist_10, fun_uf1))

UsersDF10.10 <- as.data.frame(sapply(Viplist_10, fun_uf2))

UsersDF11 <- as.data.frame(sapply(Viplist_10, fun_uf1))

UsersDF11.11 <- as.data.frame(sapply(Viplist_10, fun_uf2))

UsersList <- cbind(UsersDF0, UsersDF1, UsersDF1.1, UsersDF2, UsersDF2.2, UsersDF3, UsersDF3.3, UsersDF4, UsersDF4.4, 
                   UsersDF5, UsersDF5.5, UsersDF6, UsersDF6.6, UsersDF7, UsersDF7.7, UsersDF8, UsersDF8.8, UsersDF9, 
                   UsersDF9.9, UsersDF10, UsersDF10.10,UsersDF11, UsersDF11.11)

rm(UsersDF0, UsersDF1, UsersDF1.1, UsersDF2, UsersDF2.2, UsersDF3, UsersDF3.3, UsersDF4, UsersDF4.4, 
   UsersDF5, UsersDF5.5, UsersDF6, UsersDF6.6, UsersDF7, UsersDF7.7, UsersDF8, UsersDF8.8, UsersDF9, 
   UsersDF9.9, UsersDF10, UsersDF10.10,UsersDF11, UsersDF11.11, Userslist_4p)

UsersList <-  as.data.frame(do.call("rbind", UsersList))
UsersDF <- map_df(UsersList, extract, all) %>%
  arrange(by=id) %>%
  unique()

UsersListjson <- fromJSON(UsersList)
UsersDFjson <- fromJSON(UsersDF)

#Saving into json, xlsx and Rdata Objects

save(UsersList, file="Objects/UL.RData")
save(UsersDF, file="Objects/UDF.RData")
save(UsersListjson, file="Objects/ULjson.json")
save(UsersDFjson, file="Objects/UDFjson.json")
write.xlsx(UsersDF, file="Objects/UDF.xlsx")


