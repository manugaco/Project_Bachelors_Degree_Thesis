

#Libraries

library("tidyverse")
library("lubridate")
library("tm")
library("xlsx")
library("readxl")
library("data.table")
library("xtable")

#Cleaning script

load("Objects/UDF_TEST.RData")
load("Objects/UDF.RData")

#This was done once

#Cleaning the dataset
#by location, there is only interest in spanish users, there is going to be used a list of provinces, cities, etc

Dirty_users <- UsersDF
Dirty_users$id <- as.numeric(Dirty_users$id)
Dirty_users$location <- removePunctuation(Dirty_users$location)

Dirty_colums <- as.data.frame(str_split_fixed(Dirty_users$location, " ", 5))

Dirty_users_2 <- cbind(Dirty_users, Dirty_colums)

colnames(Dirty_users_2) <- c("id", "screenName", "followersCount", "friendsCount", "protected", "location", "lang", "l1", "l2", "l3", "l4", "l5")

Dirty_users_2$location <- Dirty_users_2$location %>%
  removeSpecChar() %>%
  tolower()

Dirty_users_2$l1 <- Dirty_users_2$l1 %>%
  as.character %>%
  removeNumbers() %>%
  removeSpecChar() %>%
  tolower()

Dirty_users_2$l2 <- Dirty_users_2$l2 %>%
  as.character %>%
  removeNumbers() %>%
  removeSpecChar() %>%
  tolower()

Dirty_users_2$l3 <- Dirty_users_2$l3 %>%
  as.character %>%
  removeNumbers() %>%
  removeSpecChar() %>%
  tolower()

Dirty_users_2$l4 <- Dirty_users_2$l4 %>%
  as.character %>%
  removeNumbers() %>%
  removeSpecChar() %>%
  tolower()

Dirty_users_2$l5 <- Dirty_users_2$l5 %>%
  as.character %>%
  removeNumbers() %>%
  removeSpecChar() %>%
  tolower()

#Apply location filters

Clean_users <- Dirty_users_2 %>%
  arrange(by = id)

munlistcleaner <- as.vector(munlist2$munlist2)

Clean_users1 <- subset(Clean_users, location %in% munlistcleaner)
Clean_users2 <- subset(Clean_users, l1 %in% munlistcleaner)
Clean_users3 <- subset(Clean_users, l2 %in% munlistcleaner)
Clean_users4 <- subset(Clean_users, l3 %in% munlistcleaner)
Clean_users5 <- subset(Clean_users, l4 %in% munlistcleaner)
Clean_users6 <- subset(Clean_users, l5 %in% munlistcleaner)

Clean_users <- rbind(Clean_users1, Clean_users2, Clean_users3, Clean_users4, Clean_users5, Clean_users6)

Clean_users$l1 <- NULL
Clean_users$l2 <- NULL
Clean_users$l3 <- NULL
Clean_users$l4 <- NULL
Clean_users$l5 <- NULL

Clean_users$id <- as.numeric(Clean_users$id)

Clean_users <- Clean_users[Reduce(`&`, lapply(Clean_users, function(x) !(is.na(x)|x==""))),]

UsersDF_clean <- Clean_users[!duplicated(Clean_users$id), ]

UsersDF_clean <- UsersDF_clean %>%
  arrange(by = location)

UsersDF_cleanprueba <- UsersDF_clean[!grepl("san francisco ca", UsersDF_clean$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("san francisco", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("los angeles ca", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("los angeles", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("san diego ca", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("san diego", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("palo alto ca", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("palo alto", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("santa clara ca", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("santa clara", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("silicon valey", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("san jose ca", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("san jose", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("santa barbara ca", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("santa barbara", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("puerto alegre brasil", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("porto alegre brasil", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("puerto alegre brasil", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("santiago de chile", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("santiago chile", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("santa monica ca", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("santa monica", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("san mateo ca", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("san mateo", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("del mar ca", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("porto portugal", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("central florida", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("orlando florida", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("north carolina", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("guadalajara méxico", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("guadalupe ca", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("san miguel de allende", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("santa fe argentina", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("santo domingord", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("santa fe argentina", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("san antonio tx", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("nevada", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("nuevo león", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("nuevo laredo", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("medellín colombia", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("medellín antioquia colombia", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("medellín bogotá colombia", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("medellín bogotá", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("medellín", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("porto alegre", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("posadas argentina", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("posadas misiones", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("posadas misiones argentina", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("lomas de zamora argentina", UsersDF_cleanprueba$location),]
UsersDF_cleanprueba <- UsersDF_cleanprueba[!grepl("luque paraguay", UsersDF_cleanprueba$location),]

UsersDF_cleanprueba <- UsersDF_cleanprueba %>%
  arrange(by = location)

UsersDF_cleanprueba <- UsersDF_cleanprueba[-c(1:3838), ]

langu <- c("es", "ca", "eu", "gl", "en", "fr", "it")

UsersDF_langprueba <- subset(UsersDF_cleanprueba, lang %in% langu)

UsersDF_langprueba <- UsersDF_langprueba %>%
  arrange(by = location)

save(UsersDF_clean, file = "Objects/UsersDF1_c.RData")

rm(Clean_users, Clean_users1, Clean_users2, Clean_users3, Clean_users4, Clean_users5, Clean_users6, Dirty_users,
   Dirty_users_2, Dirty_colums)

UsersDF_final <- UsersDF_cleanprueba
save(UsersDF_final, file = "Objects/UsersDF_final.RData")

