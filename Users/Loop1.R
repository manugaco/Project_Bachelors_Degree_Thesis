

##Complete loop

#load dataframe

UsersDF_clean

#Conditon (5000-75000)

UsersDF_clean_FR <- UsersDF_clean %>%
  group_by(id) %>% 
  filter(row_number(id) == 1) %>%
  filter(friendsCount < 75000 & friendsCount > 5000) %>%
  filter(protected == FALSE)

UsersDF_clean_FO <- UsersDF_clean %>%
  group_by(id) %>% 
  filter(row_number(id) == 1) %>%
  filter(followersCount < 75000 & followersCount > 5000) %>%
  filter(protected == FALSE)

UsersDF_loop <- rbind(UsersDF_clean_FR, UsersDF_clean_FO)

UsersDF_loop <- UsersDF_loop %>%
  arrange(by=id) %>%
  unique()

Userslist1 <- as.vector(UsersDF_loop$screenName)

Randlist1 <- sample(Userslist1, 100)

#Split friend and followers

all <- c("id", "screenName", "followersCount", "friendsCount", "protected", "location", "lang")
fri <- c("id", "screenName", "friendsCount", "protected", "location", "lang")
foll <- c("id", "screenName", "followersCount","protected", "location", "lang")

UsersDF1 <- twListToDF(lookupUsers(Randlist1))
UsersDF1 <- UsersDF1 %>%
  arrange(by=id) %>%
  dplyr:::select(all)

UsersDF1 <- UsersDF1[!duplicated(UsersDF1$id), ]

Viplist1 <- as.vector(UsersDF1$screenName)

rm(UsersDF_clean_FO, UsersDF_clean_FR, UsersDF_loop, Randlist1, UsersDF1)

#Saving users list into json, xlsx and Rdata Objects

#Loop 1 users ()

UsersDF_clean_loop1 <- as.data.frame(sapply(Viplist1, fun_uff_clean))
save(UsersDF_clean_loop1, file = "Objects/UDFLOOP1.RData")

#Loop 2 users ()

UsersDF_clean_loop2 <- as.data.frame(sapply(Viplist1, fun_uff_clean))
save(UsersDF_clean_loop2, file = "Objects/UDFLOOP1.RData")

#and so on...

#Final number of users ~ 2MM

glimpse(UsersDF)