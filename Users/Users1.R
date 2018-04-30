
#FIRST LOOP to get Users, friends and Followers information from the Viplist using the Twitter API
#twitteR and magrittr, stackoverflow and github

#Users information

Viplist #(from VIP1.R)

all <- c("id", "screenName", "followersCount", "friendsCount", "protected", "location", "lang")
fri <- c("id", "screenName", "friendsCount", "protected", "location", "lang")
foll <- c("id", "screenName", "followersCount","protected", "location", "lang")

UsersDF0 <- twListToDF(lookupUsers(Viplist))
UsersDF0 <- UsersDF0 %>%
  arrange(by=id) %>%
  unique()
UsersDF0 <- dplyr:::select(UsersDF0, all)

#Now I am going split the dataset, one for friends (5000 - 75000) another one to followers (5000 - 75000)

UsersDF0_FR <- UsersDF0 %>%
  dplyr:::select(fri)

UsersDF0_FO <- UsersDF0 %>%
  dplyr:::select(foll)

UsersDF0_FR <- UsersDF0_FR %>%
  group_by(id) %>% 
  filter(row_number(id) == 1) %>%
  filter(friendsCount < 75000 & friendsCount > 5000) %>%
  filter(protected == FALSE)

UsersDF0_FO <- UsersDF0_FO %>%
  group_by(id) %>% 
  filter(row_number(id) == 1) %>%
  filter(followersCount < 75000 & followersCount > 5000) %>%
  filter(protected == FALSE)

ViplistFR <- as.vector(UsersDF0_FR$screenName)
ViplistFO <- as.vector(UsersDF0_FO$screenName)

#Getting friends and followers

#Friends

UsersDFC_FR <- as.data.frame(sapply(ViplistFR, fun_UFR))

UsersDF_FR <- UsersDFC_FR %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR, file = "Objects/UDFFR.RData")

#Followers

ViplistFO1 <- ViplistFO[c(1:5)]
ViplistFO2 <- ViplistFO[c(6:10)]
ViplistFO3 <- ViplistFO[c(11:15)]
ViplistFO4 <- ViplistFO[c(16:20)]
ViplistFO5 <- ViplistFO[c(21:25)]
ViplistFO6 <- ViplistFO[c(26:31)]

UsersDFC_FO1 <- as.data.frame(sapply(ViplistFO1, fun_UFO_clean))
UsersDF_FO1 <- UsersDFC_FO1 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO2 <- as.data.frame(sapply(ViplistFO2, fun_UFO_clean))
UsersDF_FO2 <- UsersDFC_FO2 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO3 <- as.data.frame(sapply(ViplistFO3, fun_UFO_clean))
UsersDF_FO3 <- UsersDFC_FO3 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO4 <- as.data.frame(sapply(ViplistFO4, fun_UFO_clean))
UsersDF_FO4 <- UsersDFC_FO4 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO5 <- as.data.frame(sapply(ViplistFO5, fun_UFO_clean))
UsersDF_FO5 <- UsersDFC_FO5 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO6 <- as.data.frame(sapply(ViplistFO6, fun_UFO_clean))
UsersDF_FO6 <- UsersDFC_FO6 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO <- rbind(UsersDF_FO1, UsersDF_FO2, UsersDF_FO3, UsersDF_FO4, UsersDF_FO5, UsersDF_FO6)

save(UsersDF_FO, file = "Objects/UDFFO.RData")

#Merging the dataframes

UsersDF_1 <- rbind(UsersDF_FR, UsersDF_FO)

save(UsersDF_1, file = "Objects/UDF_1.RData")

