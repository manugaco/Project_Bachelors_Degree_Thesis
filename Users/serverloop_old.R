#server script to get friends and followers from a list of 100 random users of the dataset.

#Libraries

library("tidyverse")
library("xlsx")
library("readxl")
library("rvest")
library("twitteR")
library("tm")
library("magrittr")

#Conditon (5000-75000)

UsersDF_FR <- mainUDF %>%
  group_by(id) %>% 
  filter(row_number(id) == 1) %>%
  filter(friendsCount < 75000 & friendsCount > 5000) %>%
  filter(protected == FALSE)

Userslist1 <- as.vector(UsersDF_loop$screenName)

Randlist1 <- sample(Userslist1, 100)

#Split friend and followers

all <- c("id", "screenName", "followersCount", "friendsCount", "protected", "location", "lang")
fri <- c("id", "screenName", "friendsCount", "protected", "location", "lang")
foll <- c("id", "screenName", "followersCount","protected", "location", "lang")

UsersDF1 <- twListToDF(lookupUsers(Randlist1))
UsersDF1 <- UsersDF1 %>%
  arrange(by=id) %>%
  unique() %>%
  dplyr:::select(all)

Viplist1 <- as.vector(UsersDF1$screenName)

Viplist01 <- Viplist1[c(1:5)]
Viplist02 <- Viplist1[c(6:10)]
Viplist03 <- Viplist1[c(11:15)]
Viplist04 <- Viplist1[c(16:20)]
Viplist05 <- Viplist1[c(21:25)]
Viplist06 <- Viplist1[c(26:30)]
Viplist07 <- Viplist1[c(31:35)]
Viplist08 <- Viplist1[c(36:40)]
Viplist09 <- Viplist1[c(41:45)]
Viplist010 <- Viplist1[c(46:50)]
Viplist011 <- Viplist1[c(51:55)]
Viplist012 <- Viplist1[c(56:60)]
Viplist013 <- Viplist1[c(61:65)]
Viplist014 <- Viplist1[c(66:70)]
Viplist015 <- Viplist1[c(71:75)]
Viplist016 <- Viplist1[c(76:80)]
Viplist017 <- Viplist1[c(81:85)]
Viplist018 <- Viplist1[c(86:90)]
Viplist019 <- Viplist1[c(91:95)]
Viplist020 <- Viplist1[c(96:100)]

#functions

fun_UFR_clean <- function(x){
  user <- getUser(x)
  friends <- user$getFriends()
  if(length(friends)>0){
    friendsDf <- friends %>%
      unclass() %>%
      twListToDF()
  }
  friendsDf$id <- as.numeric(friendsDf$id)
  friendsDf <- friendsDf %>%
    arrange(by=id) %>%
    unique() %>%
    dplyr:::select(all)
  Dirty_users <- friendsDf
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
  
  Clean_users <- Dirty_users_2 %>%
    arrange(by = id) %>%
    unique()
  
  Clean_users1 <- subset(Clean_users, location %in% munlist2)
  Clean_users2 <- subset(Clean_users, l1 %in% munlist2)
  Clean_users3 <- subset(Clean_users, l2 %in% munlist2)
  Clean_users4 <- subset(Clean_users, l3 %in% munlist2)
  Clean_users5 <- subset(Clean_users, l4 %in% munlist2)
  Clean_users6 <- subset(Clean_users, l5 %in% munlist2)
  
  Clean_users <- rbind(Clean_users1, Clean_users2, Clean_users3, Clean_users4, Clean_users5, Clean_users6)
  
  Clean_users$l1 <- NULL
  Clean_users$l2 <- NULL
  Clean_users$l3 <- NULL
  Clean_users$l4 <- NULL
  Clean_users$l5 <- NULL
  
  Clean_users$id <- as.numeric(Clean_users$id)
  
  Clean_users <- Clean_users[Reduce(`&`, lapply(Clean_users, function(x) !(is.na(x)|x==""))),]
  
  UsersDF_clean <- Clean_users[!duplicated(Clean_users$id), ]
}

#Getting friends and followers

#Friends

UsersDFC_FR_01 <- as.data.frame(sapply(Viplist01, fun_UFR_clean))
UsersDF_FR_01 <- UsersDFC_FR_01 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_02 <- as.data.frame(sapply(Viplist02, fun_UFR_clean))
UsersDF_FR_02 <- UsersDFC_FR_02 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_03 <- as.data.frame(sapply(Viplist03, fun_UFR_clean))
UsersDF_FR_03 <- UsersDFC_FR_03 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_04 <- as.data.frame(sapply(Viplist04, fun_UFR_clean))
UsersDF_FR_04 <- UsersDFC_FR_04 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_05 <- as.data.frame(sapply(Viplist05, fun_UFR_clean))
UsersDF_FR_05 <- UsersDFC_FR_05 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_06 <- as.data.frame(sapply(Viplist06, fun_UFR_clean))
UsersDF_FR_06 <- UsersDFC_FR_06 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_07 <- as.data.frame(sapply(Viplist07, fun_UFR_clean))
UsersDF_FR_07 <- UsersDFC_FR_07 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_08 <- as.data.frame(sapply(Viplist08, fun_UFR_clean))
UsersDF_FR_08 <- UsersDFC_FR_08 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_09 <- as.data.frame(sapply(Viplist09, fun_UFR_clean))
UsersDF_FR_09 <- UsersDFC_FR_09 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_010 <- as.data.frame(sapply(Viplist010, fun_UFR_clean))
UsersDF_FR_010 <- UsersDFC_FR_010 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_011 <- as.data.frame(sapply(Viplist011, fun_UFR_clean))
UsersDF_FR_011 <- UsersDFC_FR_011 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_012 <- as.data.frame(sapply(Viplist012, fun_UFR_clean))
UsersDF_FR_012 <- UsersDFC_FR_012 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_013 <- as.data.frame(sapply(Viplist013, fun_UFR_clean))
UsersDF_FR_013 <- UsersDFC_FR_013 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_014 <- as.data.frame(sapply(Viplist014, fun_UFR_clean))
UsersDF_FR_014 <- UsersDFC_FR_014 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_015 <- as.data.frame(sapply(Viplist015, fun_UFR_clean))
UsersDF_FR_015 <- UsersDFC_FR_015 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_016 <- as.data.frame(sapply(Viplist016, fun_UFR_clean))
UsersDF_FR_016 <- UsersDFC_FR_016 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_017 <- as.data.frame(sapply(Viplist017, fun_UFR_clean))
UsersDF_FR_017 <- UsersDFC_FR_017 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_018 <- as.data.frame(sapply(Viplist018, fun_UFR_clean))
UsersDF_FR_018 <- UsersDFC_FR_018 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_019 <- as.data.frame(sapply(Viplist019, fun_UFR_clean))
UsersDF_FR_019 <- UsersDFC_FR_019 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FR_020 <- as.data.frame(sapply(Viplist020, fun_UFR_clean))
UsersDF_FR_020 <- UsersDFC_FR_020 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFFR <- rbind(UsersDF_FR_01, UsersDF_FR_02, UsersDF_FR_03, UsersDF_FR_04, UsersDF_FR_05, UsersDF_FR_06, UsersDF_FR_07,
      UsersDF_FR_08, UsersDF_FR_09, UsersDF_FR_010, UsersDF_FR_011, UsersDF_FR_012, UsersDF_FR_013, UsersDF_FR_014,
      UsersDF_FR_015, UsersDF_FR_015, UsersDF_FR_016, UsersDF_FR_017, UsersDF_FR_018, UsersDF_FR_019, UsersDF_FR_020)

rm(UsersDF_FR_01, UsersDF_FR_02, UsersDF_FR_03, UsersDF_FR_04, UsersDF_FR_05, UsersDF_FR_06, UsersDF_FR_07,
   UsersDF_FR_08, UsersDF_FR_09, UsersDF_FR_010, UsersDF_FR_011, UsersDF_FR_012, UsersDF_FR_013, UsersDF_FR_014,
   UsersDF_FR_015, UsersDF_FR_015, UsersDF_FR_016, UsersDF_FR_017, UsersDF_FR_018, UsersDF_FR_019, UsersDF_FR_020,
   UsersDFC_FR_01, UsersDFC_FR_02, UsersDFC_FR_03, UsersDFC_FR_04, UsersDFC_FR_05, UsersDFC_FR_06, UsersDFC_FR_07,
   UsersDFC_FR_08, UsersDFC_FR_09, UsersDFC_FR_010, UsersDFC_FR_011, UsersDFC_FR_012, UsersDFC_FR_013, UsersDFC_FR_014,
   UsersDFC_FR_015, UsersDFC_FR_015, UsersDFC_FR_016, UsersDFC_FR_017, UsersDFC_FR_018, UsersDFC_FR_019, UsersDFC_FR_020)

#Followers

sersDFC_FO_01 <- as.data.frame(sapply(Viplist01, fun_UFO_clean))
UsersDF_FO_01 <- UsersDFC_FO_01 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_02 <- as.data.frame(sapply(Viplist02, fun_UFO_clean))
UsersDF_FO_02 <- UsersDFC_FO_02 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_03 <- as.data.frame(sapply(Viplist03, fun_UFO_clean))
UsersDF_FO_03 <- UsersDFC_FO_03 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_04 <- as.data.frame(sapply(Viplist04, fun_UFO_clean))
UsersDF_FO_04 <- UsersDFC_FO_04 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_05 <- as.data.frame(sapply(Viplist05, fun_UFO_clean))
UsersDF_FO_05 <- UsersDFC_FO_05 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_06 <- as.data.frame(sapply(Viplist06, fun_UFO_clean))
UsersDF_FO_06 <- UsersDFC_FO_06 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_07 <- as.data.frame(sapply(Viplist07, fun_UFO_clean))
UsersDF_FO_07 <- UsersDFC_FO_07 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_08 <- as.data.frame(sapply(Viplist08, fun_UFO_clean))
UsersDF_FO_08 <- UsersDFC_FO_08 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_09 <- as.data.frame(sapply(Viplist09, fun_UFO_clean))
UsersDF_FO_09 <- UsersDFC_FO_09 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_010 <- as.data.frame(sapply(Viplist010, fun_UFO_clean))
UsersDF_FO_010 <- UsersDFC_FO_010 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_011 <- as.data.frame(sapply(Viplist011, fun_UFO_clean))
UsersDF_FO_011 <- UsersDFC_FO_011 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_012 <- as.data.frame(sapply(Viplist012, fun_UFO_clean))
UsersDF_FO_012 <- UsersDFC_FO_012 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_013 <- as.data.frame(sapply(Viplist013, fun_UFO_clean))
UsersDF_FO_013 <- UsersDFC_FO_013 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_014 <- as.data.frame(sapply(Viplist014, fun_UFO_clean))
UsersDF_FO_014 <- UsersDFC_FO_014 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_015 <- as.data.frame(sapply(Viplist015, fun_UFO_clean))
UsersDF_FO_015 <- UsersDFC_FO_015 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_016 <- as.data.frame(sapply(Viplist016, fun_UFO_clean))
UsersDF_FO_016 <- UsersDFC_FO_016 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_017 <- as.data.frame(sapply(Viplist017, fun_UFO_clean))
UsersDF_FO_017 <- UsersDFC_FO_017 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_018 <- as.data.frame(sapply(Viplist018, fun_UFO_clean))
UsersDF_FO_018 <- UsersDFC_FO_018 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_019 <- as.data.frame(sapply(Viplist019, fun_UFO_clean))
UsersDF_FO_019 <- UsersDFC_FO_019 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFC_FO_020 <- as.data.frame(sapply(Viplist020, fun_UFO_clean))
UsersDF_FO_020 <- UsersDFC_FO_020 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

UsersDFFO <- rbind(UsersDF_FO_01, UsersDF_FO_02, UsersDF_FO_03, UsersDF_FO_04, UsersDF_FO_05, UsersDF_FO_06, UsersDF_FO_07,
                   UsersDF_FO_08, UsersDF_FO_09, UsersDF_FO_010, UsersDF_FO_011, UsersDF_FO_012, UsersDF_FO_013, UsersDF_FO_014,
                   UsersDF_FO_015, UsersDF_FO_015, UsersDF_FO_016, UsersDF_FO_017, UsersDF_FO_018, UsersDF_FO_019, UsersDF_FO_020)

rm(UsersDF_FO_01, UsersDF_FO_02, UsersDF_FO_03, UsersDF_FO_04, UsersDF_FO_05, UsersDF_FO_06, UsersDF_FO_07,
   UsersDF_FO_08, UsersDF_FO_09, UsersDF_FO_010, UsersDF_FO_011, UsersDF_FO_012, UsersDF_FO_013, UsersDF_FO_014,
   UsersDF_FO_015, UsersDF_FO_015, UsersDF_FO_016, UsersDF_FO_017, UsersDF_FO_018, UsersDF_FO_019, UsersDF_FO_020,
   UsersDFC_FO_01, UsersDFC_FO_02, UsersDFC_FO_03, UsersDFC_FO_04, UsersDFC_FO_05, UsersDFC_FO_06, UsersDFC_FO_07,
   UsersDFC_FO_08, UsersDFC_FO_09, UsersDFC_FO_010, UsersDFC_FO_011, UsersDFC_FO_012, UsersDFC_FO_013, UsersDFC_FO_014,
   UsersDFC_FO_015, UsersDFC_FO_015, UsersDFC_FO_016, UsersDFC_FO_017, UsersDFC_FO_018, UsersDFC_FO_019, UsersDFC_FO_020)

#Merging the dataframes

UsersDF_loop <- rbind(UsersDFFR, UsersDFFO)

mainUDF1 <- rbind(mainUDF, UsersDF_loop)

mainUDF1 <- mainUDF1[!duplicated(mainUDF1$id),]

rm(UsersDFFR, UsersDFFO, UsersDF_FO, UsersDF_FR, UsersDF_loop, Randlist1, Userslist1, Viplist01, Viplist02, Viplist03, 
   Viplist04, Viplist05, Viplist06, Viplist07, Viplist08, Viplist09, Viplist010, Viplist011, Viplist012, Viplist013, 
   Viplist014, Viplist015, Viplist016, Viplist017, Viplist018, Viplist019, Viplist020)

save(mainUDF1, file = "Objects/UDF_loop1.RData")

