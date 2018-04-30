#server script to get friends from a list of 100 random users of the dataset.

#Libraries

library("tidyverse")
library("twitteR")
library("magrittr")
library("tm")

print("libraries loaded")

load(file="Objects/UDFF_C_L4.RData")
load(file="Objects/munlist2.RData")

mainUDF <- UsersDF_clean4

#API key

setup_twitter_oauth(consumer_key = "...",
                    consumer_secret = "...", 
                    access_token = "...",
                    access_secret = "...")

print("objects loaded")

#Conditon (5000-75000)

UsersDF_FR <- mainUDF %>%
  group_by(id) %>% 
  filter(row_number(id) == 1) %>%
  filter(friendsCount < 75000 & friendsCount > 5000) %>%
  filter(protected == FALSE)

Userslist1 <- as.vector(UsersDF_FR$screenName)

Randlist1 <- sample(Userslist1, 100)

print("randlist done")

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

print("Viplist done")

#Functions

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
  
  UsersDF_clean <- Clean_users[!duplicated(Clean_users$id),]
}

removeSpecChar <- function(x) gsub("[^[:alnum:][:blank:]+?&/\\-]", "", x)

print("functions loaded")

#Getting friends and followers

#Friends

print("1/20 downloading")

UsersDFC_FR_01 <- as.data.frame(sapply(Viplist01, fun_UFR_clean))
UsersDF_FR1_L1 <- UsersDFC_FR_01 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR1_L1, file = "Objects/UDCFR1_L1.RData")

print("1/20 done")
print("2/20 downloading")

UsersDFC_FR_02 <- as.data.frame(sapply(Viplist02, fun_UFR_clean))
UsersDF_FR2_L1 <- UsersDFC_FR_02 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR2_L1, file = "Objects/UDCFR2_L1.RData")

print("2/20 done")
print("3/20 downloading")

UsersDFC_FR_03 <- as.data.frame(sapply(Viplist03, fun_UFR_clean))
UsersDF_FR3_L1 <- UsersDFC_FR_03 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR3_L1, file = "Objects/UDCFR3_L1.RData")

print("3/20 done")
print("4/20 downloading")

UsersDFC_FR_04 <- as.data.frame(sapply(Viplist04, fun_UFR_clean))
UsersDF_FR4_L1 <- UsersDFC_FR_04 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR4_L1, file = "Objects/UDCFR4_L1.RData")

print("4/20 done")
print("5/20 downloading")

UsersDFC_FR_05 <- as.data.frame(sapply(Viplist05, fun_UFR_clean))
UsersDF_FR5_L1 <- UsersDFC_FR_05 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR5_L1, file = "Objects/UDCFR5_L1.RData")

print("5/20 done")
print("6/20 downloading")

UsersDFC_FR_06 <- as.data.frame(sapply(Viplist06, fun_UFR_clean))
UsersDF_FR6_L1 <- UsersDFC_FR_06 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR6_L1, file = "Objects/UDCFR6_L1.RData")

print("6/20 done")
print("7/20 downloading")

UsersDFC_FR_07 <- as.data.frame(sapply(Viplist07, fun_UFR_clean))
UsersDF_FR7_L1 <- UsersDFC_FR_07 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR7_L1, file = "Objects/UDCFR7_L1.RData")

print("7/20 done")
print("8/20 downloading")

UsersDFC_FR_08 <- as.data.frame(sapply(Viplist08, fun_UFR_clean))
UsersDF_FR8_L1 <- UsersDFC_FR_08 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR8_L1, file = "Objects/UDCFR8_L1.RData")

print("8/20 done")
print("9/20 downloading")

UsersDFC_FR_09 <- as.data.frame(sapply(Viplist09, fun_UFR_clean))
UsersDF_FR9_L1 <- UsersDFC_FR_09 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR9_L1, file = "Objects/UDCFR9_L1.RData")

print("9/20 done")
print("10/20 downloading")

UsersDFC_FR_010 <- as.data.frame(sapply(Viplist010, fun_UFR_clean))
UsersDF_FR10_L1 <- UsersDFC_FR_010 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR10_L1, file = "Objects/UDCFR10_L1.RData")

print("10/20 done")
print("11/20 downloading")

UsersDFC_FR_011 <- as.data.frame(sapply(Viplist011, fun_UFR_clean))
UsersDF_FR11_L1 <- UsersDFC_FR_011 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR11_L1, file = "Objects/UDCFR11_L1.RData")

print("11/20 done")
print("12/20 downloading")

UsersDFC_FR_012 <- as.data.frame(sapply(Viplist012, fun_UFR_clean))
UsersDF_FR12_L1 <- UsersDFC_FR_012 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR12_L1, file = "Objects/UDCFR12_L1.RData")

print("12/20 done")
print("13/20 downloading")

UsersDFC_FR_013 <- as.data.frame(sapply(Viplist013, fun_UFR_clean))
UsersDF_FR13_L1 <- UsersDFC_FR_013 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR13_L1, file = "Objects/UDCFR13_L1.RData")

print("13/20 done")
print("14/20 downloading")

UsersDFC_FR_014 <- as.data.frame(sapply(Viplist014, fun_UFR_clean))
UsersDF_FR14_L1 <- UsersDFC_FR_014 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR14_L1, file = "Objects/UDCFR14_L1.RData")

print("14/20 done")
print("15/20 downloading")

UsersDFC_FR_015 <- as.data.frame(sapply(Viplist015, fun_UFR_clean))
UsersDF_FR15_L1 <- UsersDFC_FR_015 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR15_L1, file = "Objects/UDCFR15_L1.RData")

print("15/20 done")
print("16/20 downloading")

UsersDFC_FR_016 <- as.data.frame(sapply(Viplist016, fun_UFR_clean))
UsersDF_FR16_L1 <- UsersDFC_FR_016 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR16_L1, file = "Objects/UDCFR16_L1.RData")

print("16/20 done")
print("17/20 downloading")

UsersDFC_FR_017 <- as.data.frame(sapply(Viplist017, fun_UFR_clean))
UsersDF_FR17_L1 <- UsersDFC_FR_017 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR17_L1, file = "Objects/UDCFR17_L1.RData")

print("17/20 done")
print("18/20 downloading")

UsersDFC_FR_018 <- as.data.frame(sapply(Viplist018, fun_UFR_clean))
UsersDF_FR18_L1 <- UsersDFC_FR_018 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR18_L1, file = "Objects/UDCFR18_L1.RData")

print("18/20 done")
print("19/20 downloading")

UsersDFC_FR_019 <- as.data.frame(sapply(Viplist019, fun_UFR_clean))
UsersDF_FR19_L1 <- UsersDFC_FR_019 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR19_L1, file = "Objects/UDCFR19_L1.RData")

print("19/20 done")
print("20/20 downloading")

UsersDFC_FR_020 <- as.data.frame(sapply(Viplist020, fun_UFR_clean))
UsersDF_FR29_L1 <- UsersDFC_FR_020 %>%
  map(extract) %>%
  map_df(bind_rows) %>%
  dplyr:::select(all)

save(UsersDF_FR20_L1, file = "Objects/UDCFR20_L1.RData")

print("20/20 done")
print("...end of script")
