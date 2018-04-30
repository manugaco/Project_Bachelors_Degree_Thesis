#Libraries

library("tidyverse")
library("xlsx")
library("readxl")
library("data.table")
library("rvest")
library("twitteR")

#Accessing Twitter API with public key

setup_twitter_oauth(consumer_key = "u2UthjbK6YHyQSp4sPk6yjsuV",
                    consumer_secret = "sC4mjd2WME5nH1FoWeSTuSy7JCP5DHjNtTYU1X6BwQ1vPZ0j3v", 
                    access_token = "1365606414-7vPfPxStYNq6kWEATQlT8HZBd4G83BBcX4VoS9T",
                    access_secret = "0hJq9KYC3eBRuZzJqSacmtJ4PNJ7tNLkGrQrVl00JHirs")

#Making variables from the ones of the main script to test it with low data charge (1-3 users)

Viplist_1p <- Viplist[c(99)]
Viplist_2p <- Viplist[c(98,99)]
Viplist_3p <- Viplist[c(97:99)]
Viplist_4p <- Viplist[c(95:99)]

#Getting the viplist information

all <- c("id", "name", "screenName", "location", "lang")

UsersDFp <- twListToDF(lookupUsers(Viplistv)) %>%
  arrange(by=id) %>%
  unique() %>%
  select(all)

write.xlsx(Userslist_3, "Datasets/Userslist_0.xlsx")

#1 Works (but don't use it)

UsersListv <- as.data.frame(sapply(Viplistv, fun_uff)) #Works and saves a dataframe
save(UsersListv, file="Objects/UL3.Rdata")

#2 Testing pins

#One user, friends and followers (works)

Userslist_4p <- as.data.frame(sapply(Viplist_4p, fun_uff))
Userslist_44p <- Userslist_4p
Userslist_44p <- as.data.frame(do.call("rbind", Userslist_44p))
Userslist_44p <- map_df(Userslist_44p, extract, all)
                         

#Several users, friends (works)

Userslist_3p1 <- as.data.frame(sapply(Viplist_3p, fun_uf1))
Userslist_3p1 <- as.data.frame(do.call("rbind", Userslist_3p1))
Userslist_3p1 <- map_df(Userslist_3p1, extract, all)

write.xlsx(Userslist_3p1, "Datasets/Userslist_1.xlsx")

#Several users, followers (works)

Userslist_3p2 <- as.data.frame(sapply(Viplist_3p, fun_uf2))
Userslist_3p2 <- as.data.frame(do.call("rbind", Userslist_3p2))
Userslist_3p2 <- map_df(Userslist_3p2, extract, all)

write.xlsx(Userslist_3p2, "Datasets/Userslist_2.xlsx")

#Binding users, friends and followers

Userslist_3p <- rbind(UsersDFp, Userslist_3p1, Userslist_3p2)

write.xlsx(Userslist_3p, "Datasets/Userslist_3.xlsx")

#3 Sort by id and remove repeated values with dplyr

UsersDF3p <- Userslist_3p %>%
  arrange(by=id) %>%
  unique()

write.xlsx(UsersDF3p, "Datasets/Userslist_4.xlsx")
write.csv(UsersDF3p, "Datasets/Userslist_csv.csv")

#4 Transform list to dataframe (Works)

UsersDF <- map_df(Userslist_3p, extract, all)

#5 testing

top100p <- filter(top100, Seguidopor < 75000 | 'Sigue a' < 75000)

top100p <-top100p %>%
  group_by(Cuenta) %>% 
  filter(row_number(Cuenta) == 1)

Viplistp <- as.vector(top100p$Cuenta)

UsersDFp <- twListToDF(lookupUsers(Viplistp))
UsersDFp <- UsersDFp %>%
  arrange(by=id) %>%
  unique()
UsersDFp <- dplyr:::select(UsersDFp, all)


###

Viplist_1p <- Viplist[c(99)]

UsersDFpp_clean <- as.data.frame(sapply(Viplist_1p, fun_uff_clean))

UsersDFx <- UsersDFpp_clean
UsersDFx <- as.data.frame(do.call("rbind", UsersDFx))

UsersDFx <- as.vector(UsersDFx)

UsersDFx <- map_df(as.numeric(UsersDFx), extract, all)

UsersDFppp <- as.vector(cbind(UsersDFpp, UsersDFpp_clean))

Userslist1 <- as.data.frame(do.call("rbind", UsersDFpp))
UsersDF1.1p <- map_df(Userslist1, extract, all) %>%
  arrange(by=id) %>%
  unique()

Userslist2 <- as.data.frame(do.call("rbind", UsersDFpp_clean))
UsersDF2.2p <- map_df(Userslist2, extract, all) %>%
  arrange(by=id) %>%
  unique()

UsersDF4p <- map_df(UsersDFppp, extract, all) %>%
  arrange(by=id) %>%
  unique()

UsersDF <- rbind(UsersDF1.1p, UsersDF2.2p)

###

#CIS microdata

cis_micro <- read.spss("Datasets/FID_357.sav", to.data.frame=TRUE)

