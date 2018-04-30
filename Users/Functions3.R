#Libraries

library("tidyverse")
library("xlsx")
library("readxl")
library("data.table")
library("rvest")
library("twitteR")
library("magrittr")

#Twitter API authenticaton, public key

setup_twitter_oauth(consumer_key = "...",
                    consumer_secret = "...", 
                    access_token = "...",
                    access_secret = "...")

#Loop to Viplist sapply - Users, Followers and Friends with pipes

fun_uff <- function(x){
  user <- getUser(x)
  friends <- user$getFriends()
  followers <- user$getFollowers()
  if(length(friends)>0){
    friendsDf <- friends %>%
      unclass() %>%
      twListToDF()
  }
  if(length(followers)>0){
    followersDf <- followers %>%
      unclass() %>%
      twListToDF()
  }
  UsersDF <- rbind(friendsDf, followersDf)
  UsersDF <- UsersDF %>%
    arrange(by=id) %>%
    dplyr:::select(id, name, screenName, location, lang)
} #works

fun_uf1 <- function(x){
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
    dplyr:::select(id, name, screenName, location, lang)
} #works

fun_uf2 <- function(x){       
  user <- getUser(x)
  followers <- user$getFollowers()
  if(length(followers)>0){
    followersDf <- followers %>%
      unclass() %>%
      twListToDF()
  }
  followersDf$id <- as.numeric(followersDf$id)
  followersDf <- followersDf %>%
    arrange(by=id) %>%
    unique() %>%
    dplyr:::select(id, name, screenName, location, lang)
} #works

fun_uff_clean <- function(x){
  user <- getUser(x)
  friends <- user$getFriends()
  followers <- user$getFollowers()
  if(length(friends)>0){
    friendsDf <- friends %>%
      unclass() %>%
      twListToDF()
  }
  if(length(followers)>0){
    followersDf <- followers %>%
      unclass() %>%
      twListToDF()
  }
  UsersDF <- rbind(friendsDf, followersDf)
  UsersDF <- UsersDF %>%
    arrange(by=id) %>%
    dplyr:::select(id, screenName, followersCount, friendsCount, location)
  
  Dirty_users <- UsersDF
  Dirty_users$id <- as.numeric(Dirty_users$id) #Change
  Dirty_users$location <- removePunctuation(Dirty_users$location)
  
  Dirty_colums <- as.data.frame(str_split_fixed(Dirty_users$location, " ", 2))
  
  Dirty_users <- cbind(Dirty_users, Dirty_colums)
  
  Dirty_users$id <- as.numeric(Dirty_users$id)
  Dirty_users$location <- removePunctuation(Dirty_users$location)
  Dirty_users$V1 <- removePunctuation(as.character(Dirty_users$V1))
  Dirty_users$V2 <- removePunctuation(as.character(Dirty_users$V2))
  
  Dirty_users$location <- Dirty_users$location %>%
    removeNumbers() %>%
    removeSpecChar() %>%
    tolower()
  
  Dirty_users$V1 <- Dirty_users$V1 %>%
    removeNumbers() %>%
    removeSpecChar() %>%
    tolower()
  
  Dirty_users$V2 <- Dirty_users$V2 %>%
    removeNumbers() %>%
    removeSpecChar() %>%
    tolower()
  
  Clean_users <- Dirty_users %>%
    arrange(by = id) %>%
    unique()
  
  Clean_users1 <- subset(Clean_users, location %in% munlist)
  Clean_users2 <- subset(Clean_users, V1 %in% munlist)
  Clean_users3 <- subset(Clean_users, V2 %in% munlist)
  
  Clean_users <- rbind(Clean_users1, Clean_users2, Clean_users3)
  
  Clean_users <-  Clean_users %>%
    arrange(by=id) %>%
    unique()
  
  Clean_users$V1 <- NULL
  Clean_users$V2 <- NULL
  
  Clean_users$id <- as.character(Clean_users$id) #Change
  
  UsersDFLOOP <- Clean_users[Reduce(`&`, lapply(Clean_users, function(x) !(is.na(x)|x==""))),]
} #works

removeSpecChar <- function(x) gsub("[^[:alnum:][:blank:]+?&/\\-]", "", x)
