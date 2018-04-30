

#Functions

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
    dplyr:::select(all)
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
    dplyr:::select(all)
  
  Dirty_users <- UsersDF
  Dirty_users$id <- as.numeric(Dirty_users$id)
  Dirty_users$location <- removePunctuation(Dirty_users$location)
  
  Dirty_colums <- as.data.frame(str_split_fixed(Dirty_users$location, " ", 5))
  
  Dirty_users_2 <- cbind(Dirty_users, Dirty_colums)
  
  colnames(Dirty_users_2) <- c("id", "screenName", "location", "l1", "l2", "l3", "l4", "l5")
  
  Dirty_users_2$location <- Dirty_users_2$location %>%
    removeNumbers() %>%
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
    arrange(by = id) %>%
    unique()
  
  Clean_users1 <- subset(Clean_users, location %in% munlist2)
  Clean_users2 <- subset(Clean_users, l1 %in% munlist2)
  Clean_users3 <- subset(Clean_users, l2 %in% munlist2)
  Clean_users4 <- subset(Clean_users, l3 %in% munlist2)
  Clean_users5 <- subset(Clean_users, l4 %in% munlist2)
  Clean_users6 <- subset(Clean_users, l5 %in% munlist2)
  
  Clean_users <- rbind(Clean_users1, Clean_users2, Clean_users3, Clean_users4, Clean_users5, Clean_users6)
  
  Clean_users <- Clean_users %>%
    arrange(by=id)
  
  Clean_users <- Clean_users[!duplicated(Clean_users$id),]
  
  Clean_users$l1 <- NULL
  Clean_users$l2 <- NULL
  Clean_users$l3 <- NULL
  Clean_users$l4 <- NULL
  Clean_users$l5 <- NULL
  Clean_users$l6 <- NULL
  
  UsersDF_clean <- Clean_users[Reduce(`&`, lapply(Clean_users, function(x) !(is.na(x)|x==""))),]
} #testing

fun_UFR <- function(x){
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
} #works

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
} #test

fun_UFO <- function(x){       
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
    dplyr:::select(all)
} #works

fun_UFO_clean <- function(x){       
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
    dplyr:::select(all)
  Dirty_users <- followersDf
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
} #test

removeSpecChar <- function(x) gsub("[^[:alnum:][:blank:]+?&/\\-]", "", x) #works

fun_uff_PB <- function(x){
  user <- getUser(x)
  friends <- getFriends(screen_name=user, oauth="~/Desktop/TFG/Credentials/Twitter")
  followers <- getFollowers(screen_name=user, oauth="~/Desktop/TFG/Credentials/Twitter")
  
  userdata <- getUsersBatch(ids=friends,
                            oauth_folder="~/Desktop/TFG/Credentials/Twitter")
  userdata <- getUsersBatch(ids=followers,
                            oauth_folder="~/Desktop/TFG/Credentials/Twitter")
} #testing

fun_uff_PB_clean <- function(x){
  user <- getUser(x)
  
  friends <- getFriends(screen_name=user, oauth="~/Desktop/TFG/Credentials/Twitter")
  followers <- getFollowers(screen_name=user, oauth="~/Desktop/TFG/Credentials/Twitter")
  
  userdatafr <- getUsersBatch(ids=friends,
                            oauth="~/Desktop/TFG/Credentials/Twitter")
  userdatafo <- getUsersBatch(ids=followers,
                            oauth="~/Desktop/TFG/Credentials/Twitter")
  
  UsersDF <- rbind(userdatafr, userdatafo)
  
  UsersDF <- UsersDF %>%
    arrange(by=id_str) %>%
    dplyr:::select(id_str, screen_name, followers_count, friends_count, location)
  
  Dirty_users <- UsersDF
  Dirty_users$id_str <- as.numeric(Dirty_users$id_str) #Change
  Dirty_users$location <- removePunctuation(Dirty_users$location)
  
  removeSpecChar <- function(x) gsub("[^[:alnum:][:blank:]+?&/\\-]", "", x)
  
  Dirty_colums <- as.data.frame(str_split_fixed(Dirty_users$location, " ", 2))
  
  Dirty_users <- cbind(Dirty_users, Dirty_colums)
  
  Dirty_users$id_str <- as.numeric(Dirty_users$id_str)
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
    arrange(by=id_str) %>%
    unique()
  
  Clean_users1 <- subset(Clean_users, location %in% munlist)
  Clean_users2 <- subset(Clean_users, V1 %in% munlist)
  Clean_users3 <- subset(Clean_users, V2 %in% munlist)
  
  Clean_users <- rbind(Clean_users1, Clean_users2, Clean_users3)
  
  Clean_users <- Clean_users %>%
    arrange(by=id_str) %>%
    unique()
  
  Clean_users$V1 <- NULL
  Clean_users$V2 <- NULL
  
  Clean_users$id_str <- as.character(Clean_users$id_str) #change
  
  UsersDFLOOP <- Clean_users[Reduce(`&`, lapply(Clean_users, function(x) !(is.na(x)|x==""))),]
} #testing

