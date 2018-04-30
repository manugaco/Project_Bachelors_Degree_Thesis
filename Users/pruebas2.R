
#WORKING IN THE NIGHT

top100 <- read_excel("~/Desktop/TFG/Datasets/top100_1.1.xlsx")

top100x <- filter(top100, Seguidopor < 75000 | 'Sigue a' < 75000)

top100x <-top100x %>%
  group_by(Cuenta) %>% 
  filter(row_number(Cuenta) == 1)

Viplistx <- as.vector(top100x$Cuenta)

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
    dplyr:::select(id, name, location, lang)
  
  Dirty_users <- UsersDF
  Dirty_users$id <- as.numeric(Dirty_users$id) #Change
  Dirty_users$location <- removePunctuation(Dirty_users$location)
  
  removeSpecChar <- function(x) gsub("[^[:alnum:][:blank:]+?&/\\-]", "", x)
  
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
  
  Clean_users <- subset(Clean_users, V1 %in% munlist)
  Clean_users <- subset(Clean_users, V2 %in% munlist)
  Clean_users <- subset(Clean_users, lang %in% langu)
  
  Clean_users$V1 <- NULL
  Clean_users$V2 <- NULL
  
  Clean_users$id <- as.character(Clean_users$id) #Change
  
  UsersDFLOOP <- Clean_users[Reduce(`&`, lapply(Clean_users, function(x) !(is.na(x)|x==""))),]
} #works

Usersx_clean <- base:::as.data.frame(sapply(Viplistx, fun_uff_clean))

Userlistfinal <- Usersx_clean %>%
  map(extract) %>%
  map_df(bind_rows)

#Users number()
