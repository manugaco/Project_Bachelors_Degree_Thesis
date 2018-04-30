library('rtweet')


#create 2 viplist users, one regular other recursive


ids <- 75000
f <- list()

GetFollowersRec <- function(userId, followers, page) {
  
  if (ids == 0) {
    
    # API Twitter Limit reached - Wait
    message("Waiting 15 mins...")
    total <- 15*60 # Total time = 15 min ~ 900 sec
    pb <- txtProgressBar(min = 0, max = total, style = 3) # create progress bar
    
    for (i in 1:total) {
      Sys.sleep(time = 1) # 1 second interval
      setTxtProgressBar(pb, i) # update progress bar
    }
    close(pb)
    
    # Check rate limit followers/ids query
    if (!rate_limit(token = NULL)[38,]$reset > 14.9) {
      message("Waiting 15 seconds more...")
      Sys.sleep(time = 15) # wait 15 seconds more...
    }
    
    message("Go!")
    ids <<- 75000
  }
  
  if (followers <= ids) {
    
    message(paste("Followers < ids | Number of Followers: ",
                  followers, " | Number of resting ids: ",  ids, sep = ""))
    ftemp <- get_followers(user = userId, n = followers, page = page)
    
    if (page == '-1') {
      f <<- append(f, list(ftemp)) # append followers ids
    }
    
    if (page != '-1') {
      df <- data.frame('user_id' = ftemp)
      f <<- append(f, list(df)) # append followers ids
    }
    
    ids <<- ids - followers
    message("Finished!")
    rtemp <- f
    f <<- list()
    return(rtemp)
    
  } else if (followers > ids) {
    
    message(paste("Followers > ids | Number of Followers: ",
                  followers, " | Number of resting ids: ",  ids, sep = ""))
    ftemp <- get_followers(user = userId, n = ids, page = page)
    
    if (page == '-1') {
      f <<- append(f, list(ftemp)) # append followers ids
    }
    
    if (page != '-1') {
      df <- data.frame('user_id' = ftemp)
      f <<- append(f, list(df)) # append followers ids
    }
    
    n <- ids # n = count of followers ids already acquired
    
    pageTemp <- next_cursor(ftemp) # Pagination
    
    # API Twitter Limit reached - Wait
    message("Waiting 15 mins...")
    total <- 15*60 # Total time = 15 min ~ 900 sec
    pb <- txtProgressBar(min = 0, max = total, style = 3) # create progress bar
    
    for (i in 1:total) {
      Sys.sleep(time = 1) # 1 second interval
      setTxtProgressBar(pb, i) # update progress bar
    }
    close(pb)
    
    # Check rate limit followers/ids query
    if (!rate_limit(token = NULL)[38,]$reset > 14.9) {
      message("Waiting 15 seconds more...")
      Sys.sleep(time = 15) # wait 15 seconds more...
    }
    
    message("Go!")
    ids <<- 75000
    
    # Recursive function call
    GetFollowersRecursivePagination(userId = userId,
                                    followers = followers - n,
                                    page = pageTemp)
  }
}

user1 <- lookupUsers(Viplist_1)
user1 <- user1 %>%
  arrange(by=id) %>%
  unique() %>% 
  dplyr:::select(all)

FAOClimate <- GetFollowersRec(userId = user1$id,
                              followers = user1$followers_count,
                              page = '-1')


rtweet:::get_followers(user1, retryonratelimit = TRUE)


pablo <- get_followers("Pablo_Iglesias_", retryonratelimit = TRUE)

pres <- get_followers("potus", retryonratelimit = TRUE)