
library("jsonlite")
library("RJSONIO")

#testing

top100list <- as.vector(top100$Cuenta)

top100v <- as.vector(top100$Cuenta)

Ulist <- top100v[c(1:5)]

Ulistjson <- RJSONIO:::toJSON(Ulist)
write(Ulistjson, "Python/Ulist.json")

#final dataset users vector to loop on bash

UsersDF0 <- twListToDF(lookupUsers(top100list)) %>%
  arrange(by=id) %>%
  unique() %>%
  dplyr:::select(id, screenName, followersCount, friendsCount, protected, location, lang)
UsersDF0 <- UsersDF0[!duplicated(UsersDF0$id), ]

UsersDF_final <- rbind(UsersDF_final, UsersDF0)

Users_list <- as.vector(UsersDF_final$screenName)

#Due to time limitation problems, the list needs to be reduced to 450000 users, using random sampling

Users_list_r <- sample(Users_list, 240000)

Ulist1 <- Users_list_r[c(1:80000)]
Ulist2 <- Users_list_r[c(80001:160000)]
Ulist3 <- Users_list_r[c(160001:240000)]

#JSON export format

Ulistjson1 <- RJSONIO:::toJSON(Ulist1)
write(Ulistjson1, "Objects/Ulist1.json")
Ulistjson2 <- RJSONIO:::toJSON(Ulist2)
write(Ulistjson2, "Objects/Ulist2.json")
Ulistjson3 <- RJSONIO:::toJSON(Ulist3)
write(Ulistjson3, "Objects/Ulist3.json")




