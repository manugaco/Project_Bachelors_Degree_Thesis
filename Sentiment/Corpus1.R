

#Libraries

library("tidyverse")

#Merging the samples (datasets/test-train)

DF1 <- select(DF1, c("username", "date", "retweets", "favorites", "text"))
DF2 <- select(DF2, c("username", "date", "retweets", "favorites", "text"))
DF3 <- select(DF3, c("username", "date", "retweets", "favorites", "text"))
DF4 <- select(DF4, c("username", "date", "retweets", "favorites", "text"))
DF5 <- select(DF5, c("username", "date", "retweets", "favorites", "text"))
DF6 <- select(DF6, c("username", "date", "retweets", "favorites", "text"))
DF7 <- select(DF7, c("username", "date", "retweets", "favorites", "text"))
DF8 <- select(DF8, c("username", "date", "retweets", "favorites", "text"))
DF9 <- select(DF9, c("username", "date", "retweets", "favorites", "text"))
DF10 <- select(DF10, c("username", "date", "retweets", "favorites", "text"))
DF11 <- select(DF11, c("username", "date", "retweets", "favorites", "text"))
DF12 <- select(DF12, c("username", "date", "retweets", "favorites", "text"))
DF13<- select(DF13, c("username", "date", "retweets", "favorites", "text"))
DF14 <- select(DF14, c("username", "date", "retweets", "favorites", "text"))

DF <- rbind(DF1, DF2, DF3, DF4, DF5, DF6, DF7, DF8, DF9, DF10, DF11, DF12, DF13, DF14)

DF <- DF[!duplicated(DF$text),]

rawcorpus <- DF

rm(DF1, DF2, DF3, DF4, DF5, DF6, DF7, DF8, DF9, DF10, DF11, DF12, DF13, DF14, DF)

#Split the corpus in train and test sets (70-30) randomly

corpustrain <- sample_frac(rawcorpus, 0.8)
sid <- as.numeric(rownames(corpustrain)) # because rownames() returns character
corpustest <- rawcorpus[-sid,]

corpustrain <- select(corpustrain, "text")
corpustest <- select(corpustest, "text")

#Saving the dataframes

write.csv(corpustrain, file = "Objects/Corpus/corpustrain.csv",row.names=FALSE)
write.csv(corpustest, file = "Objects/Corpus/corpustest.csv",row.names=FALSE)

rm(corpustest, corpustrain)


