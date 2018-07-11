

library("tidyverse")
library("splitstackshape")

counttweets <- read_csv2(file="~/Desktop/S3_14_13-1.csv", col_names = FALSE)

colnames(counttweets) <- c("mess")

#Splitting the columns

counttweets <- separate(data = counttweets, col = mess, into = c("mess", "stats"), sep = "\\|")

counttweets <- separate(data = counttweets, col = mess, into = c("mess", "tweets"), sep = "\\>")

counttweets <- cSplit(counttweets, "mess", " ")
counttweets <- counttweets[,-(8:19),drop=FALSE]
counttweets <- counttweets[,-c(3,5:6),drop=FALSE]
colnames(counttweets) <- c("tweets", "stats", "date", "user")

counttweets <- cSplit(counttweets, "stats", " ")
counttweets <- counttweets[,-c(5,7,9),drop=FALSE]
counttweets <- counttweets[,-(7:15),drop=FALSE]
colnames(counttweets) <- c("tweets", "date", "user", "replies", "retweets", "likes")

#Reorder the columns

counttweets <- twSample[,c(3,2,1,4,5,6)]

#Cleaning the text

counttweets <- counttweets[!(is.na(counttweets$tweets) | counttweets$tweets==""), ]

counttweets$tweets <- sub("@ ", "@", counttweets$tweets)
counttweets$tweets <- sub(" w ", "", counttweets$tweets)
counttweets$user <- sub("<", "", counttweets$user)
counttweets$tweets <- gsub("pic.twitter.com/", "http://www.", counttweets$tweets)
counttweets$tweets <- gsub("http[[:alnum:][:punct:]]*", "", counttweets$tweets)
counttweets$tweets <- gsub("[[:alnum:][:punct:]]*.html", "", counttweets$tweets)
counttweets$tweets <- gsub("rt [[:alnum:][:punct:]]*", "", counttweets$tweets)
counttweets$tweets <- gsub("(RT)((?:\\b\\W*@\\w+)+)", "", counttweets$tweets)
counttweets$tweets <- gsub("#[[:alpha:][:alnum:]]*", "", counttweets$tweets)
counttweets$tweets <- gsub("@[[:alpha:][:alnum:]]*", "", counttweets$tweets)
counttweets$tweets <- gsub("[^[:alpha:][:space:]]*", "", counttweets$tweets)

counttweets$tweets <- counttweets$tweets %>%
  tolower()

distinct(counttweets, user)
