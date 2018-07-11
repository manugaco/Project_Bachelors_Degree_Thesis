
library(plyr)
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(tidyverse)

score.sentiment <- function(sentences, valence, .progress='none') {
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, valence){
    sentence <- gsub('[[:punct:]]', '', sentence) #cleaning tweets
    sentence <- gsub('[[:cntrl:]]', '', sentence) #cleaning tweets
    sentence <- gsub('\\d+', '', sentence) #cleaning tweets
    sentence <- tolower(sentence) #cleaning tweets
    word.list <- str_split(sentence, '\\s+') #separating words
    words <- unlist(word.list)
    val.matches <- match(words, valence$Word) #find words from tweet in "Word" column of dictionary
    val.match <- valence$Rating[val.matches] #evaluating words which were found (suppose rating is in "Rating" column of dictionary).
    val.match <- na.omit(val.match)
    val.match <- as.numeric(val.match)
    score <- sum(val.match)/length(val.match) #rating of tweet (average value of evaluated words)
    return(score)
  }, valence, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences) #save results to the data frame
  return(scores.df)
}
load(file = "Objects/Models/valence.RData")
load(file = "Objects/Models/list.RData")

#Series 22

print("2017")
#December

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_12_22.RData")

DF_2017_12_22_cleanREG <- DF_2017_12_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_12_22_cleanREG$text <- as.factor(DF_2017_12_22_cleanREG$text)

scores <- score.sentiment(DF_2017_12_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_12_22 <- scores
colnames(DF_2017_12_22) <- c("text", "sentiment")

DF_2017_12_22$sentiment[DF_2017_12_22$sentiment <= 5.55555] <- "0"
DF_2017_12_22$sentiment[DF_2017_12_22$sentiment > 5.55555] <- "1"

date <- c("2017-12-22")
tots <- as.numeric(nrow(DF_2017_12_22))
negs <- as.numeric(sum(DF_2017_12_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_12_22$sentiment == "1"))
results2017_12_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_12_22 <- cbind(date, tots, poss, negs, results2017_12_22)
results22LF <- results2017_12_22

rm(DF_2017_12_22_cleanREG, scores, DF_2017_12_22, date, tots, negs, poss, results2017_12_22)

#Nobember

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_11_22.RData")

DF_2017_11_22_cleanREG <- DF_2017_11_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_11_22_cleanREG$text <- as.factor(DF_2017_11_22_cleanREG$text)

scores <- score.sentiment(DF_2017_11_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_11_22 <- scores
colnames(DF_2017_11_22) <- c("text", "sentiment")

DF_2017_11_22$sentiment[DF_2017_11_22$sentiment <= 5.55555] <- "0"
DF_2017_11_22$sentiment[DF_2017_11_22$sentiment > 5.55555] <- "1"

date <- c("2017-11-22")
tots <- as.numeric(nrow(DF_2017_11_22))
negs <- as.numeric(sum(DF_2017_11_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_11_22$sentiment == "1"))
results2017_11_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_11_22 <- cbind(date, tots, poss, negs, results2017_11_22)
results22LF <- rbind(results22LF, results2017_11_22)

rm(DF_2017_11_22_cleanREG, scores, DF_2017_11_22, date, tots, negs, poss, results2017_11_22)

#October

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_10_22.RData")

DF_2017_10_22_cleanREG <- DF_2017_10_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_10_22_cleanREG$text <- as.factor(DF_2017_10_22_cleanREG$text)

scores <- score.sentiment(DF_2017_10_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_10_22 <- scores
colnames(DF_2017_10_22) <- c("text", "sentiment")

DF_2017_10_22$sentiment[DF_2017_10_22$sentiment <= 5.55555] <- "0"
DF_2017_10_22$sentiment[DF_2017_10_22$sentiment > 5.55555] <- "1"

date <- c("2017-10-22")
tots <- as.numeric(nrow(DF_2017_10_22))
negs <- as.numeric(sum(DF_2017_10_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_10_22$sentiment == "1"))
results2017_10_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_10_22 <- cbind(date, tots, poss, negs, results2017_10_22)
results22LF <- rbind(results22LF, results2017_10_22)

rm(DF_2017_10_22_cleanREG, scores, DF_2017_10_22, date, tots, negs, poss, results2017_10_22)

#September

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_9_22.RData")

DF_2017_9_22_cleanREG <- DF_2017_9_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_9_22_cleanREG$text <- as.factor(DF_2017_9_22_cleanREG$text)

scores <- score.sentiment(DF_2017_9_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_9_22 <- scores
colnames(DF_2017_9_22) <- c("text", "sentiment")

DF_2017_9_22$sentiment[DF_2017_9_22$sentiment <= 5.55555] <- "0"
DF_2017_9_22$sentiment[DF_2017_9_22$sentiment > 5.55555] <- "1"

date <- c("2017-09-22")
tots <- as.numeric(nrow(DF_2017_9_22))
negs <- as.numeric(sum(DF_2017_9_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_9_22$sentiment == "1"))
results2017_9_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_9_22 <- cbind(date, tots, poss, negs, results2017_9_22)
results22LF <- rbind(results22LF, results2017_9_22)

rm(DF_2017_9_22_cleanREG, scores, DF_2017_9_22, date, tots, negs, poss, results2017_9_22)

#August

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_8_22.RData")

DF_2017_8_22_cleanREG <- DF_2017_8_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_8_22_cleanREG$text <- as.factor(DF_2017_8_22_cleanREG$text)

scores <- score.sentiment(DF_2017_8_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_8_22 <- scores
colnames(DF_2017_8_22) <- c("text", "sentiment")

DF_2017_8_22$sentiment[DF_2017_8_22$sentiment <= 5.55555] <- "0"
DF_2017_8_22$sentiment[DF_2017_8_22$sentiment > 5.55555] <- "1"

date <- c("2017-08-22")
tots <- as.numeric(nrow(DF_2017_8_22))
negs <- as.numeric(sum(DF_2017_8_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_8_22$sentiment == "1"))
results2017_8_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_8_22 <- cbind(date, tots, poss, negs, results2017_8_22)
results22LF <- rbind(results22LF, results2017_8_22)

rm(DF_2017_8_22_cleanREG, scores, DF_2017_8_22, date, tots, negs, poss, results2017_8_22)

#July

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_7_22.RData")

DF_2017_7_22_cleanREG <- DF_2017_7_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_7_22_cleanREG$text <- as.factor(DF_2017_7_22_cleanREG$text)

scores <- score.sentiment(DF_2017_7_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_7_22 <- scores
colnames(DF_2017_7_22) <- c("text", "sentiment")

DF_2017_7_22$sentiment[DF_2017_7_22$sentiment <= 5.55555] <- "0"
DF_2017_7_22$sentiment[DF_2017_7_22$sentiment > 5.55555] <- "1"

date <- c("2017-07-22")
tots <- as.numeric(nrow(DF_2017_7_22))
negs <- as.numeric(sum(DF_2017_7_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_7_22$sentiment == "1"))
results2017_7_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_7_22 <- cbind(date, tots, poss, negs, results2017_7_22)
results22LF <- rbind(results22LF, results2017_7_22)

rm(DF_2017_7_22_cleanREG, scores, DF_2017_7_22, date, tots, negs, poss, results2017_7_22)

#June

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_6_22.RData")

DF_2017_6_22_cleanREG <- DF_2017_6_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_6_22_cleanREG$text <- as.factor(DF_2017_6_22_cleanREG$text)

scores <- score.sentiment(DF_2017_6_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_6_22 <- scores
colnames(DF_2017_6_22) <- c("text", "sentiment")

DF_2017_6_22$sentiment[DF_2017_6_22$sentiment <= 5.55555] <- "0"
DF_2017_6_22$sentiment[DF_2017_6_22$sentiment > 5.55555] <- "1"

date <- c("2017-06-22")
tots <- as.numeric(nrow(DF_2017_6_22))
negs <- as.numeric(sum(DF_2017_6_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_6_22$sentiment == "1"))
results2017_6_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_6_22 <- cbind(date, tots, poss, negs, results2017_6_22)
results22LF <- rbind(results22LF, results2017_6_22)

rm(DF_2017_6_22_cleanREG, scores, DF_2017_6_22, date, tots, negs, poss, results2017_6_22)

#May

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_5_22.RData")

DF_2017_5_22_cleanREG <- DF_2017_5_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_5_22_cleanREG$text <- as.factor(DF_2017_5_22_cleanREG$text)

scores <- score.sentiment(DF_2017_5_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_5_22 <- scores
colnames(DF_2017_5_22) <- c("text", "sentiment")

DF_2017_5_22$sentiment[DF_2017_5_22$sentiment <= 5.55555] <- "0"
DF_2017_5_22$sentiment[DF_2017_5_22$sentiment > 5.55555] <- "1"

date <- c("2017-05-22")
tots <- as.numeric(nrow(DF_2017_5_22))
negs <- as.numeric(sum(DF_2017_5_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_5_22$sentiment == "1"))
results2017_5_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_5_22 <- cbind(date, tots, poss, negs, results2017_5_22)
results22LF <- rbind(results22LF, results2017_5_22)

rm(DF_2017_5_22_cleanREG, scores, DF_2017_5_22, date, tots, negs, poss, results2017_5_22)

#April

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_4_22.RData")

DF_2017_4_22_cleanREG <- DF_2017_4_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_4_22_cleanREG$text <- as.factor(DF_2017_4_22_cleanREG$text)

scores <- score.sentiment(DF_2017_4_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_4_22 <- scores
colnames(DF_2017_4_22) <- c("text", "sentiment")

DF_2017_4_22$sentiment[DF_2017_4_22$sentiment <= 5.55555] <- "0"
DF_2017_4_22$sentiment[DF_2017_4_22$sentiment > 5.55555] <- "1"

date <- c("2017-04-22")
tots <- as.numeric(nrow(DF_2017_4_22))
negs <- as.numeric(sum(DF_2017_4_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_4_22$sentiment == "1"))
results2017_4_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_4_22 <- cbind(date, tots, poss, negs, results2017_4_22)
results22LF <- rbind(results22LF, results2017_4_22)

rm(DF_2017_4_22_cleanREG, scores, DF_2017_4_22, date, tots, negs, poss, results2017_4_22)

#March

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_3_22.RData")

DF_2017_3_22_cleanREG <- DF_2017_3_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_3_22_cleanREG$text <- as.factor(DF_2017_3_22_cleanREG$text)

scores <- score.sentiment(DF_2017_3_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_3_22 <- scores
colnames(DF_2017_3_22) <- c("text", "sentiment")

DF_2017_3_22$sentiment[DF_2017_3_22$sentiment <= 5.55555] <- "0"
DF_2017_3_22$sentiment[DF_2017_3_22$sentiment > 5.55555] <- "1"

date <- c("2017-03-22")
tots <- as.numeric(nrow(DF_2017_3_22))
negs <- as.numeric(sum(DF_2017_3_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_3_22$sentiment == "1"))
results2017_3_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_3_22 <- cbind(date, tots, poss, negs, results2017_3_22)
results22LF <- rbind(results22LF, results2017_3_22)

rm(DF_2017_3_22_cleanREG, scores, DF_2017_3_22, date, tots, negs, poss, results2017_3_22)

#February

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_2_22.RData")

DF_2017_2_22_cleanREG <- DF_2017_2_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_2_22_cleanREG$text <- as.factor(DF_2017_2_22_cleanREG$text)

scores <- score.sentiment(DF_2017_2_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_2_22 <- scores
colnames(DF_2017_2_22) <- c("text", "sentiment")

DF_2017_2_22$sentiment[DF_2017_2_22$sentiment <= 5.55555] <- "0"
DF_2017_2_22$sentiment[DF_2017_2_22$sentiment > 5.55555] <- "1"

date <- c("2017-02-22")
tots <- as.numeric(nrow(DF_2017_2_22))
negs <- as.numeric(sum(DF_2017_2_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_2_22$sentiment == "1"))
results2017_2_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_2_22 <- cbind(date, tots, poss, negs, results2017_2_22)
results22LF <- rbind(results22LF, results2017_2_22)

rm(DF_2017_2_22_cleanREG, scores, DF_2017_2_22, date, tots, negs, poss, results2017_2_22)

#January

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2017_1_22.RData")

DF_2017_1_22_cleanREG <- DF_2017_1_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_1_22_cleanREG$text <- as.factor(DF_2017_1_22_cleanREG$text)

scores <- score.sentiment(DF_2017_1_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_1_22 <- scores
colnames(DF_2017_1_22) <- c("text", "sentiment")

DF_2017_1_22$sentiment[DF_2017_1_22$sentiment <= 5.55555] <- "0"
DF_2017_1_22$sentiment[DF_2017_1_22$sentiment > 5.55555] <- "1"

date <- c("2017-01-22")
tots <- as.numeric(nrow(DF_2017_1_22))
negs <- as.numeric(sum(DF_2017_1_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_1_22$sentiment == "1"))
results2017_1_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_1_22 <- cbind(date, tots, poss, negs, results2017_1_22)
results22LF <- rbind(results22LF, results2017_1_22)

rm(DF_2017_1_22_cleanREG, scores, DF_2017_1_22, date, tots, negs, poss, results2017_1_22)


print("2016")
#December

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_12_22.RData")

DF_2016_12_22_cleanREG <- DF_2016_12_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_12_22_cleanREG$text <- as.factor(DF_2016_12_22_cleanREG$text)

scores <- score.sentiment(DF_2016_12_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_12_22 <- scores
colnames(DF_2016_12_22) <- c("text", "sentiment")

DF_2016_12_22$sentiment[DF_2016_12_22$sentiment <= 5.55555] <- "0"
DF_2016_12_22$sentiment[DF_2016_12_22$sentiment > 5.55555] <- "1"

date <- c("2016-12-22")
tots <- as.numeric(nrow(DF_2016_12_22))
negs <- as.numeric(sum(DF_2016_12_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_12_22$sentiment == "1"))
results2016_12_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_12_22 <- cbind(date, tots, poss, negs, results2016_12_22)
results22LF <- rbind(results22LF, results2016_12_22)

rm(DF_2016_12_22_cleanREG, scores, DF_2016_12_22, date, tots, negs, poss, results2016_12_22)

#Nobember

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_11_22.RData")

DF_2016_11_22_cleanREG <- DF_2016_11_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_11_22_cleanREG$text <- as.factor(DF_2016_11_22_cleanREG$text)

scores <- score.sentiment(DF_2016_11_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_11_22 <- scores
colnames(DF_2016_11_22) <- c("text", "sentiment")

DF_2016_11_22$sentiment[DF_2016_11_22$sentiment <= 5.55555] <- "0"
DF_2016_11_22$sentiment[DF_2016_11_22$sentiment > 5.55555] <- "1"

date <- c("2016-11-22")
tots <- as.numeric(nrow(DF_2016_11_22))
negs <- as.numeric(sum(DF_2016_11_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_11_22$sentiment == "1"))
results2016_11_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_11_22 <- cbind(date, tots, poss, negs, results2016_11_22)
results22LF <- rbind(results22LF, results2016_11_22)

rm(DF_2016_11_22_cleanREG, scores, DF_2016_11_22, date, tots, negs, poss, results2016_11_22)

#October

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_10_22.RData")

DF_2016_10_22_cleanREG <- DF_2016_10_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_10_22_cleanREG$text <- as.factor(DF_2016_10_22_cleanREG$text)

scores <- score.sentiment(DF_2016_10_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_10_22 <- scores
colnames(DF_2016_10_22) <- c("text", "sentiment")

DF_2016_10_22$sentiment[DF_2016_10_22$sentiment <= 5.55555] <- "0"
DF_2016_10_22$sentiment[DF_2016_10_22$sentiment > 5.55555] <- "1"

date <- c("2016-10-22")
tots <- as.numeric(nrow(DF_2016_10_22))
negs <- as.numeric(sum(DF_2016_10_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_10_22$sentiment == "1"))
results2016_10_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_10_22 <- cbind(date, tots, poss, negs, results2016_10_22)
results22LF <- rbind(results22LF, results2016_10_22)

rm(DF_2016_10_22_cleanREG, scores, DF_2016_10_22, date, tots, negs, poss, results2016_10_22)

#September

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_9_22.RData")

DF_2016_9_22_cleanREG <- DF_2016_9_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_9_22_cleanREG$text <- as.factor(DF_2016_9_22_cleanREG$text)

scores <- score.sentiment(DF_2016_9_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_9_22 <- scores
colnames(DF_2016_9_22) <- c("text", "sentiment")

DF_2016_9_22$sentiment[DF_2016_9_22$sentiment <= 5.55555] <- "0"
DF_2016_9_22$sentiment[DF_2016_9_22$sentiment > 5.55555] <- "1"

date <- c("2016-09-22")
tots <- as.numeric(nrow(DF_2016_9_22))
negs <- as.numeric(sum(DF_2016_9_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_9_22$sentiment == "1"))
results2016_9_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_9_22 <- cbind(date, tots, poss, negs, results2016_9_22)
results22LF <- rbind(results22LF, results2016_9_22)

rm(DF_2016_9_22_cleanREG, scores, DF_2016_9_22, date, tots, negs, poss, results2016_9_22)

#August

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_8_22.RData")

DF_2016_8_22_cleanREG <- DF_2016_8_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_8_22_cleanREG$text <- as.factor(DF_2016_8_22_cleanREG$text)

scores <- score.sentiment(DF_2016_8_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_8_22 <- scores
colnames(DF_2016_8_22) <- c("text", "sentiment")

DF_2016_8_22$sentiment[DF_2016_8_22$sentiment <= 5.55555] <- "0"
DF_2016_8_22$sentiment[DF_2016_8_22$sentiment > 5.55555] <- "1"

date <- c("2016-08-22")
tots <- as.numeric(nrow(DF_2016_8_22))
negs <- as.numeric(sum(DF_2016_8_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_8_22$sentiment == "1"))
results2016_8_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_8_22 <- cbind(date, tots, poss, negs, results2016_8_22)
results22LF <- rbind(results22LF, results2016_8_22)

rm(DF_2016_8_22_cleanREG, scores, DF_2016_8_22, date, tots, negs, poss, results2016_8_22)

#July

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_7_22.RData")

DF_2016_7_22_cleanREG <- DF_2016_7_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_7_22_cleanREG$text <- as.factor(DF_2016_7_22_cleanREG$text)

scores <- score.sentiment(DF_2016_7_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_7_22 <- scores
colnames(DF_2016_7_22) <- c("text", "sentiment")

DF_2016_7_22$sentiment[DF_2016_7_22$sentiment <= 5.55555] <- "0"
DF_2016_7_22$sentiment[DF_2016_7_22$sentiment > 5.55555] <- "1"

date <- c("2016-07-22")
tots <- as.numeric(nrow(DF_2016_7_22))
negs <- as.numeric(sum(DF_2016_7_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_7_22$sentiment == "1"))
results2016_7_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_7_22 <- cbind(date, tots, poss, negs, results2016_7_22)
results22LF <- rbind(results22LF, results2016_7_22)

rm(DF_2016_7_22_cleanREG, scores, DF_2016_7_22, date, tots, negs, poss, results2016_7_22)

#June

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_6_22.RData")

DF_2016_6_22_cleanREG <- DF_2016_6_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_6_22_cleanREG$text <- as.factor(DF_2016_6_22_cleanREG$text)

scores <- score.sentiment(DF_2016_6_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_6_22 <- scores
colnames(DF_2016_6_22) <- c("text", "sentiment")

DF_2016_6_22$sentiment[DF_2016_6_22$sentiment <= 5.55555] <- "0"
DF_2016_6_22$sentiment[DF_2016_6_22$sentiment > 5.55555] <- "1"

date <- c("2016-06-22")
tots <- as.numeric(nrow(DF_2016_6_22))
negs <- as.numeric(sum(DF_2016_6_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_6_22$sentiment == "1"))
results2016_6_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_6_22 <- cbind(date, tots, poss, negs, results2016_6_22)
results22LF <- rbind(results22LF, results2016_6_22)

rm(DF_2016_6_22_cleanREG, scores, DF_2016_6_22, date, tots, negs, poss, results2016_6_22)

#May

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_5_22.RData")

DF_2016_5_22_cleanREG <- DF_2016_5_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_5_22_cleanREG$text <- as.factor(DF_2016_5_22_cleanREG$text)

scores <- score.sentiment(DF_2016_5_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_5_22 <- scores
colnames(DF_2016_5_22) <- c("text", "sentiment")

DF_2016_5_22$sentiment[DF_2016_5_22$sentiment <= 5.55555] <- "0"
DF_2016_5_22$sentiment[DF_2016_5_22$sentiment > 5.55555] <- "1"

date <- c("2016-05-22")
tots <- as.numeric(nrow(DF_2016_5_22))
negs <- as.numeric(sum(DF_2016_5_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_5_22$sentiment == "1"))
results2016_5_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_5_22 <- cbind(date, tots, poss, negs, results2016_5_22)
results22LF <- rbind(results22LF, results2016_5_22)

rm(DF_2016_5_22_cleanREG, scores, DF_2016_5_22, date, tots, negs, poss, results2016_5_22)

#April

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_4_22.RData")

DF_2016_4_22_cleanREG <- DF_2016_4_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_4_22_cleanREG$text <- as.factor(DF_2016_4_22_cleanREG$text)

scores <- score.sentiment(DF_2016_4_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_4_22 <- scores
colnames(DF_2016_4_22) <- c("text", "sentiment")

DF_2016_4_22$sentiment[DF_2016_4_22$sentiment <= 5.55555] <- "0"
DF_2016_4_22$sentiment[DF_2016_4_22$sentiment > 5.55555] <- "1"

date <- c("2016-04-22")
tots <- as.numeric(nrow(DF_2016_4_22))
negs <- as.numeric(sum(DF_2016_4_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_4_22$sentiment == "1"))
results2016_4_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_4_22 <- cbind(date, tots, poss, negs, results2016_4_22)
results22LF <- rbind(results22LF, results2016_4_22)

rm(DF_2016_4_22_cleanREG, scores, DF_2016_4_22, date, tots, negs, poss, results2016_4_22)

#March

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_3_22.RData")

DF_2016_3_22_cleanREG <- DF_2016_3_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_3_22_cleanREG$text <- as.factor(DF_2016_3_22_cleanREG$text)

scores <- score.sentiment(DF_2016_3_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_3_22 <- scores
colnames(DF_2016_3_22) <- c("text", "sentiment")

DF_2016_3_22$sentiment[DF_2016_3_22$sentiment <= 5.55555] <- "0"
DF_2016_3_22$sentiment[DF_2016_3_22$sentiment > 5.55555] <- "1"

date <- c("2016-03-22")
tots <- as.numeric(nrow(DF_2016_3_22))
negs <- as.numeric(sum(DF_2016_3_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_3_22$sentiment == "1"))
results2016_3_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_3_22 <- cbind(date, tots, poss, negs, results2016_3_22)
results22LF <- rbind(results22LF, results2016_3_22)

rm(DF_2016_3_22_cleanREG, scores, DF_2016_3_22, date, tots, negs, poss, results2016_3_22)

#February

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_2_22.RData")

DF_2016_2_22_cleanREG <- DF_2016_2_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_2_22_cleanREG$text <- as.factor(DF_2016_2_22_cleanREG$text)

scores <- score.sentiment(DF_2016_2_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_2_22 <- scores
colnames(DF_2016_2_22) <- c("text", "sentiment")

DF_2016_2_22$sentiment[DF_2016_2_22$sentiment <= 5.55555] <- "0"
DF_2016_2_22$sentiment[DF_2016_2_22$sentiment > 5.55555] <- "1"

date <- c("2016-02-22")
tots <- as.numeric(nrow(DF_2016_2_22))
negs <- as.numeric(sum(DF_2016_2_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_2_22$sentiment == "1"))
results2016_2_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_2_22 <- cbind(date, tots, poss, negs, results2016_2_22)
results22LF <- rbind(results22LF, results2016_2_22)

rm(DF_2016_2_22_cleanREG, scores, DF_2016_2_22, date, tots, negs, poss, results2016_2_22)

#January

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2016_1_22.RData")

DF_2016_1_22_cleanREG <- DF_2016_1_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_1_22_cleanREG$text <- as.factor(DF_2016_1_22_cleanREG$text)

scores <- score.sentiment(DF_2016_1_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_1_22 <- scores
colnames(DF_2016_1_22) <- c("text", "sentiment")

DF_2016_1_22$sentiment[DF_2016_1_22$sentiment <= 5.55555] <- "0"
DF_2016_1_22$sentiment[DF_2016_1_22$sentiment > 5.55555] <- "1"

date <- c("2016-01-22")
tots <- as.numeric(nrow(DF_2016_1_22))
negs <- as.numeric(sum(DF_2016_1_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_1_22$sentiment == "1"))
results2016_1_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_1_22 <- cbind(date, tots, poss, negs, results2016_1_22)
results22LF <- rbind(results22LF, results2016_1_22)

rm(DF_2016_1_22_cleanREG, scores, DF_2016_1_22, date, tots, negs, poss, results2016_1_22)


print("2015")
#December

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_12_22.RData")

DF_2015_12_22_cleanREG <- DF_2015_12_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_12_22_cleanREG$text <- as.factor(DF_2015_12_22_cleanREG$text)

scores <- score.sentiment(DF_2015_12_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_12_22 <- scores
colnames(DF_2015_12_22) <- c("text", "sentiment")

DF_2015_12_22$sentiment[DF_2015_12_22$sentiment <= 5.55555] <- "0"
DF_2015_12_22$sentiment[DF_2015_12_22$sentiment > 5.55555] <- "1"

date <- c("2015-12-22")
tots <- as.numeric(nrow(DF_2015_12_22))
negs <- as.numeric(sum(DF_2015_12_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_12_22$sentiment == "1"))
results2015_12_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_12_22 <- cbind(date, tots, poss, negs, results2015_12_22)
results22LF <- rbind(results22LF, results2015_12_22)

rm(DF_2015_12_22_cleanREG, scores, DF_2015_12_22, date, tots, negs, poss, results2015_12_22)

#Nobember

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_11_22.RData")

DF_2015_11_22_cleanREG <- DF_2015_11_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_11_22_cleanREG$text <- as.factor(DF_2015_11_22_cleanREG$text)

scores <- score.sentiment(DF_2015_11_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_11_22 <- scores
colnames(DF_2015_11_22) <- c("text", "sentiment")

DF_2015_11_22$sentiment[DF_2015_11_22$sentiment <= 5.55555] <- "0"
DF_2015_11_22$sentiment[DF_2015_11_22$sentiment > 5.55555] <- "1"

date <- c("2015-11-22")
tots <- as.numeric(nrow(DF_2015_11_22))
negs <- as.numeric(sum(DF_2015_11_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_11_22$sentiment == "1"))
results2015_11_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_11_22 <- cbind(date, tots, poss, negs, results2015_11_22)
results22LF <- rbind(results22LF, results2015_11_22)

rm(DF_2015_11_22_cleanREG, scores, DF_2015_11_22, date, tots, negs, poss, results2015_11_22)

#October

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_10_22.RData")

DF_2015_10_22_cleanREG <- DF_2015_10_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_10_22_cleanREG$text <- as.factor(DF_2015_10_22_cleanREG$text)

scores <- score.sentiment(DF_2015_10_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_10_22 <- scores
colnames(DF_2015_10_22) <- c("text", "sentiment")

DF_2015_10_22$sentiment[DF_2015_10_22$sentiment <= 5.55555] <- "0"
DF_2015_10_22$sentiment[DF_2015_10_22$sentiment > 5.55555] <- "1"

date <- c("2015-10-22")
tots <- as.numeric(nrow(DF_2015_10_22))
negs <- as.numeric(sum(DF_2015_10_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_10_22$sentiment == "1"))
results2015_10_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_10_22 <- cbind(date, tots, poss, negs, results2015_10_22)
results22LF <- rbind(results22LF, results2015_10_22)

rm(DF_2015_10_22_cleanREG, scores, DF_2015_10_22, date, tots, negs, poss, results2015_10_22)

#September

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_9_22.RData")

DF_2015_9_22_cleanREG <- DF_2015_9_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_9_22_cleanREG$text <- as.factor(DF_2015_9_22_cleanREG$text)

scores <- score.sentiment(DF_2015_9_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_9_22 <- scores
colnames(DF_2015_9_22) <- c("text", "sentiment")

DF_2015_9_22$sentiment[DF_2015_9_22$sentiment <= 5.55555] <- "0"
DF_2015_9_22$sentiment[DF_2015_9_22$sentiment > 5.55555] <- "1"

date <- c("2015-09-22")
tots <- as.numeric(nrow(DF_2015_9_22))
negs <- as.numeric(sum(DF_2015_9_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_9_22$sentiment == "1"))
results2015_9_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_9_22 <- cbind(date, tots, poss, negs, results2015_9_22)
results22LF <- rbind(results22LF, results2015_9_22)

rm(DF_2015_9_22_cleanREG, scores, DF_2015_9_22, date, tots, negs, poss, results2015_9_22)

#August

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_8_22.RData")

DF_2015_8_22_cleanREG <- DF_2015_8_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_8_22_cleanREG$text <- as.factor(DF_2015_8_22_cleanREG$text)

scores <- score.sentiment(DF_2015_8_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_8_22 <- scores
colnames(DF_2015_8_22) <- c("text", "sentiment")

DF_2015_8_22$sentiment[DF_2015_8_22$sentiment <= 5.55555] <- "0"
DF_2015_8_22$sentiment[DF_2015_8_22$sentiment > 5.55555] <- "1"

date <- c("2015-08-22")
tots <- as.numeric(nrow(DF_2015_8_22))
negs <- as.numeric(sum(DF_2015_8_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_8_22$sentiment == "1"))
results2015_8_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_8_22 <- cbind(date, tots, poss, negs, results2015_8_22)
results22LF <- rbind(results22LF, results2015_8_22)

rm(DF_2015_8_22_cleanREG, scores, DF_2015_8_22, date, tots, negs, poss, results2015_8_22)

#July

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_7_22.RData")

DF_2015_7_22_cleanREG <- DF_2015_7_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_7_22_cleanREG$text <- as.factor(DF_2015_7_22_cleanREG$text)

scores <- score.sentiment(DF_2015_7_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_7_22 <- scores
colnames(DF_2015_7_22) <- c("text", "sentiment")

DF_2015_7_22$sentiment[DF_2015_7_22$sentiment <= 5.55555] <- "0"
DF_2015_7_22$sentiment[DF_2015_7_22$sentiment > 5.55555] <- "1"

date <- c("2015-07-22")
tots <- as.numeric(nrow(DF_2015_7_22))
negs <- as.numeric(sum(DF_2015_7_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_7_22$sentiment == "1"))
results2015_7_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_7_22 <- cbind(date, tots, poss, negs, results2015_7_22)
results22LF <- rbind(results22LF, results2015_7_22)

rm(DF_2015_7_22_cleanREG, scores, DF_2015_7_22, date, tots, negs, poss, results2015_7_22)

#June

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_6_22.RData")

DF_2015_6_22_cleanREG <- DF_2015_6_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_6_22_cleanREG$text <- as.factor(DF_2015_6_22_cleanREG$text)

scores <- score.sentiment(DF_2015_6_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_6_22 <- scores
colnames(DF_2015_6_22) <- c("text", "sentiment")

DF_2015_6_22$sentiment[DF_2015_6_22$sentiment <= 5.55555] <- "0"
DF_2015_6_22$sentiment[DF_2015_6_22$sentiment > 5.55555] <- "1"

date <- c("2015-06-22")
tots <- as.numeric(nrow(DF_2015_6_22))
negs <- as.numeric(sum(DF_2015_6_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_6_22$sentiment == "1"))
results2015_6_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_6_22 <- cbind(date, tots, poss, negs, results2015_6_22)
results22LF <- rbind(results22LF, results2015_6_22)

rm(DF_2015_6_22_cleanREG, scores, DF_2015_6_22, date, tots, negs, poss, results2015_6_22)

#May

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_5_22.RData")

DF_2015_5_22_cleanREG <- DF_2015_5_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_5_22_cleanREG$text <- as.factor(DF_2015_5_22_cleanREG$text)

scores <- score.sentiment(DF_2015_5_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_5_22 <- scores
colnames(DF_2015_5_22) <- c("text", "sentiment")

DF_2015_5_22$sentiment[DF_2015_5_22$sentiment <= 5.55555] <- "0"
DF_2015_5_22$sentiment[DF_2015_5_22$sentiment > 5.55555] <- "1"

date <- c("2015-05-22")
tots <- as.numeric(nrow(DF_2015_5_22))
negs <- as.numeric(sum(DF_2015_5_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_5_22$sentiment == "1"))
results2015_5_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_5_22 <- cbind(date, tots, poss, negs, results2015_5_22)
results22LF <- rbind(results22LF, results2015_5_22)

rm(DF_2015_5_22_cleanREG, scores, DF_2015_5_22, date, tots, negs, poss, results2015_5_22)

#April

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_4_22.RData")

DF_2015_4_22_cleanREG <- DF_2015_4_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_4_22_cleanREG$text <- as.factor(DF_2015_4_22_cleanREG$text)

scores <- score.sentiment(DF_2015_4_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_4_22 <- scores
colnames(DF_2015_4_22) <- c("text", "sentiment")

DF_2015_4_22$sentiment[DF_2015_4_22$sentiment <= 5.55555] <- "0"
DF_2015_4_22$sentiment[DF_2015_4_22$sentiment > 5.55555] <- "1"

date <- c("2015-04-22")
tots <- as.numeric(nrow(DF_2015_4_22))
negs <- as.numeric(sum(DF_2015_4_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_4_22$sentiment == "1"))
results2015_4_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_4_22 <- cbind(date, tots, poss, negs, results2015_4_22)
results22LF <- rbind(results22LF, results2015_4_22)

rm(DF_2015_4_22_cleanREG, scores, DF_2015_4_22, date, tots, negs, poss, results2015_4_22)

#March

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_3_22.RData")

DF_2015_3_22_cleanREG <- DF_2015_3_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_3_22_cleanREG$text <- as.factor(DF_2015_3_22_cleanREG$text)

scores <- score.sentiment(DF_2015_3_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_3_22 <- scores
colnames(DF_2015_3_22) <- c("text", "sentiment")

DF_2015_3_22$sentiment[DF_2015_3_22$sentiment <= 5.55555] <- "0"
DF_2015_3_22$sentiment[DF_2015_3_22$sentiment > 5.55555] <- "1"

date <- c("2015-03-22")
tots <- as.numeric(nrow(DF_2015_3_22))
negs <- as.numeric(sum(DF_2015_3_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_3_22$sentiment == "1"))
results2015_3_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_3_22 <- cbind(date, tots, poss, negs, results2015_3_22)
results22LF <- rbind(results22LF, results2015_3_22)

rm(DF_2015_3_22_cleanREG, scores, DF_2015_3_22, date, tots, negs, poss, results2015_3_22)

#February

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_2_22.RData")

DF_2015_2_22_cleanREG <- DF_2015_2_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_2_22_cleanREG$text <- as.factor(DF_2015_2_22_cleanREG$text)

scores <- score.sentiment(DF_2015_2_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_2_22 <- scores
colnames(DF_2015_2_22) <- c("text", "sentiment")

DF_2015_2_22$sentiment[DF_2015_2_22$sentiment <= 5.55555] <- "0"
DF_2015_2_22$sentiment[DF_2015_2_22$sentiment > 5.55555] <- "1"

date <- c("2015-02-22")
tots <- as.numeric(nrow(DF_2015_2_22))
negs <- as.numeric(sum(DF_2015_2_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_2_22$sentiment == "1"))
results2015_2_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_2_22 <- cbind(date, tots, poss, negs, results2015_2_22)
results22LF <- rbind(results22LF, results2015_2_22)

rm(DF_2015_2_22_cleanREG, scores, DF_2015_2_22, date, tots, negs, poss, results2015_2_22)

#January

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2015_1_22.RData")

DF_2015_1_22_cleanREG <- DF_2015_1_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_1_22_cleanREG$text <- as.factor(DF_2015_1_22_cleanREG$text)

scores <- score.sentiment(DF_2015_1_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_1_22 <- scores
colnames(DF_2015_1_22) <- c("text", "sentiment")

DF_2015_1_22$sentiment[DF_2015_1_22$sentiment <= 5.55555] <- "0"
DF_2015_1_22$sentiment[DF_2015_1_22$sentiment > 5.55555] <- "1"

date <- c("2015-01-22")
tots <- as.numeric(nrow(DF_2015_1_22))
negs <- as.numeric(sum(DF_2015_1_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_1_22$sentiment == "1"))
results2015_1_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_1_22 <- cbind(date, tots, poss, negs, results2015_1_22)
results22LF <- rbind(results22LF, results2015_1_22)

rm(DF_2015_1_22_cleanREG, scores, DF_2015_1_22, date, tots, negs, poss, results2015_1_22)


print("2014")
#December

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_12_22.RData")

DF_2014_12_22_cleanREG <- DF_2014_12_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_12_22_cleanREG$text <- as.factor(DF_2014_12_22_cleanREG$text)

scores <- score.sentiment(DF_2014_12_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_12_22 <- scores
colnames(DF_2014_12_22) <- c("text", "sentiment")

DF_2014_12_22$sentiment[DF_2014_12_22$sentiment <= 5.55555] <- "0"
DF_2014_12_22$sentiment[DF_2014_12_22$sentiment > 5.55555] <- "1"

date <- c("2014-12-22")
tots <- as.numeric(nrow(DF_2014_12_22))
negs <- as.numeric(sum(DF_2014_12_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_12_22$sentiment == "1"))
results2014_12_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_12_22 <- cbind(date, tots, poss, negs, results2014_12_22)
results22LF <- rbind(results22LF, results2014_12_22)

rm(DF_2014_12_22_cleanREG, scores, DF_2014_12_22, date, tots, negs, poss, results2014_12_22)

#Nobember

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_11_22.RData")

DF_2014_11_22_cleanREG <- DF_2014_11_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_11_22_cleanREG$text <- as.factor(DF_2014_11_22_cleanREG$text)

scores <- score.sentiment(DF_2014_11_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_11_22 <- scores
colnames(DF_2014_11_22) <- c("text", "sentiment")

DF_2014_11_22$sentiment[DF_2014_11_22$sentiment <= 5.55555] <- "0"
DF_2014_11_22$sentiment[DF_2014_11_22$sentiment > 5.55555] <- "1"

date <- c("2014-11-22")
tots <- as.numeric(nrow(DF_2014_11_22))
negs <- as.numeric(sum(DF_2014_11_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_11_22$sentiment == "1"))
results2014_11_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_11_22 <- cbind(date, tots, poss, negs, results2014_11_22)
results22LF <- rbind(results22LF, results2014_11_22)

rm(DF_2014_11_22_cleanREG, scores, DF_2014_11_22, date, tots, negs, poss, results2014_11_22)

#October

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_10_22.RData")

DF_2014_10_22_cleanREG <- DF_2014_10_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_10_22_cleanREG$text <- as.factor(DF_2014_10_22_cleanREG$text)

scores <- score.sentiment(DF_2014_10_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_10_22 <- scores
colnames(DF_2014_10_22) <- c("text", "sentiment")

DF_2014_10_22$sentiment[DF_2014_10_22$sentiment <= 5.55555] <- "0"
DF_2014_10_22$sentiment[DF_2014_10_22$sentiment > 5.55555] <- "1"

date <- c("2014-10-22")
tots <- as.numeric(nrow(DF_2014_10_22))
negs <- as.numeric(sum(DF_2014_10_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_10_22$sentiment == "1"))
results2014_10_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_10_22 <- cbind(date, tots, poss, negs, results2014_10_22)
results22LF <- rbind(results22LF, results2014_10_22)

rm(DF_2014_10_22_cleanREG, scores, DF_2014_10_22, date, tots, negs, poss, results2014_10_22)

#September

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_9_22.RData")

DF_2014_9_22_cleanREG <- DF_2014_9_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_9_22_cleanREG$text <- as.factor(DF_2014_9_22_cleanREG$text)

scores <- score.sentiment(DF_2014_9_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_9_22 <- scores
colnames(DF_2014_9_22) <- c("text", "sentiment")

DF_2014_9_22$sentiment[DF_2014_9_22$sentiment <= 5.55555] <- "0"
DF_2014_9_22$sentiment[DF_2014_9_22$sentiment > 5.55555] <- "1"

date <- c("2014-09-22")
tots <- as.numeric(nrow(DF_2014_9_22))
negs <- as.numeric(sum(DF_2014_9_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_9_22$sentiment == "1"))
results2014_9_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_9_22 <- cbind(date, tots, poss, negs, results2014_9_22)
results22LF <- rbind(results22LF, results2014_9_22)

rm(DF_2014_9_22_cleanREG, scores, DF_2014_9_22, date, tots, negs, poss, results2014_9_22)

#August

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_8_22.RData")

DF_2014_8_22_cleanREG <- DF_2014_8_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_8_22_cleanREG$text <- as.factor(DF_2014_8_22_cleanREG$text)

scores <- score.sentiment(DF_2014_8_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_8_22 <- scores
colnames(DF_2014_8_22) <- c("text", "sentiment")

DF_2014_8_22$sentiment[DF_2014_8_22$sentiment <= 5.55555] <- "0"
DF_2014_8_22$sentiment[DF_2014_8_22$sentiment > 5.55555] <- "1"

date <- c("2014-08-22")
tots <- as.numeric(nrow(DF_2014_8_22))
negs <- as.numeric(sum(DF_2014_8_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_8_22$sentiment == "1"))
results2014_8_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_8_22 <- cbind(date, tots, poss, negs, results2014_8_22)
results22LF <- rbind(results22LF, results2014_8_22)

rm(DF_2014_8_22_cleanREG, scores, DF_2014_8_22, date, tots, negs, poss, results2014_8_22)

#July

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_7_22.RData")

DF_2014_7_22_cleanREG <- DF_2014_7_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_7_22_cleanREG$text <- as.factor(DF_2014_7_22_cleanREG$text)

scores <- score.sentiment(DF_2014_7_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_7_22 <- scores
colnames(DF_2014_7_22) <- c("text", "sentiment")

DF_2014_7_22$sentiment[DF_2014_7_22$sentiment <= 5.55555] <- "0"
DF_2014_7_22$sentiment[DF_2014_7_22$sentiment > 5.55555] <- "1"

date <- c("2014-07-22")
tots <- as.numeric(nrow(DF_2014_7_22))
negs <- as.numeric(sum(DF_2014_7_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_7_22$sentiment == "1"))
results2014_7_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_7_22 <- cbind(date, tots, poss, negs, results2014_7_22)
results22LF <- rbind(results22LF, results2014_7_22)

rm(DF_2014_7_22_cleanREG, scores, DF_2014_7_22, date, tots, negs, poss, results2014_7_22)

#June

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_6_22.RData")

DF_2014_6_22_cleanREG <- DF_2014_6_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_6_22_cleanREG$text <- as.factor(DF_2014_6_22_cleanREG$text)

scores <- score.sentiment(DF_2014_6_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_6_22 <- scores
colnames(DF_2014_6_22) <- c("text", "sentiment")

DF_2014_6_22$sentiment[DF_2014_6_22$sentiment <= 5.55555] <- "0"
DF_2014_6_22$sentiment[DF_2014_6_22$sentiment > 5.55555] <- "1"

date <- c("2014-06-22")
tots <- as.numeric(nrow(DF_2014_6_22))
negs <- as.numeric(sum(DF_2014_6_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_6_22$sentiment == "1"))
results2014_6_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_6_22 <- cbind(date, tots, poss, negs, results2014_6_22)
results22LF <- rbind(results22LF, results2014_6_22)

rm(DF_2014_6_22_cleanREG, scores, DF_2014_6_22, date, tots, negs, poss, results2014_6_22)

#May

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_5_22.RData")

DF_2014_5_22_cleanREG <- DF_2014_5_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_5_22_cleanREG$text <- as.factor(DF_2014_5_22_cleanREG$text)

scores <- score.sentiment(DF_2014_5_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_5_22 <- scores
colnames(DF_2014_5_22) <- c("text", "sentiment")

DF_2014_5_22$sentiment[DF_2014_5_22$sentiment <= 5.55555] <- "0"
DF_2014_5_22$sentiment[DF_2014_5_22$sentiment > 5.55555] <- "1"

date <- c("2014-05-22")
tots <- as.numeric(nrow(DF_2014_5_22))
negs <- as.numeric(sum(DF_2014_5_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_5_22$sentiment == "1"))
results2014_5_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_5_22 <- cbind(date, tots, poss, negs, results2014_5_22)
results22LF <- rbind(results22LF, results2014_5_22)

rm(DF_2014_5_22_cleanREG, scores, DF_2014_5_22, date, tots, negs, poss, results2014_5_22)

#April

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_4_22.RData")

DF_2014_4_22_cleanREG <- DF_2014_4_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_4_22_cleanREG$text <- as.factor(DF_2014_4_22_cleanREG$text)

scores <- score.sentiment(DF_2014_4_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_4_22 <- scores
colnames(DF_2014_4_22) <- c("text", "sentiment")

DF_2014_4_22$sentiment[DF_2014_4_22$sentiment <= 5.55555] <- "0"
DF_2014_4_22$sentiment[DF_2014_4_22$sentiment > 5.55555] <- "1"

date <- c("2014-04-22")
tots <- as.numeric(nrow(DF_2014_4_22))
negs <- as.numeric(sum(DF_2014_4_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_4_22$sentiment == "1"))
results2014_4_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_4_22 <- cbind(date, tots, poss, negs, results2014_4_22)
results22LF <- rbind(results22LF, results2014_4_22)

rm(DF_2014_4_22_cleanREG, scores, DF_2014_4_22, date, tots, negs, poss, results2014_4_22)

#March

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_3_22.RData")

DF_2014_3_22_cleanREG <- DF_2014_3_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_3_22_cleanREG$text <- as.factor(DF_2014_3_22_cleanREG$text)

scores <- score.sentiment(DF_2014_3_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_3_22 <- scores
colnames(DF_2014_3_22) <- c("text", "sentiment")

DF_2014_3_22$sentiment[DF_2014_3_22$sentiment <= 5.55555] <- "0"
DF_2014_3_22$sentiment[DF_2014_3_22$sentiment > 5.55555] <- "1"

date <- c("2014-03-22")
tots <- as.numeric(nrow(DF_2014_3_22))
negs <- as.numeric(sum(DF_2014_3_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_3_22$sentiment == "1"))
results2014_3_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_3_22 <- cbind(date, tots, poss, negs, results2014_3_22)
results22LF <- rbind(results22LF, results2014_3_22)

rm(DF_2014_3_22_cleanREG, scores, DF_2014_3_22, date, tots, negs, poss, results2014_3_22)

#February

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_2_22.RData")

DF_2014_2_22_cleanREG <- DF_2014_2_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_2_22_cleanREG$text <- as.factor(DF_2014_2_22_cleanREG$text)

scores <- score.sentiment(DF_2014_2_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_2_22 <- scores
colnames(DF_2014_2_22) <- c("text", "sentiment")

DF_2014_2_22$sentiment[DF_2014_2_22$sentiment <= 5.55555] <- "0"
DF_2014_2_22$sentiment[DF_2014_2_22$sentiment > 5.55555] <- "1"

date <- c("2014-02-22")
tots <- as.numeric(nrow(DF_2014_2_22))
negs <- as.numeric(sum(DF_2014_2_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_2_22$sentiment == "1"))
results2014_2_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_2_22 <- cbind(date, tots, poss, negs, results2014_2_22)
results22LF <- rbind(results22LF, results2014_2_22)

rm(DF_2014_2_22_cleanREG, scores, DF_2014_2_22, date, tots, negs, poss, results2014_2_22)

#January

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2014_1_22.RData")

DF_2014_1_22_cleanREG <- DF_2014_1_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_1_22_cleanREG$text <- as.factor(DF_2014_1_22_cleanREG$text)

scores <- score.sentiment(DF_2014_1_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_1_22 <- scores
colnames(DF_2014_1_22) <- c("text", "sentiment")

DF_2014_1_22$sentiment[DF_2014_1_22$sentiment <= 5.55555] <- "0"
DF_2014_1_22$sentiment[DF_2014_1_22$sentiment > 5.55555] <- "1"

date <- c("2014-01-22")
tots <- as.numeric(nrow(DF_2014_1_22))
negs <- as.numeric(sum(DF_2014_1_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_1_22$sentiment == "1"))
results2014_1_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_1_22 <- cbind(date, tots, poss, negs, results2014_1_22)
results22LF <- rbind(results22LF, results2014_1_22)

rm(DF_2014_1_22_cleanREG, scores, DF_2014_1_22, date, tots, negs, poss, results2014_1_22)


print("2013")
#December

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_12_22.RData")

DF_2013_12_22_cleanREG <- DF_2013_12_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_12_22_cleanREG$text <- as.factor(DF_2013_12_22_cleanREG$text)

scores <- score.sentiment(DF_2013_12_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_12_22 <- scores
colnames(DF_2013_12_22) <- c("text", "sentiment")

DF_2013_12_22$sentiment[DF_2013_12_22$sentiment <= 5.55555] <- "0"
DF_2013_12_22$sentiment[DF_2013_12_22$sentiment > 5.55555] <- "1"

date <- c("2013-12-22")
tots <- as.numeric(nrow(DF_2013_12_22))
negs <- as.numeric(sum(DF_2013_12_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_12_22$sentiment == "1"))
results2013_12_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_12_22 <- cbind(date, tots, poss, negs, results2013_12_22)
results22LF <- rbind(results22LF, results2013_12_22)

rm(DF_2013_12_22_cleanREG, scores, DF_2013_12_22, date, tots, negs, poss, results2013_12_22)

#Nobember

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_11_22.RData")

DF_2013_11_22_cleanREG <- DF_2013_11_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_11_22_cleanREG$text <- as.factor(DF_2013_11_22_cleanREG$text)

scores <- score.sentiment(DF_2013_11_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_11_22 <- scores
colnames(DF_2013_11_22) <- c("text", "sentiment")

DF_2013_11_22$sentiment[DF_2013_11_22$sentiment <= 5.55555] <- "0"
DF_2013_11_22$sentiment[DF_2013_11_22$sentiment > 5.55555] <- "1"

date <- c("2013-11-22")
tots <- as.numeric(nrow(DF_2013_11_22))
negs <- as.numeric(sum(DF_2013_11_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_11_22$sentiment == "1"))
results2013_11_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_11_22 <- cbind(date, tots, poss, negs, results2013_11_22)
results22LF <- rbind(results22LF, results2013_11_22)

rm(DF_2013_11_22_cleanREG, scores, DF_2013_11_22, date, tots, negs, poss, results2013_11_22)

#October

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_10_22.RData")

DF_2013_10_22_cleanREG <- DF_2013_10_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_10_22_cleanREG$text <- as.factor(DF_2013_10_22_cleanREG$text)

scores <- score.sentiment(DF_2013_10_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_10_22 <- scores
colnames(DF_2013_10_22) <- c("text", "sentiment")

DF_2013_10_22$sentiment[DF_2013_10_22$sentiment <= 5.55555] <- "0"
DF_2013_10_22$sentiment[DF_2013_10_22$sentiment > 5.55555] <- "1"

date <- c("2013-10-22")
tots <- as.numeric(nrow(DF_2013_10_22))
negs <- as.numeric(sum(DF_2013_10_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_10_22$sentiment == "1"))
results2013_10_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_10_22 <- cbind(date, tots, poss, negs, results2013_10_22)
results22LF <- rbind(results22LF, results2013_10_22)

rm(DF_2013_10_22_cleanREG, scores, DF_2013_10_22, date, tots, negs, poss, results2013_10_22)

#September

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_9_22.RData")

DF_2013_9_22_cleanREG <- DF_2013_9_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_9_22_cleanREG$text <- as.factor(DF_2013_9_22_cleanREG$text)

scores <- score.sentiment(DF_2013_9_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_9_22 <- scores
colnames(DF_2013_9_22) <- c("text", "sentiment")

DF_2013_9_22$sentiment[DF_2013_9_22$sentiment <= 5.55555] <- "0"
DF_2013_9_22$sentiment[DF_2013_9_22$sentiment > 5.55555] <- "1"

date <- c("2013-09-22")
tots <- as.numeric(nrow(DF_2013_9_22))
negs <- as.numeric(sum(DF_2013_9_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_9_22$sentiment == "1"))
results2013_9_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_9_22 <- cbind(date, tots, poss, negs, results2013_9_22)
results22LF <- rbind(results22LF, results2013_9_22)

rm(DF_2013_9_22_cleanREG, scores, DF_2013_9_22, date, tots, negs, poss, results2013_9_22)

#August

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_8_22.RData")

DF_2013_8_22_cleanREG <- DF_2013_8_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_8_22_cleanREG$text <- as.factor(DF_2013_8_22_cleanREG$text)

scores <- score.sentiment(DF_2013_8_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_8_22 <- scores
colnames(DF_2013_8_22) <- c("text", "sentiment")

DF_2013_8_22$sentiment[DF_2013_8_22$sentiment <= 5.55555] <- "0"
DF_2013_8_22$sentiment[DF_2013_8_22$sentiment > 5.55555] <- "1"

date <- c("2013-08-22")
tots <- as.numeric(nrow(DF_2013_8_22))
negs <- as.numeric(sum(DF_2013_8_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_8_22$sentiment == "1"))
results2013_8_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_8_22 <- cbind(date, tots, poss, negs, results2013_8_22)
results22LF <- rbind(results22LF, results2013_8_22)

rm(DF_2013_8_22_cleanREG, scores, DF_2013_8_22, date, tots, negs, poss, results2013_8_22)

#July

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_7_22.RData")

DF_2013_7_22_cleanREG <- DF_2013_7_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_7_22_cleanREG$text <- as.factor(DF_2013_7_22_cleanREG$text)

scores <- score.sentiment(DF_2013_7_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_7_22 <- scores
colnames(DF_2013_7_22) <- c("text", "sentiment")

DF_2013_7_22$sentiment[DF_2013_7_22$sentiment <= 5.55555] <- "0"
DF_2013_7_22$sentiment[DF_2013_7_22$sentiment > 5.55555] <- "1"

date <- c("2013-07-22")
tots <- as.numeric(nrow(DF_2013_7_22))
negs <- as.numeric(sum(DF_2013_7_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_7_22$sentiment == "1"))
results2013_7_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_7_22 <- cbind(date, tots, poss, negs, results2013_7_22)
results22LF <- rbind(results22LF, results2013_7_22)

rm(DF_2013_7_22_cleanREG, scores, DF_2013_7_22, date, tots, negs, poss, results2013_7_22)

#June

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_6_22.RData")

DF_2013_6_22_cleanREG <- DF_2013_6_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_6_22_cleanREG$text <- as.factor(DF_2013_6_22_cleanREG$text)

scores <- score.sentiment(DF_2013_6_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_6_22 <- scores
colnames(DF_2013_6_22) <- c("text", "sentiment")

DF_2013_6_22$sentiment[DF_2013_6_22$sentiment <= 5.55555] <- "0"
DF_2013_6_22$sentiment[DF_2013_6_22$sentiment > 5.55555] <- "1"

date <- c("2013-06-22")
tots <- as.numeric(nrow(DF_2013_6_22))
negs <- as.numeric(sum(DF_2013_6_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_6_22$sentiment == "1"))
results2013_6_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_6_22 <- cbind(date, tots, poss, negs, results2013_6_22)
results22LF <- rbind(results22LF, results2013_6_22)

rm(DF_2013_6_22_cleanREG, scores, DF_2013_6_22, date, tots, negs, poss, results2013_6_22)

#May

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_5_22.RData")

DF_2013_5_22_cleanREG <- DF_2013_5_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_5_22_cleanREG$text <- as.factor(DF_2013_5_22_cleanREG$text)

scores <- score.sentiment(DF_2013_5_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_5_22 <- scores
colnames(DF_2013_5_22) <- c("text", "sentiment")

DF_2013_5_22$sentiment[DF_2013_5_22$sentiment <= 5.55555] <- "0"
DF_2013_5_22$sentiment[DF_2013_5_22$sentiment > 5.55555] <- "1"

date <- c("2013-05-22")
tots <- as.numeric(nrow(DF_2013_5_22))
negs <- as.numeric(sum(DF_2013_5_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_5_22$sentiment == "1"))
results2013_5_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_5_22 <- cbind(date, tots, poss, negs, results2013_5_22)
results22LF <- rbind(results22LF, results2013_5_22)

rm(DF_2013_5_22_cleanREG, scores, DF_2013_5_22, date, tots, negs, poss, results2013_5_22)

#April

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_4_22.RData")

DF_2013_4_22_cleanREG <- DF_2013_4_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_4_22_cleanREG$text <- as.factor(DF_2013_4_22_cleanREG$text)

scores <- score.sentiment(DF_2013_4_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_4_22 <- scores
colnames(DF_2013_4_22) <- c("text", "sentiment")

DF_2013_4_22$sentiment[DF_2013_4_22$sentiment <= 5.55555] <- "0"
DF_2013_4_22$sentiment[DF_2013_4_22$sentiment > 5.55555] <- "1"

date <- c("2013-04-22")
tots <- as.numeric(nrow(DF_2013_4_22))
negs <- as.numeric(sum(DF_2013_4_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_4_22$sentiment == "1"))
results2013_4_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_4_22 <- cbind(date, tots, poss, negs, results2013_4_22)
results22LF <- rbind(results22LF, results2013_4_22)

rm(DF_2013_4_22_cleanREG, scores, DF_2013_4_22, date, tots, negs, poss, results2013_4_22)

#March

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_3_22.RData")

DF_2013_3_22_cleanREG <- DF_2013_3_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_3_22_cleanREG$text <- as.factor(DF_2013_3_22_cleanREG$text)

scores <- score.sentiment(DF_2013_3_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_3_22 <- scores
colnames(DF_2013_3_22) <- c("text", "sentiment")

DF_2013_3_22$sentiment[DF_2013_3_22$sentiment <= 5.55555] <- "0"
DF_2013_3_22$sentiment[DF_2013_3_22$sentiment > 5.55555] <- "1"

date <- c("2013-03-22")
tots <- as.numeric(nrow(DF_2013_3_22))
negs <- as.numeric(sum(DF_2013_3_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_3_22$sentiment == "1"))
results2013_3_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_3_22 <- cbind(date, tots, poss, negs, results2013_3_22)
results22LF <- rbind(results22LF, results2013_3_22)

rm(DF_2013_3_22_cleanREG, scores, DF_2013_3_22, date, tots, negs, poss, results2013_3_22)

#February

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_2_22.RData")

DF_2013_2_22_cleanREG <- DF_2013_2_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_2_22_cleanREG$text <- as.factor(DF_2013_2_22_cleanREG$text)

scores <- score.sentiment(DF_2013_2_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_2_22 <- scores
colnames(DF_2013_2_22) <- c("text", "sentiment")

DF_2013_2_22$sentiment[DF_2013_2_22$sentiment <= 5.55555] <- "0"
DF_2013_2_22$sentiment[DF_2013_2_22$sentiment > 5.55555] <- "1"

date <- c("2013-02-22")
tots <- as.numeric(nrow(DF_2013_2_22))
negs <- as.numeric(sum(DF_2013_2_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_2_22$sentiment == "1"))
results2013_2_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_2_22 <- cbind(date, tots, poss, negs, results2013_2_22)
results22LF <- rbind(results22LF, results2013_2_22)

rm(DF_2013_2_22_cleanREG, scores, DF_2013_2_22, date, tots, negs, poss, results2013_2_22)

#January

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2013_1_22.RData")

DF_2013_1_22_cleanREG <- DF_2013_1_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_1_22_cleanREG$text <- as.factor(DF_2013_1_22_cleanREG$text)

scores <- score.sentiment(DF_2013_1_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_1_22 <- scores
colnames(DF_2013_1_22) <- c("text", "sentiment")

DF_2013_1_22$sentiment[DF_2013_1_22$sentiment <= 5.55555] <- "0"
DF_2013_1_22$sentiment[DF_2013_1_22$sentiment > 5.55555] <- "1"

date <- c("2013-01-22")
tots <- as.numeric(nrow(DF_2013_1_22))
negs <- as.numeric(sum(DF_2013_1_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_1_22$sentiment == "1"))
results2013_1_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_1_22 <- cbind(date, tots, poss, negs, results2013_1_22)
results22LF <- rbind(results22LF, results2013_1_22)

rm(DF_2013_1_22_cleanREG, scores, DF_2013_1_22, date, tots, negs, poss, results2013_1_22)


print("2012")
#December

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_12_22.RData")

DF_2012_12_22_cleanREG <- DF_2012_12_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_12_22_cleanREG$text <- as.factor(DF_2012_12_22_cleanREG$text)

scores <- score.sentiment(DF_2012_12_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_12_22 <- scores
colnames(DF_2012_12_22) <- c("text", "sentiment")

DF_2012_12_22$sentiment[DF_2012_12_22$sentiment <= 5.55555] <- "0"
DF_2012_12_22$sentiment[DF_2012_12_22$sentiment > 5.55555] <- "1"

date <- c("2012-12-22")
tots <- as.numeric(nrow(DF_2012_12_22))
negs <- as.numeric(sum(DF_2012_12_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_12_22$sentiment == "1"))
results2012_12_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_12_22 <- cbind(date, tots, poss, negs, results2012_12_22)
results22LF <- rbind(results22LF, results2012_12_22)

rm(DF_2012_12_22_cleanREG, scores, DF_2012_12_22, date, tots, negs, poss, results2012_12_22)

#Nobember

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_11_22.RData")

DF_2012_11_22_cleanREG <- DF_2012_11_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_11_22_cleanREG$text <- as.factor(DF_2012_11_22_cleanREG$text)

scores <- score.sentiment(DF_2012_11_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_11_22 <- scores
colnames(DF_2012_11_22) <- c("text", "sentiment")

DF_2012_11_22$sentiment[DF_2012_11_22$sentiment <= 5.55555] <- "0"
DF_2012_11_22$sentiment[DF_2012_11_22$sentiment > 5.55555] <- "1"

date <- c("2012-11-22")
tots <- as.numeric(nrow(DF_2012_11_22))
negs <- as.numeric(sum(DF_2012_11_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_11_22$sentiment == "1"))
results2012_11_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_11_22 <- cbind(date, tots, poss, negs, results2012_11_22)
results22LF <- rbind(results22LF, results2012_11_22)

rm(DF_2012_11_22_cleanREG, scores, DF_2012_11_22, date, tots, negs, poss, results2012_11_22)

#October

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_10_22.RData")

DF_2012_10_22_cleanREG <- DF_2012_10_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_10_22_cleanREG$text <- as.factor(DF_2012_10_22_cleanREG$text)

scores <- score.sentiment(DF_2012_10_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_10_22 <- scores
colnames(DF_2012_10_22) <- c("text", "sentiment")

DF_2012_10_22$sentiment[DF_2012_10_22$sentiment <= 5.55555] <- "0"
DF_2012_10_22$sentiment[DF_2012_10_22$sentiment > 5.55555] <- "1"

date <- c("2012-10-22")
tots <- as.numeric(nrow(DF_2012_10_22))
negs <- as.numeric(sum(DF_2012_10_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_10_22$sentiment == "1"))
results2012_10_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_10_22 <- cbind(date, tots, poss, negs, results2012_10_22)
results22LF <- rbind(results22LF, results2012_10_22)

rm(DF_2012_10_22_cleanREG, scores, DF_2012_10_22, date, tots, negs, poss, results2012_10_22)

#September

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_9_22.RData")

DF_2012_9_22_cleanREG <- DF_2012_9_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_9_22_cleanREG$text <- as.factor(DF_2012_9_22_cleanREG$text)

scores <- score.sentiment(DF_2012_9_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_9_22 <- scores
colnames(DF_2012_9_22) <- c("text", "sentiment")

DF_2012_9_22$sentiment[DF_2012_9_22$sentiment <= 5.55555] <- "0"
DF_2012_9_22$sentiment[DF_2012_9_22$sentiment > 5.55555] <- "1"

date <- c("2012-09-22")
tots <- as.numeric(nrow(DF_2012_9_22))
negs <- as.numeric(sum(DF_2012_9_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_9_22$sentiment == "1"))
results2012_9_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_9_22 <- cbind(date, tots, poss, negs, results2012_9_22)
results22LF <- rbind(results22LF, results2012_9_22)

rm(DF_2012_9_22_cleanREG, scores, DF_2012_9_22, date, tots, negs, poss, results2012_9_22)

#August

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_8_22.RData")

DF_2012_8_22_cleanREG <- DF_2012_8_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_8_22_cleanREG$text <- as.factor(DF_2012_8_22_cleanREG$text)

scores <- score.sentiment(DF_2012_8_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_8_22 <- scores
colnames(DF_2012_8_22) <- c("text", "sentiment")

DF_2012_8_22$sentiment[DF_2012_8_22$sentiment <= 5.55555] <- "0"
DF_2012_8_22$sentiment[DF_2012_8_22$sentiment > 5.55555] <- "1"

date <- c("2012-08-22")
tots <- as.numeric(nrow(DF_2012_8_22))
negs <- as.numeric(sum(DF_2012_8_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_8_22$sentiment == "1"))
results2012_8_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_8_22 <- cbind(date, tots, poss, negs, results2012_8_22)
results22LF <- rbind(results22LF, results2012_8_22)

rm(DF_2012_8_22_cleanREG, scores, DF_2012_8_22, date, tots, negs, poss, results2012_8_22)

#July

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_7_22.RData")

DF_2012_7_22_cleanREG <- DF_2012_7_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_7_22_cleanREG$text <- as.factor(DF_2012_7_22_cleanREG$text)

scores <- score.sentiment(DF_2012_7_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_7_22 <- scores
colnames(DF_2012_7_22) <- c("text", "sentiment")

DF_2012_7_22$sentiment[DF_2012_7_22$sentiment <= 5.55555] <- "0"
DF_2012_7_22$sentiment[DF_2012_7_22$sentiment > 5.55555] <- "1"

date <- c("2012-07-22")
tots <- as.numeric(nrow(DF_2012_7_22))
negs <- as.numeric(sum(DF_2012_7_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_7_22$sentiment == "1"))
results2012_7_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_7_22 <- cbind(date, tots, poss, negs, results2012_7_22)
results22LF <- rbind(results22LF, results2012_7_22)

rm(DF_2012_7_22_cleanREG, scores, DF_2012_7_22, date, tots, negs, poss, results2012_7_22)

#June

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_6_22.RData")

DF_2012_6_22_cleanREG <- DF_2012_6_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_6_22_cleanREG$text <- as.factor(DF_2012_6_22_cleanREG$text)

scores <- score.sentiment(DF_2012_6_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_6_22 <- scores
colnames(DF_2012_6_22) <- c("text", "sentiment")

DF_2012_6_22$sentiment[DF_2012_6_22$sentiment <= 5.55555] <- "0"
DF_2012_6_22$sentiment[DF_2012_6_22$sentiment > 5.55555] <- "1"

date <- c("2012-06-22")
tots <- as.numeric(nrow(DF_2012_6_22))
negs <- as.numeric(sum(DF_2012_6_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_6_22$sentiment == "1"))
results2012_6_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_6_22 <- cbind(date, tots, poss, negs, results2012_6_22)
results22LF <- rbind(results22LF, results2012_6_22)

rm(DF_2012_6_22_cleanREG, scores, DF_2012_6_22, date, tots, negs, poss, results2012_6_22)

#May

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_5_22.RData")

DF_2012_5_22_cleanREG <- DF_2012_5_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_5_22_cleanREG$text <- as.factor(DF_2012_5_22_cleanREG$text)

scores <- score.sentiment(DF_2012_5_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_5_22 <- scores
colnames(DF_2012_5_22) <- c("text", "sentiment")

DF_2012_5_22$sentiment[DF_2012_5_22$sentiment <= 5.55555] <- "0"
DF_2012_5_22$sentiment[DF_2012_5_22$sentiment > 5.55555] <- "1"

date <- c("2012-05-22")
tots <- as.numeric(nrow(DF_2012_5_22))
negs <- as.numeric(sum(DF_2012_5_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_5_22$sentiment == "1"))
results2012_5_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_5_22 <- cbind(date, tots, poss, negs, results2012_5_22)
results22LF <- rbind(results22LF, results2012_5_22)

rm(DF_2012_5_22_cleanREG, scores, DF_2012_5_22, date, tots, negs, poss, results2012_5_22)

#April

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_4_22.RData")

DF_2012_4_22_cleanREG <- DF_2012_4_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_4_22_cleanREG$text <- as.factor(DF_2012_4_22_cleanREG$text)

scores <- score.sentiment(DF_2012_4_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_4_22 <- scores
colnames(DF_2012_4_22) <- c("text", "sentiment")

DF_2012_4_22$sentiment[DF_2012_4_22$sentiment <= 5.55555] <- "0"
DF_2012_4_22$sentiment[DF_2012_4_22$sentiment > 5.55555] <- "1"

date <- c("2012-04-22")
tots <- as.numeric(nrow(DF_2012_4_22))
negs <- as.numeric(sum(DF_2012_4_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_4_22$sentiment == "1"))
results2012_4_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_4_22 <- cbind(date, tots, poss, negs, results2012_4_22)
results22LF <- rbind(results22LF, results2012_4_22)

rm(DF_2012_4_22_cleanREG, scores, DF_2012_4_22, date, tots, negs, poss, results2012_4_22)

#March

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_3_22.RData")

DF_2012_3_22_cleanREG <- DF_2012_3_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_3_22_cleanREG$text <- as.factor(DF_2012_3_22_cleanREG$text)

scores <- score.sentiment(DF_2012_3_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_3_22 <- scores
colnames(DF_2012_3_22) <- c("text", "sentiment")

DF_2012_3_22$sentiment[DF_2012_3_22$sentiment <= 5.55555] <- "0"
DF_2012_3_22$sentiment[DF_2012_3_22$sentiment > 5.55555] <- "1"

date <- c("2012-03-22")
tots <- as.numeric(nrow(DF_2012_3_22))
negs <- as.numeric(sum(DF_2012_3_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_3_22$sentiment == "1"))
results2012_3_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_3_22 <- cbind(date, tots, poss, negs, results2012_3_22)
results22LF <- rbind(results22LF, results2012_3_22)

rm(DF_2012_3_22_cleanREG, scores, DF_2012_3_22, date, tots, negs, poss, results2012_3_22)

#February

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_2_22.RData")

DF_2012_2_22_cleanREG <- DF_2012_2_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_2_22_cleanREG$text <- as.factor(DF_2012_2_22_cleanREG$text)

scores <- score.sentiment(DF_2012_2_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_2_22 <- scores
colnames(DF_2012_2_22) <- c("text", "sentiment")

DF_2012_2_22$sentiment[DF_2012_2_22$sentiment <= 5.55555] <- "0"
DF_2012_2_22$sentiment[DF_2012_2_22$sentiment > 5.55555] <- "1"

date <- c("2012-02-22")
tots <- as.numeric(nrow(DF_2012_2_22))
negs <- as.numeric(sum(DF_2012_2_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_2_22$sentiment == "1"))
results2012_2_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_2_22 <- cbind(date, tots, poss, negs, results2012_2_22)
results22LF <- rbind(results22LF, results2012_2_22)

rm(DF_2012_2_22_cleanREG, scores, DF_2012_2_22, date, tots, negs, poss, results2012_2_22)

#January

load(file = "Objects/Tweets/Series_22/Clean/REG/DF_2012_1_22.RData")

DF_2012_1_22_cleanREG <- DF_2012_1_22_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_1_22_cleanREG$text <- as.factor(DF_2012_1_22_cleanREG$text)

scores <- score.sentiment(DF_2012_1_22_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_1_22 <- scores
colnames(DF_2012_1_22) <- c("text", "sentiment")

DF_2012_1_22$sentiment[DF_2012_1_22$sentiment <= 5.55555] <- "0"
DF_2012_1_22$sentiment[DF_2012_1_22$sentiment > 5.55555] <- "1"

date <- c("2012-01-22")
tots <- as.numeric(nrow(DF_2012_1_22))
negs <- as.numeric(sum(DF_2012_1_22$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_1_22$sentiment == "1"))
results2012_1_22 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_1_22 <- cbind(date, tots, poss, negs, results2012_1_22)
results22LF <- rbind(results22LF, results2012_1_22)

rm(DF_2012_1_22_cleanREG, scores, DF_2012_1_22, date, tots, negs, poss, results2012_1_22)

save(results22LF, file = "Objects/Models/results22LF.RData")
