
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

#Series 21

print("2017")
#December

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_12_21.RData")

DF_2017_12_21_cleanREG <- DF_2017_12_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_12_21_cleanREG$text <- as.factor(DF_2017_12_21_cleanREG$text)

scores <- score.sentiment(DF_2017_12_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_12_21 <- scores
colnames(DF_2017_12_21) <- c("text", "sentiment")

DF_2017_12_21$sentiment[DF_2017_12_21$sentiment <= 5.55555] <- "0"
DF_2017_12_21$sentiment[DF_2017_12_21$sentiment > 5.55555] <- "1"

date <- c("2017-12-21")
tots <- as.numeric(nrow(DF_2017_12_21))
negs <- as.numeric(sum(DF_2017_12_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_12_21$sentiment == "1"))
results2017_12_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_12_21 <- cbind(date, tots, poss, negs, results2017_12_21)
results21LF <- results2017_12_21

rm(DF_2017_12_21_cleanREG, scores, DF_2017_12_21, date, tots, negs, poss, results2017_12_21)

#Nobember

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_11_21.RData")

DF_2017_11_21_cleanREG <- DF_2017_11_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_11_21_cleanREG$text <- as.factor(DF_2017_11_21_cleanREG$text)

scores <- score.sentiment(DF_2017_11_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_11_21 <- scores
colnames(DF_2017_11_21) <- c("text", "sentiment")

DF_2017_11_21$sentiment[DF_2017_11_21$sentiment <= 5.55555] <- "0"
DF_2017_11_21$sentiment[DF_2017_11_21$sentiment > 5.55555] <- "1"

date <- c("2017-11-21")
tots <- as.numeric(nrow(DF_2017_11_21))
negs <- as.numeric(sum(DF_2017_11_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_11_21$sentiment == "1"))
results2017_11_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_11_21 <- cbind(date, tots, poss, negs, results2017_11_21)
results21LF <- rbind(results21LF, results2017_11_21)

rm(DF_2017_11_21_cleanREG, scores, DF_2017_11_21, date, tots, negs, poss, results2017_11_21)

#October

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_10_21.RData")

DF_2017_10_21_cleanREG <- DF_2017_10_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_10_21_cleanREG$text <- as.factor(DF_2017_10_21_cleanREG$text)

scores <- score.sentiment(DF_2017_10_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_10_21 <- scores
colnames(DF_2017_10_21) <- c("text", "sentiment")

DF_2017_10_21$sentiment[DF_2017_10_21$sentiment <= 5.55555] <- "0"
DF_2017_10_21$sentiment[DF_2017_10_21$sentiment > 5.55555] <- "1"

date <- c("2017-10-21")
tots <- as.numeric(nrow(DF_2017_10_21))
negs <- as.numeric(sum(DF_2017_10_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_10_21$sentiment == "1"))
results2017_10_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_10_21 <- cbind(date, tots, poss, negs, results2017_10_21)
results21LF <- rbind(results21LF, results2017_10_21)

rm(DF_2017_10_21_cleanREG, scores, DF_2017_10_21, date, tots, negs, poss, results2017_10_21)

#September

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_9_21.RData")

DF_2017_9_21_cleanREG <- DF_2017_9_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_9_21_cleanREG$text <- as.factor(DF_2017_9_21_cleanREG$text)

scores <- score.sentiment(DF_2017_9_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_9_21 <- scores
colnames(DF_2017_9_21) <- c("text", "sentiment")

DF_2017_9_21$sentiment[DF_2017_9_21$sentiment <= 5.55555] <- "0"
DF_2017_9_21$sentiment[DF_2017_9_21$sentiment > 5.55555] <- "1"

date <- c("2017-09-21")
tots <- as.numeric(nrow(DF_2017_9_21))
negs <- as.numeric(sum(DF_2017_9_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_9_21$sentiment == "1"))
results2017_9_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_9_21 <- cbind(date, tots, poss, negs, results2017_9_21)
results21LF <- rbind(results21LF, results2017_9_21)

rm(DF_2017_9_21_cleanREG, scores, DF_2017_9_21, date, tots, negs, poss, results2017_9_21)

#August

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_8_21.RData")

DF_2017_8_21_cleanREG <- DF_2017_8_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_8_21_cleanREG$text <- as.factor(DF_2017_8_21_cleanREG$text)

scores <- score.sentiment(DF_2017_8_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_8_21 <- scores
colnames(DF_2017_8_21) <- c("text", "sentiment")

DF_2017_8_21$sentiment[DF_2017_8_21$sentiment <= 5.55555] <- "0"
DF_2017_8_21$sentiment[DF_2017_8_21$sentiment > 5.55555] <- "1"

date <- c("2017-08-21")
tots <- as.numeric(nrow(DF_2017_8_21))
negs <- as.numeric(sum(DF_2017_8_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_8_21$sentiment == "1"))
results2017_8_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_8_21 <- cbind(date, tots, poss, negs, results2017_8_21)
results21LF <- rbind(results21LF, results2017_8_21)

rm(DF_2017_8_21_cleanREG, scores, DF_2017_8_21, date, tots, negs, poss, results2017_8_21)

#July

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_7_21.RData")

DF_2017_7_21_cleanREG <- DF_2017_7_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_7_21_cleanREG$text <- as.factor(DF_2017_7_21_cleanREG$text)

scores <- score.sentiment(DF_2017_7_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_7_21 <- scores
colnames(DF_2017_7_21) <- c("text", "sentiment")

DF_2017_7_21$sentiment[DF_2017_7_21$sentiment <= 5.55555] <- "0"
DF_2017_7_21$sentiment[DF_2017_7_21$sentiment > 5.55555] <- "1"

date <- c("2017-07-21")
tots <- as.numeric(nrow(DF_2017_7_21))
negs <- as.numeric(sum(DF_2017_7_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_7_21$sentiment == "1"))
results2017_7_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_7_21 <- cbind(date, tots, poss, negs, results2017_7_21)
results21LF <- rbind(results21LF, results2017_7_21)

rm(DF_2017_7_21_cleanREG, scores, DF_2017_7_21, date, tots, negs, poss, results2017_7_21)

#June

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_6_21.RData")

DF_2017_6_21_cleanREG <- DF_2017_6_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_6_21_cleanREG$text <- as.factor(DF_2017_6_21_cleanREG$text)

scores <- score.sentiment(DF_2017_6_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_6_21 <- scores
colnames(DF_2017_6_21) <- c("text", "sentiment")

DF_2017_6_21$sentiment[DF_2017_6_21$sentiment <= 5.55555] <- "0"
DF_2017_6_21$sentiment[DF_2017_6_21$sentiment > 5.55555] <- "1"

date <- c("2017-06-21")
tots <- as.numeric(nrow(DF_2017_6_21))
negs <- as.numeric(sum(DF_2017_6_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_6_21$sentiment == "1"))
results2017_6_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_6_21 <- cbind(date, tots, poss, negs, results2017_6_21)
results21LF <- rbind(results21LF, results2017_6_21)

rm(DF_2017_6_21_cleanREG, scores, DF_2017_6_21, date, tots, negs, poss, results2017_6_21)

#May

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_5_21.RData")

DF_2017_5_21_cleanREG <- DF_2017_5_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_5_21_cleanREG$text <- as.factor(DF_2017_5_21_cleanREG$text)

scores <- score.sentiment(DF_2017_5_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_5_21 <- scores
colnames(DF_2017_5_21) <- c("text", "sentiment")

DF_2017_5_21$sentiment[DF_2017_5_21$sentiment <= 5.55555] <- "0"
DF_2017_5_21$sentiment[DF_2017_5_21$sentiment > 5.55555] <- "1"

date <- c("2017-05-21")
tots <- as.numeric(nrow(DF_2017_5_21))
negs <- as.numeric(sum(DF_2017_5_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_5_21$sentiment == "1"))
results2017_5_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_5_21 <- cbind(date, tots, poss, negs, results2017_5_21)
results21LF <- rbind(results21LF, results2017_5_21)

rm(DF_2017_5_21_cleanREG, scores, DF_2017_5_21, date, tots, negs, poss, results2017_5_21)

#April

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_4_21.RData")

DF_2017_4_21_cleanREG <- DF_2017_4_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_4_21_cleanREG$text <- as.factor(DF_2017_4_21_cleanREG$text)

scores <- score.sentiment(DF_2017_4_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_4_21 <- scores
colnames(DF_2017_4_21) <- c("text", "sentiment")

DF_2017_4_21$sentiment[DF_2017_4_21$sentiment <= 5.55555] <- "0"
DF_2017_4_21$sentiment[DF_2017_4_21$sentiment > 5.55555] <- "1"

date <- c("2017-04-21")
tots <- as.numeric(nrow(DF_2017_4_21))
negs <- as.numeric(sum(DF_2017_4_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_4_21$sentiment == "1"))
results2017_4_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_4_21 <- cbind(date, tots, poss, negs, results2017_4_21)
results21LF <- rbind(results21LF, results2017_4_21)

rm(DF_2017_4_21_cleanREG, scores, DF_2017_4_21, date, tots, negs, poss, results2017_4_21)

#March

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_3_21.RData")

DF_2017_3_21_cleanREG <- DF_2017_3_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_3_21_cleanREG$text <- as.factor(DF_2017_3_21_cleanREG$text)

scores <- score.sentiment(DF_2017_3_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_3_21 <- scores
colnames(DF_2017_3_21) <- c("text", "sentiment")

DF_2017_3_21$sentiment[DF_2017_3_21$sentiment <= 5.55555] <- "0"
DF_2017_3_21$sentiment[DF_2017_3_21$sentiment > 5.55555] <- "1"

date <- c("2017-03-21")
tots <- as.numeric(nrow(DF_2017_3_21))
negs <- as.numeric(sum(DF_2017_3_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_3_21$sentiment == "1"))
results2017_3_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_3_21 <- cbind(date, tots, poss, negs, results2017_3_21)
results21LF <- rbind(results21LF, results2017_3_21)

rm(DF_2017_3_21_cleanREG, scores, DF_2017_3_21, date, tots, negs, poss, results2017_3_21)

#February

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_2_21.RData")

DF_2017_2_21_cleanREG <- DF_2017_2_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_2_21_cleanREG$text <- as.factor(DF_2017_2_21_cleanREG$text)

scores <- score.sentiment(DF_2017_2_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_2_21 <- scores
colnames(DF_2017_2_21) <- c("text", "sentiment")

DF_2017_2_21$sentiment[DF_2017_2_21$sentiment <= 5.55555] <- "0"
DF_2017_2_21$sentiment[DF_2017_2_21$sentiment > 5.55555] <- "1"

date <- c("2017-02-21")
tots <- as.numeric(nrow(DF_2017_2_21))
negs <- as.numeric(sum(DF_2017_2_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_2_21$sentiment == "1"))
results2017_2_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_2_21 <- cbind(date, tots, poss, negs, results2017_2_21)
results21LF <- rbind(results21LF, results2017_2_21)

rm(DF_2017_2_21_cleanREG, scores, DF_2017_2_21, date, tots, negs, poss, results2017_2_21)

#January

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2017_1_21.RData")

DF_2017_1_21_cleanREG <- DF_2017_1_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_1_21_cleanREG$text <- as.factor(DF_2017_1_21_cleanREG$text)

scores <- score.sentiment(DF_2017_1_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_1_21 <- scores
colnames(DF_2017_1_21) <- c("text", "sentiment")

DF_2017_1_21$sentiment[DF_2017_1_21$sentiment <= 5.55555] <- "0"
DF_2017_1_21$sentiment[DF_2017_1_21$sentiment > 5.55555] <- "1"

date <- c("2017-01-21")
tots <- as.numeric(nrow(DF_2017_1_21))
negs <- as.numeric(sum(DF_2017_1_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_1_21$sentiment == "1"))
results2017_1_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_1_21 <- cbind(date, tots, poss, negs, results2017_1_21)
results21LF <- rbind(results21LF, results2017_1_21)

rm(DF_2017_1_21_cleanREG, scores, DF_2017_1_21, date, tots, negs, poss, results2017_1_21)


print("2016")
#December

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_12_21.RData")

DF_2016_12_21_cleanREG <- DF_2016_12_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_12_21_cleanREG$text <- as.factor(DF_2016_12_21_cleanREG$text)

scores <- score.sentiment(DF_2016_12_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_12_21 <- scores
colnames(DF_2016_12_21) <- c("text", "sentiment")

DF_2016_12_21$sentiment[DF_2016_12_21$sentiment <= 5.55555] <- "0"
DF_2016_12_21$sentiment[DF_2016_12_21$sentiment > 5.55555] <- "1"

date <- c("2016-12-21")
tots <- as.numeric(nrow(DF_2016_12_21))
negs <- as.numeric(sum(DF_2016_12_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_12_21$sentiment == "1"))
results2016_12_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_12_21 <- cbind(date, tots, poss, negs, results2016_12_21)
results21LF <- rbind(results21LF, results2016_12_21)

rm(DF_2016_12_21_cleanREG, scores, DF_2016_12_21, date, tots, negs, poss, results2016_12_21)

#Nobember

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_11_21.RData")

DF_2016_11_21_cleanREG <- DF_2016_11_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_11_21_cleanREG$text <- as.factor(DF_2016_11_21_cleanREG$text)

scores <- score.sentiment(DF_2016_11_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_11_21 <- scores
colnames(DF_2016_11_21) <- c("text", "sentiment")

DF_2016_11_21$sentiment[DF_2016_11_21$sentiment <= 5.55555] <- "0"
DF_2016_11_21$sentiment[DF_2016_11_21$sentiment > 5.55555] <- "1"

date <- c("2016-11-21")
tots <- as.numeric(nrow(DF_2016_11_21))
negs <- as.numeric(sum(DF_2016_11_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_11_21$sentiment == "1"))
results2016_11_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_11_21 <- cbind(date, tots, poss, negs, results2016_11_21)
results21LF <- rbind(results21LF, results2016_11_21)

rm(DF_2016_11_21_cleanREG, scores, DF_2016_11_21, date, tots, negs, poss, results2016_11_21)

#October

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_10_21.RData")

DF_2016_10_21_cleanREG <- DF_2016_10_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_10_21_cleanREG$text <- as.factor(DF_2016_10_21_cleanREG$text)

scores <- score.sentiment(DF_2016_10_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_10_21 <- scores
colnames(DF_2016_10_21) <- c("text", "sentiment")

DF_2016_10_21$sentiment[DF_2016_10_21$sentiment <= 5.55555] <- "0"
DF_2016_10_21$sentiment[DF_2016_10_21$sentiment > 5.55555] <- "1"

date <- c("2016-10-21")
tots <- as.numeric(nrow(DF_2016_10_21))
negs <- as.numeric(sum(DF_2016_10_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_10_21$sentiment == "1"))
results2016_10_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_10_21 <- cbind(date, tots, poss, negs, results2016_10_21)
results21LF <- rbind(results21LF, results2016_10_21)

rm(DF_2016_10_21_cleanREG, scores, DF_2016_10_21, date, tots, negs, poss, results2016_10_21)

#September

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_9_21.RData")

DF_2016_9_21_cleanREG <- DF_2016_9_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_9_21_cleanREG$text <- as.factor(DF_2016_9_21_cleanREG$text)

scores <- score.sentiment(DF_2016_9_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_9_21 <- scores
colnames(DF_2016_9_21) <- c("text", "sentiment")

DF_2016_9_21$sentiment[DF_2016_9_21$sentiment <= 5.55555] <- "0"
DF_2016_9_21$sentiment[DF_2016_9_21$sentiment > 5.55555] <- "1"

date <- c("2016-09-21")
tots <- as.numeric(nrow(DF_2016_9_21))
negs <- as.numeric(sum(DF_2016_9_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_9_21$sentiment == "1"))
results2016_9_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_9_21 <- cbind(date, tots, poss, negs, results2016_9_21)
results21LF <- rbind(results21LF, results2016_9_21)

rm(DF_2016_9_21_cleanREG, scores, DF_2016_9_21, date, tots, negs, poss, results2016_9_21)

#August

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_8_21.RData")

DF_2016_8_21_cleanREG <- DF_2016_8_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_8_21_cleanREG$text <- as.factor(DF_2016_8_21_cleanREG$text)

scores <- score.sentiment(DF_2016_8_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_8_21 <- scores
colnames(DF_2016_8_21) <- c("text", "sentiment")

DF_2016_8_21$sentiment[DF_2016_8_21$sentiment <= 5.55555] <- "0"
DF_2016_8_21$sentiment[DF_2016_8_21$sentiment > 5.55555] <- "1"

date <- c("2016-08-21")
tots <- as.numeric(nrow(DF_2016_8_21))
negs <- as.numeric(sum(DF_2016_8_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_8_21$sentiment == "1"))
results2016_8_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_8_21 <- cbind(date, tots, poss, negs, results2016_8_21)
results21LF <- rbind(results21LF, results2016_8_21)

rm(DF_2016_8_21_cleanREG, scores, DF_2016_8_21, date, tots, negs, poss, results2016_8_21)

#July

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_7_21.RData")

DF_2016_7_21_cleanREG <- DF_2016_7_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_7_21_cleanREG$text <- as.factor(DF_2016_7_21_cleanREG$text)

scores <- score.sentiment(DF_2016_7_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_7_21 <- scores
colnames(DF_2016_7_21) <- c("text", "sentiment")

DF_2016_7_21$sentiment[DF_2016_7_21$sentiment <= 5.55555] <- "0"
DF_2016_7_21$sentiment[DF_2016_7_21$sentiment > 5.55555] <- "1"

date <- c("2016-07-21")
tots <- as.numeric(nrow(DF_2016_7_21))
negs <- as.numeric(sum(DF_2016_7_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_7_21$sentiment == "1"))
results2016_7_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_7_21 <- cbind(date, tots, poss, negs, results2016_7_21)
results21LF <- rbind(results21LF, results2016_7_21)

rm(DF_2016_7_21_cleanREG, scores, DF_2016_7_21, date, tots, negs, poss, results2016_7_21)

#June

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_6_21.RData")

DF_2016_6_21_cleanREG <- DF_2016_6_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_6_21_cleanREG$text <- as.factor(DF_2016_6_21_cleanREG$text)

scores <- score.sentiment(DF_2016_6_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_6_21 <- scores
colnames(DF_2016_6_21) <- c("text", "sentiment")

DF_2016_6_21$sentiment[DF_2016_6_21$sentiment <= 5.55555] <- "0"
DF_2016_6_21$sentiment[DF_2016_6_21$sentiment > 5.55555] <- "1"

date <- c("2016-06-21")
tots <- as.numeric(nrow(DF_2016_6_21))
negs <- as.numeric(sum(DF_2016_6_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_6_21$sentiment == "1"))
results2016_6_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_6_21 <- cbind(date, tots, poss, negs, results2016_6_21)
results21LF <- rbind(results21LF, results2016_6_21)

rm(DF_2016_6_21_cleanREG, scores, DF_2016_6_21, date, tots, negs, poss, results2016_6_21)

#May

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_5_21.RData")

DF_2016_5_21_cleanREG <- DF_2016_5_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_5_21_cleanREG$text <- as.factor(DF_2016_5_21_cleanREG$text)

scores <- score.sentiment(DF_2016_5_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_5_21 <- scores
colnames(DF_2016_5_21) <- c("text", "sentiment")

DF_2016_5_21$sentiment[DF_2016_5_21$sentiment <= 5.55555] <- "0"
DF_2016_5_21$sentiment[DF_2016_5_21$sentiment > 5.55555] <- "1"

date <- c("2016-05-21")
tots <- as.numeric(nrow(DF_2016_5_21))
negs <- as.numeric(sum(DF_2016_5_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_5_21$sentiment == "1"))
results2016_5_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_5_21 <- cbind(date, tots, poss, negs, results2016_5_21)
results21LF <- rbind(results21LF, results2016_5_21)

rm(DF_2016_5_21_cleanREG, scores, DF_2016_5_21, date, tots, negs, poss, results2016_5_21)

#April

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_4_21.RData")

DF_2016_4_21_cleanREG <- DF_2016_4_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_4_21_cleanREG$text <- as.factor(DF_2016_4_21_cleanREG$text)

scores <- score.sentiment(DF_2016_4_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_4_21 <- scores
colnames(DF_2016_4_21) <- c("text", "sentiment")

DF_2016_4_21$sentiment[DF_2016_4_21$sentiment <= 5.55555] <- "0"
DF_2016_4_21$sentiment[DF_2016_4_21$sentiment > 5.55555] <- "1"

date <- c("2016-04-21")
tots <- as.numeric(nrow(DF_2016_4_21))
negs <- as.numeric(sum(DF_2016_4_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_4_21$sentiment == "1"))
results2016_4_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_4_21 <- cbind(date, tots, poss, negs, results2016_4_21)
results21LF <- rbind(results21LF, results2016_4_21)

rm(DF_2016_4_21_cleanREG, scores, DF_2016_4_21, date, tots, negs, poss, results2016_4_21)

#March

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_3_21.RData")

DF_2016_3_21_cleanREG <- DF_2016_3_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_3_21_cleanREG$text <- as.factor(DF_2016_3_21_cleanREG$text)

scores <- score.sentiment(DF_2016_3_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_3_21 <- scores
colnames(DF_2016_3_21) <- c("text", "sentiment")

DF_2016_3_21$sentiment[DF_2016_3_21$sentiment <= 5.55555] <- "0"
DF_2016_3_21$sentiment[DF_2016_3_21$sentiment > 5.55555] <- "1"

date <- c("2016-03-21")
tots <- as.numeric(nrow(DF_2016_3_21))
negs <- as.numeric(sum(DF_2016_3_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_3_21$sentiment == "1"))
results2016_3_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_3_21 <- cbind(date, tots, poss, negs, results2016_3_21)
results21LF <- rbind(results21LF, results2016_3_21)

rm(DF_2016_3_21_cleanREG, scores, DF_2016_3_21, date, tots, negs, poss, results2016_3_21)

#February

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_2_21.RData")

DF_2016_2_21_cleanREG <- DF_2016_2_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_2_21_cleanREG$text <- as.factor(DF_2016_2_21_cleanREG$text)

scores <- score.sentiment(DF_2016_2_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_2_21 <- scores
colnames(DF_2016_2_21) <- c("text", "sentiment")

DF_2016_2_21$sentiment[DF_2016_2_21$sentiment <= 5.55555] <- "0"
DF_2016_2_21$sentiment[DF_2016_2_21$sentiment > 5.55555] <- "1"

date <- c("2016-02-21")
tots <- as.numeric(nrow(DF_2016_2_21))
negs <- as.numeric(sum(DF_2016_2_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_2_21$sentiment == "1"))
results2016_2_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_2_21 <- cbind(date, tots, poss, negs, results2016_2_21)
results21LF <- rbind(results21LF, results2016_2_21)

rm(DF_2016_2_21_cleanREG, scores, DF_2016_2_21, date, tots, negs, poss, results2016_2_21)

#January

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2016_1_21.RData")

DF_2016_1_21_cleanREG <- DF_2016_1_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_1_21_cleanREG$text <- as.factor(DF_2016_1_21_cleanREG$text)

scores <- score.sentiment(DF_2016_1_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_1_21 <- scores
colnames(DF_2016_1_21) <- c("text", "sentiment")

DF_2016_1_21$sentiment[DF_2016_1_21$sentiment <= 5.55555] <- "0"
DF_2016_1_21$sentiment[DF_2016_1_21$sentiment > 5.55555] <- "1"

date <- c("2016-01-21")
tots <- as.numeric(nrow(DF_2016_1_21))
negs <- as.numeric(sum(DF_2016_1_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_1_21$sentiment == "1"))
results2016_1_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_1_21 <- cbind(date, tots, poss, negs, results2016_1_21)
results21LF <- rbind(results21LF, results2016_1_21)

rm(DF_2016_1_21_cleanREG, scores, DF_2016_1_21, date, tots, negs, poss, results2016_1_21)


print("2015")
#December

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_12_21.RData")

DF_2015_12_21_cleanREG <- DF_2015_12_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_12_21_cleanREG$text <- as.factor(DF_2015_12_21_cleanREG$text)

scores <- score.sentiment(DF_2015_12_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_12_21 <- scores
colnames(DF_2015_12_21) <- c("text", "sentiment")

DF_2015_12_21$sentiment[DF_2015_12_21$sentiment <= 5.55555] <- "0"
DF_2015_12_21$sentiment[DF_2015_12_21$sentiment > 5.55555] <- "1"

date <- c("2015-12-21")
tots <- as.numeric(nrow(DF_2015_12_21))
negs <- as.numeric(sum(DF_2015_12_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_12_21$sentiment == "1"))
results2015_12_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_12_21 <- cbind(date, tots, poss, negs, results2015_12_21)
results21LF <- rbind(results21LF, results2015_12_21)

rm(DF_2015_12_21_cleanREG, scores, DF_2015_12_21, date, tots, negs, poss, results2015_12_21)

#Nobember

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_11_21.RData")

DF_2015_11_21_cleanREG <- DF_2015_11_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_11_21_cleanREG$text <- as.factor(DF_2015_11_21_cleanREG$text)

scores <- score.sentiment(DF_2015_11_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_11_21 <- scores
colnames(DF_2015_11_21) <- c("text", "sentiment")

DF_2015_11_21$sentiment[DF_2015_11_21$sentiment <= 5.55555] <- "0"
DF_2015_11_21$sentiment[DF_2015_11_21$sentiment > 5.55555] <- "1"

date <- c("2015-11-21")
tots <- as.numeric(nrow(DF_2015_11_21))
negs <- as.numeric(sum(DF_2015_11_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_11_21$sentiment == "1"))
results2015_11_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_11_21 <- cbind(date, tots, poss, negs, results2015_11_21)
results21LF <- rbind(results21LF, results2015_11_21)

rm(DF_2015_11_21_cleanREG, scores, DF_2015_11_21, date, tots, negs, poss, results2015_11_21)

#October

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_10_21.RData")

DF_2015_10_21_cleanREG <- DF_2015_10_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_10_21_cleanREG$text <- as.factor(DF_2015_10_21_cleanREG$text)

scores <- score.sentiment(DF_2015_10_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_10_21 <- scores
colnames(DF_2015_10_21) <- c("text", "sentiment")

DF_2015_10_21$sentiment[DF_2015_10_21$sentiment <= 5.55555] <- "0"
DF_2015_10_21$sentiment[DF_2015_10_21$sentiment > 5.55555] <- "1"

date <- c("2015-10-21")
tots <- as.numeric(nrow(DF_2015_10_21))
negs <- as.numeric(sum(DF_2015_10_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_10_21$sentiment == "1"))
results2015_10_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_10_21 <- cbind(date, tots, poss, negs, results2015_10_21)
results21LF <- rbind(results21LF, results2015_10_21)

rm(DF_2015_10_21_cleanREG, scores, DF_2015_10_21, date, tots, negs, poss, results2015_10_21)

#September

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_9_21.RData")

DF_2015_9_21_cleanREG <- DF_2015_9_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_9_21_cleanREG$text <- as.factor(DF_2015_9_21_cleanREG$text)

scores <- score.sentiment(DF_2015_9_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_9_21 <- scores
colnames(DF_2015_9_21) <- c("text", "sentiment")

DF_2015_9_21$sentiment[DF_2015_9_21$sentiment <= 5.55555] <- "0"
DF_2015_9_21$sentiment[DF_2015_9_21$sentiment > 5.55555] <- "1"

date <- c("2015-09-21")
tots <- as.numeric(nrow(DF_2015_9_21))
negs <- as.numeric(sum(DF_2015_9_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_9_21$sentiment == "1"))
results2015_9_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_9_21 <- cbind(date, tots, poss, negs, results2015_9_21)
results21LF <- rbind(results21LF, results2015_9_21)

rm(DF_2015_9_21_cleanREG, scores, DF_2015_9_21, date, tots, negs, poss, results2015_9_21)

#August

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_8_21.RData")

DF_2015_8_21_cleanREG <- DF_2015_8_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_8_21_cleanREG$text <- as.factor(DF_2015_8_21_cleanREG$text)

scores <- score.sentiment(DF_2015_8_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_8_21 <- scores
colnames(DF_2015_8_21) <- c("text", "sentiment")

DF_2015_8_21$sentiment[DF_2015_8_21$sentiment <= 5.55555] <- "0"
DF_2015_8_21$sentiment[DF_2015_8_21$sentiment > 5.55555] <- "1"

date <- c("2015-08-21")
tots <- as.numeric(nrow(DF_2015_8_21))
negs <- as.numeric(sum(DF_2015_8_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_8_21$sentiment == "1"))
results2015_8_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_8_21 <- cbind(date, tots, poss, negs, results2015_8_21)
results21LF <- rbind(results21LF, results2015_8_21)

rm(DF_2015_8_21_cleanREG, scores, DF_2015_8_21, date, tots, negs, poss, results2015_8_21)

#July

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_7_21.RData")

DF_2015_7_21_cleanREG <- DF_2015_7_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_7_21_cleanREG$text <- as.factor(DF_2015_7_21_cleanREG$text)

scores <- score.sentiment(DF_2015_7_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_7_21 <- scores
colnames(DF_2015_7_21) <- c("text", "sentiment")

DF_2015_7_21$sentiment[DF_2015_7_21$sentiment <= 5.55555] <- "0"
DF_2015_7_21$sentiment[DF_2015_7_21$sentiment > 5.55555] <- "1"

date <- c("2015-07-21")
tots <- as.numeric(nrow(DF_2015_7_21))
negs <- as.numeric(sum(DF_2015_7_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_7_21$sentiment == "1"))
results2015_7_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_7_21 <- cbind(date, tots, poss, negs, results2015_7_21)
results21LF <- rbind(results21LF, results2015_7_21)

rm(DF_2015_7_21_cleanREG, scores, DF_2015_7_21, date, tots, negs, poss, results2015_7_21)

#June

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_6_21.RData")

DF_2015_6_21_cleanREG <- DF_2015_6_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_6_21_cleanREG$text <- as.factor(DF_2015_6_21_cleanREG$text)

scores <- score.sentiment(DF_2015_6_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_6_21 <- scores
colnames(DF_2015_6_21) <- c("text", "sentiment")

DF_2015_6_21$sentiment[DF_2015_6_21$sentiment <= 5.55555] <- "0"
DF_2015_6_21$sentiment[DF_2015_6_21$sentiment > 5.55555] <- "1"

date <- c("2015-06-21")
tots <- as.numeric(nrow(DF_2015_6_21))
negs <- as.numeric(sum(DF_2015_6_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_6_21$sentiment == "1"))
results2015_6_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_6_21 <- cbind(date, tots, poss, negs, results2015_6_21)
results21LF <- rbind(results21LF, results2015_6_21)

rm(DF_2015_6_21_cleanREG, scores, DF_2015_6_21, date, tots, negs, poss, results2015_6_21)

#May

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_5_21.RData")

DF_2015_5_21_cleanREG <- DF_2015_5_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_5_21_cleanREG$text <- as.factor(DF_2015_5_21_cleanREG$text)

scores <- score.sentiment(DF_2015_5_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_5_21 <- scores
colnames(DF_2015_5_21) <- c("text", "sentiment")

DF_2015_5_21$sentiment[DF_2015_5_21$sentiment <= 5.55555] <- "0"
DF_2015_5_21$sentiment[DF_2015_5_21$sentiment > 5.55555] <- "1"

date <- c("2015-05-21")
tots <- as.numeric(nrow(DF_2015_5_21))
negs <- as.numeric(sum(DF_2015_5_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_5_21$sentiment == "1"))
results2015_5_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_5_21 <- cbind(date, tots, poss, negs, results2015_5_21)
results21LF <- rbind(results21LF, results2015_5_21)

rm(DF_2015_5_21_cleanREG, scores, DF_2015_5_21, date, tots, negs, poss, results2015_5_21)

#April

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_4_21.RData")

DF_2015_4_21_cleanREG <- DF_2015_4_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_4_21_cleanREG$text <- as.factor(DF_2015_4_21_cleanREG$text)

scores <- score.sentiment(DF_2015_4_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_4_21 <- scores
colnames(DF_2015_4_21) <- c("text", "sentiment")

DF_2015_4_21$sentiment[DF_2015_4_21$sentiment <= 5.55555] <- "0"
DF_2015_4_21$sentiment[DF_2015_4_21$sentiment > 5.55555] <- "1"

date <- c("2015-04-21")
tots <- as.numeric(nrow(DF_2015_4_21))
negs <- as.numeric(sum(DF_2015_4_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_4_21$sentiment == "1"))
results2015_4_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_4_21 <- cbind(date, tots, poss, negs, results2015_4_21)
results21LF <- rbind(results21LF, results2015_4_21)

rm(DF_2015_4_21_cleanREG, scores, DF_2015_4_21, date, tots, negs, poss, results2015_4_21)

#March

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_3_21.RData")

DF_2015_3_21_cleanREG <- DF_2015_3_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_3_21_cleanREG$text <- as.factor(DF_2015_3_21_cleanREG$text)

scores <- score.sentiment(DF_2015_3_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_3_21 <- scores
colnames(DF_2015_3_21) <- c("text", "sentiment")

DF_2015_3_21$sentiment[DF_2015_3_21$sentiment <= 5.55555] <- "0"
DF_2015_3_21$sentiment[DF_2015_3_21$sentiment > 5.55555] <- "1"

date <- c("2015-03-21")
tots <- as.numeric(nrow(DF_2015_3_21))
negs <- as.numeric(sum(DF_2015_3_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_3_21$sentiment == "1"))
results2015_3_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_3_21 <- cbind(date, tots, poss, negs, results2015_3_21)
results21LF <- rbind(results21LF, results2015_3_21)

rm(DF_2015_3_21_cleanREG, scores, DF_2015_3_21, date, tots, negs, poss, results2015_3_21)

#February

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_2_21.RData")

DF_2015_2_21_cleanREG <- DF_2015_2_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_2_21_cleanREG$text <- as.factor(DF_2015_2_21_cleanREG$text)

scores <- score.sentiment(DF_2015_2_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_2_21 <- scores
colnames(DF_2015_2_21) <- c("text", "sentiment")

DF_2015_2_21$sentiment[DF_2015_2_21$sentiment <= 5.55555] <- "0"
DF_2015_2_21$sentiment[DF_2015_2_21$sentiment > 5.55555] <- "1"

date <- c("2015-02-21")
tots <- as.numeric(nrow(DF_2015_2_21))
negs <- as.numeric(sum(DF_2015_2_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_2_21$sentiment == "1"))
results2015_2_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_2_21 <- cbind(date, tots, poss, negs, results2015_2_21)
results21LF <- rbind(results21LF, results2015_2_21)

rm(DF_2015_2_21_cleanREG, scores, DF_2015_2_21, date, tots, negs, poss, results2015_2_21)

#January

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2015_1_21.RData")

DF_2015_1_21_cleanREG <- DF_2015_1_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_1_21_cleanREG$text <- as.factor(DF_2015_1_21_cleanREG$text)

scores <- score.sentiment(DF_2015_1_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_1_21 <- scores
colnames(DF_2015_1_21) <- c("text", "sentiment")

DF_2015_1_21$sentiment[DF_2015_1_21$sentiment <= 5.55555] <- "0"
DF_2015_1_21$sentiment[DF_2015_1_21$sentiment > 5.55555] <- "1"

date <- c("2015-01-21")
tots <- as.numeric(nrow(DF_2015_1_21))
negs <- as.numeric(sum(DF_2015_1_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_1_21$sentiment == "1"))
results2015_1_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_1_21 <- cbind(date, tots, poss, negs, results2015_1_21)
results21LF <- rbind(results21LF, results2015_1_21)

rm(DF_2015_1_21_cleanREG, scores, DF_2015_1_21, date, tots, negs, poss, results2015_1_21)


print("2014")
#December

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_12_21.RData")

DF_2014_12_21_cleanREG <- DF_2014_12_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_12_21_cleanREG$text <- as.factor(DF_2014_12_21_cleanREG$text)

scores <- score.sentiment(DF_2014_12_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_12_21 <- scores
colnames(DF_2014_12_21) <- c("text", "sentiment")

DF_2014_12_21$sentiment[DF_2014_12_21$sentiment <= 5.55555] <- "0"
DF_2014_12_21$sentiment[DF_2014_12_21$sentiment > 5.55555] <- "1"

date <- c("2014-12-21")
tots <- as.numeric(nrow(DF_2014_12_21))
negs <- as.numeric(sum(DF_2014_12_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_12_21$sentiment == "1"))
results2014_12_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_12_21 <- cbind(date, tots, poss, negs, results2014_12_21)
results21LF <- rbind(results21LF, results2014_12_21)

rm(DF_2014_12_21_cleanREG, scores, DF_2014_12_21, date, tots, negs, poss, results2014_12_21)

#Nobember

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_11_21.RData")

DF_2014_11_21_cleanREG <- DF_2014_11_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_11_21_cleanREG$text <- as.factor(DF_2014_11_21_cleanREG$text)

scores <- score.sentiment(DF_2014_11_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_11_21 <- scores
colnames(DF_2014_11_21) <- c("text", "sentiment")

DF_2014_11_21$sentiment[DF_2014_11_21$sentiment <= 5.55555] <- "0"
DF_2014_11_21$sentiment[DF_2014_11_21$sentiment > 5.55555] <- "1"

date <- c("2014-11-21")
tots <- as.numeric(nrow(DF_2014_11_21))
negs <- as.numeric(sum(DF_2014_11_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_11_21$sentiment == "1"))
results2014_11_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_11_21 <- cbind(date, tots, poss, negs, results2014_11_21)
results21LF <- rbind(results21LF, results2014_11_21)

rm(DF_2014_11_21_cleanREG, scores, DF_2014_11_21, date, tots, negs, poss, results2014_11_21)

#October

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_10_21.RData")

DF_2014_10_21_cleanREG <- DF_2014_10_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_10_21_cleanREG$text <- as.factor(DF_2014_10_21_cleanREG$text)

scores <- score.sentiment(DF_2014_10_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_10_21 <- scores
colnames(DF_2014_10_21) <- c("text", "sentiment")

DF_2014_10_21$sentiment[DF_2014_10_21$sentiment <= 5.55555] <- "0"
DF_2014_10_21$sentiment[DF_2014_10_21$sentiment > 5.55555] <- "1"

date <- c("2014-10-21")
tots <- as.numeric(nrow(DF_2014_10_21))
negs <- as.numeric(sum(DF_2014_10_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_10_21$sentiment == "1"))
results2014_10_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_10_21 <- cbind(date, tots, poss, negs, results2014_10_21)
results21LF <- rbind(results21LF, results2014_10_21)

rm(DF_2014_10_21_cleanREG, scores, DF_2014_10_21, date, tots, negs, poss, results2014_10_21)

#September

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_9_21.RData")

DF_2014_9_21_cleanREG <- DF_2014_9_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_9_21_cleanREG$text <- as.factor(DF_2014_9_21_cleanREG$text)

scores <- score.sentiment(DF_2014_9_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_9_21 <- scores
colnames(DF_2014_9_21) <- c("text", "sentiment")

DF_2014_9_21$sentiment[DF_2014_9_21$sentiment <= 5.55555] <- "0"
DF_2014_9_21$sentiment[DF_2014_9_21$sentiment > 5.55555] <- "1"

date <- c("2014-09-21")
tots <- as.numeric(nrow(DF_2014_9_21))
negs <- as.numeric(sum(DF_2014_9_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_9_21$sentiment == "1"))
results2014_9_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_9_21 <- cbind(date, tots, poss, negs, results2014_9_21)
results21LF <- rbind(results21LF, results2014_9_21)

rm(DF_2014_9_21_cleanREG, scores, DF_2014_9_21, date, tots, negs, poss, results2014_9_21)

#August

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_8_21.RData")

DF_2014_8_21_cleanREG <- DF_2014_8_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_8_21_cleanREG$text <- as.factor(DF_2014_8_21_cleanREG$text)

scores <- score.sentiment(DF_2014_8_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_8_21 <- scores
colnames(DF_2014_8_21) <- c("text", "sentiment")

DF_2014_8_21$sentiment[DF_2014_8_21$sentiment <= 5.55555] <- "0"
DF_2014_8_21$sentiment[DF_2014_8_21$sentiment > 5.55555] <- "1"

date <- c("2014-08-21")
tots <- as.numeric(nrow(DF_2014_8_21))
negs <- as.numeric(sum(DF_2014_8_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_8_21$sentiment == "1"))
results2014_8_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_8_21 <- cbind(date, tots, poss, negs, results2014_8_21)
results21LF <- rbind(results21LF, results2014_8_21)

rm(DF_2014_8_21_cleanREG, scores, DF_2014_8_21, date, tots, negs, poss, results2014_8_21)

#July

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_7_21.RData")

DF_2014_7_21_cleanREG <- DF_2014_7_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_7_21_cleanREG$text <- as.factor(DF_2014_7_21_cleanREG$text)

scores <- score.sentiment(DF_2014_7_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_7_21 <- scores
colnames(DF_2014_7_21) <- c("text", "sentiment")

DF_2014_7_21$sentiment[DF_2014_7_21$sentiment <= 5.55555] <- "0"
DF_2014_7_21$sentiment[DF_2014_7_21$sentiment > 5.55555] <- "1"

date <- c("2014-07-21")
tots <- as.numeric(nrow(DF_2014_7_21))
negs <- as.numeric(sum(DF_2014_7_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_7_21$sentiment == "1"))
results2014_7_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_7_21 <- cbind(date, tots, poss, negs, results2014_7_21)
results21LF <- rbind(results21LF, results2014_7_21)

rm(DF_2014_7_21_cleanREG, scores, DF_2014_7_21, date, tots, negs, poss, results2014_7_21)

#June

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_6_21.RData")

DF_2014_6_21_cleanREG <- DF_2014_6_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_6_21_cleanREG$text <- as.factor(DF_2014_6_21_cleanREG$text)

scores <- score.sentiment(DF_2014_6_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_6_21 <- scores
colnames(DF_2014_6_21) <- c("text", "sentiment")

DF_2014_6_21$sentiment[DF_2014_6_21$sentiment <= 5.55555] <- "0"
DF_2014_6_21$sentiment[DF_2014_6_21$sentiment > 5.55555] <- "1"

date <- c("2014-06-21")
tots <- as.numeric(nrow(DF_2014_6_21))
negs <- as.numeric(sum(DF_2014_6_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_6_21$sentiment == "1"))
results2014_6_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_6_21 <- cbind(date, tots, poss, negs, results2014_6_21)
results21LF <- rbind(results21LF, results2014_6_21)

rm(DF_2014_6_21_cleanREG, scores, DF_2014_6_21, date, tots, negs, poss, results2014_6_21)

#May

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_5_21.RData")

DF_2014_5_21_cleanREG <- DF_2014_5_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_5_21_cleanREG$text <- as.factor(DF_2014_5_21_cleanREG$text)

scores <- score.sentiment(DF_2014_5_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_5_21 <- scores
colnames(DF_2014_5_21) <- c("text", "sentiment")

DF_2014_5_21$sentiment[DF_2014_5_21$sentiment <= 5.55555] <- "0"
DF_2014_5_21$sentiment[DF_2014_5_21$sentiment > 5.55555] <- "1"

date <- c("2014-05-21")
tots <- as.numeric(nrow(DF_2014_5_21))
negs <- as.numeric(sum(DF_2014_5_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_5_21$sentiment == "1"))
results2014_5_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_5_21 <- cbind(date, tots, poss, negs, results2014_5_21)
results21LF <- rbind(results21LF, results2014_5_21)

rm(DF_2014_5_21_cleanREG, scores, DF_2014_5_21, date, tots, negs, poss, results2014_5_21)

#April

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_4_21.RData")

DF_2014_4_21_cleanREG <- DF_2014_4_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_4_21_cleanREG$text <- as.factor(DF_2014_4_21_cleanREG$text)

scores <- score.sentiment(DF_2014_4_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_4_21 <- scores
colnames(DF_2014_4_21) <- c("text", "sentiment")

DF_2014_4_21$sentiment[DF_2014_4_21$sentiment <= 5.55555] <- "0"
DF_2014_4_21$sentiment[DF_2014_4_21$sentiment > 5.55555] <- "1"

date <- c("2014-04-21")
tots <- as.numeric(nrow(DF_2014_4_21))
negs <- as.numeric(sum(DF_2014_4_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_4_21$sentiment == "1"))
results2014_4_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_4_21 <- cbind(date, tots, poss, negs, results2014_4_21)
results21LF <- rbind(results21LF, results2014_4_21)

rm(DF_2014_4_21_cleanREG, scores, DF_2014_4_21, date, tots, negs, poss, results2014_4_21)

#March

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_3_21.RData")

DF_2014_3_21_cleanREG <- DF_2014_3_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_3_21_cleanREG$text <- as.factor(DF_2014_3_21_cleanREG$text)

scores <- score.sentiment(DF_2014_3_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_3_21 <- scores
colnames(DF_2014_3_21) <- c("text", "sentiment")

DF_2014_3_21$sentiment[DF_2014_3_21$sentiment <= 5.55555] <- "0"
DF_2014_3_21$sentiment[DF_2014_3_21$sentiment > 5.55555] <- "1"

date <- c("2014-03-21")
tots <- as.numeric(nrow(DF_2014_3_21))
negs <- as.numeric(sum(DF_2014_3_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_3_21$sentiment == "1"))
results2014_3_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_3_21 <- cbind(date, tots, poss, negs, results2014_3_21)
results21LF <- rbind(results21LF, results2014_3_21)

rm(DF_2014_3_21_cleanREG, scores, DF_2014_3_21, date, tots, negs, poss, results2014_3_21)

#February

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_2_21.RData")

DF_2014_2_21_cleanREG <- DF_2014_2_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_2_21_cleanREG$text <- as.factor(DF_2014_2_21_cleanREG$text)

scores <- score.sentiment(DF_2014_2_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_2_21 <- scores
colnames(DF_2014_2_21) <- c("text", "sentiment")

DF_2014_2_21$sentiment[DF_2014_2_21$sentiment <= 5.55555] <- "0"
DF_2014_2_21$sentiment[DF_2014_2_21$sentiment > 5.55555] <- "1"

date <- c("2014-02-21")
tots <- as.numeric(nrow(DF_2014_2_21))
negs <- as.numeric(sum(DF_2014_2_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_2_21$sentiment == "1"))
results2014_2_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_2_21 <- cbind(date, tots, poss, negs, results2014_2_21)
results21LF <- rbind(results21LF, results2014_2_21)

rm(DF_2014_2_21_cleanREG, scores, DF_2014_2_21, date, tots, negs, poss, results2014_2_21)

#January

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2014_1_21.RData")

DF_2014_1_21_cleanREG <- DF_2014_1_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_1_21_cleanREG$text <- as.factor(DF_2014_1_21_cleanREG$text)

scores <- score.sentiment(DF_2014_1_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_1_21 <- scores
colnames(DF_2014_1_21) <- c("text", "sentiment")

DF_2014_1_21$sentiment[DF_2014_1_21$sentiment <= 5.55555] <- "0"
DF_2014_1_21$sentiment[DF_2014_1_21$sentiment > 5.55555] <- "1"

date <- c("2014-01-21")
tots <- as.numeric(nrow(DF_2014_1_21))
negs <- as.numeric(sum(DF_2014_1_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_1_21$sentiment == "1"))
results2014_1_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_1_21 <- cbind(date, tots, poss, negs, results2014_1_21)
results21LF <- rbind(results21LF, results2014_1_21)

rm(DF_2014_1_21_cleanREG, scores, DF_2014_1_21, date, tots, negs, poss, results2014_1_21)


print("2013")
#December

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_12_21.RData")

DF_2013_12_21_cleanREG <- DF_2013_12_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_12_21_cleanREG$text <- as.factor(DF_2013_12_21_cleanREG$text)

scores <- score.sentiment(DF_2013_12_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_12_21 <- scores
colnames(DF_2013_12_21) <- c("text", "sentiment")

DF_2013_12_21$sentiment[DF_2013_12_21$sentiment <= 5.55555] <- "0"
DF_2013_12_21$sentiment[DF_2013_12_21$sentiment > 5.55555] <- "1"

date <- c("2013-12-21")
tots <- as.numeric(nrow(DF_2013_12_21))
negs <- as.numeric(sum(DF_2013_12_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_12_21$sentiment == "1"))
results2013_12_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_12_21 <- cbind(date, tots, poss, negs, results2013_12_21)
results21LF <- rbind(results21LF, results2013_12_21)

rm(DF_2013_12_21_cleanREG, scores, DF_2013_12_21, date, tots, negs, poss, results2013_12_21)

#Nobember

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_11_21.RData")

DF_2013_11_21_cleanREG <- DF_2013_11_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_11_21_cleanREG$text <- as.factor(DF_2013_11_21_cleanREG$text)

scores <- score.sentiment(DF_2013_11_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_11_21 <- scores
colnames(DF_2013_11_21) <- c("text", "sentiment")

DF_2013_11_21$sentiment[DF_2013_11_21$sentiment <= 5.55555] <- "0"
DF_2013_11_21$sentiment[DF_2013_11_21$sentiment > 5.55555] <- "1"

date <- c("2013-11-21")
tots <- as.numeric(nrow(DF_2013_11_21))
negs <- as.numeric(sum(DF_2013_11_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_11_21$sentiment == "1"))
results2013_11_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_11_21 <- cbind(date, tots, poss, negs, results2013_11_21)
results21LF <- rbind(results21LF, results2013_11_21)

rm(DF_2013_11_21_cleanREG, scores, DF_2013_11_21, date, tots, negs, poss, results2013_11_21)

#October

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_10_21.RData")

DF_2013_10_21_cleanREG <- DF_2013_10_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_10_21_cleanREG$text <- as.factor(DF_2013_10_21_cleanREG$text)

scores <- score.sentiment(DF_2013_10_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_10_21 <- scores
colnames(DF_2013_10_21) <- c("text", "sentiment")

DF_2013_10_21$sentiment[DF_2013_10_21$sentiment <= 5.55555] <- "0"
DF_2013_10_21$sentiment[DF_2013_10_21$sentiment > 5.55555] <- "1"

date <- c("2013-10-21")
tots <- as.numeric(nrow(DF_2013_10_21))
negs <- as.numeric(sum(DF_2013_10_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_10_21$sentiment == "1"))
results2013_10_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_10_21 <- cbind(date, tots, poss, negs, results2013_10_21)
results21LF <- rbind(results21LF, results2013_10_21)

rm(DF_2013_10_21_cleanREG, scores, DF_2013_10_21, date, tots, negs, poss, results2013_10_21)

#September

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_9_21.RData")

DF_2013_9_21_cleanREG <- DF_2013_9_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_9_21_cleanREG$text <- as.factor(DF_2013_9_21_cleanREG$text)

scores <- score.sentiment(DF_2013_9_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_9_21 <- scores
colnames(DF_2013_9_21) <- c("text", "sentiment")

DF_2013_9_21$sentiment[DF_2013_9_21$sentiment <= 5.55555] <- "0"
DF_2013_9_21$sentiment[DF_2013_9_21$sentiment > 5.55555] <- "1"

date <- c("2013-09-21")
tots <- as.numeric(nrow(DF_2013_9_21))
negs <- as.numeric(sum(DF_2013_9_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_9_21$sentiment == "1"))
results2013_9_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_9_21 <- cbind(date, tots, poss, negs, results2013_9_21)
results21LF <- rbind(results21LF, results2013_9_21)

rm(DF_2013_9_21_cleanREG, scores, DF_2013_9_21, date, tots, negs, poss, results2013_9_21)

#August

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_8_21.RData")

DF_2013_8_21_cleanREG <- DF_2013_8_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_8_21_cleanREG$text <- as.factor(DF_2013_8_21_cleanREG$text)

scores <- score.sentiment(DF_2013_8_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_8_21 <- scores
colnames(DF_2013_8_21) <- c("text", "sentiment")

DF_2013_8_21$sentiment[DF_2013_8_21$sentiment <= 5.55555] <- "0"
DF_2013_8_21$sentiment[DF_2013_8_21$sentiment > 5.55555] <- "1"

date <- c("2013-08-21")
tots <- as.numeric(nrow(DF_2013_8_21))
negs <- as.numeric(sum(DF_2013_8_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_8_21$sentiment == "1"))
results2013_8_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_8_21 <- cbind(date, tots, poss, negs, results2013_8_21)
results21LF <- rbind(results21LF, results2013_8_21)

rm(DF_2013_8_21_cleanREG, scores, DF_2013_8_21, date, tots, negs, poss, results2013_8_21)

#July

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_7_21.RData")

DF_2013_7_21_cleanREG <- DF_2013_7_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_7_21_cleanREG$text <- as.factor(DF_2013_7_21_cleanREG$text)

scores <- score.sentiment(DF_2013_7_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_7_21 <- scores
colnames(DF_2013_7_21) <- c("text", "sentiment")

DF_2013_7_21$sentiment[DF_2013_7_21$sentiment <= 5.55555] <- "0"
DF_2013_7_21$sentiment[DF_2013_7_21$sentiment > 5.55555] <- "1"

date <- c("2013-07-21")
tots <- as.numeric(nrow(DF_2013_7_21))
negs <- as.numeric(sum(DF_2013_7_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_7_21$sentiment == "1"))
results2013_7_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_7_21 <- cbind(date, tots, poss, negs, results2013_7_21)
results21LF <- rbind(results21LF, results2013_7_21)

rm(DF_2013_7_21_cleanREG, scores, DF_2013_7_21, date, tots, negs, poss, results2013_7_21)

#June

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_6_21.RData")

DF_2013_6_21_cleanREG <- DF_2013_6_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_6_21_cleanREG$text <- as.factor(DF_2013_6_21_cleanREG$text)

scores <- score.sentiment(DF_2013_6_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_6_21 <- scores
colnames(DF_2013_6_21) <- c("text", "sentiment")

DF_2013_6_21$sentiment[DF_2013_6_21$sentiment <= 5.55555] <- "0"
DF_2013_6_21$sentiment[DF_2013_6_21$sentiment > 5.55555] <- "1"

date <- c("2013-06-21")
tots <- as.numeric(nrow(DF_2013_6_21))
negs <- as.numeric(sum(DF_2013_6_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_6_21$sentiment == "1"))
results2013_6_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_6_21 <- cbind(date, tots, poss, negs, results2013_6_21)
results21LF <- rbind(results21LF, results2013_6_21)

rm(DF_2013_6_21_cleanREG, scores, DF_2013_6_21, date, tots, negs, poss, results2013_6_21)

#May

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_5_21.RData")

DF_2013_5_21_cleanREG <- DF_2013_5_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_5_21_cleanREG$text <- as.factor(DF_2013_5_21_cleanREG$text)

scores <- score.sentiment(DF_2013_5_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_5_21 <- scores
colnames(DF_2013_5_21) <- c("text", "sentiment")

DF_2013_5_21$sentiment[DF_2013_5_21$sentiment <= 5.55555] <- "0"
DF_2013_5_21$sentiment[DF_2013_5_21$sentiment > 5.55555] <- "1"

date <- c("2013-05-21")
tots <- as.numeric(nrow(DF_2013_5_21))
negs <- as.numeric(sum(DF_2013_5_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_5_21$sentiment == "1"))
results2013_5_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_5_21 <- cbind(date, tots, poss, negs, results2013_5_21)
results21LF <- rbind(results21LF, results2013_5_21)

rm(DF_2013_5_21_cleanREG, scores, DF_2013_5_21, date, tots, negs, poss, results2013_5_21)

#April

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_4_21.RData")

DF_2013_4_21_cleanREG <- DF_2013_4_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_4_21_cleanREG$text <- as.factor(DF_2013_4_21_cleanREG$text)

scores <- score.sentiment(DF_2013_4_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_4_21 <- scores
colnames(DF_2013_4_21) <- c("text", "sentiment")

DF_2013_4_21$sentiment[DF_2013_4_21$sentiment <= 5.55555] <- "0"
DF_2013_4_21$sentiment[DF_2013_4_21$sentiment > 5.55555] <- "1"

date <- c("2013-04-21")
tots <- as.numeric(nrow(DF_2013_4_21))
negs <- as.numeric(sum(DF_2013_4_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_4_21$sentiment == "1"))
results2013_4_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_4_21 <- cbind(date, tots, poss, negs, results2013_4_21)
results21LF <- rbind(results21LF, results2013_4_21)

rm(DF_2013_4_21_cleanREG, scores, DF_2013_4_21, date, tots, negs, poss, results2013_4_21)

#March

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_3_21.RData")

DF_2013_3_21_cleanREG <- DF_2013_3_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_3_21_cleanREG$text <- as.factor(DF_2013_3_21_cleanREG$text)

scores <- score.sentiment(DF_2013_3_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_3_21 <- scores
colnames(DF_2013_3_21) <- c("text", "sentiment")

DF_2013_3_21$sentiment[DF_2013_3_21$sentiment <= 5.55555] <- "0"
DF_2013_3_21$sentiment[DF_2013_3_21$sentiment > 5.55555] <- "1"

date <- c("2013-03-21")
tots <- as.numeric(nrow(DF_2013_3_21))
negs <- as.numeric(sum(DF_2013_3_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_3_21$sentiment == "1"))
results2013_3_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_3_21 <- cbind(date, tots, poss, negs, results2013_3_21)
results21LF <- rbind(results21LF, results2013_3_21)

rm(DF_2013_3_21_cleanREG, scores, DF_2013_3_21, date, tots, negs, poss, results2013_3_21)

#February

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_2_21.RData")

DF_2013_2_21_cleanREG <- DF_2013_2_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_2_21_cleanREG$text <- as.factor(DF_2013_2_21_cleanREG$text)

scores <- score.sentiment(DF_2013_2_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_2_21 <- scores
colnames(DF_2013_2_21) <- c("text", "sentiment")

DF_2013_2_21$sentiment[DF_2013_2_21$sentiment <= 5.55555] <- "0"
DF_2013_2_21$sentiment[DF_2013_2_21$sentiment > 5.55555] <- "1"

date <- c("2013-02-21")
tots <- as.numeric(nrow(DF_2013_2_21))
negs <- as.numeric(sum(DF_2013_2_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_2_21$sentiment == "1"))
results2013_2_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_2_21 <- cbind(date, tots, poss, negs, results2013_2_21)
results21LF <- rbind(results21LF, results2013_2_21)

rm(DF_2013_2_21_cleanREG, scores, DF_2013_2_21, date, tots, negs, poss, results2013_2_21)

#January

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2013_1_21.RData")

DF_2013_1_21_cleanREG <- DF_2013_1_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_1_21_cleanREG$text <- as.factor(DF_2013_1_21_cleanREG$text)

scores <- score.sentiment(DF_2013_1_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_1_21 <- scores
colnames(DF_2013_1_21) <- c("text", "sentiment")

DF_2013_1_21$sentiment[DF_2013_1_21$sentiment <= 5.55555] <- "0"
DF_2013_1_21$sentiment[DF_2013_1_21$sentiment > 5.55555] <- "1"

date <- c("2013-01-21")
tots <- as.numeric(nrow(DF_2013_1_21))
negs <- as.numeric(sum(DF_2013_1_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_1_21$sentiment == "1"))
results2013_1_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_1_21 <- cbind(date, tots, poss, negs, results2013_1_21)
results21LF <- rbind(results21LF, results2013_1_21)

rm(DF_2013_1_21_cleanREG, scores, DF_2013_1_21, date, tots, negs, poss, results2013_1_21)


print("2012")
#December

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_12_21.RData")

DF_2012_12_21_cleanREG <- DF_2012_12_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_12_21_cleanREG$text <- as.factor(DF_2012_12_21_cleanREG$text)

scores <- score.sentiment(DF_2012_12_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_12_21 <- scores
colnames(DF_2012_12_21) <- c("text", "sentiment")

DF_2012_12_21$sentiment[DF_2012_12_21$sentiment <= 5.55555] <- "0"
DF_2012_12_21$sentiment[DF_2012_12_21$sentiment > 5.55555] <- "1"

date <- c("2012-12-21")
tots <- as.numeric(nrow(DF_2012_12_21))
negs <- as.numeric(sum(DF_2012_12_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_12_21$sentiment == "1"))
results2012_12_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_12_21 <- cbind(date, tots, poss, negs, results2012_12_21)
results21LF <- rbind(results21LF, results2012_12_21)

rm(DF_2012_12_21_cleanREG, scores, DF_2012_12_21, date, tots, negs, poss, results2012_12_21)

#Nobember

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_11_21.RData")

DF_2012_11_21_cleanREG <- DF_2012_11_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_11_21_cleanREG$text <- as.factor(DF_2012_11_21_cleanREG$text)

scores <- score.sentiment(DF_2012_11_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_11_21 <- scores
colnames(DF_2012_11_21) <- c("text", "sentiment")

DF_2012_11_21$sentiment[DF_2012_11_21$sentiment <= 5.55555] <- "0"
DF_2012_11_21$sentiment[DF_2012_11_21$sentiment > 5.55555] <- "1"

date <- c("2012-11-21")
tots <- as.numeric(nrow(DF_2012_11_21))
negs <- as.numeric(sum(DF_2012_11_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_11_21$sentiment == "1"))
results2012_11_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_11_21 <- cbind(date, tots, poss, negs, results2012_11_21)
results21LF <- rbind(results21LF, results2012_11_21)

rm(DF_2012_11_21_cleanREG, scores, DF_2012_11_21, date, tots, negs, poss, results2012_11_21)

#October

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_10_21.RData")

DF_2012_10_21_cleanREG <- DF_2012_10_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_10_21_cleanREG$text <- as.factor(DF_2012_10_21_cleanREG$text)

scores <- score.sentiment(DF_2012_10_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_10_21 <- scores
colnames(DF_2012_10_21) <- c("text", "sentiment")

DF_2012_10_21$sentiment[DF_2012_10_21$sentiment <= 5.55555] <- "0"
DF_2012_10_21$sentiment[DF_2012_10_21$sentiment > 5.55555] <- "1"

date <- c("2012-10-21")
tots <- as.numeric(nrow(DF_2012_10_21))
negs <- as.numeric(sum(DF_2012_10_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_10_21$sentiment == "1"))
results2012_10_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_10_21 <- cbind(date, tots, poss, negs, results2012_10_21)
results21LF <- rbind(results21LF, results2012_10_21)

rm(DF_2012_10_21_cleanREG, scores, DF_2012_10_21, date, tots, negs, poss, results2012_10_21)

#September

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_9_21.RData")

DF_2012_9_21_cleanREG <- DF_2012_9_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_9_21_cleanREG$text <- as.factor(DF_2012_9_21_cleanREG$text)

scores <- score.sentiment(DF_2012_9_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_9_21 <- scores
colnames(DF_2012_9_21) <- c("text", "sentiment")

DF_2012_9_21$sentiment[DF_2012_9_21$sentiment <= 5.55555] <- "0"
DF_2012_9_21$sentiment[DF_2012_9_21$sentiment > 5.55555] <- "1"

date <- c("2012-09-21")
tots <- as.numeric(nrow(DF_2012_9_21))
negs <- as.numeric(sum(DF_2012_9_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_9_21$sentiment == "1"))
results2012_9_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_9_21 <- cbind(date, tots, poss, negs, results2012_9_21)
results21LF <- rbind(results21LF, results2012_9_21)

rm(DF_2012_9_21_cleanREG, scores, DF_2012_9_21, date, tots, negs, poss, results2012_9_21)

#August

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_8_21.RData")

DF_2012_8_21_cleanREG <- DF_2012_8_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_8_21_cleanREG$text <- as.factor(DF_2012_8_21_cleanREG$text)

scores <- score.sentiment(DF_2012_8_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_8_21 <- scores
colnames(DF_2012_8_21) <- c("text", "sentiment")

DF_2012_8_21$sentiment[DF_2012_8_21$sentiment <= 5.55555] <- "0"
DF_2012_8_21$sentiment[DF_2012_8_21$sentiment > 5.55555] <- "1"

date <- c("2012-08-21")
tots <- as.numeric(nrow(DF_2012_8_21))
negs <- as.numeric(sum(DF_2012_8_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_8_21$sentiment == "1"))
results2012_8_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_8_21 <- cbind(date, tots, poss, negs, results2012_8_21)
results21LF <- rbind(results21LF, results2012_8_21)

rm(DF_2012_8_21_cleanREG, scores, DF_2012_8_21, date, tots, negs, poss, results2012_8_21)

#July

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_7_21.RData")

DF_2012_7_21_cleanREG <- DF_2012_7_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_7_21_cleanREG$text <- as.factor(DF_2012_7_21_cleanREG$text)

scores <- score.sentiment(DF_2012_7_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_7_21 <- scores
colnames(DF_2012_7_21) <- c("text", "sentiment")

DF_2012_7_21$sentiment[DF_2012_7_21$sentiment <= 5.55555] <- "0"
DF_2012_7_21$sentiment[DF_2012_7_21$sentiment > 5.55555] <- "1"

date <- c("2012-07-21")
tots <- as.numeric(nrow(DF_2012_7_21))
negs <- as.numeric(sum(DF_2012_7_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_7_21$sentiment == "1"))
results2012_7_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_7_21 <- cbind(date, tots, poss, negs, results2012_7_21)
results21LF <- rbind(results21LF, results2012_7_21)

rm(DF_2012_7_21_cleanREG, scores, DF_2012_7_21, date, tots, negs, poss, results2012_7_21)

#June

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_6_21.RData")

DF_2012_6_21_cleanREG <- DF_2012_6_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_6_21_cleanREG$text <- as.factor(DF_2012_6_21_cleanREG$text)

scores <- score.sentiment(DF_2012_6_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_6_21 <- scores
colnames(DF_2012_6_21) <- c("text", "sentiment")

DF_2012_6_21$sentiment[DF_2012_6_21$sentiment <= 5.55555] <- "0"
DF_2012_6_21$sentiment[DF_2012_6_21$sentiment > 5.55555] <- "1"

date <- c("2012-06-21")
tots <- as.numeric(nrow(DF_2012_6_21))
negs <- as.numeric(sum(DF_2012_6_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_6_21$sentiment == "1"))
results2012_6_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_6_21 <- cbind(date, tots, poss, negs, results2012_6_21)
results21LF <- rbind(results21LF, results2012_6_21)

rm(DF_2012_6_21_cleanREG, scores, DF_2012_6_21, date, tots, negs, poss, results2012_6_21)

#May

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_5_21.RData")

DF_2012_5_21_cleanREG <- DF_2012_5_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_5_21_cleanREG$text <- as.factor(DF_2012_5_21_cleanREG$text)

scores <- score.sentiment(DF_2012_5_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_5_21 <- scores
colnames(DF_2012_5_21) <- c("text", "sentiment")

DF_2012_5_21$sentiment[DF_2012_5_21$sentiment <= 5.55555] <- "0"
DF_2012_5_21$sentiment[DF_2012_5_21$sentiment > 5.55555] <- "1"

date <- c("2012-05-21")
tots <- as.numeric(nrow(DF_2012_5_21))
negs <- as.numeric(sum(DF_2012_5_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_5_21$sentiment == "1"))
results2012_5_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_5_21 <- cbind(date, tots, poss, negs, results2012_5_21)
results21LF <- rbind(results21LF, results2012_5_21)

rm(DF_2012_5_21_cleanREG, scores, DF_2012_5_21, date, tots, negs, poss, results2012_5_21)

#April

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_4_21.RData")

DF_2012_4_21_cleanREG <- DF_2012_4_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_4_21_cleanREG$text <- as.factor(DF_2012_4_21_cleanREG$text)

scores <- score.sentiment(DF_2012_4_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_4_21 <- scores
colnames(DF_2012_4_21) <- c("text", "sentiment")

DF_2012_4_21$sentiment[DF_2012_4_21$sentiment <= 5.55555] <- "0"
DF_2012_4_21$sentiment[DF_2012_4_21$sentiment > 5.55555] <- "1"

date <- c("2012-04-21")
tots <- as.numeric(nrow(DF_2012_4_21))
negs <- as.numeric(sum(DF_2012_4_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_4_21$sentiment == "1"))
results2012_4_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_4_21 <- cbind(date, tots, poss, negs, results2012_4_21)
results21LF <- rbind(results21LF, results2012_4_21)

rm(DF_2012_4_21_cleanREG, scores, DF_2012_4_21, date, tots, negs, poss, results2012_4_21)

#March

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_3_21.RData")

DF_2012_3_21_cleanREG <- DF_2012_3_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_3_21_cleanREG$text <- as.factor(DF_2012_3_21_cleanREG$text)

scores <- score.sentiment(DF_2012_3_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_3_21 <- scores
colnames(DF_2012_3_21) <- c("text", "sentiment")

DF_2012_3_21$sentiment[DF_2012_3_21$sentiment <= 5.55555] <- "0"
DF_2012_3_21$sentiment[DF_2012_3_21$sentiment > 5.55555] <- "1"

date <- c("2012-03-21")
tots <- as.numeric(nrow(DF_2012_3_21))
negs <- as.numeric(sum(DF_2012_3_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_3_21$sentiment == "1"))
results2012_3_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_3_21 <- cbind(date, tots, poss, negs, results2012_3_21)
results21LF <- rbind(results21LF, results2012_3_21)

rm(DF_2012_3_21_cleanREG, scores, DF_2012_3_21, date, tots, negs, poss, results2012_3_21)

#February

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_2_21.RData")

DF_2012_2_21_cleanREG <- DF_2012_2_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_2_21_cleanREG$text <- as.factor(DF_2012_2_21_cleanREG$text)

scores <- score.sentiment(DF_2012_2_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_2_21 <- scores
colnames(DF_2012_2_21) <- c("text", "sentiment")

DF_2012_2_21$sentiment[DF_2012_2_21$sentiment <= 5.55555] <- "0"
DF_2012_2_21$sentiment[DF_2012_2_21$sentiment > 5.55555] <- "1"

date <- c("2012-02-21")
tots <- as.numeric(nrow(DF_2012_2_21))
negs <- as.numeric(sum(DF_2012_2_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_2_21$sentiment == "1"))
results2012_2_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_2_21 <- cbind(date, tots, poss, negs, results2012_2_21)
results21LF <- rbind(results21LF, results2012_2_21)

rm(DF_2012_2_21_cleanREG, scores, DF_2012_2_21, date, tots, negs, poss, results2012_2_21)

#January

load(file = "Objects/Tweets/Series_21/Clean/REG/DF_2012_1_21.RData")

DF_2012_1_21_cleanREG <- DF_2012_1_21_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_1_21_cleanREG$text <- as.factor(DF_2012_1_21_cleanREG$text)

scores <- score.sentiment(DF_2012_1_21_cleanREG$text, valence, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_1_21 <- scores
colnames(DF_2012_1_21) <- c("text", "sentiment")

DF_2012_1_21$sentiment[DF_2012_1_21$sentiment <= 5.55555] <- "0"
DF_2012_1_21$sentiment[DF_2012_1_21$sentiment > 5.55555] <- "1"

date <- c("2012-01-21")
tots <- as.numeric(nrow(DF_2012_1_21))
negs <- as.numeric(sum(DF_2012_1_21$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_1_21$sentiment == "1"))
results2012_1_21 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_1_21 <- cbind(date, tots, poss, negs, results2012_1_21)
results21LF <- rbind(results21LF, results2012_1_21)

rm(DF_2012_1_21_cleanREG, scores, DF_2012_1_21, date, tots, negs, poss, results2012_1_21)