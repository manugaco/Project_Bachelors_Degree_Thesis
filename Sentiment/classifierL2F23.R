

library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(SnowballC)


score.sentiment <- function(sentences, pos.words, neg.words, .progress='none'){
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}
load(file = "Objects/Models/list.RData")

pos <- scan('Datasets/Words/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
pos <- wordStem(pos, language = "spanish")
pos.words <- unique(pos)
neg <- scan('Datasets/Words/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
neg <- wordStem(neg, language = "spanish")
neg.words <- unique(neg)

#Series 23

print("2017")
#December

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_12_23.RData")

DF_2017_12_23_cleanREG <- DF_2017_12_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_12_23_cleanREG$text <- as.factor(DF_2017_12_23_cleanREG$text)

scores <- score.sentiment(DF_2017_12_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_12_23 <- scores
colnames(DF_2017_12_23) <- c("text", "sentiment")

DF_2017_12_23$sentiment[DF_2017_12_23$sentiment < 0] <- "0"
DF_2017_12_23$sentiment[DF_2017_12_23$sentiment > 0] <- "1"

date <- c("2017-12-23")
tots <- as.numeric(nrow(DF_2017_12_23))
negs <- as.numeric(sum(DF_2017_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_12_23$sentiment == "1"))
results2017_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_12_23 <- cbind(date, tots, poss, negs, results2017_12_23)
results23L2F <- results2017_12_23

rm(DF_2017_12_23_cleanREG, scores, DF_2017_12_23, date, tots, negs, poss, results2017_12_23)

#Nobember

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_11_23.RData")

DF_2017_11_23_cleanREG <- DF_2017_11_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_11_23_cleanREG$text <- as.factor(DF_2017_11_23_cleanREG$text)

scores <- score.sentiment(DF_2017_11_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_11_23 <- scores
colnames(DF_2017_11_23) <- c("text", "sentiment")

DF_2017_11_23$sentiment[DF_2017_11_23$sentiment < 0] <- "0"
DF_2017_11_23$sentiment[DF_2017_11_23$sentiment > 0] <- "1"

date <- c("2017-11-23")
tots <- as.numeric(nrow(DF_2017_11_23))
negs <- as.numeric(sum(DF_2017_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_11_23$sentiment == "1"))
results2017_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_11_23 <- cbind(date, tots, poss, negs, results2017_11_23)
results23L2F <- rbind(results23L2F, results2017_11_23)

rm(DF_2017_11_23_cleanREG, scores, DF_2017_11_23, date, tots, negs, poss, results2017_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_10_23.RData")

DF_2017_10_23_cleanREG <- DF_2017_10_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_10_23_cleanREG$text <- as.factor(DF_2017_10_23_cleanREG$text)

scores <- score.sentiment(DF_2017_10_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_10_23 <- scores
colnames(DF_2017_10_23) <- c("text", "sentiment")

DF_2017_10_23$sentiment[DF_2017_10_23$sentiment < 0] <- "0"
DF_2017_10_23$sentiment[DF_2017_10_23$sentiment > 0] <- "1"

date <- c("2017-10-23")
tots <- as.numeric(nrow(DF_2017_10_23))
negs <- as.numeric(sum(DF_2017_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_10_23$sentiment == "1"))
results2017_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_10_23 <- cbind(date, tots, poss, negs, results2017_10_23)
results23L2F <- rbind(results23L2F, results2017_10_23)

rm(DF_2017_10_23_cleanREG, scores, DF_2017_10_23, date, tots, negs, poss, results2017_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_9_23.RData")

DF_2017_9_23_cleanREG <- DF_2017_9_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_9_23_cleanREG$text <- as.factor(DF_2017_9_23_cleanREG$text)

scores <- score.sentiment(DF_2017_9_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_9_23 <- scores
colnames(DF_2017_9_23) <- c("text", "sentiment")

DF_2017_9_23$sentiment[DF_2017_9_23$sentiment < 0] <- "0"
DF_2017_9_23$sentiment[DF_2017_9_23$sentiment > 0] <- "1"

date <- c("2017-09-23")
tots <- as.numeric(nrow(DF_2017_9_23))
negs <- as.numeric(sum(DF_2017_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_9_23$sentiment == "1"))
results2017_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_9_23 <- cbind(date, tots, poss, negs, results2017_9_23)
results23L2F <- rbind(results23L2F, results2017_9_23)

rm(DF_2017_9_23_cleanREG, scores, DF_2017_9_23, date, tots, negs, poss, results2017_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_8_23.RData")

DF_2017_8_23_cleanREG <- DF_2017_8_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_8_23_cleanREG$text <- as.factor(DF_2017_8_23_cleanREG$text)

scores <- score.sentiment(DF_2017_8_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_8_23 <- scores
colnames(DF_2017_8_23) <- c("text", "sentiment")

DF_2017_8_23$sentiment[DF_2017_8_23$sentiment < 0] <- "0"
DF_2017_8_23$sentiment[DF_2017_8_23$sentiment > 0] <- "1"

date <- c("2017-08-23")
tots <- as.numeric(nrow(DF_2017_8_23))
negs <- as.numeric(sum(DF_2017_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_8_23$sentiment == "1"))
results2017_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_8_23 <- cbind(date, tots, poss, negs, results2017_8_23)
results23L2F <- rbind(results23L2F, results2017_8_23)

rm(DF_2017_8_23_cleanREG, scores, DF_2017_8_23, date, tots, negs, poss, results2017_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_7_23.RData")

DF_2017_7_23_cleanREG <- DF_2017_7_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_7_23_cleanREG$text <- as.factor(DF_2017_7_23_cleanREG$text)

scores <- score.sentiment(DF_2017_7_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_7_23 <- scores
colnames(DF_2017_7_23) <- c("text", "sentiment")

DF_2017_7_23$sentiment[DF_2017_7_23$sentiment < 0] <- "0"
DF_2017_7_23$sentiment[DF_2017_7_23$sentiment > 0] <- "1"

date <- c("2017-07-23")
tots <- as.numeric(nrow(DF_2017_7_23))
negs <- as.numeric(sum(DF_2017_7_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_7_23$sentiment == "1"))
results2017_7_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_7_23 <- cbind(date, tots, poss, negs, results2017_7_23)
results23L2F <- rbind(results23L2F, results2017_7_23)

rm(DF_2017_7_23_cleanREG, scores, DF_2017_7_23, date, tots, negs, poss, results2017_7_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_6_23.RData")

DF_2017_6_23_cleanREG <- DF_2017_6_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_6_23_cleanREG$text <- as.factor(DF_2017_6_23_cleanREG$text)

scores <- score.sentiment(DF_2017_6_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_6_23 <- scores
colnames(DF_2017_6_23) <- c("text", "sentiment")

DF_2017_6_23$sentiment[DF_2017_6_23$sentiment < 0] <- "0"
DF_2017_6_23$sentiment[DF_2017_6_23$sentiment > 0] <- "1"

date <- c("2017-06-23")
tots <- as.numeric(nrow(DF_2017_6_23))
negs <- as.numeric(sum(DF_2017_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_6_23$sentiment == "1"))
results2017_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_6_23 <- cbind(date, tots, poss, negs, results2017_6_23)
results23L2F <- rbind(results23L2F, results2017_6_23)

rm(DF_2017_6_23_cleanREG, scores, DF_2017_6_23, date, tots, negs, poss, results2017_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_5_23.RData")

DF_2017_5_23_cleanREG <- DF_2017_5_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_5_23_cleanREG$text <- as.factor(DF_2017_5_23_cleanREG$text)

scores <- score.sentiment(DF_2017_5_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_5_23 <- scores
colnames(DF_2017_5_23) <- c("text", "sentiment")

DF_2017_5_23$sentiment[DF_2017_5_23$sentiment < 0] <- "0"
DF_2017_5_23$sentiment[DF_2017_5_23$sentiment > 0] <- "1"

date <- c("2017-05-23")
tots <- as.numeric(nrow(DF_2017_5_23))
negs <- as.numeric(sum(DF_2017_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_5_23$sentiment == "1"))
results2017_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_5_23 <- cbind(date, tots, poss, negs, results2017_5_23)
results23L2F <- rbind(results23L2F, results2017_5_23)

rm(DF_2017_5_23_cleanREG, scores, DF_2017_5_23, date, tots, negs, poss, results2017_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_4_23.RData")

DF_2017_4_23_cleanREG <- DF_2017_4_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_4_23_cleanREG$text <- as.factor(DF_2017_4_23_cleanREG$text)

scores <- score.sentiment(DF_2017_4_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_4_23 <- scores
colnames(DF_2017_4_23) <- c("text", "sentiment")

DF_2017_4_23$sentiment[DF_2017_4_23$sentiment < 0] <- "0"
DF_2017_4_23$sentiment[DF_2017_4_23$sentiment > 0] <- "1"

date <- c("2017-04-23")
tots <- as.numeric(nrow(DF_2017_4_23))
negs <- as.numeric(sum(DF_2017_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_4_23$sentiment == "1"))
results2017_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_4_23 <- cbind(date, tots, poss, negs, results2017_4_23)
results23L2F <- rbind(results23L2F, results2017_4_23)

rm(DF_2017_4_23_cleanREG, scores, DF_2017_4_23, date, tots, negs, poss, results2017_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_3_23.RData")

DF_2017_3_23_cleanREG <- DF_2017_3_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_3_23_cleanREG$text <- as.factor(DF_2017_3_23_cleanREG$text)

scores <- score.sentiment(DF_2017_3_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_3_23 <- scores
colnames(DF_2017_3_23) <- c("text", "sentiment")

DF_2017_3_23$sentiment[DF_2017_3_23$sentiment < 0] <- "0"
DF_2017_3_23$sentiment[DF_2017_3_23$sentiment > 0] <- "1"

date <- c("2017-03-23")
tots <- as.numeric(nrow(DF_2017_3_23))
negs <- as.numeric(sum(DF_2017_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_3_23$sentiment == "1"))
results2017_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_3_23 <- cbind(date, tots, poss, negs, results2017_3_23)
results23L2F <- rbind(results23L2F, results2017_3_23)

rm(DF_2017_3_23_cleanREG, scores, DF_2017_3_23, date, tots, negs, poss, results2017_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_2_23.RData")

DF_2017_2_23_cleanREG <- DF_2017_2_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_2_23_cleanREG$text <- as.factor(DF_2017_2_23_cleanREG$text)

scores <- score.sentiment(DF_2017_2_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_2_23 <- scores
colnames(DF_2017_2_23) <- c("text", "sentiment")

DF_2017_2_23$sentiment[DF_2017_2_23$sentiment < 0] <- "0"
DF_2017_2_23$sentiment[DF_2017_2_23$sentiment > 0] <- "1"

date <- c("2017-02-23")
tots <- as.numeric(nrow(DF_2017_2_23))
negs <- as.numeric(sum(DF_2017_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_2_23$sentiment == "1"))
results2017_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_2_23 <- cbind(date, tots, poss, negs, results2017_2_23)
results23L2F <- rbind(results23L2F, results2017_2_23)

rm(DF_2017_2_23_cleanREG, scores, DF_2017_2_23, date, tots, negs, poss, results2017_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2017_1_23.RData")

DF_2017_1_23_cleanREG <- DF_2017_1_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2017_1_23_cleanREG$text <- as.factor(DF_2017_1_23_cleanREG$text)

scores <- score.sentiment(DF_2017_1_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2017_1_23 <- scores
colnames(DF_2017_1_23) <- c("text", "sentiment")

DF_2017_1_23$sentiment[DF_2017_1_23$sentiment < 0] <- "0"
DF_2017_1_23$sentiment[DF_2017_1_23$sentiment > 0] <- "1"

date <- c("2017-01-23")
tots <- as.numeric(nrow(DF_2017_1_23))
negs <- as.numeric(sum(DF_2017_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_1_23$sentiment == "1"))
results2017_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_1_23 <- cbind(date, tots, poss, negs, results2017_1_23)
results23L2F <- rbind(results23L2F, results2017_1_23)

rm(DF_2017_1_23_cleanREG, scores, DF_2017_1_23, date, tots, negs, poss, results2017_1_23)


print("2016")
#December

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_12_23.RData")

DF_2016_12_23_cleanREG <- DF_2016_12_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_12_23_cleanREG$text <- as.factor(DF_2016_12_23_cleanREG$text)

scores <- score.sentiment(DF_2016_12_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_12_23 <- scores
colnames(DF_2016_12_23) <- c("text", "sentiment")

DF_2016_12_23$sentiment[DF_2016_12_23$sentiment < 0] <- "0"
DF_2016_12_23$sentiment[DF_2016_12_23$sentiment > 0] <- "1"

date <- c("2016-12-23")
tots <- as.numeric(nrow(DF_2016_12_23))
negs <- as.numeric(sum(DF_2016_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_12_23$sentiment == "1"))
results2016_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_12_23 <- cbind(date, tots, poss, negs, results2016_12_23)
results23L2F <- rbind(results23L2F, results2016_12_23)

rm(DF_2016_12_23_cleanREG, scores, DF_2016_12_23, date, tots, negs, poss, results2016_12_23)

#Nobember

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_11_23.RData")

DF_2016_11_23_cleanREG <- DF_2016_11_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_11_23_cleanREG$text <- as.factor(DF_2016_11_23_cleanREG$text)

scores <- score.sentiment(DF_2016_11_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_11_23 <- scores
colnames(DF_2016_11_23) <- c("text", "sentiment")

DF_2016_11_23$sentiment[DF_2016_11_23$sentiment < 0] <- "0"
DF_2016_11_23$sentiment[DF_2016_11_23$sentiment > 0] <- "1"

date <- c("2016-11-23")
tots <- as.numeric(nrow(DF_2016_11_23))
negs <- as.numeric(sum(DF_2016_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_11_23$sentiment == "1"))
results2016_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_11_23 <- cbind(date, tots, poss, negs, results2016_11_23)
results23L2F <- rbind(results23L2F, results2016_11_23)

rm(DF_2016_11_23_cleanREG, scores, DF_2016_11_23, date, tots, negs, poss, results2016_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_10_23.RData")

DF_2016_10_23_cleanREG <- DF_2016_10_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_10_23_cleanREG$text <- as.factor(DF_2016_10_23_cleanREG$text)

scores <- score.sentiment(DF_2016_10_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_10_23 <- scores
colnames(DF_2016_10_23) <- c("text", "sentiment")

DF_2016_10_23$sentiment[DF_2016_10_23$sentiment < 0] <- "0"
DF_2016_10_23$sentiment[DF_2016_10_23$sentiment > 0] <- "1"

date <- c("2016-10-23")
tots <- as.numeric(nrow(DF_2016_10_23))
negs <- as.numeric(sum(DF_2016_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_10_23$sentiment == "1"))
results2016_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_10_23 <- cbind(date, tots, poss, negs, results2016_10_23)
results23L2F <- rbind(results23L2F, results2016_10_23)

rm(DF_2016_10_23_cleanREG, scores, DF_2016_10_23, date, tots, negs, poss, results2016_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_9_23.RData")

DF_2016_9_23_cleanREG <- DF_2016_9_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_9_23_cleanREG$text <- as.factor(DF_2016_9_23_cleanREG$text)

scores <- score.sentiment(DF_2016_9_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_9_23 <- scores
colnames(DF_2016_9_23) <- c("text", "sentiment")

DF_2016_9_23$sentiment[DF_2016_9_23$sentiment < 0] <- "0"
DF_2016_9_23$sentiment[DF_2016_9_23$sentiment > 0] <- "1"

date <- c("2016-09-23")
tots <- as.numeric(nrow(DF_2016_9_23))
negs <- as.numeric(sum(DF_2016_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_9_23$sentiment == "1"))
results2016_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_9_23 <- cbind(date, tots, poss, negs, results2016_9_23)
results23L2F <- rbind(results23L2F, results2016_9_23)

rm(DF_2016_9_23_cleanREG, scores, DF_2016_9_23, date, tots, negs, poss, results2016_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_8_23.RData")

DF_2016_8_23_cleanREG <- DF_2016_8_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_8_23_cleanREG$text <- as.factor(DF_2016_8_23_cleanREG$text)

scores <- score.sentiment(DF_2016_8_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_8_23 <- scores
colnames(DF_2016_8_23) <- c("text", "sentiment")

DF_2016_8_23$sentiment[DF_2016_8_23$sentiment < 0] <- "0"
DF_2016_8_23$sentiment[DF_2016_8_23$sentiment > 0] <- "1"

date <- c("2016-08-23")
tots <- as.numeric(nrow(DF_2016_8_23))
negs <- as.numeric(sum(DF_2016_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_8_23$sentiment == "1"))
results2016_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_8_23 <- cbind(date, tots, poss, negs, results2016_8_23)
results23L2F <- rbind(results23L2F, results2016_8_23)

rm(DF_2016_8_23_cleanREG, scores, DF_2016_8_23, date, tots, negs, poss, results2016_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_7_23.RData")

DF_2016_7_23_cleanREG <- DF_2016_7_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_7_23_cleanREG$text <- as.factor(DF_2016_7_23_cleanREG$text)

scores <- score.sentiment(DF_2016_7_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_7_23 <- scores
colnames(DF_2016_7_23) <- c("text", "sentiment")

DF_2016_7_23$sentiment[DF_2016_7_23$sentiment < 0] <- "0"
DF_2016_7_23$sentiment[DF_2016_7_23$sentiment > 0] <- "1"

date <- c("2016-07-23")
tots <- as.numeric(nrow(DF_2016_7_23))
negs <- as.numeric(sum(DF_2016_7_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_7_23$sentiment == "1"))
results2016_7_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_7_23 <- cbind(date, tots, poss, negs, results2016_7_23)
results23L2F <- rbind(results23L2F, results2016_7_23)

rm(DF_2016_7_23_cleanREG, scores, DF_2016_7_23, date, tots, negs, poss, results2016_7_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_6_23.RData")

DF_2016_6_23_cleanREG <- DF_2016_6_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_6_23_cleanREG$text <- as.factor(DF_2016_6_23_cleanREG$text)

scores <- score.sentiment(DF_2016_6_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_6_23 <- scores
colnames(DF_2016_6_23) <- c("text", "sentiment")

DF_2016_6_23$sentiment[DF_2016_6_23$sentiment < 0] <- "0"
DF_2016_6_23$sentiment[DF_2016_6_23$sentiment > 0] <- "1"

date <- c("2016-06-23")
tots <- as.numeric(nrow(DF_2016_6_23))
negs <- as.numeric(sum(DF_2016_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_6_23$sentiment == "1"))
results2016_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_6_23 <- cbind(date, tots, poss, negs, results2016_6_23)
results23L2F <- rbind(results23L2F, results2016_6_23)

rm(DF_2016_6_23_cleanREG, scores, DF_2016_6_23, date, tots, negs, poss, results2016_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_5_23.RData")

DF_2016_5_23_cleanREG <- DF_2016_5_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_5_23_cleanREG$text <- as.factor(DF_2016_5_23_cleanREG$text)

scores <- score.sentiment(DF_2016_5_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_5_23 <- scores
colnames(DF_2016_5_23) <- c("text", "sentiment")

DF_2016_5_23$sentiment[DF_2016_5_23$sentiment < 0] <- "0"
DF_2016_5_23$sentiment[DF_2016_5_23$sentiment > 0] <- "1"

date <- c("2016-05-23")
tots <- as.numeric(nrow(DF_2016_5_23))
negs <- as.numeric(sum(DF_2016_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_5_23$sentiment == "1"))
results2016_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_5_23 <- cbind(date, tots, poss, negs, results2016_5_23)
results23L2F <- rbind(results23L2F, results2016_5_23)

rm(DF_2016_5_23_cleanREG, scores, DF_2016_5_23, date, tots, negs, poss, results2016_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_4_23.RData")

DF_2016_4_23_cleanREG <- DF_2016_4_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_4_23_cleanREG$text <- as.factor(DF_2016_4_23_cleanREG$text)

scores <- score.sentiment(DF_2016_4_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_4_23 <- scores
colnames(DF_2016_4_23) <- c("text", "sentiment")

DF_2016_4_23$sentiment[DF_2016_4_23$sentiment < 0] <- "0"
DF_2016_4_23$sentiment[DF_2016_4_23$sentiment > 0] <- "1"

date <- c("2016-04-23")
tots <- as.numeric(nrow(DF_2016_4_23))
negs <- as.numeric(sum(DF_2016_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_4_23$sentiment == "1"))
results2016_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_4_23 <- cbind(date, tots, poss, negs, results2016_4_23)
results23L2F <- rbind(results23L2F, results2016_4_23)

rm(DF_2016_4_23_cleanREG, scores, DF_2016_4_23, date, tots, negs, poss, results2016_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_3_23.RData")

DF_2016_3_23_cleanREG <- DF_2016_3_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_3_23_cleanREG$text <- as.factor(DF_2016_3_23_cleanREG$text)

scores <- score.sentiment(DF_2016_3_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_3_23 <- scores
colnames(DF_2016_3_23) <- c("text", "sentiment")

DF_2016_3_23$sentiment[DF_2016_3_23$sentiment < 0] <- "0"
DF_2016_3_23$sentiment[DF_2016_3_23$sentiment > 0] <- "1"

date <- c("2016-03-23")
tots <- as.numeric(nrow(DF_2016_3_23))
negs <- as.numeric(sum(DF_2016_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_3_23$sentiment == "1"))
results2016_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_3_23 <- cbind(date, tots, poss, negs, results2016_3_23)
results23L2F <- rbind(results23L2F, results2016_3_23)

rm(DF_2016_3_23_cleanREG, scores, DF_2016_3_23, date, tots, negs, poss, results2016_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_2_23.RData")

DF_2016_2_23_cleanREG <- DF_2016_2_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_2_23_cleanREG$text <- as.factor(DF_2016_2_23_cleanREG$text)

scores <- score.sentiment(DF_2016_2_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_2_23 <- scores
colnames(DF_2016_2_23) <- c("text", "sentiment")

DF_2016_2_23$sentiment[DF_2016_2_23$sentiment < 0] <- "0"
DF_2016_2_23$sentiment[DF_2016_2_23$sentiment > 0] <- "1"

date <- c("2016-02-23")
tots <- as.numeric(nrow(DF_2016_2_23))
negs <- as.numeric(sum(DF_2016_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_2_23$sentiment == "1"))
results2016_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_2_23 <- cbind(date, tots, poss, negs, results2016_2_23)
results23L2F <- rbind(results23L2F, results2016_2_23)

rm(DF_2016_2_23_cleanREG, scores, DF_2016_2_23, date, tots, negs, poss, results2016_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2016_1_23.RData")

DF_2016_1_23_cleanREG <- DF_2016_1_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2016_1_23_cleanREG$text <- as.factor(DF_2016_1_23_cleanREG$text)

scores <- score.sentiment(DF_2016_1_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2016_1_23 <- scores
colnames(DF_2016_1_23) <- c("text", "sentiment")

DF_2016_1_23$sentiment[DF_2016_1_23$sentiment < 0] <- "0"
DF_2016_1_23$sentiment[DF_2016_1_23$sentiment > 0] <- "1"

date <- c("2016-01-23")
tots <- as.numeric(nrow(DF_2016_1_23))
negs <- as.numeric(sum(DF_2016_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_1_23$sentiment == "1"))
results2016_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_1_23 <- cbind(date, tots, poss, negs, results2016_1_23)
results23L2F <- rbind(results23L2F, results2016_1_23)

rm(DF_2016_1_23_cleanREG, scores, DF_2016_1_23, date, tots, negs, poss, results2016_1_23)


print("2015")
#December

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_12_23.RData")

DF_2015_12_23_cleanREG <- DF_2015_12_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_12_23_cleanREG$text <- as.factor(DF_2015_12_23_cleanREG$text)

scores <- score.sentiment(DF_2015_12_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_12_23 <- scores
colnames(DF_2015_12_23) <- c("text", "sentiment")

DF_2015_12_23$sentiment[DF_2015_12_23$sentiment < 0] <- "0"
DF_2015_12_23$sentiment[DF_2015_12_23$sentiment > 0] <- "1"

date <- c("2015-12-23")
tots <- as.numeric(nrow(DF_2015_12_23))
negs <- as.numeric(sum(DF_2015_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_12_23$sentiment == "1"))
results2015_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_12_23 <- cbind(date, tots, poss, negs, results2015_12_23)
results23L2F <- rbind(results23L2F, results2015_12_23)

rm(DF_2015_12_23_cleanREG, scores, DF_2015_12_23, date, tots, negs, poss, results2015_12_23)

#Nobember

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_11_23.RData")

DF_2015_11_23_cleanREG <- DF_2015_11_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_11_23_cleanREG$text <- as.factor(DF_2015_11_23_cleanREG$text)

scores <- score.sentiment(DF_2015_11_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_11_23 <- scores
colnames(DF_2015_11_23) <- c("text", "sentiment")

DF_2015_11_23$sentiment[DF_2015_11_23$sentiment < 0] <- "0"
DF_2015_11_23$sentiment[DF_2015_11_23$sentiment > 0] <- "1"

date <- c("2015-11-23")
tots <- as.numeric(nrow(DF_2015_11_23))
negs <- as.numeric(sum(DF_2015_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_11_23$sentiment == "1"))
results2015_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_11_23 <- cbind(date, tots, poss, negs, results2015_11_23)
results23L2F <- rbind(results23L2F, results2015_11_23)

rm(DF_2015_11_23_cleanREG, scores, DF_2015_11_23, date, tots, negs, poss, results2015_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_10_23.RData")

DF_2015_10_23_cleanREG <- DF_2015_10_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_10_23_cleanREG$text <- as.factor(DF_2015_10_23_cleanREG$text)

scores <- score.sentiment(DF_2015_10_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_10_23 <- scores
colnames(DF_2015_10_23) <- c("text", "sentiment")

DF_2015_10_23$sentiment[DF_2015_10_23$sentiment < 0] <- "0"
DF_2015_10_23$sentiment[DF_2015_10_23$sentiment > 0] <- "1"

date <- c("2015-10-23")
tots <- as.numeric(nrow(DF_2015_10_23))
negs <- as.numeric(sum(DF_2015_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_10_23$sentiment == "1"))
results2015_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_10_23 <- cbind(date, tots, poss, negs, results2015_10_23)
results23L2F <- rbind(results23L2F, results2015_10_23)

rm(DF_2015_10_23_cleanREG, scores, DF_2015_10_23, date, tots, negs, poss, results2015_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_9_23.RData")

DF_2015_9_23_cleanREG <- DF_2015_9_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_9_23_cleanREG$text <- as.factor(DF_2015_9_23_cleanREG$text)

scores <- score.sentiment(DF_2015_9_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_9_23 <- scores
colnames(DF_2015_9_23) <- c("text", "sentiment")

DF_2015_9_23$sentiment[DF_2015_9_23$sentiment < 0] <- "0"
DF_2015_9_23$sentiment[DF_2015_9_23$sentiment > 0] <- "1"

date <- c("2015-09-23")
tots <- as.numeric(nrow(DF_2015_9_23))
negs <- as.numeric(sum(DF_2015_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_9_23$sentiment == "1"))
results2015_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_9_23 <- cbind(date, tots, poss, negs, results2015_9_23)
results23L2F <- rbind(results23L2F, results2015_9_23)

rm(DF_2015_9_23_cleanREG, scores, DF_2015_9_23, date, tots, negs, poss, results2015_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_8_23.RData")

DF_2015_8_23_cleanREG <- DF_2015_8_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_8_23_cleanREG$text <- as.factor(DF_2015_8_23_cleanREG$text)

scores <- score.sentiment(DF_2015_8_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_8_23 <- scores
colnames(DF_2015_8_23) <- c("text", "sentiment")

DF_2015_8_23$sentiment[DF_2015_8_23$sentiment < 0] <- "0"
DF_2015_8_23$sentiment[DF_2015_8_23$sentiment > 0] <- "1"

date <- c("2015-08-23")
tots <- as.numeric(nrow(DF_2015_8_23))
negs <- as.numeric(sum(DF_2015_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_8_23$sentiment == "1"))
results2015_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_8_23 <- cbind(date, tots, poss, negs, results2015_8_23)
results23L2F <- rbind(results23L2F, results2015_8_23)

rm(DF_2015_8_23_cleanREG, scores, DF_2015_8_23, date, tots, negs, poss, results2015_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_7_23.RData")

DF_2015_7_23_cleanREG <- DF_2015_7_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_7_23_cleanREG$text <- as.factor(DF_2015_7_23_cleanREG$text)

scores <- score.sentiment(DF_2015_7_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_7_23 <- scores
colnames(DF_2015_7_23) <- c("text", "sentiment")

DF_2015_7_23$sentiment[DF_2015_7_23$sentiment < 0] <- "0"
DF_2015_7_23$sentiment[DF_2015_7_23$sentiment > 0] <- "1"

date <- c("2015-07-23")
tots <- as.numeric(nrow(DF_2015_7_23))
negs <- as.numeric(sum(DF_2015_7_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_7_23$sentiment == "1"))
results2015_7_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_7_23 <- cbind(date, tots, poss, negs, results2015_7_23)
results23L2F <- rbind(results23L2F, results2015_7_23)

rm(DF_2015_7_23_cleanREG, scores, DF_2015_7_23, date, tots, negs, poss, results2015_7_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_6_23.RData")

DF_2015_6_23_cleanREG <- DF_2015_6_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_6_23_cleanREG$text <- as.factor(DF_2015_6_23_cleanREG$text)

scores <- score.sentiment(DF_2015_6_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_6_23 <- scores
colnames(DF_2015_6_23) <- c("text", "sentiment")

DF_2015_6_23$sentiment[DF_2015_6_23$sentiment < 0] <- "0"
DF_2015_6_23$sentiment[DF_2015_6_23$sentiment > 0] <- "1"

date <- c("2015-06-23")
tots <- as.numeric(nrow(DF_2015_6_23))
negs <- as.numeric(sum(DF_2015_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_6_23$sentiment == "1"))
results2015_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_6_23 <- cbind(date, tots, poss, negs, results2015_6_23)
results23L2F <- rbind(results23L2F, results2015_6_23)

rm(DF_2015_6_23_cleanREG, scores, DF_2015_6_23, date, tots, negs, poss, results2015_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_5_23.RData")

DF_2015_5_23_cleanREG <- DF_2015_5_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_5_23_cleanREG$text <- as.factor(DF_2015_5_23_cleanREG$text)

scores <- score.sentiment(DF_2015_5_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_5_23 <- scores
colnames(DF_2015_5_23) <- c("text", "sentiment")

DF_2015_5_23$sentiment[DF_2015_5_23$sentiment < 0] <- "0"
DF_2015_5_23$sentiment[DF_2015_5_23$sentiment > 0] <- "1"

date <- c("2015-05-23")
tots <- as.numeric(nrow(DF_2015_5_23))
negs <- as.numeric(sum(DF_2015_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_5_23$sentiment == "1"))
results2015_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_5_23 <- cbind(date, tots, poss, negs, results2015_5_23)
results23L2F <- rbind(results23L2F, results2015_5_23)

rm(DF_2015_5_23_cleanREG, scores, DF_2015_5_23, date, tots, negs, poss, results2015_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_4_23.RData")

DF_2015_4_23_cleanREG <- DF_2015_4_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_4_23_cleanREG$text <- as.factor(DF_2015_4_23_cleanREG$text)

scores <- score.sentiment(DF_2015_4_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_4_23 <- scores
colnames(DF_2015_4_23) <- c("text", "sentiment")

DF_2015_4_23$sentiment[DF_2015_4_23$sentiment < 0] <- "0"
DF_2015_4_23$sentiment[DF_2015_4_23$sentiment > 0] <- "1"

date <- c("2015-04-23")
tots <- as.numeric(nrow(DF_2015_4_23))
negs <- as.numeric(sum(DF_2015_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_4_23$sentiment == "1"))
results2015_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_4_23 <- cbind(date, tots, poss, negs, results2015_4_23)
results23L2F <- rbind(results23L2F, results2015_4_23)

rm(DF_2015_4_23_cleanREG, scores, DF_2015_4_23, date, tots, negs, poss, results2015_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_3_23.RData")

DF_2015_3_23_cleanREG <- DF_2015_3_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_3_23_cleanREG$text <- as.factor(DF_2015_3_23_cleanREG$text)

scores <- score.sentiment(DF_2015_3_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_3_23 <- scores
colnames(DF_2015_3_23) <- c("text", "sentiment")

DF_2015_3_23$sentiment[DF_2015_3_23$sentiment < 0] <- "0"
DF_2015_3_23$sentiment[DF_2015_3_23$sentiment > 0] <- "1"

date <- c("2015-03-23")
tots <- as.numeric(nrow(DF_2015_3_23))
negs <- as.numeric(sum(DF_2015_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_3_23$sentiment == "1"))
results2015_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_3_23 <- cbind(date, tots, poss, negs, results2015_3_23)
results23L2F <- rbind(results23L2F, results2015_3_23)

rm(DF_2015_3_23_cleanREG, scores, DF_2015_3_23, date, tots, negs, poss, results2015_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_2_23.RData")

DF_2015_2_23_cleanREG <- DF_2015_2_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_2_23_cleanREG$text <- as.factor(DF_2015_2_23_cleanREG$text)

scores <- score.sentiment(DF_2015_2_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_2_23 <- scores
colnames(DF_2015_2_23) <- c("text", "sentiment")

DF_2015_2_23$sentiment[DF_2015_2_23$sentiment < 0] <- "0"
DF_2015_2_23$sentiment[DF_2015_2_23$sentiment > 0] <- "1"

date <- c("2015-02-23")
tots <- as.numeric(nrow(DF_2015_2_23))
negs <- as.numeric(sum(DF_2015_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_2_23$sentiment == "1"))
results2015_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_2_23 <- cbind(date, tots, poss, negs, results2015_2_23)
results23L2F <- rbind(results23L2F, results2015_2_23)

rm(DF_2015_2_23_cleanREG, scores, DF_2015_2_23, date, tots, negs, poss, results2015_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2015_1_23.RData")

DF_2015_1_23_cleanREG <- DF_2015_1_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2015_1_23_cleanREG$text <- as.factor(DF_2015_1_23_cleanREG$text)

scores <- score.sentiment(DF_2015_1_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2015_1_23 <- scores
colnames(DF_2015_1_23) <- c("text", "sentiment")

DF_2015_1_23$sentiment[DF_2015_1_23$sentiment < 0] <- "0"
DF_2015_1_23$sentiment[DF_2015_1_23$sentiment > 0] <- "1"

date <- c("2015-01-23")
tots <- as.numeric(nrow(DF_2015_1_23))
negs <- as.numeric(sum(DF_2015_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_1_23$sentiment == "1"))
results2015_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_1_23 <- cbind(date, tots, poss, negs, results2015_1_23)
results23L2F <- rbind(results23L2F, results2015_1_23)

rm(DF_2015_1_23_cleanREG, scores, DF_2015_1_23, date, tots, negs, poss, results2015_1_23)


print("2014")
#December

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_12_23.RData")

DF_2014_12_23_cleanREG <- DF_2014_12_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_12_23_cleanREG$text <- as.factor(DF_2014_12_23_cleanREG$text)

scores <- score.sentiment(DF_2014_12_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_12_23 <- scores
colnames(DF_2014_12_23) <- c("text", "sentiment")

DF_2014_12_23$sentiment[DF_2014_12_23$sentiment < 0] <- "0"
DF_2014_12_23$sentiment[DF_2014_12_23$sentiment > 0] <- "1"

date <- c("2014-12-23")
tots <- as.numeric(nrow(DF_2014_12_23))
negs <- as.numeric(sum(DF_2014_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_12_23$sentiment == "1"))
results2014_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_12_23 <- cbind(date, tots, poss, negs, results2014_12_23)
results23L2F <- rbind(results23L2F, results2014_12_23)

rm(DF_2014_12_23_cleanREG, scores, DF_2014_12_23, date, tots, negs, poss, results2014_12_23)

#Nobember

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_11_23.RData")

DF_2014_11_23_cleanREG <- DF_2014_11_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_11_23_cleanREG$text <- as.factor(DF_2014_11_23_cleanREG$text)

scores <- score.sentiment(DF_2014_11_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_11_23 <- scores
colnames(DF_2014_11_23) <- c("text", "sentiment")

DF_2014_11_23$sentiment[DF_2014_11_23$sentiment < 0] <- "0"
DF_2014_11_23$sentiment[DF_2014_11_23$sentiment > 0] <- "1"

date <- c("2014-11-23")
tots <- as.numeric(nrow(DF_2014_11_23))
negs <- as.numeric(sum(DF_2014_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_11_23$sentiment == "1"))
results2014_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_11_23 <- cbind(date, tots, poss, negs, results2014_11_23)
results23L2F <- rbind(results23L2F, results2014_11_23)

rm(DF_2014_11_23_cleanREG, scores, DF_2014_11_23, date, tots, negs, poss, results2014_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_10_23.RData")

DF_2014_10_23_cleanREG <- DF_2014_10_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_10_23_cleanREG$text <- as.factor(DF_2014_10_23_cleanREG$text)

scores <- score.sentiment(DF_2014_10_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_10_23 <- scores
colnames(DF_2014_10_23) <- c("text", "sentiment")

DF_2014_10_23$sentiment[DF_2014_10_23$sentiment < 0] <- "0"
DF_2014_10_23$sentiment[DF_2014_10_23$sentiment > 0] <- "1"

date <- c("2014-10-23")
tots <- as.numeric(nrow(DF_2014_10_23))
negs <- as.numeric(sum(DF_2014_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_10_23$sentiment == "1"))
results2014_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_10_23 <- cbind(date, tots, poss, negs, results2014_10_23)
results23L2F <- rbind(results23L2F, results2014_10_23)

rm(DF_2014_10_23_cleanREG, scores, DF_2014_10_23, date, tots, negs, poss, results2014_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_9_23.RData")

DF_2014_9_23_cleanREG <- DF_2014_9_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_9_23_cleanREG$text <- as.factor(DF_2014_9_23_cleanREG$text)

scores <- score.sentiment(DF_2014_9_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_9_23 <- scores
colnames(DF_2014_9_23) <- c("text", "sentiment")

DF_2014_9_23$sentiment[DF_2014_9_23$sentiment < 0] <- "0"
DF_2014_9_23$sentiment[DF_2014_9_23$sentiment > 0] <- "1"

date <- c("2014-09-23")
tots <- as.numeric(nrow(DF_2014_9_23))
negs <- as.numeric(sum(DF_2014_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_9_23$sentiment == "1"))
results2014_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_9_23 <- cbind(date, tots, poss, negs, results2014_9_23)
results23L2F <- rbind(results23L2F, results2014_9_23)

rm(DF_2014_9_23_cleanREG, scores, DF_2014_9_23, date, tots, negs, poss, results2014_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_8_23.RData")

DF_2014_8_23_cleanREG <- DF_2014_8_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_8_23_cleanREG$text <- as.factor(DF_2014_8_23_cleanREG$text)

scores <- score.sentiment(DF_2014_8_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_8_23 <- scores
colnames(DF_2014_8_23) <- c("text", "sentiment")

DF_2014_8_23$sentiment[DF_2014_8_23$sentiment < 0] <- "0"
DF_2014_8_23$sentiment[DF_2014_8_23$sentiment > 0] <- "1"

date <- c("2014-08-23")
tots <- as.numeric(nrow(DF_2014_8_23))
negs <- as.numeric(sum(DF_2014_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_8_23$sentiment == "1"))
results2014_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_8_23 <- cbind(date, tots, poss, negs, results2014_8_23)
results23L2F <- rbind(results23L2F, results2014_8_23)

rm(DF_2014_8_23_cleanREG, scores, DF_2014_8_23, date, tots, negs, poss, results2014_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_7_23.RData")

DF_2014_7_23_cleanREG <- DF_2014_7_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_7_23_cleanREG$text <- as.factor(DF_2014_7_23_cleanREG$text)

scores <- score.sentiment(DF_2014_7_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_7_23 <- scores
colnames(DF_2014_7_23) <- c("text", "sentiment")

DF_2014_7_23$sentiment[DF_2014_7_23$sentiment < 0] <- "0"
DF_2014_7_23$sentiment[DF_2014_7_23$sentiment > 0] <- "1"

date <- c("2014-07-23")
tots <- as.numeric(nrow(DF_2014_7_23))
negs <- as.numeric(sum(DF_2014_7_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_7_23$sentiment == "1"))
results2014_7_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_7_23 <- cbind(date, tots, poss, negs, results2014_7_23)
results23L2F <- rbind(results23L2F, results2014_7_23)

rm(DF_2014_7_23_cleanREG, scores, DF_2014_7_23, date, tots, negs, poss, results2014_7_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_6_23.RData")

DF_2014_6_23_cleanREG <- DF_2014_6_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_6_23_cleanREG$text <- as.factor(DF_2014_6_23_cleanREG$text)

scores <- score.sentiment(DF_2014_6_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_6_23 <- scores
colnames(DF_2014_6_23) <- c("text", "sentiment")

DF_2014_6_23$sentiment[DF_2014_6_23$sentiment < 0] <- "0"
DF_2014_6_23$sentiment[DF_2014_6_23$sentiment > 0] <- "1"

date <- c("2014-06-23")
tots <- as.numeric(nrow(DF_2014_6_23))
negs <- as.numeric(sum(DF_2014_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_6_23$sentiment == "1"))
results2014_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_6_23 <- cbind(date, tots, poss, negs, results2014_6_23)
results23L2F <- rbind(results23L2F, results2014_6_23)

rm(DF_2014_6_23_cleanREG, scores, DF_2014_6_23, date, tots, negs, poss, results2014_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_5_23.RData")

DF_2014_5_23_cleanREG <- DF_2014_5_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_5_23_cleanREG$text <- as.factor(DF_2014_5_23_cleanREG$text)

scores <- score.sentiment(DF_2014_5_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_5_23 <- scores
colnames(DF_2014_5_23) <- c("text", "sentiment")

DF_2014_5_23$sentiment[DF_2014_5_23$sentiment < 0] <- "0"
DF_2014_5_23$sentiment[DF_2014_5_23$sentiment > 0] <- "1"

date <- c("2014-05-23")
tots <- as.numeric(nrow(DF_2014_5_23))
negs <- as.numeric(sum(DF_2014_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_5_23$sentiment == "1"))
results2014_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_5_23 <- cbind(date, tots, poss, negs, results2014_5_23)
results23L2F <- rbind(results23L2F, results2014_5_23)

rm(DF_2014_5_23_cleanREG, scores, DF_2014_5_23, date, tots, negs, poss, results2014_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_4_23.RData")

DF_2014_4_23_cleanREG <- DF_2014_4_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_4_23_cleanREG$text <- as.factor(DF_2014_4_23_cleanREG$text)

scores <- score.sentiment(DF_2014_4_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_4_23 <- scores
colnames(DF_2014_4_23) <- c("text", "sentiment")

DF_2014_4_23$sentiment[DF_2014_4_23$sentiment < 0] <- "0"
DF_2014_4_23$sentiment[DF_2014_4_23$sentiment > 0] <- "1"

date <- c("2014-04-23")
tots <- as.numeric(nrow(DF_2014_4_23))
negs <- as.numeric(sum(DF_2014_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_4_23$sentiment == "1"))
results2014_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_4_23 <- cbind(date, tots, poss, negs, results2014_4_23)
results23L2F <- rbind(results23L2F, results2014_4_23)

rm(DF_2014_4_23_cleanREG, scores, DF_2014_4_23, date, tots, negs, poss, results2014_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_3_23.RData")

DF_2014_3_23_cleanREG <- DF_2014_3_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_3_23_cleanREG$text <- as.factor(DF_2014_3_23_cleanREG$text)

scores <- score.sentiment(DF_2014_3_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_3_23 <- scores
colnames(DF_2014_3_23) <- c("text", "sentiment")

DF_2014_3_23$sentiment[DF_2014_3_23$sentiment < 0] <- "0"
DF_2014_3_23$sentiment[DF_2014_3_23$sentiment > 0] <- "1"

date <- c("2014-03-23")
tots <- as.numeric(nrow(DF_2014_3_23))
negs <- as.numeric(sum(DF_2014_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_3_23$sentiment == "1"))
results2014_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_3_23 <- cbind(date, tots, poss, negs, results2014_3_23)
results23L2F <- rbind(results23L2F, results2014_3_23)

rm(DF_2014_3_23_cleanREG, scores, DF_2014_3_23, date, tots, negs, poss, results2014_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_2_23.RData")

DF_2014_2_23_cleanREG <- DF_2014_2_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_2_23_cleanREG$text <- as.factor(DF_2014_2_23_cleanREG$text)

scores <- score.sentiment(DF_2014_2_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_2_23 <- scores
colnames(DF_2014_2_23) <- c("text", "sentiment")

DF_2014_2_23$sentiment[DF_2014_2_23$sentiment < 0] <- "0"
DF_2014_2_23$sentiment[DF_2014_2_23$sentiment > 0] <- "1"

date <- c("2014-02-23")
tots <- as.numeric(nrow(DF_2014_2_23))
negs <- as.numeric(sum(DF_2014_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_2_23$sentiment == "1"))
results2014_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_2_23 <- cbind(date, tots, poss, negs, results2014_2_23)
results23L2F <- rbind(results23L2F, results2014_2_23)

rm(DF_2014_2_23_cleanREG, scores, DF_2014_2_23, date, tots, negs, poss, results2014_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2014_1_23.RData")

DF_2014_1_23_cleanREG <- DF_2014_1_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2014_1_23_cleanREG$text <- as.factor(DF_2014_1_23_cleanREG$text)

scores <- score.sentiment(DF_2014_1_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2014_1_23 <- scores
colnames(DF_2014_1_23) <- c("text", "sentiment")

DF_2014_1_23$sentiment[DF_2014_1_23$sentiment < 0] <- "0"
DF_2014_1_23$sentiment[DF_2014_1_23$sentiment > 0] <- "1"

date <- c("2014-01-23")
tots <- as.numeric(nrow(DF_2014_1_23))
negs <- as.numeric(sum(DF_2014_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_1_23$sentiment == "1"))
results2014_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_1_23 <- cbind(date, tots, poss, negs, results2014_1_23)
results23L2F <- rbind(results23L2F, results2014_1_23)

rm(DF_2014_1_23_cleanREG, scores, DF_2014_1_23, date, tots, negs, poss, results2014_1_23)


print("2013")
#December

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_12_23.RData")

DF_2013_12_23_cleanREG <- DF_2013_12_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_12_23_cleanREG$text <- as.factor(DF_2013_12_23_cleanREG$text)

scores <- score.sentiment(DF_2013_12_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_12_23 <- scores
colnames(DF_2013_12_23) <- c("text", "sentiment")

DF_2013_12_23$sentiment[DF_2013_12_23$sentiment < 0] <- "0"
DF_2013_12_23$sentiment[DF_2013_12_23$sentiment > 0] <- "1"

date <- c("2013-12-23")
tots <- as.numeric(nrow(DF_2013_12_23))
negs <- as.numeric(sum(DF_2013_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_12_23$sentiment == "1"))
results2013_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_12_23 <- cbind(date, tots, poss, negs, results2013_12_23)
results23L2F <- rbind(results23L2F, results2013_12_23)

rm(DF_2013_12_23_cleanREG, scores, DF_2013_12_23, date, tots, negs, poss, results2013_12_23)

#Nobember

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_11_23.RData")

DF_2013_11_23_cleanREG <- DF_2013_11_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_11_23_cleanREG$text <- as.factor(DF_2013_11_23_cleanREG$text)

scores <- score.sentiment(DF_2013_11_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_11_23 <- scores
colnames(DF_2013_11_23) <- c("text", "sentiment")

DF_2013_11_23$sentiment[DF_2013_11_23$sentiment < 0] <- "0"
DF_2013_11_23$sentiment[DF_2013_11_23$sentiment > 0] <- "1"

date <- c("2013-11-23")
tots <- as.numeric(nrow(DF_2013_11_23))
negs <- as.numeric(sum(DF_2013_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_11_23$sentiment == "1"))
results2013_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_11_23 <- cbind(date, tots, poss, negs, results2013_11_23)
results23L2F <- rbind(results23L2F, results2013_11_23)

rm(DF_2013_11_23_cleanREG, scores, DF_2013_11_23, date, tots, negs, poss, results2013_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_10_23.RData")

DF_2013_10_23_cleanREG <- DF_2013_10_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_10_23_cleanREG$text <- as.factor(DF_2013_10_23_cleanREG$text)

scores <- score.sentiment(DF_2013_10_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_10_23 <- scores
colnames(DF_2013_10_23) <- c("text", "sentiment")

DF_2013_10_23$sentiment[DF_2013_10_23$sentiment < 0] <- "0"
DF_2013_10_23$sentiment[DF_2013_10_23$sentiment > 0] <- "1"

date <- c("2013-10-23")
tots <- as.numeric(nrow(DF_2013_10_23))
negs <- as.numeric(sum(DF_2013_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_10_23$sentiment == "1"))
results2013_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_10_23 <- cbind(date, tots, poss, negs, results2013_10_23)
results23L2F <- rbind(results23L2F, results2013_10_23)

rm(DF_2013_10_23_cleanREG, scores, DF_2013_10_23, date, tots, negs, poss, results2013_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_9_23.RData")

DF_2013_9_23_cleanREG <- DF_2013_9_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_9_23_cleanREG$text <- as.factor(DF_2013_9_23_cleanREG$text)

scores <- score.sentiment(DF_2013_9_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_9_23 <- scores
colnames(DF_2013_9_23) <- c("text", "sentiment")

DF_2013_9_23$sentiment[DF_2013_9_23$sentiment < 0] <- "0"
DF_2013_9_23$sentiment[DF_2013_9_23$sentiment > 0] <- "1"

date <- c("2013-09-23")
tots <- as.numeric(nrow(DF_2013_9_23))
negs <- as.numeric(sum(DF_2013_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_9_23$sentiment == "1"))
results2013_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_9_23 <- cbind(date, tots, poss, negs, results2013_9_23)
results23L2F <- rbind(results23L2F, results2013_9_23)

rm(DF_2013_9_23_cleanREG, scores, DF_2013_9_23, date, tots, negs, poss, results2013_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_8_23.RData")

DF_2013_8_23_cleanREG <- DF_2013_8_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_8_23_cleanREG$text <- as.factor(DF_2013_8_23_cleanREG$text)

scores <- score.sentiment(DF_2013_8_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_8_23 <- scores
colnames(DF_2013_8_23) <- c("text", "sentiment")

DF_2013_8_23$sentiment[DF_2013_8_23$sentiment < 0] <- "0"
DF_2013_8_23$sentiment[DF_2013_8_23$sentiment > 0] <- "1"

date <- c("2013-08-23")
tots <- as.numeric(nrow(DF_2013_8_23))
negs <- as.numeric(sum(DF_2013_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_8_23$sentiment == "1"))
results2013_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_8_23 <- cbind(date, tots, poss, negs, results2013_8_23)
results23L2F <- rbind(results23L2F, results2013_8_23)

rm(DF_2013_8_23_cleanREG, scores, DF_2013_8_23, date, tots, negs, poss, results2013_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_7_23.RData")

DF_2013_7_23_cleanREG <- DF_2013_7_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_7_23_cleanREG$text <- as.factor(DF_2013_7_23_cleanREG$text)

scores <- score.sentiment(DF_2013_7_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_7_23 <- scores
colnames(DF_2013_7_23) <- c("text", "sentiment")

DF_2013_7_23$sentiment[DF_2013_7_23$sentiment < 0] <- "0"
DF_2013_7_23$sentiment[DF_2013_7_23$sentiment > 0] <- "1"

date <- c("2013-07-23")
tots <- as.numeric(nrow(DF_2013_7_23))
negs <- as.numeric(sum(DF_2013_7_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_7_23$sentiment == "1"))
results2013_7_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_7_23 <- cbind(date, tots, poss, negs, results2013_7_23)
results23L2F <- rbind(results23L2F, results2013_7_23)

rm(DF_2013_7_23_cleanREG, scores, DF_2013_7_23, date, tots, negs, poss, results2013_7_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_6_23.RData")

DF_2013_6_23_cleanREG <- DF_2013_6_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_6_23_cleanREG$text <- as.factor(DF_2013_6_23_cleanREG$text)

scores <- score.sentiment(DF_2013_6_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_6_23 <- scores
colnames(DF_2013_6_23) <- c("text", "sentiment")

DF_2013_6_23$sentiment[DF_2013_6_23$sentiment < 0] <- "0"
DF_2013_6_23$sentiment[DF_2013_6_23$sentiment > 0] <- "1"

date <- c("2013-06-23")
tots <- as.numeric(nrow(DF_2013_6_23))
negs <- as.numeric(sum(DF_2013_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_6_23$sentiment == "1"))
results2013_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_6_23 <- cbind(date, tots, poss, negs, results2013_6_23)
results23L2F <- rbind(results23L2F, results2013_6_23)

rm(DF_2013_6_23_cleanREG, scores, DF_2013_6_23, date, tots, negs, poss, results2013_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_5_23.RData")

DF_2013_5_23_cleanREG <- DF_2013_5_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_5_23_cleanREG$text <- as.factor(DF_2013_5_23_cleanREG$text)

scores <- score.sentiment(DF_2013_5_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_5_23 <- scores
colnames(DF_2013_5_23) <- c("text", "sentiment")

DF_2013_5_23$sentiment[DF_2013_5_23$sentiment < 0] <- "0"
DF_2013_5_23$sentiment[DF_2013_5_23$sentiment > 0] <- "1"

date <- c("2013-05-23")
tots <- as.numeric(nrow(DF_2013_5_23))
negs <- as.numeric(sum(DF_2013_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_5_23$sentiment == "1"))
results2013_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_5_23 <- cbind(date, tots, poss, negs, results2013_5_23)
results23L2F <- rbind(results23L2F, results2013_5_23)

rm(DF_2013_5_23_cleanREG, scores, DF_2013_5_23, date, tots, negs, poss, results2013_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_4_23.RData")

DF_2013_4_23_cleanREG <- DF_2013_4_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_4_23_cleanREG$text <- as.factor(DF_2013_4_23_cleanREG$text)

scores <- score.sentiment(DF_2013_4_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_4_23 <- scores
colnames(DF_2013_4_23) <- c("text", "sentiment")

DF_2013_4_23$sentiment[DF_2013_4_23$sentiment < 0] <- "0"
DF_2013_4_23$sentiment[DF_2013_4_23$sentiment > 0] <- "1"

date <- c("2013-04-23")
tots <- as.numeric(nrow(DF_2013_4_23))
negs <- as.numeric(sum(DF_2013_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_4_23$sentiment == "1"))
results2013_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_4_23 <- cbind(date, tots, poss, negs, results2013_4_23)
results23L2F <- rbind(results23L2F, results2013_4_23)

rm(DF_2013_4_23_cleanREG, scores, DF_2013_4_23, date, tots, negs, poss, results2013_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_3_23.RData")

DF_2013_3_23_cleanREG <- DF_2013_3_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_3_23_cleanREG$text <- as.factor(DF_2013_3_23_cleanREG$text)

scores <- score.sentiment(DF_2013_3_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_3_23 <- scores
colnames(DF_2013_3_23) <- c("text", "sentiment")

DF_2013_3_23$sentiment[DF_2013_3_23$sentiment < 0] <- "0"
DF_2013_3_23$sentiment[DF_2013_3_23$sentiment > 0] <- "1"

date <- c("2013-03-23")
tots <- as.numeric(nrow(DF_2013_3_23))
negs <- as.numeric(sum(DF_2013_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_3_23$sentiment == "1"))
results2013_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_3_23 <- cbind(date, tots, poss, negs, results2013_3_23)
results23L2F <- rbind(results23L2F, results2013_3_23)

rm(DF_2013_3_23_cleanREG, scores, DF_2013_3_23, date, tots, negs, poss, results2013_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_2_23.RData")

DF_2013_2_23_cleanREG <- DF_2013_2_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_2_23_cleanREG$text <- as.factor(DF_2013_2_23_cleanREG$text)

scores <- score.sentiment(DF_2013_2_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_2_23 <- scores
colnames(DF_2013_2_23) <- c("text", "sentiment")

DF_2013_2_23$sentiment[DF_2013_2_23$sentiment < 0] <- "0"
DF_2013_2_23$sentiment[DF_2013_2_23$sentiment > 0] <- "1"

date <- c("2013-02-23")
tots <- as.numeric(nrow(DF_2013_2_23))
negs <- as.numeric(sum(DF_2013_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_2_23$sentiment == "1"))
results2013_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_2_23 <- cbind(date, tots, poss, negs, results2013_2_23)
results23L2F <- rbind(results23L2F, results2013_2_23)

rm(DF_2013_2_23_cleanREG, scores, DF_2013_2_23, date, tots, negs, poss, results2013_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2013_1_23.RData")

DF_2013_1_23_cleanREG <- DF_2013_1_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2013_1_23_cleanREG$text <- as.factor(DF_2013_1_23_cleanREG$text)

scores <- score.sentiment(DF_2013_1_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2013_1_23 <- scores
colnames(DF_2013_1_23) <- c("text", "sentiment")

DF_2013_1_23$sentiment[DF_2013_1_23$sentiment < 0] <- "0"
DF_2013_1_23$sentiment[DF_2013_1_23$sentiment > 0] <- "1"

date <- c("2013-01-23")
tots <- as.numeric(nrow(DF_2013_1_23))
negs <- as.numeric(sum(DF_2013_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_1_23$sentiment == "1"))
results2013_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_1_23 <- cbind(date, tots, poss, negs, results2013_1_23)
results23L2F <- rbind(results23L2F, results2013_1_23)

rm(DF_2013_1_23_cleanREG, scores, DF_2013_1_23, date, tots, negs, poss, results2013_1_23)


print("2012")
#December

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_12_23.RData")

DF_2012_12_23_cleanREG <- DF_2012_12_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_12_23_cleanREG$text <- as.factor(DF_2012_12_23_cleanREG$text)

scores <- score.sentiment(DF_2012_12_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_12_23 <- scores
colnames(DF_2012_12_23) <- c("text", "sentiment")

DF_2012_12_23$sentiment[DF_2012_12_23$sentiment < 0] <- "0"
DF_2012_12_23$sentiment[DF_2012_12_23$sentiment > 0] <- "1"

date <- c("2012-12-23")
tots <- as.numeric(nrow(DF_2012_12_23))
negs <- as.numeric(sum(DF_2012_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_12_23$sentiment == "1"))
results2012_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_12_23 <- cbind(date, tots, poss, negs, results2012_12_23)
results23L2F <- rbind(results23L2F, results2012_12_23)

rm(DF_2012_12_23_cleanREG, scores, DF_2012_12_23, date, tots, negs, poss, results2012_12_23)

#Nobember

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_11_23.RData")

DF_2012_11_23_cleanREG <- DF_2012_11_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_11_23_cleanREG$text <- as.factor(DF_2012_11_23_cleanREG$text)

scores <- score.sentiment(DF_2012_11_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_11_23 <- scores
colnames(DF_2012_11_23) <- c("text", "sentiment")

DF_2012_11_23$sentiment[DF_2012_11_23$sentiment < 0] <- "0"
DF_2012_11_23$sentiment[DF_2012_11_23$sentiment > 0] <- "1"

date <- c("2012-11-23")
tots <- as.numeric(nrow(DF_2012_11_23))
negs <- as.numeric(sum(DF_2012_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_11_23$sentiment == "1"))
results2012_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_11_23 <- cbind(date, tots, poss, negs, results2012_11_23)
results23L2F <- rbind(results23L2F, results2012_11_23)

rm(DF_2012_11_23_cleanREG, scores, DF_2012_11_23, date, tots, negs, poss, results2012_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_10_23.RData")

DF_2012_10_23_cleanREG <- DF_2012_10_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_10_23_cleanREG$text <- as.factor(DF_2012_10_23_cleanREG$text)

scores <- score.sentiment(DF_2012_10_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_10_23 <- scores
colnames(DF_2012_10_23) <- c("text", "sentiment")

DF_2012_10_23$sentiment[DF_2012_10_23$sentiment < 0] <- "0"
DF_2012_10_23$sentiment[DF_2012_10_23$sentiment > 0] <- "1"

date <- c("2012-10-23")
tots <- as.numeric(nrow(DF_2012_10_23))
negs <- as.numeric(sum(DF_2012_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_10_23$sentiment == "1"))
results2012_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_10_23 <- cbind(date, tots, poss, negs, results2012_10_23)
results23L2F <- rbind(results23L2F, results2012_10_23)

rm(DF_2012_10_23_cleanREG, scores, DF_2012_10_23, date, tots, negs, poss, results2012_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_9_23.RData")

DF_2012_9_23_cleanREG <- DF_2012_9_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_9_23_cleanREG$text <- as.factor(DF_2012_9_23_cleanREG$text)

scores <- score.sentiment(DF_2012_9_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_9_23 <- scores
colnames(DF_2012_9_23) <- c("text", "sentiment")

DF_2012_9_23$sentiment[DF_2012_9_23$sentiment < 0] <- "0"
DF_2012_9_23$sentiment[DF_2012_9_23$sentiment > 0] <- "1"

date <- c("2012-09-23")
tots <- as.numeric(nrow(DF_2012_9_23))
negs <- as.numeric(sum(DF_2012_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_9_23$sentiment == "1"))
results2012_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_9_23 <- cbind(date, tots, poss, negs, results2012_9_23)
results23L2F <- rbind(results23L2F, results2012_9_23)

rm(DF_2012_9_23_cleanREG, scores, DF_2012_9_23, date, tots, negs, poss, results2012_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_8_23.RData")

DF_2012_8_23_cleanREG <- DF_2012_8_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_8_23_cleanREG$text <- as.factor(DF_2012_8_23_cleanREG$text)

scores <- score.sentiment(DF_2012_8_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_8_23 <- scores
colnames(DF_2012_8_23) <- c("text", "sentiment")

DF_2012_8_23$sentiment[DF_2012_8_23$sentiment < 0] <- "0"
DF_2012_8_23$sentiment[DF_2012_8_23$sentiment > 0] <- "1"

date <- c("2012-08-23")
tots <- as.numeric(nrow(DF_2012_8_23))
negs <- as.numeric(sum(DF_2012_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_8_23$sentiment == "1"))
results2012_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_8_23 <- cbind(date, tots, poss, negs, results2012_8_23)
results23L2F <- rbind(results23L2F, results2012_8_23)

rm(DF_2012_8_23_cleanREG, scores, DF_2012_8_23, date, tots, negs, poss, results2012_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_7_23.RData")

DF_2012_7_23_cleanREG <- DF_2012_7_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_7_23_cleanREG$text <- as.factor(DF_2012_7_23_cleanREG$text)

scores <- score.sentiment(DF_2012_7_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_7_23 <- scores
colnames(DF_2012_7_23) <- c("text", "sentiment")

DF_2012_7_23$sentiment[DF_2012_7_23$sentiment < 0] <- "0"
DF_2012_7_23$sentiment[DF_2012_7_23$sentiment > 0] <- "1"

date <- c("2012-07-23")
tots <- as.numeric(nrow(DF_2012_7_23))
negs <- as.numeric(sum(DF_2012_7_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_7_23$sentiment == "1"))
results2012_7_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_7_23 <- cbind(date, tots, poss, negs, results2012_7_23)
results23L2F <- rbind(results23L2F, results2012_7_23)

rm(DF_2012_7_23_cleanREG, scores, DF_2012_7_23, date, tots, negs, poss, results2012_7_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_6_23.RData")

DF_2012_6_23_cleanREG <- DF_2012_6_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_6_23_cleanREG$text <- as.factor(DF_2012_6_23_cleanREG$text)

scores <- score.sentiment(DF_2012_6_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_6_23 <- scores
colnames(DF_2012_6_23) <- c("text", "sentiment")

DF_2012_6_23$sentiment[DF_2012_6_23$sentiment < 0] <- "0"
DF_2012_6_23$sentiment[DF_2012_6_23$sentiment > 0] <- "1"

date <- c("2012-06-23")
tots <- as.numeric(nrow(DF_2012_6_23))
negs <- as.numeric(sum(DF_2012_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_6_23$sentiment == "1"))
results2012_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_6_23 <- cbind(date, tots, poss, negs, results2012_6_23)
results23L2F <- rbind(results23L2F, results2012_6_23)

rm(DF_2012_6_23_cleanREG, scores, DF_2012_6_23, date, tots, negs, poss, results2012_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_5_23.RData")

DF_2012_5_23_cleanREG <- DF_2012_5_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_5_23_cleanREG$text <- as.factor(DF_2012_5_23_cleanREG$text)

scores <- score.sentiment(DF_2012_5_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_5_23 <- scores
colnames(DF_2012_5_23) <- c("text", "sentiment")

DF_2012_5_23$sentiment[DF_2012_5_23$sentiment < 0] <- "0"
DF_2012_5_23$sentiment[DF_2012_5_23$sentiment > 0] <- "1"

date <- c("2012-05-23")
tots <- as.numeric(nrow(DF_2012_5_23))
negs <- as.numeric(sum(DF_2012_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_5_23$sentiment == "1"))
results2012_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_5_23 <- cbind(date, tots, poss, negs, results2012_5_23)
results23L2F <- rbind(results23L2F, results2012_5_23)

rm(DF_2012_5_23_cleanREG, scores, DF_2012_5_23, date, tots, negs, poss, results2012_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_4_23.RData")

DF_2012_4_23_cleanREG <- DF_2012_4_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_4_23_cleanREG$text <- as.factor(DF_2012_4_23_cleanREG$text)

scores <- score.sentiment(DF_2012_4_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_4_23 <- scores
colnames(DF_2012_4_23) <- c("text", "sentiment")

DF_2012_4_23$sentiment[DF_2012_4_23$sentiment < 0] <- "0"
DF_2012_4_23$sentiment[DF_2012_4_23$sentiment > 0] <- "1"

date <- c("2012-04-23")
tots <- as.numeric(nrow(DF_2012_4_23))
negs <- as.numeric(sum(DF_2012_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_4_23$sentiment == "1"))
results2012_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_4_23 <- cbind(date, tots, poss, negs, results2012_4_23)
results23L2F <- rbind(results23L2F, results2012_4_23)

rm(DF_2012_4_23_cleanREG, scores, DF_2012_4_23, date, tots, negs, poss, results2012_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_3_23.RData")

DF_2012_3_23_cleanREG <- DF_2012_3_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_3_23_cleanREG$text <- as.factor(DF_2012_3_23_cleanREG$text)

scores <- score.sentiment(DF_2012_3_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_3_23 <- scores
colnames(DF_2012_3_23) <- c("text", "sentiment")

DF_2012_3_23$sentiment[DF_2012_3_23$sentiment < 0] <- "0"
DF_2012_3_23$sentiment[DF_2012_3_23$sentiment > 0] <- "1"

date <- c("2012-03-23")
tots <- as.numeric(nrow(DF_2012_3_23))
negs <- as.numeric(sum(DF_2012_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_3_23$sentiment == "1"))
results2012_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_3_23 <- cbind(date, tots, poss, negs, results2012_3_23)
results23L2F <- rbind(results23L2F, results2012_3_23)

rm(DF_2012_3_23_cleanREG, scores, DF_2012_3_23, date, tots, negs, poss, results2012_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_2_23.RData")

DF_2012_2_23_cleanREG <- DF_2012_2_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_2_23_cleanREG$text <- as.factor(DF_2012_2_23_cleanREG$text)

scores <- score.sentiment(DF_2012_2_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_2_23 <- scores
colnames(DF_2012_2_23) <- c("text", "sentiment")

DF_2012_2_23$sentiment[DF_2012_2_23$sentiment < 0] <- "0"
DF_2012_2_23$sentiment[DF_2012_2_23$sentiment > 0] <- "1"

date <- c("2012-02-23")
tots <- as.numeric(nrow(DF_2012_2_23))
negs <- as.numeric(sum(DF_2012_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_2_23$sentiment == "1"))
results2012_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_2_23 <- cbind(date, tots, poss, negs, results2012_2_23)
results23L2F <- rbind(results23L2F, results2012_2_23)

rm(DF_2012_2_23_cleanREG, scores, DF_2012_2_23, date, tots, negs, poss, results2012_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/REG/DF_2012_1_23.RData")

DF_2012_1_23_cleanREG <- DF_2012_1_23_cleanREG %>%
  filter(str_detect(text, list))

DF_2012_1_23_cleanREG$text <- as.factor(DF_2012_1_23_cleanREG$text)

scores <- score.sentiment(DF_2012_1_23_cleanREG$text,  pos.words, neg.words, .progress='text')

scores <- scores[, c(2,1)]

DF_2012_1_23 <- scores
colnames(DF_2012_1_23) <- c("text", "sentiment")

DF_2012_1_23$sentiment[DF_2012_1_23$sentiment < 0] <- "0"
DF_2012_1_23$sentiment[DF_2012_1_23$sentiment > 0] <- "1"

date <- c("2012-01-23")
tots <- as.numeric(nrow(DF_2012_1_23))
negs <- as.numeric(sum(DF_2012_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_1_23$sentiment == "1"))
results2012_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_1_23 <- cbind(date, tots, poss, negs, results2012_1_23)
results23L2F <- rbind(results23L2F, results2012_1_23)

rm(DF_2012_1_23_cleanREG, scores, DF_2012_1_23, date, tots, negs, poss, results2012_1_23)

save(results23L2F, file = "Objects/Models/results23L2F.RData")
