

#Classification function

classifier_glmnet <- function(x){
  
  #loading libraries
  
  library(tidyverse)
  library(purrrlyr)
  library(text2vec)
  library(caret)
  library(glmnet)
  library(ggrepel)
  
  load(file = "Objects/Models/vectorizer.RData")
  load(file = "Objects/Models/list.RData")
  load(file = "Objects/Models/glmnet_classifier.RData")
  conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")
  
  df_tweets <- x

  df_tweets <- df_tweets %>%
  filter(str_detect(text, list))
  
  df_tweets <- df_tweets %>%
    dmap_at('text', conv_fun)
  
  prep_fun <- tolower
  tok_fun <- word_tokenizer
  
  it_tweets <- itoken(df_tweets$text,
                      preprocessor = prep_fun,
                      tokenizer = tok_fun,
                      progressbar = TRUE)

  dtm_tweets <- create_dtm(it_tweets, vectorizer)
  
  tfidf <- TfIdf$new()
  
  dtm_tweets_tfidf <- fit_transform(dtm_tweets, tfidf)
  
  preds_tweets <- predict(glmnet_classifier, dtm_tweets_tfidf, type = 'response')[ ,1]
  
  df_tweets <<- cbind(df_tweets, preds_tweets)
  }

t1 <- Sys.time()

#Series 25
print("2017")
#December

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_12_25.RData")

classifier_glmnet(DF_2017_12_25_cleanESP)

DF_2017_12_25 <- df_tweets
colnames(DF_2017_12_25) <- c("text", "sentiment")

DF_2017_12_25$sentiment[DF_2017_12_25$sentiment <= 0.5] <- "0"
DF_2017_12_25$sentiment[DF_2017_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_12_25))
negs <- as.numeric(sum(DF_2017_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_12_25$sentiment == "1"))
results2017_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_12_25 <- cbind(tots, poss, negs, results2017_12_25)
results <- results2017_12_25

rm(DF_2017_12_25_cleanESP, df_tweets, DF_2017_12_25, tots, negs, poss, results2017_12_25)

#November

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_11_25.RData")

classifier_glmnet(DF_2017_11_25_cleanESP)

DF_2017_11_25 <- df_tweets
colnames(DF_2017_11_25) <- c("text", "sentiment")

DF_2017_11_25$sentiment[DF_2017_11_25$sentiment <= 0.5] <- "0"
DF_2017_11_25$sentiment[DF_2017_11_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_11_25))
negs <- as.numeric(sum(DF_2017_11_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_11_25$sentiment == "1"))
results2017_11_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_11_25 <- cbind(tots, poss, negs, results2017_11_25)
results <- rbind(results, results2017_11_25)

rm(DF_2017_11_25_cleanESP, df_tweets, DF_2017_11_25, tots, negs, poss, results2017_11_25)

#October

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_10_25.RData")

classifier_glmnet(DF_2017_10_25_cleanESP)

DF_2017_10_25 <- df_tweets
colnames(DF_2017_10_25) <- c("text", "sentiment")

DF_2017_10_25$sentiment[DF_2017_10_25$sentiment <= 0.5] <- "0"
DF_2017_10_25$sentiment[DF_2017_10_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_10_25))
negs <- as.numeric(sum(DF_2017_10_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_10_25$sentiment == "1"))
results2017_10_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_10_25 <- cbind(tots, poss, negs, results2017_10_25)
results <- rbind(results, results2017_10_25)

rm(DF_2017_10_25_cleanESP, df_tweets, DF_2017_10_25, tots, negs, poss, results2017_10_25)

#September

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_9_25.RData")

classifier_glmnet(DF_2017_9_25_cleanESP)

DF_2017_9_25 <- df_tweets
colnames(DF_2017_9_25) <- c("text", "sentiment")

DF_2017_9_25$sentiment[DF_2017_9_25$sentiment <= 0.5] <- "0"
DF_2017_9_25$sentiment[DF_2017_9_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_9_25))
negs <- as.numeric(sum(DF_2017_9_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_9_25$sentiment == "1"))
results2017_9_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_9_25 <- cbind(tots, poss, negs, results2017_9_25)
results <- rbind(results, results2017_9_25)

rm(DF_2017_9_25_cleanESP, df_tweets, DF_2017_9_25, tots, negs, poss, results2017_9_25)

#August

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_8_25.RData")

classifier_glmnet(DF_2017_8_25_cleanESP)

DF_2017_8_25 <- df_tweets
colnames(DF_2017_8_25) <- c("text", "sentiment")

DF_2017_8_25$sentiment[DF_2017_8_25$sentiment <= 0.5] <- "0"
DF_2017_8_25$sentiment[DF_2017_8_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_8_25))
negs <- as.numeric(sum(DF_2017_8_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_8_25$sentiment == "1"))
results2017_8_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_8_25 <- cbind(tots, poss, negs, results2017_8_25)
results <- rbind(results, results2017_8_25)

rm(DF_2017_8_25_cleanESP, df_tweets, DF_2017_8_25, tots, negs, poss, results2017_8_25)

#July

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_12_25.RData")

classifier_glmnet(DF_2017_12_25_cleanESP)

DF_2017_12_25 <- df_tweets
colnames(DF_2017_12_25) <- c("text", "sentiment")

DF_2017_12_25$sentiment[DF_2017_12_25$sentiment <= 0.5] <- "0"
DF_2017_12_25$sentiment[DF_2017_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_12_25))
negs <- as.numeric(sum(DF_2017_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_12_25$sentiment == "1"))
results2017_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_12_25 <- cbind(tots, poss, negs, results2017_12_25)
results <- rbind(results, results2017_12_25)

rm(DF_2017_12_25_cleanESP, df_tweets, DF_2017_12_25, tots, negs, poss, results2017_12_25)

#June

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_6_25.RData")

classifier_glmnet(DF_2017_6_25_cleanESP)

DF_2017_6_25 <- df_tweets
colnames(DF_2017_6_25) <- c("text", "sentiment")

DF_2017_6_25$sentiment[DF_2017_6_25$sentiment <= 0.5] <- "0"
DF_2017_6_25$sentiment[DF_2017_6_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_6_25))
negs <- as.numeric(sum(DF_2017_6_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_6_25$sentiment == "1"))
results2017_6_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_6_25 <- cbind(tots, poss, negs, results2017_6_25)
results <- rbind(results, results2017_6_25)

rm(DF_2017_6_25_cleanESP, df_tweets, DF_2017_6_25, tots, negs, poss, results2017_6_25)

#May

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_5_25.RData")

classifier_glmnet(DF_2017_5_25_cleanESP)

DF_2017_5_25 <- df_tweets
colnames(DF_2017_5_25) <- c("text", "sentiment")

DF_2017_5_25$sentiment[DF_2017_5_25$sentiment <= 0.5] <- "0"
DF_2017_5_25$sentiment[DF_2017_5_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_5_25))
negs <- as.numeric(sum(DF_2017_5_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_5_25$sentiment == "1"))
results2017_5_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_5_25 <- cbind(tots, poss, negs, results2017_5_25)
results <- rbind(results, results2017_5_25)

rm(DF_2017_5_25_cleanESP, df_tweets, DF_2017_5_25, tots, negs, poss, results2017_5_25)

#April

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_4_25.RData")

classifier_glmnet(DF_2017_4_25_cleanESP)

DF_2017_4_25 <- df_tweets
colnames(DF_2017_4_25) <- c("text", "sentiment")

DF_2017_4_25$sentiment[DF_2017_4_25$sentiment <= 0.5] <- "0"
DF_2017_4_25$sentiment[DF_2017_4_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_4_25))
negs <- as.numeric(sum(DF_2017_4_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_4_25$sentiment == "1"))
results2017_4_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_4_25 <- cbind(tots, poss, negs, results2017_4_25)
results <- rbind(results, results2017_4_25)

rm(DF_2017_4_25_cleanESP, df_tweets, DF_2017_4_25, tots, negs, poss, results2017_4_25)

#March

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_3_25.RData")

classifier_glmnet(DF_2017_3_25_cleanESP)

DF_2017_3_25 <- df_tweets
colnames(DF_2017_3_25) <- c("text", "sentiment")

DF_2017_3_25$sentiment[DF_2017_3_25$sentiment <= 0.5] <- "0"
DF_2017_3_25$sentiment[DF_2017_3_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_3_25))
negs <- as.numeric(sum(DF_2017_3_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_3_25$sentiment == "1"))
results2017_3_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_3_25 <- cbind(tots, poss, negs, results2017_3_25)
results <- rbind(results, results2017_3_25)

rm(DF_2017_3_25_cleanESP, df_tweets, DF_2017_3_25, tots, negs, poss, results2017_3_25)

#February

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_2_25.RData")

classifier_glmnet(DF_2017_2_25_cleanESP)

DF_2017_2_25 <- df_tweets
colnames(DF_2017_2_25) <- c("text", "sentiment")

DF_2017_2_25$sentiment[DF_2017_2_25$sentiment <= 0.5] <- "0"
DF_2017_2_25$sentiment[DF_2017_2_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_2_25))
negs <- as.numeric(sum(DF_2017_2_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_2_25$sentiment == "1"))
results2017_2_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_2_25 <- cbind(tots, poss, negs, results2017_2_25)
results <- rbind(results, results2017_2_25)

rm(DF_2017_2_25_cleanESP, df_tweets, DF_2017_2_25, tots, negs, poss, results2017_2_25)

#January

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_1_25.RData")

classifier_glmnet(DF_2017_1_25_cleanESP)

DF_2017_1_25 <- df_tweets
colnames(DF_2017_1_25) <- c("text", "sentiment")

DF_2017_1_25$sentiment[DF_2017_1_25$sentiment <= 0.5] <- "0"
DF_2017_1_25$sentiment[DF_2017_1_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_1_25))
negs <- as.numeric(sum(DF_2017_1_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_1_25$sentiment == "1"))
results2017_1_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_1_25 <- cbind(tots, poss, negs, results2017_1_25)
results <- rbind(results, results2017_1_25)

rm(DF_2017_1_25_cleanESP, df_tweets, DF_2017_1_25, tots, negs, poss, results2017_1_25)

print("2016")
#December

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_12_25.RData")

classifier_glmnet(DF_2016_12_25_cleanESP)

DF_2016_12_25 <- df_tweets
colnames(DF_2016_12_25) <- c("text", "sentiment")

DF_2016_12_25$sentiment[DF_2016_12_25$sentiment <= 0.5] <- "0"
DF_2016_12_25$sentiment[DF_2016_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_12_25))
negs <- as.numeric(sum(DF_2016_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_12_25$sentiment == "1"))
results2016_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_12_25 <- cbind(tots, poss, negs, results2016_12_25)
results <- rbind(results, results2016_12_25)

rm(DF_2016_12_25_cleanESP, df_tweets, DF_2016_12_25, tots, negs, poss, results2016_12_25)

#November

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_11_25.RData")

classifier_glmnet(DF_2016_11_25_cleanESP)

DF_2016_11_25 <- df_tweets
colnames(DF_2016_11_25) <- c("text", "sentiment")

DF_2016_11_25$sentiment[DF_2016_11_25$sentiment <= 0.5] <- "0"
DF_2016_11_25$sentiment[DF_2016_11_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_11_25))
negs <- as.numeric(sum(DF_2016_11_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_11_25$sentiment == "1"))
results2016_11_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_11_25 <- cbind(tots, poss, negs, results2016_11_25)
results <- rbind(results, results2016_11_25)

rm(DF_2016_11_25_cleanESP, df_tweets, DF_2016_11_25, tots, negs, poss, results2016_11_25)

#October

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_10_25.RData")

classifier_glmnet(DF_2016_10_25_cleanESP)

DF_2016_10_25 <- df_tweets
colnames(DF_2016_10_25) <- c("text", "sentiment")

DF_2016_10_25$sentiment[DF_2016_10_25$sentiment <= 0.5] <- "0"
DF_2016_10_25$sentiment[DF_2016_10_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_10_25))
negs <- as.numeric(sum(DF_2016_10_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_10_25$sentiment == "1"))
results2016_10_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_10_25 <- cbind(tots, poss, negs, results2016_10_25)
results <- rbind(results, results2016_10_25)

rm(DF_2016_10_25_cleanESP, df_tweets, DF_2016_10_25, tots, negs, poss, results2016_10_25)

#September

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_9_25.RData")

classifier_glmnet(DF_2016_9_25_cleanESP)

DF_2016_9_25 <- df_tweets
colnames(DF_2016_9_25) <- c("text", "sentiment")

DF_2016_9_25$sentiment[DF_2016_9_25$sentiment <= 0.5] <- "0"
DF_2016_9_25$sentiment[DF_2016_9_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_9_25))
negs <- as.numeric(sum(DF_2016_9_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_9_25$sentiment == "1"))
results2016_9_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_9_25 <- cbind(tots, poss, negs, results2016_9_25)
results <- rbind(results, results2016_9_25)

rm(DF_2016_9_25_cleanESP, df_tweets, DF_2016_9_25, tots, negs, poss, results2016_9_25)

#August

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_8_25.RData")

classifier_glmnet(DF_2016_8_25_cleanESP)

DF_2016_8_25 <- df_tweets
colnames(DF_2016_8_25) <- c("text", "sentiment")

DF_2016_8_25$sentiment[DF_2016_8_25$sentiment <= 0.5] <- "0"
DF_2016_8_25$sentiment[DF_2016_8_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_8_25))
negs <- as.numeric(sum(DF_2016_8_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_8_25$sentiment == "1"))
results2016_8_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_8_25 <- cbind(tots, poss, negs, results2016_8_25)
results <- rbind(results, results2016_8_25)

rm(DF_2016_8_25_cleanESP, df_tweets, DF_2016_8_25, tots, negs, poss, results2016_8_25)

#July

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_12_25.RData")

classifier_glmnet(DF_2016_12_25_cleanESP)

DF_2016_12_25 <- df_tweets
colnames(DF_2016_12_25) <- c("text", "sentiment")

DF_2016_12_25$sentiment[DF_2016_12_25$sentiment <= 0.5] <- "0"
DF_2016_12_25$sentiment[DF_2016_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_12_25))
negs <- as.numeric(sum(DF_2016_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_12_25$sentiment == "1"))
results2016_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_12_25 <- cbind(tots, poss, negs, results2016_12_25)
results <- rbind(results, results2016_12_25)

rm(DF_2016_12_25_cleanESP, df_tweets, DF_2016_12_25, tots, negs, poss, results2016_12_25)

#June

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_6_25.RData")

classifier_glmnet(DF_2016_6_25_cleanESP)

DF_2016_6_25 <- df_tweets
colnames(DF_2016_6_25) <- c("text", "sentiment")

DF_2016_6_25$sentiment[DF_2016_6_25$sentiment <= 0.5] <- "0"
DF_2016_6_25$sentiment[DF_2016_6_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_6_25))
negs <- as.numeric(sum(DF_2016_6_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_6_25$sentiment == "1"))
results2016_6_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_6_25 <- cbind(tots, poss, negs, results2016_6_25)
results <- rbind(results, results2016_6_25)

rm(DF_2016_6_25_cleanESP, df_tweets, DF_2016_6_25, tots, negs, poss, results2016_6_25)

#May

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_5_25.RData")

classifier_glmnet(DF_2016_5_25_cleanESP)

DF_2016_5_25 <- df_tweets
colnames(DF_2016_5_25) <- c("text", "sentiment")

DF_2016_5_25$sentiment[DF_2016_5_25$sentiment <= 0.5] <- "0"
DF_2016_5_25$sentiment[DF_2016_5_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_5_25))
negs <- as.numeric(sum(DF_2016_5_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_5_25$sentiment == "1"))
results2016_5_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_5_25 <- cbind(tots, poss, negs, results2016_5_25)
results <- rbind(results, results2016_5_25)

rm(DF_2016_5_25_cleanESP, df_tweets, DF_2016_5_25, tots, negs, poss, results2016_5_25)

#April

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_4_25.RData")

classifier_glmnet(DF_2016_4_25_cleanESP)

DF_2016_4_25 <- df_tweets
colnames(DF_2016_4_25) <- c("text", "sentiment")

DF_2016_4_25$sentiment[DF_2016_4_25$sentiment <= 0.5] <- "0"
DF_2016_4_25$sentiment[DF_2016_4_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_4_25))
negs <- as.numeric(sum(DF_2016_4_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_4_25$sentiment == "1"))
results2016_4_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_4_25 <- cbind(tots, poss, negs, results2016_4_25)
results <- rbind(results, results2016_4_25)

rm(DF_2016_4_25_cleanESP, df_tweets, DF_2016_4_25, tots, negs, poss, results2016_4_25)

#March

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_3_25.RData")

classifier_glmnet(DF_2016_3_25_cleanESP)

DF_2016_3_25 <- df_tweets
colnames(DF_2016_3_25) <- c("text", "sentiment")

DF_2016_3_25$sentiment[DF_2016_3_25$sentiment <= 0.5] <- "0"
DF_2016_3_25$sentiment[DF_2016_3_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_3_25))
negs <- as.numeric(sum(DF_2016_3_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_3_25$sentiment == "1"))
results2016_3_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_3_25 <- cbind(tots, poss, negs, results2016_3_25)
results <- rbind(results, results2016_3_25)

rm(DF_2016_3_25_cleanESP, df_tweets, DF_2016_3_25, tots, negs, poss, results2016_3_25)

#February

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_2_25.RData")

classifier_glmnet(DF_2016_2_25_cleanESP)

DF_2016_2_25 <- df_tweets
colnames(DF_2016_2_25) <- c("text", "sentiment")

DF_2016_2_25$sentiment[DF_2016_2_25$sentiment <= 0.5] <- "0"
DF_2016_2_25$sentiment[DF_2016_2_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_2_25))
negs <- as.numeric(sum(DF_2016_2_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_2_25$sentiment == "1"))
results2016_2_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_2_25 <- cbind(tots, poss, negs, results2016_2_25)
results <- rbind(results, results2016_2_25)

rm(DF_2016_2_25_cleanESP, df_tweets, DF_2016_2_25, tots, negs, poss, results2016_2_25)

#January

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_1_25.RData")

classifier_glmnet(DF_2016_1_25_cleanESP)

DF_2016_1_25 <- df_tweets
colnames(DF_2016_1_25) <- c("text", "sentiment")

DF_2016_1_25$sentiment[DF_2016_1_25$sentiment <= 0.5] <- "0"
DF_2016_1_25$sentiment[DF_2016_1_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_1_25))
negs <- as.numeric(sum(DF_2016_1_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_1_25$sentiment == "1"))
results2016_1_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_1_25 <- cbind(tots, poss, negs, results2016_1_25)
results <- rbind(results, results2016_1_25)

rm(DF_2016_1_25_cleanESP, df_tweets, DF_2016_1_25, tots, negs, poss, results2016_1_25)

print("2015")
#December

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_12_25.RData")

classifier_glmnet(DF_2015_12_25_cleanESP)

DF_2015_12_25 <- df_tweets
colnames(DF_2015_12_25) <- c("text", "sentiment")

DF_2015_12_25$sentiment[DF_2015_12_25$sentiment <= 0.5] <- "0"
DF_2015_12_25$sentiment[DF_2015_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_12_25))
negs <- as.numeric(sum(DF_2015_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_12_25$sentiment == "1"))
results2015_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_12_25 <- cbind(tots, poss, negs, results2015_12_25)
results <- rbind(results, results2015_12_25)

rm(DF_2015_12_25_cleanESP, df_tweets, DF_2015_12_25, tots, negs, poss, results2015_12_25)

#November

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_11_25.RData")

classifier_glmnet(DF_2015_11_25_cleanESP)

DF_2015_11_25 <- df_tweets
colnames(DF_2015_11_25) <- c("text", "sentiment")

DF_2015_11_25$sentiment[DF_2015_11_25$sentiment <= 0.5] <- "0"
DF_2015_11_25$sentiment[DF_2015_11_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_11_25))
negs <- as.numeric(sum(DF_2015_11_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_11_25$sentiment == "1"))
results2015_11_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_11_25 <- cbind(tots, poss, negs, results2015_11_25)
results <- rbind(results, results2015_11_25)

rm(DF_2015_11_25_cleanESP, df_tweets, DF_2015_11_25, tots, negs, poss, results2015_11_25)

#October

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_10_25.RData")

classifier_glmnet(DF_2015_10_25_cleanESP)

DF_2015_10_25 <- df_tweets
colnames(DF_2015_10_25) <- c("text", "sentiment")

DF_2015_10_25$sentiment[DF_2015_10_25$sentiment <= 0.5] <- "0"
DF_2015_10_25$sentiment[DF_2015_10_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_10_25))
negs <- as.numeric(sum(DF_2015_10_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_10_25$sentiment == "1"))
results2015_10_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_10_25 <- cbind(tots, poss, negs, results2015_10_25)
results <- rbind(results, results2015_10_25)

rm(DF_2015_10_25_cleanESP, df_tweets, DF_2015_10_25, tots, negs, poss, results2015_10_25)

#September

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_9_25.RData")

classifier_glmnet(DF_2015_9_25_cleanESP)

DF_2015_9_25 <- df_tweets
colnames(DF_2015_9_25) <- c("text", "sentiment")

DF_2015_9_25$sentiment[DF_2015_9_25$sentiment <= 0.5] <- "0"
DF_2015_9_25$sentiment[DF_2015_9_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_9_25))
negs <- as.numeric(sum(DF_2015_9_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_9_25$sentiment == "1"))
results2015_9_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_9_25 <- cbind(tots, poss, negs, results2015_9_25)
results <- rbind(results, results2015_9_25)

rm(DF_2015_9_25_cleanESP, df_tweets, DF_2015_9_25, tots, negs, poss, results2015_9_25)

#August

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_8_25.RData")

classifier_glmnet(DF_2015_8_25_cleanESP)

DF_2015_8_25 <- df_tweets
colnames(DF_2015_8_25) <- c("text", "sentiment")

DF_2015_8_25$sentiment[DF_2015_8_25$sentiment <= 0.5] <- "0"
DF_2015_8_25$sentiment[DF_2015_8_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_8_25))
negs <- as.numeric(sum(DF_2015_8_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_8_25$sentiment == "1"))
results2015_8_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_8_25 <- cbind(tots, poss, negs, results2015_8_25)
results <- rbind(results, results2015_8_25)

rm(DF_2015_8_25_cleanESP, df_tweets, DF_2015_8_25, tots, negs, poss, results2015_8_25)

#July

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_12_25.RData")

classifier_glmnet(DF_2015_12_25_cleanESP)

DF_2015_12_25 <- df_tweets
colnames(DF_2015_12_25) <- c("text", "sentiment")

DF_2015_12_25$sentiment[DF_2015_12_25$sentiment <= 0.5] <- "0"
DF_2015_12_25$sentiment[DF_2015_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_12_25))
negs <- as.numeric(sum(DF_2015_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_12_25$sentiment == "1"))
results2015_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_12_25 <- cbind(tots, poss, negs, results2015_12_25)
results <- rbind(results, results2015_12_25)

rm(DF_2015_12_25_cleanESP, df_tweets, DF_2015_12_25, tots, negs, poss, results2015_12_25)

#June

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_6_25.RData")

classifier_glmnet(DF_2015_6_25_cleanESP)

DF_2015_6_25 <- df_tweets
colnames(DF_2015_6_25) <- c("text", "sentiment")

DF_2015_6_25$sentiment[DF_2015_6_25$sentiment <= 0.5] <- "0"
DF_2015_6_25$sentiment[DF_2015_6_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_6_25))
negs <- as.numeric(sum(DF_2015_6_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_6_25$sentiment == "1"))
results2015_6_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_6_25 <- cbind(tots, poss, negs, results2015_6_25)
results <- rbind(results, results2015_6_25)

rm(DF_2015_6_25_cleanESP, df_tweets, DF_2015_6_25, tots, negs, poss, results2015_6_25)

#May

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_5_25.RData")

classifier_glmnet(DF_2015_5_25_cleanESP)

DF_2015_5_25 <- df_tweets
colnames(DF_2015_5_25) <- c("text", "sentiment")

DF_2015_5_25$sentiment[DF_2015_5_25$sentiment <= 0.5] <- "0"
DF_2015_5_25$sentiment[DF_2015_5_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_5_25))
negs <- as.numeric(sum(DF_2015_5_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_5_25$sentiment == "1"))
results2015_5_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_5_25 <- cbind(tots, poss, negs, results2015_5_25)
results <- rbind(results, results2015_5_25)

rm(DF_2015_5_25_cleanESP, df_tweets, DF_2015_5_25, tots, negs, poss, results2015_5_25)

#April

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_4_25.RData")

classifier_glmnet(DF_2015_4_25_cleanESP)

DF_2015_4_25 <- df_tweets
colnames(DF_2015_4_25) <- c("text", "sentiment")

DF_2015_4_25$sentiment[DF_2015_4_25$sentiment <= 0.5] <- "0"
DF_2015_4_25$sentiment[DF_2015_4_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_4_25))
negs <- as.numeric(sum(DF_2015_4_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_4_25$sentiment == "1"))
results2015_4_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_4_25 <- cbind(tots, poss, negs, results2015_4_25)
results <- rbind(results, results2015_4_25)

rm(DF_2015_4_25_cleanESP, df_tweets, DF_2015_4_25, tots, negs, poss, results2015_4_25)

#March

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_3_25.RData")

classifier_glmnet(DF_2015_3_25_cleanESP)

DF_2015_3_25 <- df_tweets
colnames(DF_2015_3_25) <- c("text", "sentiment")

DF_2015_3_25$sentiment[DF_2015_3_25$sentiment <= 0.5] <- "0"
DF_2015_3_25$sentiment[DF_2015_3_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_3_25))
negs <- as.numeric(sum(DF_2015_3_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_3_25$sentiment == "1"))
results2015_3_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_3_25 <- cbind(tots, poss, negs, results2015_3_25)
results <- rbind(results, results2015_3_25)

rm(DF_2015_3_25_cleanESP, df_tweets, DF_2015_3_25, tots, negs, poss, results2015_3_25)

#February

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_2_25.RData")

classifier_glmnet(DF_2015_2_25_cleanESP)

DF_2015_2_25 <- df_tweets
colnames(DF_2015_2_25) <- c("text", "sentiment")

DF_2015_2_25$sentiment[DF_2015_2_25$sentiment <= 0.5] <- "0"
DF_2015_2_25$sentiment[DF_2015_2_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_2_25))
negs <- as.numeric(sum(DF_2015_2_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_2_25$sentiment == "1"))
results2015_2_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_2_25 <- cbind(tots, poss, negs, results2015_2_25)
results <- rbind(results, results2015_2_25)

rm(DF_2015_2_25_cleanESP, df_tweets, DF_2015_2_25, tots, negs, poss, results2015_2_25)

#January

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_1_25.RData")

classifier_glmnet(DF_2015_1_25_cleanESP)

DF_2015_1_25 <- df_tweets
colnames(DF_2015_1_25) <- c("text", "sentiment")

DF_2015_1_25$sentiment[DF_2015_1_25$sentiment <= 0.5] <- "0"
DF_2015_1_25$sentiment[DF_2015_1_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_1_25))
negs <- as.numeric(sum(DF_2015_1_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_1_25$sentiment == "1"))
results2015_1_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_1_25 <- cbind(tots, poss, negs, results2015_1_25)
results <- rbind(results, results2015_1_25)

rm(DF_2015_1_25_cleanESP, df_tweets, DF_2015_1_25, tots, negs, poss, results2015_1_25)

print("2014")
#December

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_12_25.RData")

classifier_glmnet(DF_2014_12_25_cleanESP)

DF_2014_12_25 <- df_tweets
colnames(DF_2014_12_25) <- c("text", "sentiment")

DF_2014_12_25$sentiment[DF_2014_12_25$sentiment <= 0.5] <- "0"
DF_2014_12_25$sentiment[DF_2014_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_12_25))
negs <- as.numeric(sum(DF_2014_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_12_25$sentiment == "1"))
results2014_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_12_25 <- cbind(tots, poss, negs, results2014_12_25)
results <- rbind(results, results2014_12_25)

rm(DF_2014_12_25_cleanESP, df_tweets, DF_2014_12_25, tots, negs, poss, results2014_12_25)

#November

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_11_25.RData")

classifier_glmnet(DF_2014_11_25_cleanESP)

DF_2014_11_25 <- df_tweets
colnames(DF_2014_11_25) <- c("text", "sentiment")

DF_2014_11_25$sentiment[DF_2014_11_25$sentiment <= 0.5] <- "0"
DF_2014_11_25$sentiment[DF_2014_11_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_11_25))
negs <- as.numeric(sum(DF_2014_11_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_11_25$sentiment == "1"))
results2014_11_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_11_25 <- cbind(tots, poss, negs, results2014_11_25)
results <- rbind(results, results2014_11_25)

rm(DF_2014_11_25_cleanESP, df_tweets, DF_2014_11_25, tots, negs, poss, results2014_11_25)

#October

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_10_25.RData")

classifier_glmnet(DF_2014_10_25_cleanESP)

DF_2014_10_25 <- df_tweets
colnames(DF_2014_10_25) <- c("text", "sentiment")

DF_2014_10_25$sentiment[DF_2014_10_25$sentiment <= 0.5] <- "0"
DF_2014_10_25$sentiment[DF_2014_10_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_10_25))
negs <- as.numeric(sum(DF_2014_10_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_10_25$sentiment == "1"))
results2014_10_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_10_25 <- cbind(tots, poss, negs, results2014_10_25)
results <- rbind(results, results2014_10_25)

rm(DF_2014_10_25_cleanESP, df_tweets, DF_2014_10_25, tots, negs, poss, results2014_10_25)

#September

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_9_25.RData")

classifier_glmnet(DF_2014_9_25_cleanESP)

DF_2014_9_25 <- df_tweets
colnames(DF_2014_9_25) <- c("text", "sentiment")

DF_2014_9_25$sentiment[DF_2014_9_25$sentiment <= 0.5] <- "0"
DF_2014_9_25$sentiment[DF_2014_9_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_9_25))
negs <- as.numeric(sum(DF_2014_9_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_9_25$sentiment == "1"))
results2014_9_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_9_25 <- cbind(tots, poss, negs, results2014_9_25)
results <- rbind(results, results2014_9_25)

rm(DF_2014_9_25_cleanESP, df_tweets, DF_2014_9_25, tots, negs, poss, results2014_9_25)

#August

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_8_25.RData")

classifier_glmnet(DF_2014_8_25_cleanESP)

DF_2014_8_25 <- df_tweets
colnames(DF_2014_8_25) <- c("text", "sentiment")

DF_2014_8_25$sentiment[DF_2014_8_25$sentiment <= 0.5] <- "0"
DF_2014_8_25$sentiment[DF_2014_8_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_8_25))
negs <- as.numeric(sum(DF_2014_8_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_8_25$sentiment == "1"))
results2014_8_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_8_25 <- cbind(tots, poss, negs, results2014_8_25)
results <- rbind(results, results2014_8_25)

rm(DF_2014_8_25_cleanESP, df_tweets, DF_2014_8_25, tots, negs, poss, results2014_8_25)

#July

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_12_25.RData")

classifier_glmnet(DF_2014_12_25_cleanESP)

DF_2014_12_25 <- df_tweets
colnames(DF_2014_12_25) <- c("text", "sentiment")

DF_2014_12_25$sentiment[DF_2014_12_25$sentiment <= 0.5] <- "0"
DF_2014_12_25$sentiment[DF_2014_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_12_25))
negs <- as.numeric(sum(DF_2014_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_12_25$sentiment == "1"))
results2014_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_12_25 <- cbind(tots, poss, negs, results2014_12_25)
results <- rbind(results, results2014_12_25)

rm(DF_2014_12_25_cleanESP, df_tweets, DF_2014_12_25, tots, negs, poss, results2014_12_25)

#June

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_6_25.RData")

classifier_glmnet(DF_2014_6_25_cleanESP)

DF_2014_6_25 <- df_tweets
colnames(DF_2014_6_25) <- c("text", "sentiment")

DF_2014_6_25$sentiment[DF_2014_6_25$sentiment <= 0.5] <- "0"
DF_2014_6_25$sentiment[DF_2014_6_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_6_25))
negs <- as.numeric(sum(DF_2014_6_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_6_25$sentiment == "1"))
results2014_6_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_6_25 <- cbind(tots, poss, negs, results2014_6_25)
results <- rbind(results, results2014_6_25)

rm(DF_2014_6_25_cleanESP, df_tweets, DF_2014_6_25, tots, negs, poss, results2014_6_25)

#May

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_5_25.RData")

classifier_glmnet(DF_2014_5_25_cleanESP)

DF_2014_5_25 <- df_tweets
colnames(DF_2014_5_25) <- c("text", "sentiment")

DF_2014_5_25$sentiment[DF_2014_5_25$sentiment <= 0.5] <- "0"
DF_2014_5_25$sentiment[DF_2014_5_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_5_25))
negs <- as.numeric(sum(DF_2014_5_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_5_25$sentiment == "1"))
results2014_5_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_5_25 <- cbind(tots, poss, negs, results2014_5_25)
results <- rbind(results, results2014_5_25)

rm(DF_2014_5_25_cleanESP, df_tweets, DF_2014_5_25, tots, negs, poss, results2014_5_25)

#April

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_4_25.RData")

classifier_glmnet(DF_2014_4_25_cleanESP)

DF_2014_4_25 <- df_tweets
colnames(DF_2014_4_25) <- c("text", "sentiment")

DF_2014_4_25$sentiment[DF_2014_4_25$sentiment <= 0.5] <- "0"
DF_2014_4_25$sentiment[DF_2014_4_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_4_25))
negs <- as.numeric(sum(DF_2014_4_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_4_25$sentiment == "1"))
results2014_4_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_4_25 <- cbind(tots, poss, negs, results2014_4_25)
results <- rbind(results, results2014_4_25)

rm(DF_2014_4_25_cleanESP, df_tweets, DF_2014_4_25, tots, negs, poss, results2014_4_25)

#March

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_3_25.RData")

classifier_glmnet(DF_2014_3_25_cleanESP)

DF_2014_3_25 <- df_tweets
colnames(DF_2014_3_25) <- c("text", "sentiment")

DF_2014_3_25$sentiment[DF_2014_3_25$sentiment <= 0.5] <- "0"
DF_2014_3_25$sentiment[DF_2014_3_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_3_25))
negs <- as.numeric(sum(DF_2014_3_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_3_25$sentiment == "1"))
results2014_3_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_3_25 <- cbind(tots, poss, negs, results2014_3_25)
results <- rbind(results, results2014_3_25)

rm(DF_2014_3_25_cleanESP, df_tweets, DF_2014_3_25, tots, negs, poss, results2014_3_25)

#February

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_2_25.RData")

classifier_glmnet(DF_2014_2_25_cleanESP)

DF_2014_2_25 <- df_tweets
colnames(DF_2014_2_25) <- c("text", "sentiment")

DF_2014_2_25$sentiment[DF_2014_2_25$sentiment <= 0.5] <- "0"
DF_2014_2_25$sentiment[DF_2014_2_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_2_25))
negs <- as.numeric(sum(DF_2014_2_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_2_25$sentiment == "1"))
results2014_2_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_2_25 <- cbind(tots, poss, negs, results2014_2_25)
results <- rbind(results, results2014_2_25)

rm(DF_2014_2_25_cleanESP, df_tweets, DF_2014_2_25, tots, negs, poss, results2014_2_25)

#January

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_1_25.RData")

classifier_glmnet(DF_2014_1_25_cleanESP)

DF_2014_1_25 <- df_tweets
colnames(DF_2014_1_25) <- c("text", "sentiment")

DF_2014_1_25$sentiment[DF_2014_1_25$sentiment <= 0.5] <- "0"
DF_2014_1_25$sentiment[DF_2014_1_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_1_25))
negs <- as.numeric(sum(DF_2014_1_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_1_25$sentiment == "1"))
results2014_1_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_1_25 <- cbind(tots, poss, negs, results2014_1_25)
results <- rbind(results, results2014_1_25)

rm(DF_2014_1_25_cleanESP, df_tweets, DF_2014_1_25, tots, negs, poss, results2014_1_25)

print("2013")
#December

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_12_25.RData")

classifier_glmnet(DF_2013_12_25_cleanESP)

DF_2013_12_25 <- df_tweets
colnames(DF_2013_12_25) <- c("text", "sentiment")

DF_2013_12_25$sentiment[DF_2013_12_25$sentiment <= 0.5] <- "0"
DF_2013_12_25$sentiment[DF_2013_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_12_25))
negs <- as.numeric(sum(DF_2013_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_12_25$sentiment == "1"))
results2013_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_12_25 <- cbind(tots, poss, negs, results2013_12_25)
results <- rbind(results, results2013_12_25)

rm(DF_2013_12_25_cleanESP, df_tweets, DF_2013_12_25, tots, negs, poss, results2013_12_25)

#November

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_11_25.RData")

classifier_glmnet(DF_2013_11_25_cleanESP)

DF_2013_11_25 <- df_tweets
colnames(DF_2013_11_25) <- c("text", "sentiment")

DF_2013_11_25$sentiment[DF_2013_11_25$sentiment <= 0.5] <- "0"
DF_2013_11_25$sentiment[DF_2013_11_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_11_25))
negs <- as.numeric(sum(DF_2013_11_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_11_25$sentiment == "1"))
results2013_11_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_11_25 <- cbind(tots, poss, negs, results2013_11_25)
results <- rbind(results, results2013_11_25)

rm(DF_2013_11_25_cleanESP, df_tweets, DF_2013_11_25, tots, negs, poss, results2013_11_25)

#October

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_10_25.RData")

classifier_glmnet(DF_2013_10_25_cleanESP)

DF_2013_10_25 <- df_tweets
colnames(DF_2013_10_25) <- c("text", "sentiment")

DF_2013_10_25$sentiment[DF_2013_10_25$sentiment <= 0.5] <- "0"
DF_2013_10_25$sentiment[DF_2013_10_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_10_25))
negs <- as.numeric(sum(DF_2013_10_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_10_25$sentiment == "1"))
results2013_10_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_10_25 <- cbind(tots, poss, negs, results2013_10_25)
results <- rbind(results, results2013_10_25)

rm(DF_2013_10_25_cleanESP, df_tweets, DF_2013_10_25, tots, negs, poss, results2013_10_25)

#September

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_9_25.RData")

classifier_glmnet(DF_2013_9_25_cleanESP)

DF_2013_9_25 <- df_tweets
colnames(DF_2013_9_25) <- c("text", "sentiment")

DF_2013_9_25$sentiment[DF_2013_9_25$sentiment <= 0.5] <- "0"
DF_2013_9_25$sentiment[DF_2013_9_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_9_25))
negs <- as.numeric(sum(DF_2013_9_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_9_25$sentiment == "1"))
results2013_9_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_9_25 <- cbind(tots, poss, negs, results2013_9_25)
results <- rbind(results, results2013_9_25)

rm(DF_2013_9_25_cleanESP, df_tweets, DF_2013_9_25, tots, negs, poss, results2013_9_25)

#August

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_8_25.RData")

classifier_glmnet(DF_2013_8_25_cleanESP)

DF_2013_8_25 <- df_tweets
colnames(DF_2013_8_25) <- c("text", "sentiment")

DF_2013_8_25$sentiment[DF_2013_8_25$sentiment <= 0.5] <- "0"
DF_2013_8_25$sentiment[DF_2013_8_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_8_25))
negs <- as.numeric(sum(DF_2013_8_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_8_25$sentiment == "1"))
results2013_8_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_8_25 <- cbind(tots, poss, negs, results2013_8_25)
results <- rbind(results, results2013_8_25)

rm(DF_2013_8_25_cleanESP, df_tweets, DF_2013_8_25, tots, negs, poss, results2013_8_25)

#July

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_12_25.RData")

classifier_glmnet(DF_2013_12_25_cleanESP)

DF_2013_12_25 <- df_tweets
colnames(DF_2013_12_25) <- c("text", "sentiment")

DF_2013_12_25$sentiment[DF_2013_12_25$sentiment <= 0.5] <- "0"
DF_2013_12_25$sentiment[DF_2013_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_12_25))
negs <- as.numeric(sum(DF_2013_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_12_25$sentiment == "1"))
results2013_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_12_25 <- cbind(tots, poss, negs, results2013_12_25)
results <- rbind(results, results2013_12_25)

rm(DF_2013_12_25_cleanESP, df_tweets, DF_2013_12_25, tots, negs, poss, results2013_12_25)

#June

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_6_25.RData")

classifier_glmnet(DF_2013_6_25_cleanESP)

DF_2013_6_25 <- df_tweets
colnames(DF_2013_6_25) <- c("text", "sentiment")

DF_2013_6_25$sentiment[DF_2013_6_25$sentiment <= 0.5] <- "0"
DF_2013_6_25$sentiment[DF_2013_6_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_6_25))
negs <- as.numeric(sum(DF_2013_6_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_6_25$sentiment == "1"))
results2013_6_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_6_25 <- cbind(tots, poss, negs, results2013_6_25)
results <- rbind(results, results2013_6_25)

rm(DF_2013_6_25_cleanESP, df_tweets, DF_2013_6_25, tots, negs, poss, results2013_6_25)

#May

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_5_25.RData")

classifier_glmnet(DF_2013_5_25_cleanESP)

DF_2013_5_25 <- df_tweets
colnames(DF_2013_5_25) <- c("text", "sentiment")

DF_2013_5_25$sentiment[DF_2013_5_25$sentiment <= 0.5] <- "0"
DF_2013_5_25$sentiment[DF_2013_5_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_5_25))
negs <- as.numeric(sum(DF_2013_5_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_5_25$sentiment == "1"))
results2013_5_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_5_25 <- cbind(tots, poss, negs, results2013_5_25)
results <- rbind(results, results2013_5_25)

rm(DF_2013_5_25_cleanESP, df_tweets, DF_2013_5_25, tots, negs, poss, results2013_5_25)

#April

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_4_25.RData")

classifier_glmnet(DF_2013_4_25_cleanESP)

DF_2013_4_25 <- df_tweets
colnames(DF_2013_4_25) <- c("text", "sentiment")

DF_2013_4_25$sentiment[DF_2013_4_25$sentiment <= 0.5] <- "0"
DF_2013_4_25$sentiment[DF_2013_4_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_4_25))
negs <- as.numeric(sum(DF_2013_4_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_4_25$sentiment == "1"))
results2013_4_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_4_25 <- cbind(tots, poss, negs, results2013_4_25)
results <- rbind(results, results2013_4_25)

rm(DF_2013_4_25_cleanESP, df_tweets, DF_2013_4_25, tots, negs, poss, results2013_4_25)

#March

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_3_25.RData")

classifier_glmnet(DF_2013_3_25_cleanESP)

DF_2013_3_25 <- df_tweets
colnames(DF_2013_3_25) <- c("text", "sentiment")

DF_2013_3_25$sentiment[DF_2013_3_25$sentiment <= 0.5] <- "0"
DF_2013_3_25$sentiment[DF_2013_3_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_3_25))
negs <- as.numeric(sum(DF_2013_3_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_3_25$sentiment == "1"))
results2013_3_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_3_25 <- cbind(tots, poss, negs, results2013_3_25)
results <- rbind(results, results2013_3_25)

rm(DF_2013_3_25_cleanESP, df_tweets, DF_2013_3_25, tots, negs, poss, results2013_3_25)

#February

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_2_25.RData")

classifier_glmnet(DF_2013_2_25_cleanESP)

DF_2013_2_25 <- df_tweets
colnames(DF_2013_2_25) <- c("text", "sentiment")

DF_2013_2_25$sentiment[DF_2013_2_25$sentiment <= 0.5] <- "0"
DF_2013_2_25$sentiment[DF_2013_2_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_2_25))
negs <- as.numeric(sum(DF_2013_2_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_2_25$sentiment == "1"))
results2013_2_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_2_25 <- cbind(tots, poss, negs, results2013_2_25)
results <- rbind(results, results2013_2_25)

rm(DF_2013_2_25_cleanESP, df_tweets, DF_2013_2_25, tots, negs, poss, results2013_2_25)

#January

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_1_25.RData")

classifier_glmnet(DF_2013_1_25_cleanESP)

DF_2013_1_25 <- df_tweets
colnames(DF_2013_1_25) <- c("text", "sentiment")

DF_2013_1_25$sentiment[DF_2013_1_25$sentiment <= 0.5] <- "0"
DF_2013_1_25$sentiment[DF_2013_1_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_1_25))
negs <- as.numeric(sum(DF_2013_1_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_1_25$sentiment == "1"))
results2013_1_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_1_25 <- cbind(tots, poss, negs, results2013_1_25)
results <- rbind(results, results2013_1_25)

rm(DF_2013_1_25_cleanESP, df_tweets, DF_2013_1_25, tots, negs, poss, results2013_1_25)

print("2012")
#December

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_12_25.RData")

classifier_glmnet(DF_2012_12_25_cleanESP)

DF_2012_12_25 <- df_tweets
colnames(DF_2012_12_25) <- c("text", "sentiment")

DF_2012_12_25$sentiment[DF_2012_12_25$sentiment <= 0.5] <- "0"
DF_2012_12_25$sentiment[DF_2012_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_12_25))
negs <- as.numeric(sum(DF_2012_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_12_25$sentiment == "1"))
results2012_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_12_25 <- cbind(tots, poss, negs, results2012_12_25)
results <- rbind(results, results2012_12_25)

rm(DF_2012_12_25_cleanESP, df_tweets, DF_2012_12_25, tots, negs, poss, results2012_12_25)

#November

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_11_25.RData")

classifier_glmnet(DF_2012_11_25_cleanESP)

DF_2012_11_25 <- df_tweets
colnames(DF_2012_11_25) <- c("text", "sentiment")

DF_2012_11_25$sentiment[DF_2012_11_25$sentiment <= 0.5] <- "0"
DF_2012_11_25$sentiment[DF_2012_11_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_11_25))
negs <- as.numeric(sum(DF_2012_11_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_11_25$sentiment == "1"))
results2012_11_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_11_25 <- cbind(tots, poss, negs, results2012_11_25)
results <- rbind(results, results2012_11_25)

rm(DF_2012_11_25_cleanESP, df_tweets, DF_2012_11_25, tots, negs, poss, results2012_11_25)

#October

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_10_25.RData")

classifier_glmnet(DF_2012_10_25_cleanESP)

DF_2012_10_25 <- df_tweets
colnames(DF_2012_10_25) <- c("text", "sentiment")

DF_2012_10_25$sentiment[DF_2012_10_25$sentiment <= 0.5] <- "0"
DF_2012_10_25$sentiment[DF_2012_10_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_10_25))
negs <- as.numeric(sum(DF_2012_10_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_10_25$sentiment == "1"))
results2012_10_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_10_25 <- cbind(tots, poss, negs, results2012_10_25)
results <- rbind(results, results2012_10_25)

rm(DF_2012_10_25_cleanESP, df_tweets, DF_2012_10_25, tots, negs, poss, results2012_10_25)

#September

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_9_25.RData")

classifier_glmnet(DF_2012_9_25_cleanESP)

DF_2012_9_25 <- df_tweets
colnames(DF_2012_9_25) <- c("text", "sentiment")

DF_2012_9_25$sentiment[DF_2012_9_25$sentiment <= 0.5] <- "0"
DF_2012_9_25$sentiment[DF_2012_9_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_9_25))
negs <- as.numeric(sum(DF_2012_9_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_9_25$sentiment == "1"))
results2012_9_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_9_25 <- cbind(tots, poss, negs, results2012_9_25)
results <- rbind(results, results2012_9_25)

rm(DF_2012_9_25_cleanESP, df_tweets, DF_2012_9_25, tots, negs, poss, results2012_9_25)

#August

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_8_25.RData")

classifier_glmnet(DF_2012_8_25_cleanESP)

DF_2012_8_25 <- df_tweets
colnames(DF_2012_8_25) <- c("text", "sentiment")

DF_2012_8_25$sentiment[DF_2012_8_25$sentiment <= 0.5] <- "0"
DF_2012_8_25$sentiment[DF_2012_8_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_8_25))
negs <- as.numeric(sum(DF_2012_8_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_8_25$sentiment == "1"))
results2012_8_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_8_25 <- cbind(tots, poss, negs, results2012_8_25)
results <- rbind(results, results2012_8_25)

rm(DF_2012_8_25_cleanESP, df_tweets, DF_2012_8_25, tots, negs, poss, results2012_8_25)

#July

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_12_25.RData")

classifier_glmnet(DF_2012_12_25_cleanESP)

DF_2012_12_25 <- df_tweets
colnames(DF_2012_12_25) <- c("text", "sentiment")

DF_2012_12_25$sentiment[DF_2012_12_25$sentiment <= 0.5] <- "0"
DF_2012_12_25$sentiment[DF_2012_12_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_12_25))
negs <- as.numeric(sum(DF_2012_12_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_12_25$sentiment == "1"))
results2012_12_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_12_25 <- cbind(tots, poss, negs, results2012_12_25)
results <- rbind(results, results2012_12_25)

rm(DF_2012_12_25_cleanESP, df_tweets, DF_2012_12_25, tots, negs, poss, results2012_12_25)

#June

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_6_25.RData")

classifier_glmnet(DF_2012_6_25_cleanESP)

DF_2012_6_25 <- df_tweets
colnames(DF_2012_6_25) <- c("text", "sentiment")

DF_2012_6_25$sentiment[DF_2012_6_25$sentiment <= 0.5] <- "0"
DF_2012_6_25$sentiment[DF_2012_6_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_6_25))
negs <- as.numeric(sum(DF_2012_6_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_6_25$sentiment == "1"))
results2012_6_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_6_25 <- cbind(tots, poss, negs, results2012_6_25)
results <- rbind(results, results2012_6_25)

rm(DF_2012_6_25_cleanESP, df_tweets, DF_2012_6_25, tots, negs, poss, results2012_6_25)

#May

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_5_25.RData")

classifier_glmnet(DF_2012_5_25_cleanESP)

DF_2012_5_25 <- df_tweets
colnames(DF_2012_5_25) <- c("text", "sentiment")

DF_2012_5_25$sentiment[DF_2012_5_25$sentiment <= 0.5] <- "0"
DF_2012_5_25$sentiment[DF_2012_5_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_5_25))
negs <- as.numeric(sum(DF_2012_5_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_5_25$sentiment == "1"))
results2012_5_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_5_25 <- cbind(tots, poss, negs, results2012_5_25)
results <- rbind(results, results2012_5_25)

rm(DF_2012_5_25_cleanESP, df_tweets, DF_2012_5_25, tots, negs, poss, results2012_5_25)

#April

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_4_25.RData")

classifier_glmnet(DF_2012_4_25_cleanESP)

DF_2012_4_25 <- df_tweets
colnames(DF_2012_4_25) <- c("text", "sentiment")

DF_2012_4_25$sentiment[DF_2012_4_25$sentiment <= 0.5] <- "0"
DF_2012_4_25$sentiment[DF_2012_4_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_4_25))
negs <- as.numeric(sum(DF_2012_4_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_4_25$sentiment == "1"))
results2012_4_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_4_25 <- cbind(tots, poss, negs, results2012_4_25)
results <- rbind(results, results2012_4_25)

rm(DF_2012_4_25_cleanESP, df_tweets, DF_2012_4_25, tots, negs, poss, results2012_4_25)

#March

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_3_25.RData")

classifier_glmnet(DF_2012_3_25_cleanESP)

DF_2012_3_25 <- df_tweets
colnames(DF_2012_3_25) <- c("text", "sentiment")

DF_2012_3_25$sentiment[DF_2012_3_25$sentiment <= 0.5] <- "0"
DF_2012_3_25$sentiment[DF_2012_3_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_3_25))
negs <- as.numeric(sum(DF_2012_3_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_3_25$sentiment == "1"))
results2012_3_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_3_25 <- cbind(tots, poss, negs, results2012_3_25)
results <- rbind(results, results2012_3_25)

rm(DF_2012_3_25_cleanESP, df_tweets, DF_2012_3_25, tots, negs, poss, results2012_3_25)

#February

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_2_25.RData")

classifier_glmnet(DF_2012_2_25_cleanESP)

DF_2012_2_25 <- df_tweets
colnames(DF_2012_2_25) <- c("text", "sentiment")

DF_2012_2_25$sentiment[DF_2012_2_25$sentiment <= 0.5] <- "0"
DF_2012_2_25$sentiment[DF_2012_2_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_2_25))
negs <- as.numeric(sum(DF_2012_2_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_2_25$sentiment == "1"))
results2012_2_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_2_25 <- cbind(tots, poss, negs, results2012_2_25)
results <- rbind(results, results2012_2_25)

rm(DF_2012_2_25_cleanESP, df_tweets, DF_2012_2_25, tots, negs, poss, results2012_2_25)

#January

load(file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_1_25.RData")

classifier_glmnet(DF_2012_1_25_cleanESP)

DF_2012_1_25 <- df_tweets
colnames(DF_2012_1_25) <- c("text", "sentiment")

DF_2012_1_25$sentiment[DF_2012_1_25$sentiment <= 0.5] <- "0"
DF_2012_1_25$sentiment[DF_2012_1_25$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_1_25))
negs <- as.numeric(sum(DF_2012_1_25$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_1_25$sentiment == "1"))
results2012_1_25 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_1_25 <- cbind(tots, poss, negs, results2012_1_25)
results25 <- rbind(results, results2012_1_25)

rm(DF_2012_1_25_cleanESP, df_tweets, DF_2012_1_25, tots, negs, poss, results2012_1_25)

save(results25, file = "Objects/Models/resultsF25.RData")

print(difftime(Sys.time(), t1, units = 'mins'))

