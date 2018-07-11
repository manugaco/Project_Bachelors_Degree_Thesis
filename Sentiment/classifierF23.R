

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

#Series 23
print("2017")
#December

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_12_23.RData")

classifier_glmnet(DF_2017_12_23_cleanESP)

DF_2017_12_23 <- df_tweets
colnames(DF_2017_12_23) <- c("text", "sentiment")

DF_2017_12_23$sentiment[DF_2017_12_23$sentiment <= 0.5] <- "0"
DF_2017_12_23$sentiment[DF_2017_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_12_23))
negs <- as.numeric(sum(DF_2017_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_12_23$sentiment == "1"))
results2017_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_12_23 <- cbind(tots, poss, negs, results2017_12_23)
results <- results2017_12_23

rm(DF_2017_12_23_cleanESP, df_tweets, DF_2017_12_23, tots, negs, poss, results2017_12_23)

#November

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_11_23.RData")

classifier_glmnet(DF_2017_11_23_cleanESP)

DF_2017_11_23 <- df_tweets
colnames(DF_2017_11_23) <- c("text", "sentiment")

DF_2017_11_23$sentiment[DF_2017_11_23$sentiment <= 0.5] <- "0"
DF_2017_11_23$sentiment[DF_2017_11_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_11_23))
negs <- as.numeric(sum(DF_2017_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_11_23$sentiment == "1"))
results2017_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_11_23 <- cbind(tots, poss, negs, results2017_11_23)
results <- rbind(results, results2017_11_23)

rm(DF_2017_11_23_cleanESP, df_tweets, DF_2017_11_23, tots, negs, poss, results2017_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_10_23.RData")

classifier_glmnet(DF_2017_10_23_cleanESP)

DF_2017_10_23 <- df_tweets
colnames(DF_2017_10_23) <- c("text", "sentiment")

DF_2017_10_23$sentiment[DF_2017_10_23$sentiment <= 0.5] <- "0"
DF_2017_10_23$sentiment[DF_2017_10_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_10_23))
negs <- as.numeric(sum(DF_2017_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_10_23$sentiment == "1"))
results2017_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_10_23 <- cbind(tots, poss, negs, results2017_10_23)
results <- rbind(results, results2017_10_23)

rm(DF_2017_10_23_cleanESP, df_tweets, DF_2017_10_23, tots, negs, poss, results2017_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_9_23.RData")

classifier_glmnet(DF_2017_9_23_cleanESP)

DF_2017_9_23 <- df_tweets
colnames(DF_2017_9_23) <- c("text", "sentiment")

DF_2017_9_23$sentiment[DF_2017_9_23$sentiment <= 0.5] <- "0"
DF_2017_9_23$sentiment[DF_2017_9_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_9_23))
negs <- as.numeric(sum(DF_2017_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_9_23$sentiment == "1"))
results2017_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_9_23 <- cbind(tots, poss, negs, results2017_9_23)
results <- rbind(results, results2017_9_23)

rm(DF_2017_9_23_cleanESP, df_tweets, DF_2017_9_23, tots, negs, poss, results2017_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_8_23.RData")

classifier_glmnet(DF_2017_8_23_cleanESP)

DF_2017_8_23 <- df_tweets
colnames(DF_2017_8_23) <- c("text", "sentiment")

DF_2017_8_23$sentiment[DF_2017_8_23$sentiment <= 0.5] <- "0"
DF_2017_8_23$sentiment[DF_2017_8_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_8_23))
negs <- as.numeric(sum(DF_2017_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_8_23$sentiment == "1"))
results2017_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_8_23 <- cbind(tots, poss, negs, results2017_8_23)
results <- rbind(results, results2017_8_23)

rm(DF_2017_8_23_cleanESP, df_tweets, DF_2017_8_23, tots, negs, poss, results2017_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_12_23.RData")

classifier_glmnet(DF_2017_12_23_cleanESP)

DF_2017_12_23 <- df_tweets
colnames(DF_2017_12_23) <- c("text", "sentiment")

DF_2017_12_23$sentiment[DF_2017_12_23$sentiment <= 0.5] <- "0"
DF_2017_12_23$sentiment[DF_2017_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_12_23))
negs <- as.numeric(sum(DF_2017_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_12_23$sentiment == "1"))
results2017_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_12_23 <- cbind(tots, poss, negs, results2017_12_23)
results <- rbind(results, results2017_12_23)

rm(DF_2017_12_23_cleanESP, df_tweets, DF_2017_12_23, tots, negs, poss, results2017_12_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_6_23.RData")

classifier_glmnet(DF_2017_6_23_cleanESP)

DF_2017_6_23 <- df_tweets
colnames(DF_2017_6_23) <- c("text", "sentiment")

DF_2017_6_23$sentiment[DF_2017_6_23$sentiment <= 0.5] <- "0"
DF_2017_6_23$sentiment[DF_2017_6_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_6_23))
negs <- as.numeric(sum(DF_2017_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_6_23$sentiment == "1"))
results2017_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_6_23 <- cbind(tots, poss, negs, results2017_6_23)
results <- rbind(results, results2017_6_23)

rm(DF_2017_6_23_cleanESP, df_tweets, DF_2017_6_23, tots, negs, poss, results2017_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_5_23.RData")

classifier_glmnet(DF_2017_5_23_cleanESP)

DF_2017_5_23 <- df_tweets
colnames(DF_2017_5_23) <- c("text", "sentiment")

DF_2017_5_23$sentiment[DF_2017_5_23$sentiment <= 0.5] <- "0"
DF_2017_5_23$sentiment[DF_2017_5_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_5_23))
negs <- as.numeric(sum(DF_2017_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_5_23$sentiment == "1"))
results2017_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_5_23 <- cbind(tots, poss, negs, results2017_5_23)
results <- rbind(results, results2017_5_23)

rm(DF_2017_5_23_cleanESP, df_tweets, DF_2017_5_23, tots, negs, poss, results2017_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_4_23.RData")

classifier_glmnet(DF_2017_4_23_cleanESP)

DF_2017_4_23 <- df_tweets
colnames(DF_2017_4_23) <- c("text", "sentiment")

DF_2017_4_23$sentiment[DF_2017_4_23$sentiment <= 0.5] <- "0"
DF_2017_4_23$sentiment[DF_2017_4_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_4_23))
negs <- as.numeric(sum(DF_2017_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_4_23$sentiment == "1"))
results2017_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_4_23 <- cbind(tots, poss, negs, results2017_4_23)
results <- rbind(results, results2017_4_23)

rm(DF_2017_4_23_cleanESP, df_tweets, DF_2017_4_23, tots, negs, poss, results2017_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_3_23.RData")

classifier_glmnet(DF_2017_3_23_cleanESP)

DF_2017_3_23 <- df_tweets
colnames(DF_2017_3_23) <- c("text", "sentiment")

DF_2017_3_23$sentiment[DF_2017_3_23$sentiment <= 0.5] <- "0"
DF_2017_3_23$sentiment[DF_2017_3_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_3_23))
negs <- as.numeric(sum(DF_2017_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_3_23$sentiment == "1"))
results2017_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_3_23 <- cbind(tots, poss, negs, results2017_3_23)
results <- rbind(results, results2017_3_23)

rm(DF_2017_3_23_cleanESP, df_tweets, DF_2017_3_23, tots, negs, poss, results2017_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_2_23.RData")

classifier_glmnet(DF_2017_2_23_cleanESP)

DF_2017_2_23 <- df_tweets
colnames(DF_2017_2_23) <- c("text", "sentiment")

DF_2017_2_23$sentiment[DF_2017_2_23$sentiment <= 0.5] <- "0"
DF_2017_2_23$sentiment[DF_2017_2_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_2_23))
negs <- as.numeric(sum(DF_2017_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_2_23$sentiment == "1"))
results2017_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_2_23 <- cbind(tots, poss, negs, results2017_2_23)
results <- rbind(results, results2017_2_23)

rm(DF_2017_2_23_cleanESP, df_tweets, DF_2017_2_23, tots, negs, poss, results2017_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2017_1_23.RData")

classifier_glmnet(DF_2017_1_23_cleanESP)

DF_2017_1_23 <- df_tweets
colnames(DF_2017_1_23) <- c("text", "sentiment")

DF_2017_1_23$sentiment[DF_2017_1_23$sentiment <= 0.5] <- "0"
DF_2017_1_23$sentiment[DF_2017_1_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_1_23))
negs <- as.numeric(sum(DF_2017_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_1_23$sentiment == "1"))
results2017_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_1_23 <- cbind(tots, poss, negs, results2017_1_23)
results <- rbind(results, results2017_1_23)

rm(DF_2017_1_23_cleanESP, df_tweets, DF_2017_1_23, tots, negs, poss, results2017_1_23)

print("2016")
#December

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_12_23.RData")

classifier_glmnet(DF_2016_12_23_cleanESP)

DF_2016_12_23 <- df_tweets
colnames(DF_2016_12_23) <- c("text", "sentiment")

DF_2016_12_23$sentiment[DF_2016_12_23$sentiment <= 0.5] <- "0"
DF_2016_12_23$sentiment[DF_2016_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_12_23))
negs <- as.numeric(sum(DF_2016_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_12_23$sentiment == "1"))
results2016_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_12_23 <- cbind(tots, poss, negs, results2016_12_23)
results <- rbind(results, results2016_12_23)

rm(DF_2016_12_23_cleanESP, df_tweets, DF_2016_12_23, tots, negs, poss, results2016_12_23)

#November

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_11_23.RData")

classifier_glmnet(DF_2016_11_23_cleanESP)

DF_2016_11_23 <- df_tweets
colnames(DF_2016_11_23) <- c("text", "sentiment")

DF_2016_11_23$sentiment[DF_2016_11_23$sentiment <= 0.5] <- "0"
DF_2016_11_23$sentiment[DF_2016_11_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_11_23))
negs <- as.numeric(sum(DF_2016_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_11_23$sentiment == "1"))
results2016_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_11_23 <- cbind(tots, poss, negs, results2016_11_23)
results <- rbind(results, results2016_11_23)

rm(DF_2016_11_23_cleanESP, df_tweets, DF_2016_11_23, tots, negs, poss, results2016_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_10_23.RData")

classifier_glmnet(DF_2016_10_23_cleanESP)

DF_2016_10_23 <- df_tweets
colnames(DF_2016_10_23) <- c("text", "sentiment")

DF_2016_10_23$sentiment[DF_2016_10_23$sentiment <= 0.5] <- "0"
DF_2016_10_23$sentiment[DF_2016_10_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_10_23))
negs <- as.numeric(sum(DF_2016_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_10_23$sentiment == "1"))
results2016_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_10_23 <- cbind(tots, poss, negs, results2016_10_23)
results <- rbind(results, results2016_10_23)

rm(DF_2016_10_23_cleanESP, df_tweets, DF_2016_10_23, tots, negs, poss, results2016_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_9_23.RData")

classifier_glmnet(DF_2016_9_23_cleanESP)

DF_2016_9_23 <- df_tweets
colnames(DF_2016_9_23) <- c("text", "sentiment")

DF_2016_9_23$sentiment[DF_2016_9_23$sentiment <= 0.5] <- "0"
DF_2016_9_23$sentiment[DF_2016_9_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_9_23))
negs <- as.numeric(sum(DF_2016_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_9_23$sentiment == "1"))
results2016_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_9_23 <- cbind(tots, poss, negs, results2016_9_23)
results <- rbind(results, results2016_9_23)

rm(DF_2016_9_23_cleanESP, df_tweets, DF_2016_9_23, tots, negs, poss, results2016_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_8_23.RData")

classifier_glmnet(DF_2016_8_23_cleanESP)

DF_2016_8_23 <- df_tweets
colnames(DF_2016_8_23) <- c("text", "sentiment")

DF_2016_8_23$sentiment[DF_2016_8_23$sentiment <= 0.5] <- "0"
DF_2016_8_23$sentiment[DF_2016_8_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_8_23))
negs <- as.numeric(sum(DF_2016_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_8_23$sentiment == "1"))
results2016_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_8_23 <- cbind(tots, poss, negs, results2016_8_23)
results <- rbind(results, results2016_8_23)

rm(DF_2016_8_23_cleanESP, df_tweets, DF_2016_8_23, tots, negs, poss, results2016_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_12_23.RData")

classifier_glmnet(DF_2016_12_23_cleanESP)

DF_2016_12_23 <- df_tweets
colnames(DF_2016_12_23) <- c("text", "sentiment")

DF_2016_12_23$sentiment[DF_2016_12_23$sentiment <= 0.5] <- "0"
DF_2016_12_23$sentiment[DF_2016_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_12_23))
negs <- as.numeric(sum(DF_2016_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_12_23$sentiment == "1"))
results2016_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_12_23 <- cbind(tots, poss, negs, results2016_12_23)
results <- rbind(results, results2016_12_23)

rm(DF_2016_12_23_cleanESP, df_tweets, DF_2016_12_23, tots, negs, poss, results2016_12_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_6_23.RData")

classifier_glmnet(DF_2016_6_23_cleanESP)

DF_2016_6_23 <- df_tweets
colnames(DF_2016_6_23) <- c("text", "sentiment")

DF_2016_6_23$sentiment[DF_2016_6_23$sentiment <= 0.5] <- "0"
DF_2016_6_23$sentiment[DF_2016_6_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_6_23))
negs <- as.numeric(sum(DF_2016_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_6_23$sentiment == "1"))
results2016_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_6_23 <- cbind(tots, poss, negs, results2016_6_23)
results <- rbind(results, results2016_6_23)

rm(DF_2016_6_23_cleanESP, df_tweets, DF_2016_6_23, tots, negs, poss, results2016_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_5_23.RData")

classifier_glmnet(DF_2016_5_23_cleanESP)

DF_2016_5_23 <- df_tweets
colnames(DF_2016_5_23) <- c("text", "sentiment")

DF_2016_5_23$sentiment[DF_2016_5_23$sentiment <= 0.5] <- "0"
DF_2016_5_23$sentiment[DF_2016_5_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_5_23))
negs <- as.numeric(sum(DF_2016_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_5_23$sentiment == "1"))
results2016_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_5_23 <- cbind(tots, poss, negs, results2016_5_23)
results <- rbind(results, results2016_5_23)

rm(DF_2016_5_23_cleanESP, df_tweets, DF_2016_5_23, tots, negs, poss, results2016_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_4_23.RData")

classifier_glmnet(DF_2016_4_23_cleanESP)

DF_2016_4_23 <- df_tweets
colnames(DF_2016_4_23) <- c("text", "sentiment")

DF_2016_4_23$sentiment[DF_2016_4_23$sentiment <= 0.5] <- "0"
DF_2016_4_23$sentiment[DF_2016_4_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_4_23))
negs <- as.numeric(sum(DF_2016_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_4_23$sentiment == "1"))
results2016_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_4_23 <- cbind(tots, poss, negs, results2016_4_23)
results <- rbind(results, results2016_4_23)

rm(DF_2016_4_23_cleanESP, df_tweets, DF_2016_4_23, tots, negs, poss, results2016_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_3_23.RData")

classifier_glmnet(DF_2016_3_23_cleanESP)

DF_2016_3_23 <- df_tweets
colnames(DF_2016_3_23) <- c("text", "sentiment")

DF_2016_3_23$sentiment[DF_2016_3_23$sentiment <= 0.5] <- "0"
DF_2016_3_23$sentiment[DF_2016_3_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_3_23))
negs <- as.numeric(sum(DF_2016_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_3_23$sentiment == "1"))
results2016_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_3_23 <- cbind(tots, poss, negs, results2016_3_23)
results <- rbind(results, results2016_3_23)

rm(DF_2016_3_23_cleanESP, df_tweets, DF_2016_3_23, tots, negs, poss, results2016_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_2_23.RData")

classifier_glmnet(DF_2016_2_23_cleanESP)

DF_2016_2_23 <- df_tweets
colnames(DF_2016_2_23) <- c("text", "sentiment")

DF_2016_2_23$sentiment[DF_2016_2_23$sentiment <= 0.5] <- "0"
DF_2016_2_23$sentiment[DF_2016_2_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_2_23))
negs <- as.numeric(sum(DF_2016_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_2_23$sentiment == "1"))
results2016_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_2_23 <- cbind(tots, poss, negs, results2016_2_23)
results <- rbind(results, results2016_2_23)

rm(DF_2016_2_23_cleanESP, df_tweets, DF_2016_2_23, tots, negs, poss, results2016_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2016_1_23.RData")

classifier_glmnet(DF_2016_1_23_cleanESP)

DF_2016_1_23 <- df_tweets
colnames(DF_2016_1_23) <- c("text", "sentiment")

DF_2016_1_23$sentiment[DF_2016_1_23$sentiment <= 0.5] <- "0"
DF_2016_1_23$sentiment[DF_2016_1_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_1_23))
negs <- as.numeric(sum(DF_2016_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_1_23$sentiment == "1"))
results2016_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_1_23 <- cbind(tots, poss, negs, results2016_1_23)
results <- rbind(results, results2016_1_23)

rm(DF_2016_1_23_cleanESP, df_tweets, DF_2016_1_23, tots, negs, poss, results2016_1_23)

print("2015")
#December

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_12_23.RData")

classifier_glmnet(DF_2015_12_23_cleanESP)

DF_2015_12_23 <- df_tweets
colnames(DF_2015_12_23) <- c("text", "sentiment")

DF_2015_12_23$sentiment[DF_2015_12_23$sentiment <= 0.5] <- "0"
DF_2015_12_23$sentiment[DF_2015_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_12_23))
negs <- as.numeric(sum(DF_2015_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_12_23$sentiment == "1"))
results2015_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_12_23 <- cbind(tots, poss, negs, results2015_12_23)
results <- rbind(results, results2015_12_23)

rm(DF_2015_12_23_cleanESP, df_tweets, DF_2015_12_23, tots, negs, poss, results2015_12_23)

#November

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_11_23.RData")

classifier_glmnet(DF_2015_11_23_cleanESP)

DF_2015_11_23 <- df_tweets
colnames(DF_2015_11_23) <- c("text", "sentiment")

DF_2015_11_23$sentiment[DF_2015_11_23$sentiment <= 0.5] <- "0"
DF_2015_11_23$sentiment[DF_2015_11_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_11_23))
negs <- as.numeric(sum(DF_2015_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_11_23$sentiment == "1"))
results2015_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_11_23 <- cbind(tots, poss, negs, results2015_11_23)
results <- rbind(results, results2015_11_23)

rm(DF_2015_11_23_cleanESP, df_tweets, DF_2015_11_23, tots, negs, poss, results2015_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_10_23.RData")

classifier_glmnet(DF_2015_10_23_cleanESP)

DF_2015_10_23 <- df_tweets
colnames(DF_2015_10_23) <- c("text", "sentiment")

DF_2015_10_23$sentiment[DF_2015_10_23$sentiment <= 0.5] <- "0"
DF_2015_10_23$sentiment[DF_2015_10_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_10_23))
negs <- as.numeric(sum(DF_2015_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_10_23$sentiment == "1"))
results2015_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_10_23 <- cbind(tots, poss, negs, results2015_10_23)
results <- rbind(results, results2015_10_23)

rm(DF_2015_10_23_cleanESP, df_tweets, DF_2015_10_23, tots, negs, poss, results2015_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_9_23.RData")

classifier_glmnet(DF_2015_9_23_cleanESP)

DF_2015_9_23 <- df_tweets
colnames(DF_2015_9_23) <- c("text", "sentiment")

DF_2015_9_23$sentiment[DF_2015_9_23$sentiment <= 0.5] <- "0"
DF_2015_9_23$sentiment[DF_2015_9_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_9_23))
negs <- as.numeric(sum(DF_2015_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_9_23$sentiment == "1"))
results2015_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_9_23 <- cbind(tots, poss, negs, results2015_9_23)
results <- rbind(results, results2015_9_23)

rm(DF_2015_9_23_cleanESP, df_tweets, DF_2015_9_23, tots, negs, poss, results2015_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_8_23.RData")

classifier_glmnet(DF_2015_8_23_cleanESP)

DF_2015_8_23 <- df_tweets
colnames(DF_2015_8_23) <- c("text", "sentiment")

DF_2015_8_23$sentiment[DF_2015_8_23$sentiment <= 0.5] <- "0"
DF_2015_8_23$sentiment[DF_2015_8_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_8_23))
negs <- as.numeric(sum(DF_2015_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_8_23$sentiment == "1"))
results2015_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_8_23 <- cbind(tots, poss, negs, results2015_8_23)
results <- rbind(results, results2015_8_23)

rm(DF_2015_8_23_cleanESP, df_tweets, DF_2015_8_23, tots, negs, poss, results2015_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_12_23.RData")

classifier_glmnet(DF_2015_12_23_cleanESP)

DF_2015_12_23 <- df_tweets
colnames(DF_2015_12_23) <- c("text", "sentiment")

DF_2015_12_23$sentiment[DF_2015_12_23$sentiment <= 0.5] <- "0"
DF_2015_12_23$sentiment[DF_2015_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_12_23))
negs <- as.numeric(sum(DF_2015_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_12_23$sentiment == "1"))
results2015_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_12_23 <- cbind(tots, poss, negs, results2015_12_23)
results <- rbind(results, results2015_12_23)

rm(DF_2015_12_23_cleanESP, df_tweets, DF_2015_12_23, tots, negs, poss, results2015_12_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_6_23.RData")

classifier_glmnet(DF_2015_6_23_cleanESP)

DF_2015_6_23 <- df_tweets
colnames(DF_2015_6_23) <- c("text", "sentiment")

DF_2015_6_23$sentiment[DF_2015_6_23$sentiment <= 0.5] <- "0"
DF_2015_6_23$sentiment[DF_2015_6_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_6_23))
negs <- as.numeric(sum(DF_2015_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_6_23$sentiment == "1"))
results2015_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_6_23 <- cbind(tots, poss, negs, results2015_6_23)
results <- rbind(results, results2015_6_23)

rm(DF_2015_6_23_cleanESP, df_tweets, DF_2015_6_23, tots, negs, poss, results2015_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_5_23.RData")

classifier_glmnet(DF_2015_5_23_cleanESP)

DF_2015_5_23 <- df_tweets
colnames(DF_2015_5_23) <- c("text", "sentiment")

DF_2015_5_23$sentiment[DF_2015_5_23$sentiment <= 0.5] <- "0"
DF_2015_5_23$sentiment[DF_2015_5_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_5_23))
negs <- as.numeric(sum(DF_2015_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_5_23$sentiment == "1"))
results2015_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_5_23 <- cbind(tots, poss, negs, results2015_5_23)
results <- rbind(results, results2015_5_23)

rm(DF_2015_5_23_cleanESP, df_tweets, DF_2015_5_23, tots, negs, poss, results2015_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_4_23.RData")

classifier_glmnet(DF_2015_4_23_cleanESP)

DF_2015_4_23 <- df_tweets
colnames(DF_2015_4_23) <- c("text", "sentiment")

DF_2015_4_23$sentiment[DF_2015_4_23$sentiment <= 0.5] <- "0"
DF_2015_4_23$sentiment[DF_2015_4_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_4_23))
negs <- as.numeric(sum(DF_2015_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_4_23$sentiment == "1"))
results2015_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_4_23 <- cbind(tots, poss, negs, results2015_4_23)
results <- rbind(results, results2015_4_23)

rm(DF_2015_4_23_cleanESP, df_tweets, DF_2015_4_23, tots, negs, poss, results2015_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_3_23.RData")

classifier_glmnet(DF_2015_3_23_cleanESP)

DF_2015_3_23 <- df_tweets
colnames(DF_2015_3_23) <- c("text", "sentiment")

DF_2015_3_23$sentiment[DF_2015_3_23$sentiment <= 0.5] <- "0"
DF_2015_3_23$sentiment[DF_2015_3_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_3_23))
negs <- as.numeric(sum(DF_2015_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_3_23$sentiment == "1"))
results2015_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_3_23 <- cbind(tots, poss, negs, results2015_3_23)
results <- rbind(results, results2015_3_23)

rm(DF_2015_3_23_cleanESP, df_tweets, DF_2015_3_23, tots, negs, poss, results2015_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_2_23.RData")

classifier_glmnet(DF_2015_2_23_cleanESP)

DF_2015_2_23 <- df_tweets
colnames(DF_2015_2_23) <- c("text", "sentiment")

DF_2015_2_23$sentiment[DF_2015_2_23$sentiment <= 0.5] <- "0"
DF_2015_2_23$sentiment[DF_2015_2_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_2_23))
negs <- as.numeric(sum(DF_2015_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_2_23$sentiment == "1"))
results2015_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_2_23 <- cbind(tots, poss, negs, results2015_2_23)
results <- rbind(results, results2015_2_23)

rm(DF_2015_2_23_cleanESP, df_tweets, DF_2015_2_23, tots, negs, poss, results2015_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2015_1_23.RData")

classifier_glmnet(DF_2015_1_23_cleanESP)

DF_2015_1_23 <- df_tweets
colnames(DF_2015_1_23) <- c("text", "sentiment")

DF_2015_1_23$sentiment[DF_2015_1_23$sentiment <= 0.5] <- "0"
DF_2015_1_23$sentiment[DF_2015_1_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_1_23))
negs <- as.numeric(sum(DF_2015_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_1_23$sentiment == "1"))
results2015_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_1_23 <- cbind(tots, poss, negs, results2015_1_23)
results <- rbind(results, results2015_1_23)

rm(DF_2015_1_23_cleanESP, df_tweets, DF_2015_1_23, tots, negs, poss, results2015_1_23)

print("2014")
#December

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_12_23.RData")

classifier_glmnet(DF_2014_12_23_cleanESP)

DF_2014_12_23 <- df_tweets
colnames(DF_2014_12_23) <- c("text", "sentiment")

DF_2014_12_23$sentiment[DF_2014_12_23$sentiment <= 0.5] <- "0"
DF_2014_12_23$sentiment[DF_2014_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_12_23))
negs <- as.numeric(sum(DF_2014_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_12_23$sentiment == "1"))
results2014_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_12_23 <- cbind(tots, poss, negs, results2014_12_23)
results <- rbind(results, results2014_12_23)

rm(DF_2014_12_23_cleanESP, df_tweets, DF_2014_12_23, tots, negs, poss, results2014_12_23)

#November

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_11_23.RData")

classifier_glmnet(DF_2014_11_23_cleanESP)

DF_2014_11_23 <- df_tweets
colnames(DF_2014_11_23) <- c("text", "sentiment")

DF_2014_11_23$sentiment[DF_2014_11_23$sentiment <= 0.5] <- "0"
DF_2014_11_23$sentiment[DF_2014_11_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_11_23))
negs <- as.numeric(sum(DF_2014_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_11_23$sentiment == "1"))
results2014_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_11_23 <- cbind(tots, poss, negs, results2014_11_23)
results <- rbind(results, results2014_11_23)

rm(DF_2014_11_23_cleanESP, df_tweets, DF_2014_11_23, tots, negs, poss, results2014_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_10_23.RData")

classifier_glmnet(DF_2014_10_23_cleanESP)

DF_2014_10_23 <- df_tweets
colnames(DF_2014_10_23) <- c("text", "sentiment")

DF_2014_10_23$sentiment[DF_2014_10_23$sentiment <= 0.5] <- "0"
DF_2014_10_23$sentiment[DF_2014_10_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_10_23))
negs <- as.numeric(sum(DF_2014_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_10_23$sentiment == "1"))
results2014_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_10_23 <- cbind(tots, poss, negs, results2014_10_23)
results <- rbind(results, results2014_10_23)

rm(DF_2014_10_23_cleanESP, df_tweets, DF_2014_10_23, tots, negs, poss, results2014_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_9_23.RData")

classifier_glmnet(DF_2014_9_23_cleanESP)

DF_2014_9_23 <- df_tweets
colnames(DF_2014_9_23) <- c("text", "sentiment")

DF_2014_9_23$sentiment[DF_2014_9_23$sentiment <= 0.5] <- "0"
DF_2014_9_23$sentiment[DF_2014_9_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_9_23))
negs <- as.numeric(sum(DF_2014_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_9_23$sentiment == "1"))
results2014_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_9_23 <- cbind(tots, poss, negs, results2014_9_23)
results <- rbind(results, results2014_9_23)

rm(DF_2014_9_23_cleanESP, df_tweets, DF_2014_9_23, tots, negs, poss, results2014_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_8_23.RData")

classifier_glmnet(DF_2014_8_23_cleanESP)

DF_2014_8_23 <- df_tweets
colnames(DF_2014_8_23) <- c("text", "sentiment")

DF_2014_8_23$sentiment[DF_2014_8_23$sentiment <= 0.5] <- "0"
DF_2014_8_23$sentiment[DF_2014_8_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_8_23))
negs <- as.numeric(sum(DF_2014_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_8_23$sentiment == "1"))
results2014_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_8_23 <- cbind(tots, poss, negs, results2014_8_23)
results <- rbind(results, results2014_8_23)

rm(DF_2014_8_23_cleanESP, df_tweets, DF_2014_8_23, tots, negs, poss, results2014_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_12_23.RData")

classifier_glmnet(DF_2014_12_23_cleanESP)

DF_2014_12_23 <- df_tweets
colnames(DF_2014_12_23) <- c("text", "sentiment")

DF_2014_12_23$sentiment[DF_2014_12_23$sentiment <= 0.5] <- "0"
DF_2014_12_23$sentiment[DF_2014_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_12_23))
negs <- as.numeric(sum(DF_2014_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_12_23$sentiment == "1"))
results2014_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_12_23 <- cbind(tots, poss, negs, results2014_12_23)
results <- rbind(results, results2014_12_23)

rm(DF_2014_12_23_cleanESP, df_tweets, DF_2014_12_23, tots, negs, poss, results2014_12_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_6_23.RData")

classifier_glmnet(DF_2014_6_23_cleanESP)

DF_2014_6_23 <- df_tweets
colnames(DF_2014_6_23) <- c("text", "sentiment")

DF_2014_6_23$sentiment[DF_2014_6_23$sentiment <= 0.5] <- "0"
DF_2014_6_23$sentiment[DF_2014_6_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_6_23))
negs <- as.numeric(sum(DF_2014_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_6_23$sentiment == "1"))
results2014_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_6_23 <- cbind(tots, poss, negs, results2014_6_23)
results <- rbind(results, results2014_6_23)

rm(DF_2014_6_23_cleanESP, df_tweets, DF_2014_6_23, tots, negs, poss, results2014_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_5_23.RData")

classifier_glmnet(DF_2014_5_23_cleanESP)

DF_2014_5_23 <- df_tweets
colnames(DF_2014_5_23) <- c("text", "sentiment")

DF_2014_5_23$sentiment[DF_2014_5_23$sentiment <= 0.5] <- "0"
DF_2014_5_23$sentiment[DF_2014_5_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_5_23))
negs <- as.numeric(sum(DF_2014_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_5_23$sentiment == "1"))
results2014_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_5_23 <- cbind(tots, poss, negs, results2014_5_23)
results <- rbind(results, results2014_5_23)

rm(DF_2014_5_23_cleanESP, df_tweets, DF_2014_5_23, tots, negs, poss, results2014_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_4_23.RData")

classifier_glmnet(DF_2014_4_23_cleanESP)

DF_2014_4_23 <- df_tweets
colnames(DF_2014_4_23) <- c("text", "sentiment")

DF_2014_4_23$sentiment[DF_2014_4_23$sentiment <= 0.5] <- "0"
DF_2014_4_23$sentiment[DF_2014_4_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_4_23))
negs <- as.numeric(sum(DF_2014_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_4_23$sentiment == "1"))
results2014_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_4_23 <- cbind(tots, poss, negs, results2014_4_23)
results <- rbind(results, results2014_4_23)

rm(DF_2014_4_23_cleanESP, df_tweets, DF_2014_4_23, tots, negs, poss, results2014_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_3_23.RData")

classifier_glmnet(DF_2014_3_23_cleanESP)

DF_2014_3_23 <- df_tweets
colnames(DF_2014_3_23) <- c("text", "sentiment")

DF_2014_3_23$sentiment[DF_2014_3_23$sentiment <= 0.5] <- "0"
DF_2014_3_23$sentiment[DF_2014_3_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_3_23))
negs <- as.numeric(sum(DF_2014_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_3_23$sentiment == "1"))
results2014_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_3_23 <- cbind(tots, poss, negs, results2014_3_23)
results <- rbind(results, results2014_3_23)

rm(DF_2014_3_23_cleanESP, df_tweets, DF_2014_3_23, tots, negs, poss, results2014_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_2_23.RData")

classifier_glmnet(DF_2014_2_23_cleanESP)

DF_2014_2_23 <- df_tweets
colnames(DF_2014_2_23) <- c("text", "sentiment")

DF_2014_2_23$sentiment[DF_2014_2_23$sentiment <= 0.5] <- "0"
DF_2014_2_23$sentiment[DF_2014_2_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_2_23))
negs <- as.numeric(sum(DF_2014_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_2_23$sentiment == "1"))
results2014_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_2_23 <- cbind(tots, poss, negs, results2014_2_23)
results <- rbind(results, results2014_2_23)

rm(DF_2014_2_23_cleanESP, df_tweets, DF_2014_2_23, tots, negs, poss, results2014_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2014_1_23.RData")

classifier_glmnet(DF_2014_1_23_cleanESP)

DF_2014_1_23 <- df_tweets
colnames(DF_2014_1_23) <- c("text", "sentiment")

DF_2014_1_23$sentiment[DF_2014_1_23$sentiment <= 0.5] <- "0"
DF_2014_1_23$sentiment[DF_2014_1_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_1_23))
negs <- as.numeric(sum(DF_2014_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_1_23$sentiment == "1"))
results2014_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_1_23 <- cbind(tots, poss, negs, results2014_1_23)
results <- rbind(results, results2014_1_23)

rm(DF_2014_1_23_cleanESP, df_tweets, DF_2014_1_23, tots, negs, poss, results2014_1_23)

print("2013")
#December

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_12_23.RData")

classifier_glmnet(DF_2013_12_23_cleanESP)

DF_2013_12_23 <- df_tweets
colnames(DF_2013_12_23) <- c("text", "sentiment")

DF_2013_12_23$sentiment[DF_2013_12_23$sentiment <= 0.5] <- "0"
DF_2013_12_23$sentiment[DF_2013_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_12_23))
negs <- as.numeric(sum(DF_2013_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_12_23$sentiment == "1"))
results2013_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_12_23 <- cbind(tots, poss, negs, results2013_12_23)
results <- rbind(results, results2013_12_23)

rm(DF_2013_12_23_cleanESP, df_tweets, DF_2013_12_23, tots, negs, poss, results2013_12_23)

#November

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_11_23.RData")

classifier_glmnet(DF_2013_11_23_cleanESP)

DF_2013_11_23 <- df_tweets
colnames(DF_2013_11_23) <- c("text", "sentiment")

DF_2013_11_23$sentiment[DF_2013_11_23$sentiment <= 0.5] <- "0"
DF_2013_11_23$sentiment[DF_2013_11_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_11_23))
negs <- as.numeric(sum(DF_2013_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_11_23$sentiment == "1"))
results2013_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_11_23 <- cbind(tots, poss, negs, results2013_11_23)
results <- rbind(results, results2013_11_23)

rm(DF_2013_11_23_cleanESP, df_tweets, DF_2013_11_23, tots, negs, poss, results2013_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_10_23.RData")

classifier_glmnet(DF_2013_10_23_cleanESP)

DF_2013_10_23 <- df_tweets
colnames(DF_2013_10_23) <- c("text", "sentiment")

DF_2013_10_23$sentiment[DF_2013_10_23$sentiment <= 0.5] <- "0"
DF_2013_10_23$sentiment[DF_2013_10_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_10_23))
negs <- as.numeric(sum(DF_2013_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_10_23$sentiment == "1"))
results2013_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_10_23 <- cbind(tots, poss, negs, results2013_10_23)
results <- rbind(results, results2013_10_23)

rm(DF_2013_10_23_cleanESP, df_tweets, DF_2013_10_23, tots, negs, poss, results2013_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_9_23.RData")

classifier_glmnet(DF_2013_9_23_cleanESP)

DF_2013_9_23 <- df_tweets
colnames(DF_2013_9_23) <- c("text", "sentiment")

DF_2013_9_23$sentiment[DF_2013_9_23$sentiment <= 0.5] <- "0"
DF_2013_9_23$sentiment[DF_2013_9_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_9_23))
negs <- as.numeric(sum(DF_2013_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_9_23$sentiment == "1"))
results2013_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_9_23 <- cbind(tots, poss, negs, results2013_9_23)
results <- rbind(results, results2013_9_23)

rm(DF_2013_9_23_cleanESP, df_tweets, DF_2013_9_23, tots, negs, poss, results2013_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_8_23.RData")

classifier_glmnet(DF_2013_8_23_cleanESP)

DF_2013_8_23 <- df_tweets
colnames(DF_2013_8_23) <- c("text", "sentiment")

DF_2013_8_23$sentiment[DF_2013_8_23$sentiment <= 0.5] <- "0"
DF_2013_8_23$sentiment[DF_2013_8_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_8_23))
negs <- as.numeric(sum(DF_2013_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_8_23$sentiment == "1"))
results2013_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_8_23 <- cbind(tots, poss, negs, results2013_8_23)
results <- rbind(results, results2013_8_23)

rm(DF_2013_8_23_cleanESP, df_tweets, DF_2013_8_23, tots, negs, poss, results2013_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_12_23.RData")

classifier_glmnet(DF_2013_12_23_cleanESP)

DF_2013_12_23 <- df_tweets
colnames(DF_2013_12_23) <- c("text", "sentiment")

DF_2013_12_23$sentiment[DF_2013_12_23$sentiment <= 0.5] <- "0"
DF_2013_12_23$sentiment[DF_2013_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_12_23))
negs <- as.numeric(sum(DF_2013_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_12_23$sentiment == "1"))
results2013_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_12_23 <- cbind(tots, poss, negs, results2013_12_23)
results <- rbind(results, results2013_12_23)

rm(DF_2013_12_23_cleanESP, df_tweets, DF_2013_12_23, tots, negs, poss, results2013_12_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_6_23.RData")

classifier_glmnet(DF_2013_6_23_cleanESP)

DF_2013_6_23 <- df_tweets
colnames(DF_2013_6_23) <- c("text", "sentiment")

DF_2013_6_23$sentiment[DF_2013_6_23$sentiment <= 0.5] <- "0"
DF_2013_6_23$sentiment[DF_2013_6_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_6_23))
negs <- as.numeric(sum(DF_2013_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_6_23$sentiment == "1"))
results2013_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_6_23 <- cbind(tots, poss, negs, results2013_6_23)
results <- rbind(results, results2013_6_23)

rm(DF_2013_6_23_cleanESP, df_tweets, DF_2013_6_23, tots, negs, poss, results2013_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_5_23.RData")

classifier_glmnet(DF_2013_5_23_cleanESP)

DF_2013_5_23 <- df_tweets
colnames(DF_2013_5_23) <- c("text", "sentiment")

DF_2013_5_23$sentiment[DF_2013_5_23$sentiment <= 0.5] <- "0"
DF_2013_5_23$sentiment[DF_2013_5_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_5_23))
negs <- as.numeric(sum(DF_2013_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_5_23$sentiment == "1"))
results2013_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_5_23 <- cbind(tots, poss, negs, results2013_5_23)
results <- rbind(results, results2013_5_23)

rm(DF_2013_5_23_cleanESP, df_tweets, DF_2013_5_23, tots, negs, poss, results2013_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_4_23.RData")

classifier_glmnet(DF_2013_4_23_cleanESP)

DF_2013_4_23 <- df_tweets
colnames(DF_2013_4_23) <- c("text", "sentiment")

DF_2013_4_23$sentiment[DF_2013_4_23$sentiment <= 0.5] <- "0"
DF_2013_4_23$sentiment[DF_2013_4_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_4_23))
negs <- as.numeric(sum(DF_2013_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_4_23$sentiment == "1"))
results2013_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_4_23 <- cbind(tots, poss, negs, results2013_4_23)
results <- rbind(results, results2013_4_23)

rm(DF_2013_4_23_cleanESP, df_tweets, DF_2013_4_23, tots, negs, poss, results2013_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_3_23.RData")

classifier_glmnet(DF_2013_3_23_cleanESP)

DF_2013_3_23 <- df_tweets
colnames(DF_2013_3_23) <- c("text", "sentiment")

DF_2013_3_23$sentiment[DF_2013_3_23$sentiment <= 0.5] <- "0"
DF_2013_3_23$sentiment[DF_2013_3_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_3_23))
negs <- as.numeric(sum(DF_2013_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_3_23$sentiment == "1"))
results2013_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_3_23 <- cbind(tots, poss, negs, results2013_3_23)
results <- rbind(results, results2013_3_23)

rm(DF_2013_3_23_cleanESP, df_tweets, DF_2013_3_23, tots, negs, poss, results2013_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_2_23.RData")

classifier_glmnet(DF_2013_2_23_cleanESP)

DF_2013_2_23 <- df_tweets
colnames(DF_2013_2_23) <- c("text", "sentiment")

DF_2013_2_23$sentiment[DF_2013_2_23$sentiment <= 0.5] <- "0"
DF_2013_2_23$sentiment[DF_2013_2_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_2_23))
negs <- as.numeric(sum(DF_2013_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_2_23$sentiment == "1"))
results2013_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_2_23 <- cbind(tots, poss, negs, results2013_2_23)
results <- rbind(results, results2013_2_23)

rm(DF_2013_2_23_cleanESP, df_tweets, DF_2013_2_23, tots, negs, poss, results2013_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2013_1_23.RData")

classifier_glmnet(DF_2013_1_23_cleanESP)

DF_2013_1_23 <- df_tweets
colnames(DF_2013_1_23) <- c("text", "sentiment")

DF_2013_1_23$sentiment[DF_2013_1_23$sentiment <= 0.5] <- "0"
DF_2013_1_23$sentiment[DF_2013_1_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_1_23))
negs <- as.numeric(sum(DF_2013_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_1_23$sentiment == "1"))
results2013_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_1_23 <- cbind(tots, poss, negs, results2013_1_23)
results <- rbind(results, results2013_1_23)

rm(DF_2013_1_23_cleanESP, df_tweets, DF_2013_1_23, tots, negs, poss, results2013_1_23)

print("2012")
#December

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_12_23.RData")

classifier_glmnet(DF_2012_12_23_cleanESP)

DF_2012_12_23 <- df_tweets
colnames(DF_2012_12_23) <- c("text", "sentiment")

DF_2012_12_23$sentiment[DF_2012_12_23$sentiment <= 0.5] <- "0"
DF_2012_12_23$sentiment[DF_2012_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_12_23))
negs <- as.numeric(sum(DF_2012_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_12_23$sentiment == "1"))
results2012_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_12_23 <- cbind(tots, poss, negs, results2012_12_23)
results <- rbind(results, results2012_12_23)

rm(DF_2012_12_23_cleanESP, df_tweets, DF_2012_12_23, tots, negs, poss, results2012_12_23)

#November

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_11_23.RData")

classifier_glmnet(DF_2012_11_23_cleanESP)

DF_2012_11_23 <- df_tweets
colnames(DF_2012_11_23) <- c("text", "sentiment")

DF_2012_11_23$sentiment[DF_2012_11_23$sentiment <= 0.5] <- "0"
DF_2012_11_23$sentiment[DF_2012_11_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_11_23))
negs <- as.numeric(sum(DF_2012_11_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_11_23$sentiment == "1"))
results2012_11_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_11_23 <- cbind(tots, poss, negs, results2012_11_23)
results <- rbind(results, results2012_11_23)

rm(DF_2012_11_23_cleanESP, df_tweets, DF_2012_11_23, tots, negs, poss, results2012_11_23)

#October

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_10_23.RData")

classifier_glmnet(DF_2012_10_23_cleanESP)

DF_2012_10_23 <- df_tweets
colnames(DF_2012_10_23) <- c("text", "sentiment")

DF_2012_10_23$sentiment[DF_2012_10_23$sentiment <= 0.5] <- "0"
DF_2012_10_23$sentiment[DF_2012_10_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_10_23))
negs <- as.numeric(sum(DF_2012_10_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_10_23$sentiment == "1"))
results2012_10_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_10_23 <- cbind(tots, poss, negs, results2012_10_23)
results <- rbind(results, results2012_10_23)

rm(DF_2012_10_23_cleanESP, df_tweets, DF_2012_10_23, tots, negs, poss, results2012_10_23)

#September

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_9_23.RData")

classifier_glmnet(DF_2012_9_23_cleanESP)

DF_2012_9_23 <- df_tweets
colnames(DF_2012_9_23) <- c("text", "sentiment")

DF_2012_9_23$sentiment[DF_2012_9_23$sentiment <= 0.5] <- "0"
DF_2012_9_23$sentiment[DF_2012_9_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_9_23))
negs <- as.numeric(sum(DF_2012_9_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_9_23$sentiment == "1"))
results2012_9_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_9_23 <- cbind(tots, poss, negs, results2012_9_23)
results <- rbind(results, results2012_9_23)

rm(DF_2012_9_23_cleanESP, df_tweets, DF_2012_9_23, tots, negs, poss, results2012_9_23)

#August

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_8_23.RData")

classifier_glmnet(DF_2012_8_23_cleanESP)

DF_2012_8_23 <- df_tweets
colnames(DF_2012_8_23) <- c("text", "sentiment")

DF_2012_8_23$sentiment[DF_2012_8_23$sentiment <= 0.5] <- "0"
DF_2012_8_23$sentiment[DF_2012_8_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_8_23))
negs <- as.numeric(sum(DF_2012_8_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_8_23$sentiment == "1"))
results2012_8_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_8_23 <- cbind(tots, poss, negs, results2012_8_23)
results <- rbind(results, results2012_8_23)

rm(DF_2012_8_23_cleanESP, df_tweets, DF_2012_8_23, tots, negs, poss, results2012_8_23)

#July

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_12_23.RData")

classifier_glmnet(DF_2012_12_23_cleanESP)

DF_2012_12_23 <- df_tweets
colnames(DF_2012_12_23) <- c("text", "sentiment")

DF_2012_12_23$sentiment[DF_2012_12_23$sentiment <= 0.5] <- "0"
DF_2012_12_23$sentiment[DF_2012_12_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_12_23))
negs <- as.numeric(sum(DF_2012_12_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_12_23$sentiment == "1"))
results2012_12_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_12_23 <- cbind(tots, poss, negs, results2012_12_23)
results <- rbind(results, results2012_12_23)

rm(DF_2012_12_23_cleanESP, df_tweets, DF_2012_12_23, tots, negs, poss, results2012_12_23)

#June

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_6_23.RData")

classifier_glmnet(DF_2012_6_23_cleanESP)

DF_2012_6_23 <- df_tweets
colnames(DF_2012_6_23) <- c("text", "sentiment")

DF_2012_6_23$sentiment[DF_2012_6_23$sentiment <= 0.5] <- "0"
DF_2012_6_23$sentiment[DF_2012_6_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_6_23))
negs <- as.numeric(sum(DF_2012_6_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_6_23$sentiment == "1"))
results2012_6_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_6_23 <- cbind(tots, poss, negs, results2012_6_23)
results <- rbind(results, results2012_6_23)

rm(DF_2012_6_23_cleanESP, df_tweets, DF_2012_6_23, tots, negs, poss, results2012_6_23)

#May

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_5_23.RData")

classifier_glmnet(DF_2012_5_23_cleanESP)

DF_2012_5_23 <- df_tweets
colnames(DF_2012_5_23) <- c("text", "sentiment")

DF_2012_5_23$sentiment[DF_2012_5_23$sentiment <= 0.5] <- "0"
DF_2012_5_23$sentiment[DF_2012_5_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_5_23))
negs <- as.numeric(sum(DF_2012_5_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_5_23$sentiment == "1"))
results2012_5_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_5_23 <- cbind(tots, poss, negs, results2012_5_23)
results <- rbind(results, results2012_5_23)

rm(DF_2012_5_23_cleanESP, df_tweets, DF_2012_5_23, tots, negs, poss, results2012_5_23)

#April

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_4_23.RData")

classifier_glmnet(DF_2012_4_23_cleanESP)

DF_2012_4_23 <- df_tweets
colnames(DF_2012_4_23) <- c("text", "sentiment")

DF_2012_4_23$sentiment[DF_2012_4_23$sentiment <= 0.5] <- "0"
DF_2012_4_23$sentiment[DF_2012_4_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_4_23))
negs <- as.numeric(sum(DF_2012_4_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_4_23$sentiment == "1"))
results2012_4_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_4_23 <- cbind(tots, poss, negs, results2012_4_23)
results <- rbind(results, results2012_4_23)

rm(DF_2012_4_23_cleanESP, df_tweets, DF_2012_4_23, tots, negs, poss, results2012_4_23)

#March

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_3_23.RData")

classifier_glmnet(DF_2012_3_23_cleanESP)

DF_2012_3_23 <- df_tweets
colnames(DF_2012_3_23) <- c("text", "sentiment")

DF_2012_3_23$sentiment[DF_2012_3_23$sentiment <= 0.5] <- "0"
DF_2012_3_23$sentiment[DF_2012_3_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_3_23))
negs <- as.numeric(sum(DF_2012_3_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_3_23$sentiment == "1"))
results2012_3_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_3_23 <- cbind(tots, poss, negs, results2012_3_23)
results <- rbind(results, results2012_3_23)

rm(DF_2012_3_23_cleanESP, df_tweets, DF_2012_3_23, tots, negs, poss, results2012_3_23)

#February

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_2_23.RData")

classifier_glmnet(DF_2012_2_23_cleanESP)

DF_2012_2_23 <- df_tweets
colnames(DF_2012_2_23) <- c("text", "sentiment")

DF_2012_2_23$sentiment[DF_2012_2_23$sentiment <= 0.5] <- "0"
DF_2012_2_23$sentiment[DF_2012_2_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_2_23))
negs <- as.numeric(sum(DF_2012_2_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_2_23$sentiment == "1"))
results2012_2_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_2_23 <- cbind(tots, poss, negs, results2012_2_23)
results <- rbind(results, results2012_2_23)

rm(DF_2012_2_23_cleanESP, df_tweets, DF_2012_2_23, tots, negs, poss, results2012_2_23)

#January

load(file = "Objects/Tweets/Series_23/Clean/ESP/DF_2012_1_23.RData")

classifier_glmnet(DF_2012_1_23_cleanESP)

DF_2012_1_23 <- df_tweets
colnames(DF_2012_1_23) <- c("text", "sentiment")

DF_2012_1_23$sentiment[DF_2012_1_23$sentiment <= 0.5] <- "0"
DF_2012_1_23$sentiment[DF_2012_1_23$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_1_23))
negs <- as.numeric(sum(DF_2012_1_23$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_1_23$sentiment == "1"))
results2012_1_23 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_1_23 <- cbind(tots, poss, negs, results2012_1_23)
results23 <- rbind(results, results2012_1_23)

rm(DF_2012_1_23_cleanESP, df_tweets, DF_2012_1_23, tots, negs, poss, results2012_1_23)

save(results23, file = "Objects/Models/resultsF23.RData")

print(difftime(Sys.time(), t1, units = 'mins'))

