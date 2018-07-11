

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

#Series 24
print("2017")
#December

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_12_24.RData")

classifier_glmnet(DF_2017_12_24_cleanESP)

DF_2017_12_24 <- df_tweets
colnames(DF_2017_12_24) <- c("text", "sentiment")

DF_2017_12_24$sentiment[DF_2017_12_24$sentiment <= 0.5] <- "0"
DF_2017_12_24$sentiment[DF_2017_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_12_24))
negs <- as.numeric(sum(DF_2017_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_12_24$sentiment == "1"))
results2017_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_12_24 <- cbind(tots, poss, negs, results2017_12_24)
results <- results2017_12_24

rm(DF_2017_12_24_cleanESP, df_tweets, DF_2017_12_24, tots, negs, poss, results2017_12_24)

#November

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_11_24.RData")

classifier_glmnet(DF_2017_11_24_cleanESP)

DF_2017_11_24 <- df_tweets
colnames(DF_2017_11_24) <- c("text", "sentiment")

DF_2017_11_24$sentiment[DF_2017_11_24$sentiment <= 0.5] <- "0"
DF_2017_11_24$sentiment[DF_2017_11_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_11_24))
negs <- as.numeric(sum(DF_2017_11_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_11_24$sentiment == "1"))
results2017_11_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_11_24 <- cbind(tots, poss, negs, results2017_11_24)
results <- rbind(results, results2017_11_24)

rm(DF_2017_11_24_cleanESP, df_tweets, DF_2017_11_24, tots, negs, poss, results2017_11_24)

#October

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_10_24.RData")

classifier_glmnet(DF_2017_10_24_cleanESP)

DF_2017_10_24 <- df_tweets
colnames(DF_2017_10_24) <- c("text", "sentiment")

DF_2017_10_24$sentiment[DF_2017_10_24$sentiment <= 0.5] <- "0"
DF_2017_10_24$sentiment[DF_2017_10_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_10_24))
negs <- as.numeric(sum(DF_2017_10_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_10_24$sentiment == "1"))
results2017_10_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_10_24 <- cbind(tots, poss, negs, results2017_10_24)
results <- rbind(results, results2017_10_24)

rm(DF_2017_10_24_cleanESP, df_tweets, DF_2017_10_24, tots, negs, poss, results2017_10_24)

#September

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_9_24.RData")

classifier_glmnet(DF_2017_9_24_cleanESP)

DF_2017_9_24 <- df_tweets
colnames(DF_2017_9_24) <- c("text", "sentiment")

DF_2017_9_24$sentiment[DF_2017_9_24$sentiment <= 0.5] <- "0"
DF_2017_9_24$sentiment[DF_2017_9_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_9_24))
negs <- as.numeric(sum(DF_2017_9_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_9_24$sentiment == "1"))
results2017_9_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_9_24 <- cbind(tots, poss, negs, results2017_9_24)
results <- rbind(results, results2017_9_24)

rm(DF_2017_9_24_cleanESP, df_tweets, DF_2017_9_24, tots, negs, poss, results2017_9_24)

#August

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_8_24.RData")

classifier_glmnet(DF_2017_8_24_cleanESP)

DF_2017_8_24 <- df_tweets
colnames(DF_2017_8_24) <- c("text", "sentiment")

DF_2017_8_24$sentiment[DF_2017_8_24$sentiment <= 0.5] <- "0"
DF_2017_8_24$sentiment[DF_2017_8_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_8_24))
negs <- as.numeric(sum(DF_2017_8_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_8_24$sentiment == "1"))
results2017_8_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_8_24 <- cbind(tots, poss, negs, results2017_8_24)
results <- rbind(results, results2017_8_24)

rm(DF_2017_8_24_cleanESP, df_tweets, DF_2017_8_24, tots, negs, poss, results2017_8_24)

#July

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_12_24.RData")

classifier_glmnet(DF_2017_12_24_cleanESP)

DF_2017_12_24 <- df_tweets
colnames(DF_2017_12_24) <- c("text", "sentiment")

DF_2017_12_24$sentiment[DF_2017_12_24$sentiment <= 0.5] <- "0"
DF_2017_12_24$sentiment[DF_2017_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_12_24))
negs <- as.numeric(sum(DF_2017_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_12_24$sentiment == "1"))
results2017_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_12_24 <- cbind(tots, poss, negs, results2017_12_24)
results <- rbind(results, results2017_12_24)

rm(DF_2017_12_24_cleanESP, df_tweets, DF_2017_12_24, tots, negs, poss, results2017_12_24)

#June

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_6_24.RData")

classifier_glmnet(DF_2017_6_24_cleanESP)

DF_2017_6_24 <- df_tweets
colnames(DF_2017_6_24) <- c("text", "sentiment")

DF_2017_6_24$sentiment[DF_2017_6_24$sentiment <= 0.5] <- "0"
DF_2017_6_24$sentiment[DF_2017_6_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_6_24))
negs <- as.numeric(sum(DF_2017_6_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_6_24$sentiment == "1"))
results2017_6_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_6_24 <- cbind(tots, poss, negs, results2017_6_24)
results <- rbind(results, results2017_6_24)

rm(DF_2017_6_24_cleanESP, df_tweets, DF_2017_6_24, tots, negs, poss, results2017_6_24)

#May

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_5_24.RData")

classifier_glmnet(DF_2017_5_24_cleanESP)

DF_2017_5_24 <- df_tweets
colnames(DF_2017_5_24) <- c("text", "sentiment")

DF_2017_5_24$sentiment[DF_2017_5_24$sentiment <= 0.5] <- "0"
DF_2017_5_24$sentiment[DF_2017_5_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_5_24))
negs <- as.numeric(sum(DF_2017_5_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_5_24$sentiment == "1"))
results2017_5_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_5_24 <- cbind(tots, poss, negs, results2017_5_24)
results <- rbind(results, results2017_5_24)

rm(DF_2017_5_24_cleanESP, df_tweets, DF_2017_5_24, tots, negs, poss, results2017_5_24)

#April

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_4_24.RData")

classifier_glmnet(DF_2017_4_24_cleanESP)

DF_2017_4_24 <- df_tweets
colnames(DF_2017_4_24) <- c("text", "sentiment")

DF_2017_4_24$sentiment[DF_2017_4_24$sentiment <= 0.5] <- "0"
DF_2017_4_24$sentiment[DF_2017_4_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_4_24))
negs <- as.numeric(sum(DF_2017_4_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_4_24$sentiment == "1"))
results2017_4_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_4_24 <- cbind(tots, poss, negs, results2017_4_24)
results <- rbind(results, results2017_4_24)

rm(DF_2017_4_24_cleanESP, df_tweets, DF_2017_4_24, tots, negs, poss, results2017_4_24)

#March

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_3_24.RData")

classifier_glmnet(DF_2017_3_24_cleanESP)

DF_2017_3_24 <- df_tweets
colnames(DF_2017_3_24) <- c("text", "sentiment")

DF_2017_3_24$sentiment[DF_2017_3_24$sentiment <= 0.5] <- "0"
DF_2017_3_24$sentiment[DF_2017_3_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_3_24))
negs <- as.numeric(sum(DF_2017_3_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_3_24$sentiment == "1"))
results2017_3_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_3_24 <- cbind(tots, poss, negs, results2017_3_24)
results <- rbind(results, results2017_3_24)

rm(DF_2017_3_24_cleanESP, df_tweets, DF_2017_3_24, tots, negs, poss, results2017_3_24)

#February

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_2_24.RData")

classifier_glmnet(DF_2017_2_24_cleanESP)

DF_2017_2_24 <- df_tweets
colnames(DF_2017_2_24) <- c("text", "sentiment")

DF_2017_2_24$sentiment[DF_2017_2_24$sentiment <= 0.5] <- "0"
DF_2017_2_24$sentiment[DF_2017_2_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_2_24))
negs <- as.numeric(sum(DF_2017_2_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_2_24$sentiment == "1"))
results2017_2_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_2_24 <- cbind(tots, poss, negs, results2017_2_24)
results <- rbind(results, results2017_2_24)

rm(DF_2017_2_24_cleanESP, df_tweets, DF_2017_2_24, tots, negs, poss, results2017_2_24)

#January

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2017_1_24.RData")

classifier_glmnet(DF_2017_1_24_cleanESP)

DF_2017_1_24 <- df_tweets
colnames(DF_2017_1_24) <- c("text", "sentiment")

DF_2017_1_24$sentiment[DF_2017_1_24$sentiment <= 0.5] <- "0"
DF_2017_1_24$sentiment[DF_2017_1_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2017_1_24))
negs <- as.numeric(sum(DF_2017_1_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2017_1_24$sentiment == "1"))
results2017_1_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2017_1_24 <- cbind(tots, poss, negs, results2017_1_24)
results <- rbind(results, results2017_1_24)

rm(DF_2017_1_24_cleanESP, df_tweets, DF_2017_1_24, tots, negs, poss, results2017_1_24)

print("2016")
#December

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_12_24.RData")

classifier_glmnet(DF_2016_12_24_cleanESP)

DF_2016_12_24 <- df_tweets
colnames(DF_2016_12_24) <- c("text", "sentiment")

DF_2016_12_24$sentiment[DF_2016_12_24$sentiment <= 0.5] <- "0"
DF_2016_12_24$sentiment[DF_2016_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_12_24))
negs <- as.numeric(sum(DF_2016_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_12_24$sentiment == "1"))
results2016_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_12_24 <- cbind(tots, poss, negs, results2016_12_24)
results <- rbind(results, results2016_12_24)

rm(DF_2016_12_24_cleanESP, df_tweets, DF_2016_12_24, tots, negs, poss, results2016_12_24)

#November

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_11_24.RData")

classifier_glmnet(DF_2016_11_24_cleanESP)

DF_2016_11_24 <- df_tweets
colnames(DF_2016_11_24) <- c("text", "sentiment")

DF_2016_11_24$sentiment[DF_2016_11_24$sentiment <= 0.5] <- "0"
DF_2016_11_24$sentiment[DF_2016_11_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_11_24))
negs <- as.numeric(sum(DF_2016_11_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_11_24$sentiment == "1"))
results2016_11_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_11_24 <- cbind(tots, poss, negs, results2016_11_24)
results <- rbind(results, results2016_11_24)

rm(DF_2016_11_24_cleanESP, df_tweets, DF_2016_11_24, tots, negs, poss, results2016_11_24)

#October

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_10_24.RData")

classifier_glmnet(DF_2016_10_24_cleanESP)

DF_2016_10_24 <- df_tweets
colnames(DF_2016_10_24) <- c("text", "sentiment")

DF_2016_10_24$sentiment[DF_2016_10_24$sentiment <= 0.5] <- "0"
DF_2016_10_24$sentiment[DF_2016_10_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_10_24))
negs <- as.numeric(sum(DF_2016_10_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_10_24$sentiment == "1"))
results2016_10_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_10_24 <- cbind(tots, poss, negs, results2016_10_24)
results <- rbind(results, results2016_10_24)

rm(DF_2016_10_24_cleanESP, df_tweets, DF_2016_10_24, tots, negs, poss, results2016_10_24)

#September

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_9_24.RData")

classifier_glmnet(DF_2016_9_24_cleanESP)

DF_2016_9_24 <- df_tweets
colnames(DF_2016_9_24) <- c("text", "sentiment")

DF_2016_9_24$sentiment[DF_2016_9_24$sentiment <= 0.5] <- "0"
DF_2016_9_24$sentiment[DF_2016_9_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_9_24))
negs <- as.numeric(sum(DF_2016_9_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_9_24$sentiment == "1"))
results2016_9_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_9_24 <- cbind(tots, poss, negs, results2016_9_24)
results <- rbind(results, results2016_9_24)

rm(DF_2016_9_24_cleanESP, df_tweets, DF_2016_9_24, tots, negs, poss, results2016_9_24)

#August

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_8_24.RData")

classifier_glmnet(DF_2016_8_24_cleanESP)

DF_2016_8_24 <- df_tweets
colnames(DF_2016_8_24) <- c("text", "sentiment")

DF_2016_8_24$sentiment[DF_2016_8_24$sentiment <= 0.5] <- "0"
DF_2016_8_24$sentiment[DF_2016_8_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_8_24))
negs <- as.numeric(sum(DF_2016_8_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_8_24$sentiment == "1"))
results2016_8_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_8_24 <- cbind(tots, poss, negs, results2016_8_24)
results <- rbind(results, results2016_8_24)

rm(DF_2016_8_24_cleanESP, df_tweets, DF_2016_8_24, tots, negs, poss, results2016_8_24)

#July

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_12_24.RData")

classifier_glmnet(DF_2016_12_24_cleanESP)

DF_2016_12_24 <- df_tweets
colnames(DF_2016_12_24) <- c("text", "sentiment")

DF_2016_12_24$sentiment[DF_2016_12_24$sentiment <= 0.5] <- "0"
DF_2016_12_24$sentiment[DF_2016_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_12_24))
negs <- as.numeric(sum(DF_2016_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_12_24$sentiment == "1"))
results2016_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_12_24 <- cbind(tots, poss, negs, results2016_12_24)
results <- rbind(results, results2016_12_24)

rm(DF_2016_12_24_cleanESP, df_tweets, DF_2016_12_24, tots, negs, poss, results2016_12_24)

#June

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_6_24.RData")

classifier_glmnet(DF_2016_6_24_cleanESP)

DF_2016_6_24 <- df_tweets
colnames(DF_2016_6_24) <- c("text", "sentiment")

DF_2016_6_24$sentiment[DF_2016_6_24$sentiment <= 0.5] <- "0"
DF_2016_6_24$sentiment[DF_2016_6_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_6_24))
negs <- as.numeric(sum(DF_2016_6_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_6_24$sentiment == "1"))
results2016_6_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_6_24 <- cbind(tots, poss, negs, results2016_6_24)
results <- rbind(results, results2016_6_24)

rm(DF_2016_6_24_cleanESP, df_tweets, DF_2016_6_24, tots, negs, poss, results2016_6_24)

#May

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_5_24.RData")

classifier_glmnet(DF_2016_5_24_cleanESP)

DF_2016_5_24 <- df_tweets
colnames(DF_2016_5_24) <- c("text", "sentiment")

DF_2016_5_24$sentiment[DF_2016_5_24$sentiment <= 0.5] <- "0"
DF_2016_5_24$sentiment[DF_2016_5_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_5_24))
negs <- as.numeric(sum(DF_2016_5_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_5_24$sentiment == "1"))
results2016_5_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_5_24 <- cbind(tots, poss, negs, results2016_5_24)
results <- rbind(results, results2016_5_24)

rm(DF_2016_5_24_cleanESP, df_tweets, DF_2016_5_24, tots, negs, poss, results2016_5_24)

#April

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_4_24.RData")

classifier_glmnet(DF_2016_4_24_cleanESP)

DF_2016_4_24 <- df_tweets
colnames(DF_2016_4_24) <- c("text", "sentiment")

DF_2016_4_24$sentiment[DF_2016_4_24$sentiment <= 0.5] <- "0"
DF_2016_4_24$sentiment[DF_2016_4_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_4_24))
negs <- as.numeric(sum(DF_2016_4_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_4_24$sentiment == "1"))
results2016_4_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_4_24 <- cbind(tots, poss, negs, results2016_4_24)
results <- rbind(results, results2016_4_24)

rm(DF_2016_4_24_cleanESP, df_tweets, DF_2016_4_24, tots, negs, poss, results2016_4_24)

#March

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_3_24.RData")

classifier_glmnet(DF_2016_3_24_cleanESP)

DF_2016_3_24 <- df_tweets
colnames(DF_2016_3_24) <- c("text", "sentiment")

DF_2016_3_24$sentiment[DF_2016_3_24$sentiment <= 0.5] <- "0"
DF_2016_3_24$sentiment[DF_2016_3_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_3_24))
negs <- as.numeric(sum(DF_2016_3_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_3_24$sentiment == "1"))
results2016_3_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_3_24 <- cbind(tots, poss, negs, results2016_3_24)
results <- rbind(results, results2016_3_24)

rm(DF_2016_3_24_cleanESP, df_tweets, DF_2016_3_24, tots, negs, poss, results2016_3_24)

#February

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_2_24.RData")

classifier_glmnet(DF_2016_2_24_cleanESP)

DF_2016_2_24 <- df_tweets
colnames(DF_2016_2_24) <- c("text", "sentiment")

DF_2016_2_24$sentiment[DF_2016_2_24$sentiment <= 0.5] <- "0"
DF_2016_2_24$sentiment[DF_2016_2_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_2_24))
negs <- as.numeric(sum(DF_2016_2_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_2_24$sentiment == "1"))
results2016_2_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_2_24 <- cbind(tots, poss, negs, results2016_2_24)
results <- rbind(results, results2016_2_24)

rm(DF_2016_2_24_cleanESP, df_tweets, DF_2016_2_24, tots, negs, poss, results2016_2_24)

#January

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2016_1_24.RData")

classifier_glmnet(DF_2016_1_24_cleanESP)

DF_2016_1_24 <- df_tweets
colnames(DF_2016_1_24) <- c("text", "sentiment")

DF_2016_1_24$sentiment[DF_2016_1_24$sentiment <= 0.5] <- "0"
DF_2016_1_24$sentiment[DF_2016_1_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2016_1_24))
negs <- as.numeric(sum(DF_2016_1_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2016_1_24$sentiment == "1"))
results2016_1_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2016_1_24 <- cbind(tots, poss, negs, results2016_1_24)
results <- rbind(results, results2016_1_24)

rm(DF_2016_1_24_cleanESP, df_tweets, DF_2016_1_24, tots, negs, poss, results2016_1_24)

print("2015")
#December

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_12_24.RData")

classifier_glmnet(DF_2015_12_24_cleanESP)

DF_2015_12_24 <- df_tweets
colnames(DF_2015_12_24) <- c("text", "sentiment")

DF_2015_12_24$sentiment[DF_2015_12_24$sentiment <= 0.5] <- "0"
DF_2015_12_24$sentiment[DF_2015_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_12_24))
negs <- as.numeric(sum(DF_2015_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_12_24$sentiment == "1"))
results2015_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_12_24 <- cbind(tots, poss, negs, results2015_12_24)
results <- rbind(results, results2015_12_24)

rm(DF_2015_12_24_cleanESP, df_tweets, DF_2015_12_24, tots, negs, poss, results2015_12_24)

#November

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_11_24.RData")

classifier_glmnet(DF_2015_11_24_cleanESP)

DF_2015_11_24 <- df_tweets
colnames(DF_2015_11_24) <- c("text", "sentiment")

DF_2015_11_24$sentiment[DF_2015_11_24$sentiment <= 0.5] <- "0"
DF_2015_11_24$sentiment[DF_2015_11_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_11_24))
negs <- as.numeric(sum(DF_2015_11_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_11_24$sentiment == "1"))
results2015_11_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_11_24 <- cbind(tots, poss, negs, results2015_11_24)
results <- rbind(results, results2015_11_24)

rm(DF_2015_11_24_cleanESP, df_tweets, DF_2015_11_24, tots, negs, poss, results2015_11_24)

#October

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_10_24.RData")

classifier_glmnet(DF_2015_10_24_cleanESP)

DF_2015_10_24 <- df_tweets
colnames(DF_2015_10_24) <- c("text", "sentiment")

DF_2015_10_24$sentiment[DF_2015_10_24$sentiment <= 0.5] <- "0"
DF_2015_10_24$sentiment[DF_2015_10_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_10_24))
negs <- as.numeric(sum(DF_2015_10_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_10_24$sentiment == "1"))
results2015_10_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_10_24 <- cbind(tots, poss, negs, results2015_10_24)
results <- rbind(results, results2015_10_24)

rm(DF_2015_10_24_cleanESP, df_tweets, DF_2015_10_24, tots, negs, poss, results2015_10_24)

#September

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_9_24.RData")

classifier_glmnet(DF_2015_9_24_cleanESP)

DF_2015_9_24 <- df_tweets
colnames(DF_2015_9_24) <- c("text", "sentiment")

DF_2015_9_24$sentiment[DF_2015_9_24$sentiment <= 0.5] <- "0"
DF_2015_9_24$sentiment[DF_2015_9_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_9_24))
negs <- as.numeric(sum(DF_2015_9_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_9_24$sentiment == "1"))
results2015_9_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_9_24 <- cbind(tots, poss, negs, results2015_9_24)
results <- rbind(results, results2015_9_24)

rm(DF_2015_9_24_cleanESP, df_tweets, DF_2015_9_24, tots, negs, poss, results2015_9_24)

#August

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_8_24.RData")

classifier_glmnet(DF_2015_8_24_cleanESP)

DF_2015_8_24 <- df_tweets
colnames(DF_2015_8_24) <- c("text", "sentiment")

DF_2015_8_24$sentiment[DF_2015_8_24$sentiment <= 0.5] <- "0"
DF_2015_8_24$sentiment[DF_2015_8_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_8_24))
negs <- as.numeric(sum(DF_2015_8_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_8_24$sentiment == "1"))
results2015_8_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_8_24 <- cbind(tots, poss, negs, results2015_8_24)
results <- rbind(results, results2015_8_24)

rm(DF_2015_8_24_cleanESP, df_tweets, DF_2015_8_24, tots, negs, poss, results2015_8_24)

#July

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_12_24.RData")

classifier_glmnet(DF_2015_12_24_cleanESP)

DF_2015_12_24 <- df_tweets
colnames(DF_2015_12_24) <- c("text", "sentiment")

DF_2015_12_24$sentiment[DF_2015_12_24$sentiment <= 0.5] <- "0"
DF_2015_12_24$sentiment[DF_2015_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_12_24))
negs <- as.numeric(sum(DF_2015_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_12_24$sentiment == "1"))
results2015_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_12_24 <- cbind(tots, poss, negs, results2015_12_24)
results <- rbind(results, results2015_12_24)

rm(DF_2015_12_24_cleanESP, df_tweets, DF_2015_12_24, tots, negs, poss, results2015_12_24)

#June

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_6_24.RData")

classifier_glmnet(DF_2015_6_24_cleanESP)

DF_2015_6_24 <- df_tweets
colnames(DF_2015_6_24) <- c("text", "sentiment")

DF_2015_6_24$sentiment[DF_2015_6_24$sentiment <= 0.5] <- "0"
DF_2015_6_24$sentiment[DF_2015_6_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_6_24))
negs <- as.numeric(sum(DF_2015_6_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_6_24$sentiment == "1"))
results2015_6_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_6_24 <- cbind(tots, poss, negs, results2015_6_24)
results <- rbind(results, results2015_6_24)

rm(DF_2015_6_24_cleanESP, df_tweets, DF_2015_6_24, tots, negs, poss, results2015_6_24)

#May

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_5_24.RData")

classifier_glmnet(DF_2015_5_24_cleanESP)

DF_2015_5_24 <- df_tweets
colnames(DF_2015_5_24) <- c("text", "sentiment")

DF_2015_5_24$sentiment[DF_2015_5_24$sentiment <= 0.5] <- "0"
DF_2015_5_24$sentiment[DF_2015_5_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_5_24))
negs <- as.numeric(sum(DF_2015_5_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_5_24$sentiment == "1"))
results2015_5_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_5_24 <- cbind(tots, poss, negs, results2015_5_24)
results <- rbind(results, results2015_5_24)

rm(DF_2015_5_24_cleanESP, df_tweets, DF_2015_5_24, tots, negs, poss, results2015_5_24)

#April

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_4_24.RData")

classifier_glmnet(DF_2015_4_24_cleanESP)

DF_2015_4_24 <- df_tweets
colnames(DF_2015_4_24) <- c("text", "sentiment")

DF_2015_4_24$sentiment[DF_2015_4_24$sentiment <= 0.5] <- "0"
DF_2015_4_24$sentiment[DF_2015_4_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_4_24))
negs <- as.numeric(sum(DF_2015_4_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_4_24$sentiment == "1"))
results2015_4_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_4_24 <- cbind(tots, poss, negs, results2015_4_24)
results <- rbind(results, results2015_4_24)

rm(DF_2015_4_24_cleanESP, df_tweets, DF_2015_4_24, tots, negs, poss, results2015_4_24)

#March

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_3_24.RData")

classifier_glmnet(DF_2015_3_24_cleanESP)

DF_2015_3_24 <- df_tweets
colnames(DF_2015_3_24) <- c("text", "sentiment")

DF_2015_3_24$sentiment[DF_2015_3_24$sentiment <= 0.5] <- "0"
DF_2015_3_24$sentiment[DF_2015_3_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_3_24))
negs <- as.numeric(sum(DF_2015_3_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_3_24$sentiment == "1"))
results2015_3_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_3_24 <- cbind(tots, poss, negs, results2015_3_24)
results <- rbind(results, results2015_3_24)

rm(DF_2015_3_24_cleanESP, df_tweets, DF_2015_3_24, tots, negs, poss, results2015_3_24)

#February

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_2_24.RData")

classifier_glmnet(DF_2015_2_24_cleanESP)

DF_2015_2_24 <- df_tweets
colnames(DF_2015_2_24) <- c("text", "sentiment")

DF_2015_2_24$sentiment[DF_2015_2_24$sentiment <= 0.5] <- "0"
DF_2015_2_24$sentiment[DF_2015_2_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_2_24))
negs <- as.numeric(sum(DF_2015_2_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_2_24$sentiment == "1"))
results2015_2_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_2_24 <- cbind(tots, poss, negs, results2015_2_24)
results <- rbind(results, results2015_2_24)

rm(DF_2015_2_24_cleanESP, df_tweets, DF_2015_2_24, tots, negs, poss, results2015_2_24)

#January

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2015_1_24.RData")

classifier_glmnet(DF_2015_1_24_cleanESP)

DF_2015_1_24 <- df_tweets
colnames(DF_2015_1_24) <- c("text", "sentiment")

DF_2015_1_24$sentiment[DF_2015_1_24$sentiment <= 0.5] <- "0"
DF_2015_1_24$sentiment[DF_2015_1_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2015_1_24))
negs <- as.numeric(sum(DF_2015_1_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2015_1_24$sentiment == "1"))
results2015_1_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2015_1_24 <- cbind(tots, poss, negs, results2015_1_24)
results <- rbind(results, results2015_1_24)

rm(DF_2015_1_24_cleanESP, df_tweets, DF_2015_1_24, tots, negs, poss, results2015_1_24)

print("2014")
#December

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_12_24.RData")

classifier_glmnet(DF_2014_12_24_cleanESP)

DF_2014_12_24 <- df_tweets
colnames(DF_2014_12_24) <- c("text", "sentiment")

DF_2014_12_24$sentiment[DF_2014_12_24$sentiment <= 0.5] <- "0"
DF_2014_12_24$sentiment[DF_2014_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_12_24))
negs <- as.numeric(sum(DF_2014_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_12_24$sentiment == "1"))
results2014_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_12_24 <- cbind(tots, poss, negs, results2014_12_24)
results <- rbind(results, results2014_12_24)

rm(DF_2014_12_24_cleanESP, df_tweets, DF_2014_12_24, tots, negs, poss, results2014_12_24)

#November

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_11_24.RData")

classifier_glmnet(DF_2014_11_24_cleanESP)

DF_2014_11_24 <- df_tweets
colnames(DF_2014_11_24) <- c("text", "sentiment")

DF_2014_11_24$sentiment[DF_2014_11_24$sentiment <= 0.5] <- "0"
DF_2014_11_24$sentiment[DF_2014_11_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_11_24))
negs <- as.numeric(sum(DF_2014_11_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_11_24$sentiment == "1"))
results2014_11_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_11_24 <- cbind(tots, poss, negs, results2014_11_24)
results <- rbind(results, results2014_11_24)

rm(DF_2014_11_24_cleanESP, df_tweets, DF_2014_11_24, tots, negs, poss, results2014_11_24)

#October

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_10_24.RData")

classifier_glmnet(DF_2014_10_24_cleanESP)

DF_2014_10_24 <- df_tweets
colnames(DF_2014_10_24) <- c("text", "sentiment")

DF_2014_10_24$sentiment[DF_2014_10_24$sentiment <= 0.5] <- "0"
DF_2014_10_24$sentiment[DF_2014_10_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_10_24))
negs <- as.numeric(sum(DF_2014_10_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_10_24$sentiment == "1"))
results2014_10_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_10_24 <- cbind(tots, poss, negs, results2014_10_24)
results <- rbind(results, results2014_10_24)

rm(DF_2014_10_24_cleanESP, df_tweets, DF_2014_10_24, tots, negs, poss, results2014_10_24)

#September

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_9_24.RData")

classifier_glmnet(DF_2014_9_24_cleanESP)

DF_2014_9_24 <- df_tweets
colnames(DF_2014_9_24) <- c("text", "sentiment")

DF_2014_9_24$sentiment[DF_2014_9_24$sentiment <= 0.5] <- "0"
DF_2014_9_24$sentiment[DF_2014_9_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_9_24))
negs <- as.numeric(sum(DF_2014_9_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_9_24$sentiment == "1"))
results2014_9_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_9_24 <- cbind(tots, poss, negs, results2014_9_24)
results <- rbind(results, results2014_9_24)

rm(DF_2014_9_24_cleanESP, df_tweets, DF_2014_9_24, tots, negs, poss, results2014_9_24)

#August

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_8_24.RData")

classifier_glmnet(DF_2014_8_24_cleanESP)

DF_2014_8_24 <- df_tweets
colnames(DF_2014_8_24) <- c("text", "sentiment")

DF_2014_8_24$sentiment[DF_2014_8_24$sentiment <= 0.5] <- "0"
DF_2014_8_24$sentiment[DF_2014_8_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_8_24))
negs <- as.numeric(sum(DF_2014_8_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_8_24$sentiment == "1"))
results2014_8_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_8_24 <- cbind(tots, poss, negs, results2014_8_24)
results <- rbind(results, results2014_8_24)

rm(DF_2014_8_24_cleanESP, df_tweets, DF_2014_8_24, tots, negs, poss, results2014_8_24)

#July

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_12_24.RData")

classifier_glmnet(DF_2014_12_24_cleanESP)

DF_2014_12_24 <- df_tweets
colnames(DF_2014_12_24) <- c("text", "sentiment")

DF_2014_12_24$sentiment[DF_2014_12_24$sentiment <= 0.5] <- "0"
DF_2014_12_24$sentiment[DF_2014_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_12_24))
negs <- as.numeric(sum(DF_2014_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_12_24$sentiment == "1"))
results2014_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_12_24 <- cbind(tots, poss, negs, results2014_12_24)
results <- rbind(results, results2014_12_24)

rm(DF_2014_12_24_cleanESP, df_tweets, DF_2014_12_24, tots, negs, poss, results2014_12_24)

#June

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_6_24.RData")

classifier_glmnet(DF_2014_6_24_cleanESP)

DF_2014_6_24 <- df_tweets
colnames(DF_2014_6_24) <- c("text", "sentiment")

DF_2014_6_24$sentiment[DF_2014_6_24$sentiment <= 0.5] <- "0"
DF_2014_6_24$sentiment[DF_2014_6_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_6_24))
negs <- as.numeric(sum(DF_2014_6_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_6_24$sentiment == "1"))
results2014_6_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_6_24 <- cbind(tots, poss, negs, results2014_6_24)
results <- rbind(results, results2014_6_24)

rm(DF_2014_6_24_cleanESP, df_tweets, DF_2014_6_24, tots, negs, poss, results2014_6_24)

#May

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_5_24.RData")

classifier_glmnet(DF_2014_5_24_cleanESP)

DF_2014_5_24 <- df_tweets
colnames(DF_2014_5_24) <- c("text", "sentiment")

DF_2014_5_24$sentiment[DF_2014_5_24$sentiment <= 0.5] <- "0"
DF_2014_5_24$sentiment[DF_2014_5_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_5_24))
negs <- as.numeric(sum(DF_2014_5_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_5_24$sentiment == "1"))
results2014_5_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_5_24 <- cbind(tots, poss, negs, results2014_5_24)
results <- rbind(results, results2014_5_24)

rm(DF_2014_5_24_cleanESP, df_tweets, DF_2014_5_24, tots, negs, poss, results2014_5_24)

#April

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_4_24.RData")

classifier_glmnet(DF_2014_4_24_cleanESP)

DF_2014_4_24 <- df_tweets
colnames(DF_2014_4_24) <- c("text", "sentiment")

DF_2014_4_24$sentiment[DF_2014_4_24$sentiment <= 0.5] <- "0"
DF_2014_4_24$sentiment[DF_2014_4_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_4_24))
negs <- as.numeric(sum(DF_2014_4_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_4_24$sentiment == "1"))
results2014_4_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_4_24 <- cbind(tots, poss, negs, results2014_4_24)
results <- rbind(results, results2014_4_24)

rm(DF_2014_4_24_cleanESP, df_tweets, DF_2014_4_24, tots, negs, poss, results2014_4_24)

#March

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_3_24.RData")

classifier_glmnet(DF_2014_3_24_cleanESP)

DF_2014_3_24 <- df_tweets
colnames(DF_2014_3_24) <- c("text", "sentiment")

DF_2014_3_24$sentiment[DF_2014_3_24$sentiment <= 0.5] <- "0"
DF_2014_3_24$sentiment[DF_2014_3_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_3_24))
negs <- as.numeric(sum(DF_2014_3_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_3_24$sentiment == "1"))
results2014_3_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_3_24 <- cbind(tots, poss, negs, results2014_3_24)
results <- rbind(results, results2014_3_24)

rm(DF_2014_3_24_cleanESP, df_tweets, DF_2014_3_24, tots, negs, poss, results2014_3_24)

#February

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_2_24.RData")

classifier_glmnet(DF_2014_2_24_cleanESP)

DF_2014_2_24 <- df_tweets
colnames(DF_2014_2_24) <- c("text", "sentiment")

DF_2014_2_24$sentiment[DF_2014_2_24$sentiment <= 0.5] <- "0"
DF_2014_2_24$sentiment[DF_2014_2_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_2_24))
negs <- as.numeric(sum(DF_2014_2_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_2_24$sentiment == "1"))
results2014_2_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_2_24 <- cbind(tots, poss, negs, results2014_2_24)
results <- rbind(results, results2014_2_24)

rm(DF_2014_2_24_cleanESP, df_tweets, DF_2014_2_24, tots, negs, poss, results2014_2_24)

#January

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2014_1_24.RData")

classifier_glmnet(DF_2014_1_24_cleanESP)

DF_2014_1_24 <- df_tweets
colnames(DF_2014_1_24) <- c("text", "sentiment")

DF_2014_1_24$sentiment[DF_2014_1_24$sentiment <= 0.5] <- "0"
DF_2014_1_24$sentiment[DF_2014_1_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2014_1_24))
negs <- as.numeric(sum(DF_2014_1_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2014_1_24$sentiment == "1"))
results2014_1_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2014_1_24 <- cbind(tots, poss, negs, results2014_1_24)
results <- rbind(results, results2014_1_24)

rm(DF_2014_1_24_cleanESP, df_tweets, DF_2014_1_24, tots, negs, poss, results2014_1_24)

print("2013")
#December

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_12_24.RData")

classifier_glmnet(DF_2013_12_24_cleanESP)

DF_2013_12_24 <- df_tweets
colnames(DF_2013_12_24) <- c("text", "sentiment")

DF_2013_12_24$sentiment[DF_2013_12_24$sentiment <= 0.5] <- "0"
DF_2013_12_24$sentiment[DF_2013_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_12_24))
negs <- as.numeric(sum(DF_2013_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_12_24$sentiment == "1"))
results2013_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_12_24 <- cbind(tots, poss, negs, results2013_12_24)
results <- rbind(results, results2013_12_24)

rm(DF_2013_12_24_cleanESP, df_tweets, DF_2013_12_24, tots, negs, poss, results2013_12_24)

#November

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_11_24.RData")

classifier_glmnet(DF_2013_11_24_cleanESP)

DF_2013_11_24 <- df_tweets
colnames(DF_2013_11_24) <- c("text", "sentiment")

DF_2013_11_24$sentiment[DF_2013_11_24$sentiment <= 0.5] <- "0"
DF_2013_11_24$sentiment[DF_2013_11_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_11_24))
negs <- as.numeric(sum(DF_2013_11_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_11_24$sentiment == "1"))
results2013_11_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_11_24 <- cbind(tots, poss, negs, results2013_11_24)
results <- rbind(results, results2013_11_24)

rm(DF_2013_11_24_cleanESP, df_tweets, DF_2013_11_24, tots, negs, poss, results2013_11_24)

#October

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_10_24.RData")

classifier_glmnet(DF_2013_10_24_cleanESP)

DF_2013_10_24 <- df_tweets
colnames(DF_2013_10_24) <- c("text", "sentiment")

DF_2013_10_24$sentiment[DF_2013_10_24$sentiment <= 0.5] <- "0"
DF_2013_10_24$sentiment[DF_2013_10_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_10_24))
negs <- as.numeric(sum(DF_2013_10_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_10_24$sentiment == "1"))
results2013_10_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_10_24 <- cbind(tots, poss, negs, results2013_10_24)
results <- rbind(results, results2013_10_24)

rm(DF_2013_10_24_cleanESP, df_tweets, DF_2013_10_24, tots, negs, poss, results2013_10_24)

#September

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_9_24.RData")

classifier_glmnet(DF_2013_9_24_cleanESP)

DF_2013_9_24 <- df_tweets
colnames(DF_2013_9_24) <- c("text", "sentiment")

DF_2013_9_24$sentiment[DF_2013_9_24$sentiment <= 0.5] <- "0"
DF_2013_9_24$sentiment[DF_2013_9_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_9_24))
negs <- as.numeric(sum(DF_2013_9_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_9_24$sentiment == "1"))
results2013_9_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_9_24 <- cbind(tots, poss, negs, results2013_9_24)
results <- rbind(results, results2013_9_24)

rm(DF_2013_9_24_cleanESP, df_tweets, DF_2013_9_24, tots, negs, poss, results2013_9_24)

#August

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_8_24.RData")

classifier_glmnet(DF_2013_8_24_cleanESP)

DF_2013_8_24 <- df_tweets
colnames(DF_2013_8_24) <- c("text", "sentiment")

DF_2013_8_24$sentiment[DF_2013_8_24$sentiment <= 0.5] <- "0"
DF_2013_8_24$sentiment[DF_2013_8_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_8_24))
negs <- as.numeric(sum(DF_2013_8_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_8_24$sentiment == "1"))
results2013_8_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_8_24 <- cbind(tots, poss, negs, results2013_8_24)
results <- rbind(results, results2013_8_24)

rm(DF_2013_8_24_cleanESP, df_tweets, DF_2013_8_24, tots, negs, poss, results2013_8_24)

#July

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_12_24.RData")

classifier_glmnet(DF_2013_12_24_cleanESP)

DF_2013_12_24 <- df_tweets
colnames(DF_2013_12_24) <- c("text", "sentiment")

DF_2013_12_24$sentiment[DF_2013_12_24$sentiment <= 0.5] <- "0"
DF_2013_12_24$sentiment[DF_2013_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_12_24))
negs <- as.numeric(sum(DF_2013_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_12_24$sentiment == "1"))
results2013_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_12_24 <- cbind(tots, poss, negs, results2013_12_24)
results <- rbind(results, results2013_12_24)

rm(DF_2013_12_24_cleanESP, df_tweets, DF_2013_12_24, tots, negs, poss, results2013_12_24)

#June

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_6_24.RData")

classifier_glmnet(DF_2013_6_24_cleanESP)

DF_2013_6_24 <- df_tweets
colnames(DF_2013_6_24) <- c("text", "sentiment")

DF_2013_6_24$sentiment[DF_2013_6_24$sentiment <= 0.5] <- "0"
DF_2013_6_24$sentiment[DF_2013_6_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_6_24))
negs <- as.numeric(sum(DF_2013_6_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_6_24$sentiment == "1"))
results2013_6_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_6_24 <- cbind(tots, poss, negs, results2013_6_24)
results <- rbind(results, results2013_6_24)

rm(DF_2013_6_24_cleanESP, df_tweets, DF_2013_6_24, tots, negs, poss, results2013_6_24)

#May

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_5_24.RData")

classifier_glmnet(DF_2013_5_24_cleanESP)

DF_2013_5_24 <- df_tweets
colnames(DF_2013_5_24) <- c("text", "sentiment")

DF_2013_5_24$sentiment[DF_2013_5_24$sentiment <= 0.5] <- "0"
DF_2013_5_24$sentiment[DF_2013_5_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_5_24))
negs <- as.numeric(sum(DF_2013_5_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_5_24$sentiment == "1"))
results2013_5_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_5_24 <- cbind(tots, poss, negs, results2013_5_24)
results <- rbind(results, results2013_5_24)

rm(DF_2013_5_24_cleanESP, df_tweets, DF_2013_5_24, tots, negs, poss, results2013_5_24)

#April

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_4_24.RData")

classifier_glmnet(DF_2013_4_24_cleanESP)

DF_2013_4_24 <- df_tweets
colnames(DF_2013_4_24) <- c("text", "sentiment")

DF_2013_4_24$sentiment[DF_2013_4_24$sentiment <= 0.5] <- "0"
DF_2013_4_24$sentiment[DF_2013_4_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_4_24))
negs <- as.numeric(sum(DF_2013_4_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_4_24$sentiment == "1"))
results2013_4_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_4_24 <- cbind(tots, poss, negs, results2013_4_24)
results <- rbind(results, results2013_4_24)

rm(DF_2013_4_24_cleanESP, df_tweets, DF_2013_4_24, tots, negs, poss, results2013_4_24)

#March

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_3_24.RData")

classifier_glmnet(DF_2013_3_24_cleanESP)

DF_2013_3_24 <- df_tweets
colnames(DF_2013_3_24) <- c("text", "sentiment")

DF_2013_3_24$sentiment[DF_2013_3_24$sentiment <= 0.5] <- "0"
DF_2013_3_24$sentiment[DF_2013_3_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_3_24))
negs <- as.numeric(sum(DF_2013_3_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_3_24$sentiment == "1"))
results2013_3_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_3_24 <- cbind(tots, poss, negs, results2013_3_24)
results <- rbind(results, results2013_3_24)

rm(DF_2013_3_24_cleanESP, df_tweets, DF_2013_3_24, tots, negs, poss, results2013_3_24)

#February

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_2_24.RData")

classifier_glmnet(DF_2013_2_24_cleanESP)

DF_2013_2_24 <- df_tweets
colnames(DF_2013_2_24) <- c("text", "sentiment")

DF_2013_2_24$sentiment[DF_2013_2_24$sentiment <= 0.5] <- "0"
DF_2013_2_24$sentiment[DF_2013_2_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_2_24))
negs <- as.numeric(sum(DF_2013_2_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_2_24$sentiment == "1"))
results2013_2_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_2_24 <- cbind(tots, poss, negs, results2013_2_24)
results <- rbind(results, results2013_2_24)

rm(DF_2013_2_24_cleanESP, df_tweets, DF_2013_2_24, tots, negs, poss, results2013_2_24)

#January

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2013_1_24.RData")

classifier_glmnet(DF_2013_1_24_cleanESP)

DF_2013_1_24 <- df_tweets
colnames(DF_2013_1_24) <- c("text", "sentiment")

DF_2013_1_24$sentiment[DF_2013_1_24$sentiment <= 0.5] <- "0"
DF_2013_1_24$sentiment[DF_2013_1_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2013_1_24))
negs <- as.numeric(sum(DF_2013_1_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2013_1_24$sentiment == "1"))
results2013_1_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2013_1_24 <- cbind(tots, poss, negs, results2013_1_24)
results <- rbind(results, results2013_1_24)

rm(DF_2013_1_24_cleanESP, df_tweets, DF_2013_1_24, tots, negs, poss, results2013_1_24)

print("2012")
#December

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_12_24.RData")

classifier_glmnet(DF_2012_12_24_cleanESP)

DF_2012_12_24 <- df_tweets
colnames(DF_2012_12_24) <- c("text", "sentiment")

DF_2012_12_24$sentiment[DF_2012_12_24$sentiment <= 0.5] <- "0"
DF_2012_12_24$sentiment[DF_2012_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_12_24))
negs <- as.numeric(sum(DF_2012_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_12_24$sentiment == "1"))
results2012_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_12_24 <- cbind(tots, poss, negs, results2012_12_24)
results <- rbind(results, results2012_12_24)

rm(DF_2012_12_24_cleanESP, df_tweets, DF_2012_12_24, tots, negs, poss, results2012_12_24)

#November

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_11_24.RData")

classifier_glmnet(DF_2012_11_24_cleanESP)

DF_2012_11_24 <- df_tweets
colnames(DF_2012_11_24) <- c("text", "sentiment")

DF_2012_11_24$sentiment[DF_2012_11_24$sentiment <= 0.5] <- "0"
DF_2012_11_24$sentiment[DF_2012_11_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_11_24))
negs <- as.numeric(sum(DF_2012_11_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_11_24$sentiment == "1"))
results2012_11_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_11_24 <- cbind(tots, poss, negs, results2012_11_24)
results <- rbind(results, results2012_11_24)

rm(DF_2012_11_24_cleanESP, df_tweets, DF_2012_11_24, tots, negs, poss, results2012_11_24)

#October

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_10_24.RData")

classifier_glmnet(DF_2012_10_24_cleanESP)

DF_2012_10_24 <- df_tweets
colnames(DF_2012_10_24) <- c("text", "sentiment")

DF_2012_10_24$sentiment[DF_2012_10_24$sentiment <= 0.5] <- "0"
DF_2012_10_24$sentiment[DF_2012_10_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_10_24))
negs <- as.numeric(sum(DF_2012_10_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_10_24$sentiment == "1"))
results2012_10_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_10_24 <- cbind(tots, poss, negs, results2012_10_24)
results <- rbind(results, results2012_10_24)

rm(DF_2012_10_24_cleanESP, df_tweets, DF_2012_10_24, tots, negs, poss, results2012_10_24)

#September

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_9_24.RData")

classifier_glmnet(DF_2012_9_24_cleanESP)

DF_2012_9_24 <- df_tweets
colnames(DF_2012_9_24) <- c("text", "sentiment")

DF_2012_9_24$sentiment[DF_2012_9_24$sentiment <= 0.5] <- "0"
DF_2012_9_24$sentiment[DF_2012_9_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_9_24))
negs <- as.numeric(sum(DF_2012_9_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_9_24$sentiment == "1"))
results2012_9_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_9_24 <- cbind(tots, poss, negs, results2012_9_24)
results <- rbind(results, results2012_9_24)

rm(DF_2012_9_24_cleanESP, df_tweets, DF_2012_9_24, tots, negs, poss, results2012_9_24)

#August

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_8_24.RData")

classifier_glmnet(DF_2012_8_24_cleanESP)

DF_2012_8_24 <- df_tweets
colnames(DF_2012_8_24) <- c("text", "sentiment")

DF_2012_8_24$sentiment[DF_2012_8_24$sentiment <= 0.5] <- "0"
DF_2012_8_24$sentiment[DF_2012_8_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_8_24))
negs <- as.numeric(sum(DF_2012_8_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_8_24$sentiment == "1"))
results2012_8_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_8_24 <- cbind(tots, poss, negs, results2012_8_24)
results <- rbind(results, results2012_8_24)

rm(DF_2012_8_24_cleanESP, df_tweets, DF_2012_8_24, tots, negs, poss, results2012_8_24)

#July

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_12_24.RData")

classifier_glmnet(DF_2012_12_24_cleanESP)

DF_2012_12_24 <- df_tweets
colnames(DF_2012_12_24) <- c("text", "sentiment")

DF_2012_12_24$sentiment[DF_2012_12_24$sentiment <= 0.5] <- "0"
DF_2012_12_24$sentiment[DF_2012_12_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_12_24))
negs <- as.numeric(sum(DF_2012_12_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_12_24$sentiment == "1"))
results2012_12_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_12_24 <- cbind(tots, poss, negs, results2012_12_24)
results <- rbind(results, results2012_12_24)

rm(DF_2012_12_24_cleanESP, df_tweets, DF_2012_12_24, tots, negs, poss, results2012_12_24)

#June

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_6_24.RData")

classifier_glmnet(DF_2012_6_24_cleanESP)

DF_2012_6_24 <- df_tweets
colnames(DF_2012_6_24) <- c("text", "sentiment")

DF_2012_6_24$sentiment[DF_2012_6_24$sentiment <= 0.5] <- "0"
DF_2012_6_24$sentiment[DF_2012_6_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_6_24))
negs <- as.numeric(sum(DF_2012_6_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_6_24$sentiment == "1"))
results2012_6_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_6_24 <- cbind(tots, poss, negs, results2012_6_24)
results <- rbind(results, results2012_6_24)

rm(DF_2012_6_24_cleanESP, df_tweets, DF_2012_6_24, tots, negs, poss, results2012_6_24)

#May

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_5_24.RData")

classifier_glmnet(DF_2012_5_24_cleanESP)

DF_2012_5_24 <- df_tweets
colnames(DF_2012_5_24) <- c("text", "sentiment")

DF_2012_5_24$sentiment[DF_2012_5_24$sentiment <= 0.5] <- "0"
DF_2012_5_24$sentiment[DF_2012_5_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_5_24))
negs <- as.numeric(sum(DF_2012_5_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_5_24$sentiment == "1"))
results2012_5_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_5_24 <- cbind(tots, poss, negs, results2012_5_24)
results <- rbind(results, results2012_5_24)

rm(DF_2012_5_24_cleanESP, df_tweets, DF_2012_5_24, tots, negs, poss, results2012_5_24)

#April

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_4_24.RData")

classifier_glmnet(DF_2012_4_24_cleanESP)

DF_2012_4_24 <- df_tweets
colnames(DF_2012_4_24) <- c("text", "sentiment")

DF_2012_4_24$sentiment[DF_2012_4_24$sentiment <= 0.5] <- "0"
DF_2012_4_24$sentiment[DF_2012_4_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_4_24))
negs <- as.numeric(sum(DF_2012_4_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_4_24$sentiment == "1"))
results2012_4_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_4_24 <- cbind(tots, poss, negs, results2012_4_24)
results <- rbind(results, results2012_4_24)

rm(DF_2012_4_24_cleanESP, df_tweets, DF_2012_4_24, tots, negs, poss, results2012_4_24)

#March

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_3_24.RData")

classifier_glmnet(DF_2012_3_24_cleanESP)

DF_2012_3_24 <- df_tweets
colnames(DF_2012_3_24) <- c("text", "sentiment")

DF_2012_3_24$sentiment[DF_2012_3_24$sentiment <= 0.5] <- "0"
DF_2012_3_24$sentiment[DF_2012_3_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_3_24))
negs <- as.numeric(sum(DF_2012_3_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_3_24$sentiment == "1"))
results2012_3_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_3_24 <- cbind(tots, poss, negs, results2012_3_24)
results <- rbind(results, results2012_3_24)

rm(DF_2012_3_24_cleanESP, df_tweets, DF_2012_3_24, tots, negs, poss, results2012_3_24)

#February

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_2_24.RData")

classifier_glmnet(DF_2012_2_24_cleanESP)

DF_2012_2_24 <- df_tweets
colnames(DF_2012_2_24) <- c("text", "sentiment")

DF_2012_2_24$sentiment[DF_2012_2_24$sentiment <= 0.5] <- "0"
DF_2012_2_24$sentiment[DF_2012_2_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_2_24))
negs <- as.numeric(sum(DF_2012_2_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_2_24$sentiment == "1"))
results2012_2_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_2_24 <- cbind(tots, poss, negs, results2012_2_24)
results <- rbind(results, results2012_2_24)

rm(DF_2012_2_24_cleanESP, df_tweets, DF_2012_2_24, tots, negs, poss, results2012_2_24)

#January

load(file = "Objects/Tweets/Series_24/Clean/ESP/DF_2012_1_24.RData")

classifier_glmnet(DF_2012_1_24_cleanESP)

DF_2012_1_24 <- df_tweets
colnames(DF_2012_1_24) <- c("text", "sentiment")

DF_2012_1_24$sentiment[DF_2012_1_24$sentiment <= 0.5] <- "0"
DF_2012_1_24$sentiment[DF_2012_1_24$sentiment > 0.5] <- "1"

tots <- as.numeric(nrow(DF_2012_1_24))
negs <- as.numeric(sum(DF_2012_1_24$sentiment == "0"))
poss <- as.numeric(sum(DF_2012_1_24$sentiment == "1"))
results2012_1_24 <- data.frame(Score = ((poss/tots)-(negs/tots)))
results2012_1_24 <- cbind(tots, poss, negs, results2012_1_24)
results24 <- rbind(results, results2012_1_24)

rm(DF_2012_1_24_cleanESP, df_tweets, DF_2012_1_24, tots, negs, poss, results2012_1_24)

save(results24, file = "Objects/Models/resultsF24.RData")

print(difftime(Sys.time(), t1, units = 'mins'))

