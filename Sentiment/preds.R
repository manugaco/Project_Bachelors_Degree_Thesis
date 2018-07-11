
library(tidyverse)
library(purrrlyr)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)


# function for converting some symbols
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

##### loading classified tweets ######

TASScorpus_clean$sentiment <- gsub("Neg", "0",  TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("Pos", "1",  TASScorpus_clean$sentiment)

tweets_classified <- TASScorpus_clean %>%
  dmap_at('content', conv_fun)


#splitting on train and test
set.seed(2340)
trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]


# define preprocessing function and tokenization function
prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(tweets_train$content, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   progressbar = TRUE)
it_test <- itoken(tweets_test$content, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  progressbar = TRUE)

# creating vocabulary and document-term matrix

vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)

# define tf-idf model

tfidf <- TfIdf$new()

# fit the model to the train data and transform it with the fitted model

dtm_train_tfidf <- fit_transform(dtm_train, tfidf)

# apply pre-trained tf-idf transformation to test data

dtm_test_tfidf  <- create_dtm(it_test, vectorizer) %>% 
  transform(tfidf)

# train the model
t1 <- Sys.time()
glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf,
                               y = tweets_train[['sentiment']], 
                               family = 'binomial', 
                               alpha = 1,
                               type.measure = "auc",
                               nfolds = 10,
                               thresh = 1,
                               maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'mins'))

plot(glmnet_classifier)

glmnet_classifier$lambda.min #value of λ that gives minimum mean cross-validated error

glmnet_classifier$lambda.1se #most regularized model such that error is within one standard error of the minimum

preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response', s = "lambda.min")[ ,1]
auc(as.numeric(tweets_test$sentiment), preds)

predictions <- data.frame(preds)

alg_tweets <- cbind(tweets_test, predictions)

alg_tweets$preds[alg_tweets$preds <= "0.5"] <- "0"
alg_tweets$preds <- as.numeric(alg_tweets$preds)
alg_tweets$preds[alg_tweets$preds > "0.5"] <- "1"

table(alg_tweets$preds, alg_tweets$sentiment)

# save the model for future using
save(vectorizer, file = "Objects/vectorizer.RData")
save(glmnet_classifier, file ="Objects/glmnet_classifier.RData")



