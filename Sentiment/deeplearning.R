
# loading packages

library(tidyverse)
library(purrrlyr)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)

conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

# data splitting on train and test
set.seed(123)

load("Objects/TASS/TASScorpus_clean.RData")

alg_tweets <- TASScorpus_clean

alg_tweets$sentiment[alg_tweets$sentiment == "Neg"] <- "0"
alg_tweets$sentiment[alg_tweets$sentiment == "Pos"] <- "1"

set.seed(123)

trainIndex <- createDataPartition(alg_tweets$sentiment, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

tweets_train <- alg_tweets[trainIndex, ]
tweets_test <- alg_tweets[-trainIndex, ]

# define preprocessing function and tokenization function

prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(as.character(tweets_train$content), 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun,
                   progressbar = TRUE)
it_test <- itoken(as.character(tweets_test$content), 
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
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # 10-fold cross-validation
                               nfolds = 10,
                               # high value is less accurate, but has faster training
                               thresh = 1e-2,
                               # again lower number of iterations for faster training
                               maxit = 1e3)

print(difftime(Sys.time(), t1, units = 'mins'))

plot(glmnet_classifier)

glmnet_classifier$lambda.min #value of λ that gives minimum mean cross-validated error
glmnet_classifier$lambda.1se #most regularized model such that error is within one standard error of the minimum

print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response', s = "lambda.min")[ ,1]

predictions <- data.frame(preds)

results <- cbind(tweets_test, predictions)

#Testing accuracy

results$preds[results$preds < "0.5"] <- "0"
results$preds[results$preds >= "0.5"] <- "1"

resultstable <- table(observed = results$sentiment, predicted = results$preds)

resultstable

accuracy <- ((resultstable[1,1] + resultstable[2,2])/8793)
print(accuracy)

#Accuracy - 85,2%

save(glmnet_classifier,file = "Objects/Models/glmnet_classifier.RData")
save(vectorizer,file = "Objects/Models/vectorizer.RData")


