library(twitteR)
library(ROAuth)
library(tidyverse)
library(purrrlyr)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)

### loading and preprocessing a training set of tweets
# function for converting some symbols
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

##### loading classified tweets ######
# source: <a class="vglnk" href="http://help.sentiment140.com/for-students/" rel="nofollow"><span>http</span><span>://</span><span>help</span><span>.</span><span>sentiment140</span><span>.</span><span>com</span><span>/</span><span>for</span><span>-</span><span>students</span><span>/</span></a>
# 0 - the polarity of the tweet (0 = negative, 4 = positive)
# 1 - the id of the tweet
# 2 - the date of the tweet
# 3 - the query. If there is no query, then this value is NO_QUERY.
# 4 - the user that tweeted
# 5 - the text of the tweet

TASScorpus_clean$sentiment <- gsub("Neg", "0",  TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("Pos", "1",  TASScorpus_clean$sentiment)

tweets_classified <- TASScorpus_clean %>%
  # converting some symbols
  dmap_at('content', conv_fun)


# data splitting on train and test
set.seed(2340)
trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]

##### Vectorization #####
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
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # 10-fold cross-validation
                               nfolds = 10,
                               # high value is less accurate, but has faster training
                               thresh = 1,
                               # again lower number of iterations for faster training
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




load(file = "Objects/vectorizer.RData")
load(file = "Objects/glmnet_classifier.RData")
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

df_tweets <- dataset %>%
  # converting some symbols
  dmap_at('text', conv_fun)

prep_fun <- tolower
tok_fun <- word_tokenizer
# preprocessing and tokenization
it_tweets <- itoken(df_tweets$text,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    progressbar = TRUE)



# creating vocabulary and document-term matrix
dtm_tweets <- create_dtm(it_tweets, vectorizer)

# transforming data with tf-idf
tfidf <- TfIdf$new()

dtm_tweets_tfidf <- fit_transform(dtm_tweets, tfidf)

# predict probabilities of positiveness
preds_tweets <- predict(glmnet_classifier, dtm_tweets_tfidf, type = 'response')[ ,1]

# adding rates to initial dataset
df_tweets$sentiment <- preds_tweets





