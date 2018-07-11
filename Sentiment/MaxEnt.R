

#Maximum Entropy Algorithm

library("RTextTools")
library(tidyverse)
library("e1071")
library("ggplot2")
library("maxent")
library("klaR")
library("caret")
library("sentiment")

#Building the dtm (document-term matrix)

corpusME <- Corpus(VectorSource(alg_tweets$content))
matrixME <- DocumentTermMatrix(corpusME)
sparseME <- as.compressed.matrix(matrixME)

#Cross-Validation and optimization of Maximum Entropy model hyperparameters (maxent package)

set.seed(123)

sentimentsME <- as.vector(alg_tweets$sentiment) #vector of values

ME_cv <- tune.maxent(sparseME, sentimentsME, nfold=10, showall = TRUE, verbose = TRUE)

summary(ME_cv)

#Performances table

ME_cvDF <- data.frame(ME_cv)

table(ME_cv) #L2 regularicer = 0.8

modelME_fitted <- maxent(sparseME[1:8000,], as.factor(alg_tweets[1:8000,2]), l2_regularizer=0.8, verbose = TRUE)

modelME_fitted

#Testing accuracy

predictedME_fitted <- data.frame(predict.maxent(modelME_fitted, sparseME[1:10000,]))

table(observed = alg_tweets[8001:10000, 2], predicted = predictedME_fitted[8001:10000, 1])

recall_accuracy(alg_tweets[8001:10000, 2], predictedME_fitted[8001:10000, 1]) #0.8285 - 82.85%

errorsME_fitted <- sum(alg_tweets[8001:10000, 2] != predictedME_fitted[8001:10000, 1])
errorsME_fitted <- 100 * mean(alg_tweets[8001:10000, 2] != predictedME_fitted[8001:10000, 1])

paste("Number of errors =", errorsME_fitted)
paste("Error % =", round(errorsME_fitted, 2), "%") #17.15%

