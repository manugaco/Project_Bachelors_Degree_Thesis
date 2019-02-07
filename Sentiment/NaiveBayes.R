

#Algorith tests (from script "CorpusTASS")

library("RTextTools")
library("e1071")
library("ggplot2")
library("klaR")
library("caret")

#Building the dtm (document-term matrix)

matrix <- create_matrix(alg_tweets[,1], language="spanish")
mat <- as.matrix(matrix)

#Training the model (Naive Bayes Algorith) ---

modelNB <- naiveBayes(mat[1:15000,], as.factor(alg_tweets[1:15000,2])) #1:15000

modelNB

#Testing accuracy

set.seed(123)

predictedNB <- predict(modelNB, mat[15001:18750,]) #15001:18750

table(observed = alg_tweets[15001:22500, 2], predicted = predictedNB)

recall_accuracy(alg_tweets[15001:22500, 2], predictedNB) #0.5865333

errorsNB <- sum(alg_tweets[15001:22500, 2] != predictedNB)
errorsNB <- 100 * mean(alg_tweets[15001:22500, 2] != predictedNB)

paste("Number of errors =", errorsNB)
paste("Error% =", round(errorsNB, 2),"%") #44.49%

#Cross-Validation and optimization of NaiveBayes model hyperparameters (Caret Package)

set.seed(123)

corpusNB <- Corpus(VectorSource(alg_tweets))
matrixNB <- DocumentTermMatrix(corpusNB)
sparseNB <- as.compressed.matrix(matrixNB)

NB_cv <- train(matrixNB[1:18750,], corpusNB[1:18750, 2],'nb', trControl=trainControl(method='cv',number=10))

summary(NB_cv)

predictedNB <- predict(modelNB, mat[15001:18750,]) #15001:18750

table(observed = alg_tweets[15001:18750, 2], predicted = predictedNB)

recall_accuracy(alg_tweets[15001:18750, 2], predictedNB) #0.5551118

errorsNB <- sum(alg_tweets[15001:18750, 2] != predictedNB)
errorsNB <- 100 * mean(alg_tweets[15001:18750, 2] != predictedNB)

modelNB_fitted <- naiveBayes(x = matriz_tfidf_train, y = as.factor(tweets_train$autor), cost = 0.5, scale = TRUE)

predicciones <- predict(object = modelo_svm, newdata = matriz_tfidf_test)
table(observado = tweets_test$autor, predicho = predicciones)

