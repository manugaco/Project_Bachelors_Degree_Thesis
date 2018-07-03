

#Cross-validation

library("tidyverse")
library("RTextTools")
library("e1071")
library("caret")
library("ggplot2")
library("sentiment")
library("klaR")
library("kernlab")

#Training method with a sample 20000 of the original dataset to save time testing

#RTextTools and e1071 configuration dataset

set.seed(123)
pos <- data.frame(sample_n((TASScorpustrain_pos), 8000))
neg <- data.frame(sample_n((TASScorpustrain_neg), 8000))
test <- data.frame(sample_n((TASScorpustest), 4000))

alg_tweets <- rbind(pos, neg, test)

save(alg_tweets, file = "Objects/TASS/TASSalg_tweets.RData")

matrix_dtm <- create_matrix(alg_tweets[,1], language="spanish")
mat_dtm <- as.matrix(matrix_dtm)

container <- create_container(matrix_dtm, as.numeric(as.factor(alg_tweets[,2])),
                             trainSize = 1:16000, testSize = 16001:20000, virgin = FALSE)

#caret configuration dataset

vectorcv <- alg_tweets$sentiment
matrixcv <- data.frame(alg_tweets$content)

#Cross-validation for classification models, 10 fold validation

#Naive Bayes (caret)

set.seed(123)

trControl <- trainControl(method = "cv", number = 10, verbose = TRUE)

cv_nb <- train(matrixcv, vectorcv, trControl=trControl, method = "nb")

cv_nb$modelInfo #Accuracy 56,25%

#Support Vector Machines

set.seed(123)

cv_svm <- cross_validate(container, 10, algorithm ="SVM", verbose = TRUE)

cv_svm #Accuracy 82,22%

#Making the models to test the accuracy in split mode

models = train_models(container, algorithms=c("SVM"), verbose = TRUE)

results = classify_models(container, models)

#Accuracy table

table(as.numeric(as.factor(alg_tweets[16001:20000, 2])), results[,"SVM_LABEL"])


#Recall accuracy

recall_accuracy(as.numeric(as.factor(alg_tweets[16001:20000, 2])), results[,"SVM_LABEL"]) #85.12%

save(cv_nb, file = "Objects/Models/CV_NBresults.RData")
save(cv_svm, file = "Objects/Models/CV_SVMresults.RData")


save(results, file = "Objects/Models/ResultsRTxtools.RData")



