

#SVM algorithm

library("RTextTools")
library("e1071")
library("ggplot2")
library("klaR")
library("caret")
library("tidyverse")
library("SnowballC")

#Building the dtm (document-term matrix)

matrix_dtm <- create_matrix(alg_tweets[,1], language="spanish")
mat_dtm <- as.matrix(matrix_dtm)

str(mat_dtm)

save(mat_dtm, file = "Objects/TASS/TASSmatrix_dtm.RData")

#Cross-Validation and optimization of SVM model parameters (e1071 package)

set.seed(123)

svm_cv <- tune("svm", train.x =  mat_dtm[1:16000,],
               train.y = as.factor(alg_tweets[1:16000,2]),
               kernel = "linear", 
               ranges = list(cost = c(0.1)))

tune.control(svm_cv)

#Performances plot of cross-validation parameters

summary(svm_cv)

svm_cv$best.parameters #cost - 0.1
svm_cv$best.performance #0.1544 (15.44% error)

save(svm_cv, file = "Objects/Models/scm_cv.RData")

#Support Vector Machine (SVM) Algorithm model

modelSVM <- svm(mat_dtm[1:16000,], as.factor(alg_tweets[1:16000,2]), 
                       kernel = "linear", cost = 0.1)

#Testing accuracy in the sample

set.seed(123)

predictedSVM <- predict(object = modelSVM, mat_dtm[16001:20000,])

resultsSVM <- table(observed = alg_tweets[16001:20000, 2], predicted = predictedSVM)

accuracySVM <- recall_accuracy(alg_tweets[16001:20000, 2], predictedSVM) #0.8468 - 84.68 %

errorsSVM <- sum(alg_tweets[16001:20000, 2] != predictedSVM)
errorsSVM <- 100 * mean(alg_tweets[16001:20000, 2] != predictedSVM)

paste("Number of errors =", errorsSVM)
paste("Error % =", round(errorsSVM,2),"%") #15.32 %

save(modelSVM, file = "Objects/Models/SVM85.RData")
save(predictedSVM, file = "Objects/Models/SVM85_pred.RData")
save(resultsSVM, file = "Objects/Models/resultsSVM85.RData")
save(accuracySVM, file = "Objects/Models/accuracySVM85.RData")

#Testing accuracy on the TASS dataset

T1 <- data.frame(sample_n(TASScorpus_clean, 1000))

levels(T1$content) <- as.vector(alg_tweets$content)

#new_matrix <- create_matrix(cbind(NYTimes["Title"],NYTimes["Subject"]), 
#language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf, originalMatrix=matrix)

#originalMatrix=(you original training matrix)

levels(T1$b) <- C("a", "b", "c")

matrix_comp <- create_matrix(T1[,1], language = "spanish", originalMatrix = mat_dtm) #testing
mat_comp <- as.matrix(matrix_comp)

str(mat_comp)

predictedSVM_fittedTASS <- predict(object = modelSVM_fitted, mat_comp[1:1000,], decision.values=TRUE, probability=TRUE) 

table(observed = TASScorpus_clean[1:43966, 2], predicted = predictedSVM_fittedTASS)

recall_accuracy(TASScorpus_clean[1:43966, 2], predictedSVM_fittedTASS) #

errorsSVM_fittedTASS <- sum(TASScorpus_clean[1:43966, 2] != predictedSVM_fittedTASS)
errorsSVM_fittedTASS <- 100 * mean(TASScorpus_clean[1:43966, 2] != predictedSVM_fittedTASS)

paste("Number of errors =", errorsSVM_fittedTASS)
paste("Error % =", round(errorsSVM_fittedTASS, 2),"%") #
