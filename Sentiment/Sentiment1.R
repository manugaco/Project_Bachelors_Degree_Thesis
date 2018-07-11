

#Algorith tests

library("RTextTools")
library("tidyverse")
library("e1071")
library("tm")
library("sentiment")
library("qdap")
library("SnowballC")
library("xtable")
library("devtools")
library("sentR")

#Testing the accuracy of the sentiment packages

#sentiment package

TASScorpus <- as.character(TASScorpus_clean$content)

sentiments <- sentiment::sentiment(TASScorpus)

table(sentiments$polarity)

sentiments$polarity <- gsub("positive", "Pos", sentiments$polarity)
sentiments$polarity <- gsub("negative", "Neg", sentiments$polarity)
sentiments$polarity <- gsub("neutral", "Neu", sentiments$polarity)

#Confusion Matrix (testing accuracy)

table(observed = TASScorpus_clean$sentiments, predicted = sentiments$polarity)

#qdap package

polarity <- qdap::polarity(TASScorpus)

polaritydf <- data.frame(unlist(lapply(polarity, '[[', 3)))
polaritydf <- as.data.frame(polaritydf[-nrow(polaritydf),])

colnames(polaritydf) <- c("qdap")

polaritydf$qdap <- as.character(polaritydf$qdap)
polaritydf$qdap[polaritydf$qdap > 0] <- "Pos"
polaritydf$qdap[polaritydf$qdap < 0] <- "Neg"
polaritydf$qdap[polaritydf$qdap == 0] <- "Neu"

table(observed = TASScorpus_clean$sentiments, predicted = polaritydf$qdap)

#sentR (words classifier)

testw <- TASScorpus

pos <- read_delim("~/Desktop/TFG/Datasets/Corpus/isol/positivas_mejorada.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
posw <- as.vector(pos)
neg <- read.csv("~/Desktop/TFG/Datasets/Corpus/isol/negativas_mejorada.csv")

negw <- as.vector(neg)

out <- classify.aggregate(testw, posw, negw)
out

out <- classify.naivebayes(testw)

sentR <- data.frame(out)

table(observed = TASScorpus_clean[1:6285, 2], predicted = sentR$SENT)

#Microsoft Cognitive Services API
library(mscstexta4r)
textaInit()



