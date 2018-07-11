
require("XML")
library("tidyverse")
library("tm")
library("caTools")

#Importing the TASS xml files

cols <- c("tweetid", "user", "content", "date", "lang", "sentiments")

#2017 Inter

corpusa <- xmlParse("http://www.sepln.org/workshops/tass/tass_data/dataset/intertass-train-tagged.xml")
rootNodea <- xmlRoot(corpusa)
dataa <- xmlSApply(rootNodea,function(x) xmlSApply(x, xmlValue))
corpusa <- data.frame(t(dataa),row.names=NULL)

corpusb <- xmlParse("http://www.sepln.org/workshops/tass/tass_data/dataset/intertass-development-tagged.xml")
rootNodeb <- xmlRoot(corpusb)
datab <- xmlSApply(rootNodeb,function(x) xmlSApply(x, xmlValue))
corpusb <- data.frame(t(datab),row.names=NULL)

corpusc <- xmlParse("http://www.sepln.org/workshops/tass/tass_data/dataset/intertass-test.xml")
rootNodec <- xmlRoot(corpusc)
datac <- xmlSApply(rootNodec,function(x) xmlSApply(x, xmlValue))
corpusc <- data.frame(t(datac),row.names=NULL)
corpusc <- subset(corpusc, select = -sentiment)

corpusc_2 <- read.table("http://www.sepln.org/workshops/tass/tass_data/dataset/intertass-sentiment.qrel",
                        sep="\t", header=FALSE)

colnames(corpusc_2) <- c("id", "sentiment")

corpusc <- cbind(corpusc, corpusc_2$sentiment)

colnames(corpusc) <- c("tweetid", "user", "content", "date", "lang", "sentiment")

TASScorpus <- rbind(corpusa, corpusb, corpusc)

TASScorpus <- TASScorpus[!duplicated(TASScorpus), ]
TASScorpus <- TASScorpus[!(TASScorpus$sentiment == ""), ]

rm(corpusa, rootNodea, dataa, corpusb, rootNodeb, datab, corpusc, rootNodec, datac, corpusc_2)

#2012 general

corpusa <- xmlParse("http://www.sepln.org/workshops/tass/tass_data/dataset/general-train-tagged-3l.xml")
rootNodea <- xmlRoot(corpusa)
dataa <- xmlSApply(rootNodea,function(x) xmlSApply(x, xmlValue))
corpusa <- data.frame(t(dataa),row.names=NULL)

corpusa <- corpusa %>%
  dplyr:::select(cols)

colnames(corpusa) <- c("tweetid", "user", "content", "date", "lang", "sentiment")

corpusb <- xmlParse("http://www.sepln.org/workshops/tass/tass_data/dataset/general-test-tagged-3l.xml")
rootNodeb <- xmlRoot(corpusb)
datab <- xmlSApply(rootNodeb,function(x) xmlSApply(x, xmlValue))
corpusb <- data.frame(t(datab),row.names=NULL)

corpusb <- corpusb %>%
  dplyr:::select(cols)

colnames(corpusb) <- c("tweetid", "user", "content", "date", "lang", "sentiment")

TASScorpus <- rbind(TASScorpus, corpusa, corpusb)
TASScorpus <- TASScorpus[!duplicated(TASScorpus), ]
TASScorpus <- TASScorpus[!(TASScorpus$sentiment == ""), ]

rm(corpusa, rootNodea, dataa, corpusb, rootNodeb, datab)

save(TASScorpus, file = "Objects/Tweets/TASScorpus.RData")

#Selecting the labeled text and its related sentiment

TASScorpus_clean <- select(TASScorpus, c("content", "sentiment"))

#Cleaning the text column

TASScorpus_clean <- TASScorpus_clean[!(is.na(TASScorpus_clean$content) | TASScorpus_clean$content==""), ]

TASScorpus_clean$content <- gsub("http://www. ", "http://www.", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub("https://www. ", "http://www.", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub("http:// ", "http://www.", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub("https:// ", "http://www.", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub(" http://", "http://www.", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub("http[[:alnum:][:punct:]]*", "", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub("pic.twitter.com/", "http://www.", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub("#[[:alpha:][:alnum:]]*", "", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub("@[[:alpha:][:alnum:]]*", "", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub(" RT", "", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub("RT", "", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub("[[:alnum:][:punct:]]*.es", "", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub("[[:alnum:][:punct:]]*.html", "", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub(" #[[:alpha:][:alnum:]]*", "", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub(" @[[:alpha:][:alnum:]]*", "", TASScorpus_clean$content)
TASScorpus_clean$content <- gsub("[^[:alpha:][:space:]]*", "", TASScorpus_clean$content)

#Cleaning the sentiment column

TASScorpus_clean$sentiment <- gsub("AGREEMENT[[:alpha:][:alnum:]]*", "", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("DIS[[:alpha:][:alnum:]][[:punct:]]*", "", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("@[[:alpha:][:alnum:]]*", "", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("#[[:alpha:][:alnum:]]*", "", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("NEU[[:alpha:][:alnum:]]*", "", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("NONE[[:alpha:][:alnum:]]*", "", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("N[[:alpha:][:alnum:]]*", "Neg", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("P[[:alpha:][:alnum:]]*", "Pos", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("_[[:alpha:][:alnum:]]*", "", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("[[:alpha:][:alnum:]]*NEU", "NEU", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub(" [[:alpha:][:alnum:]]*", "", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("psoe[[:alpha:][:alnum:]]*", "", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("pp[[:alpha:][:alnum:]]*", "", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("rajoy", "", TASScorpus_clean$sentiment)
TASScorpus_clean$sentiment <- gsub("[[:punct:]][[:alpha:][:alnum:]]*", "", TASScorpus_clean$sentiment)

TASScorpus_clean <- TASScorpus_clean[!(TASScorpus_clean$sentiment == ""), ]

TASScorpus_clean$content <- TASScorpus_clean$content %>%
  tolower() %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

TASScorpusv <- as.vector(TASScorpus_clean$content)

save(TASScorpusv, file = "Objects/Tweets/TASScorpusv.RData")

TASScorpus_clean <- TASScorpus_clean[!(TASScorpus_clean$content == ""), ]

TASScorpus_clean$content <- removeWords(TASScorpus_clean$content , stopwords("spanish"))

#Stemming the column

TASScorpus_clean_content <- as.data.frame(stemDocument(TASScorpus_clean$content, language="spanish"))

colnames(TASScorpus_clean_content) <- c("content")

TASScorpus_clean_sentiment <- select(TASScorpus_clean, "sentiment")

TASScorpus_clean <- cbind(TASScorpus_clean_content, TASScorpus_clean_sentiment)

rm(TASScorpus_clean_content, TASScorpus_clean_sentiment)

TASScorpus_clean <- TASScorpus_clean[!(TASScorpus_clean$content == ""), ]

TASScorpus_clean[TASScorpus_clean==" "] <- NA
TASScorpus_clean[TASScorpus_clean==""] <- NA

TASScorpus_clean <- na.omit(TASScorpus_clean)

TASScorpus_clean <- TASScorpus_clean[!duplicated(TASScorpus_clean), ]

save(TASScorpus_clean, file = "Objects/Tweets/TASScorpus_clean.RData")

#Final count of positive and negative words

table(TASScorpus_clean$sentiment)

glimpse(TASScorpus_clean)

#Split randomly the dataset into train and test samples

set.seed(123)

TASSsize <- floor(0.8*nrow(TASScorpus_clean))
corpusbase <- sample(seq_len(nrow(TASScorpus_clean)), size = TASSsize)

TASScorpustrain <- TASScorpus_clean[corpusbase, ]
TASScorpustest <- TASScorpus_clean[-corpusbase, ]

rm(TASSsize, corpusbase)

#Checking there are no repeated values in both samples

nrow(intersect(TASScorpustrain, TASScorpustest)) #0

#Split the training set into positive and negative

TASScorpustrain_pos <- TASScorpustrain[(TASScorpustrain$sentiment == "Pos"), ]

TASScorpustrain_neg <- TASScorpustrain[(TASScorpustrain$sentiment == "Neg"), ]

save(TASScorpustest, file = "Objects/Tweets/TASScorpustest.RData")
save(TASScorpustrain_pos, file = "Objects/Tweets/TASScorpustrain_pos.RData")
save(TASScorpustrain_neg, file = "Objects/Tweets/TASScorpustrain_neg.RData")

