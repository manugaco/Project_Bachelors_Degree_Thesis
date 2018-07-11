
library("tidyverse")
library("splitstackshape")
library("textcat")
library("cldr")
library("lubridate")
library("tm")
library("xlsx")
library("readxl")
library("data.table")
library("xtable")
library("SnowballC")
library("wordcloud")
library("factoextra")
library("graph")
library("Rgraphviz")
library("topicmodels")
require(devtools)
library("sentiment")
library("RJSONIO")

#Import datasets and merge

Corpus_1 <- read_delim("~/Desktop/TFG/Datasets/Test-Train/corpustrain.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)

#Select columns

Corpus_1 <- select(Corpus_1, c("text"))

#Cleaning the text (regular expresions)

Corpus_1 <- Corpus_1[!(is.na(Corpus_1$text) | Corpus_1$text==""), ]
Corpus_1$text <- gsub("http://www. ", "http://www.", Corpus_1$text)
Corpus_1$text <- gsub("https://www. ", "http://www.", Corpus_1$text)
Corpus_1$text <- gsub("http:// ", "http://www.", Corpus_1$text)
Corpus_1$text <- gsub("https:// ", "http://www.", Corpus_1$text)
Corpus_1$text <- gsub("pic.twitter.com/", "http://www.", Corpus_1$text)
Corpus_1$text <- gsub(" …", ".html", Corpus_1$text)
Corpus_1$text <- gsub("/s tatus/", "", Corpus_1$text)
Corpus_1$text <- gsub(".0", "", Corpus_1$text)
Corpus_1$text <- gsub("http[[:alnum:][:punct:]]*", "", Corpus_1$text)
Corpus_1$text <- gsub("[[:alnum:][:punct:]]*.html", "", Corpus_1$text)
Corpus_1$text <- gsub("rt [[:alnum:][:punct:]]*", "", Corpus_1$text)
Corpus_1$text <- gsub("RT [[:alnum:][:punct:]]*", "", Corpus_1$text)
Corpus_1$text <- gsub("#[[:alpha:][:alnum:]]*", "", Corpus_1$text)
Corpus_1$text <- gsub("@[[:alpha:][:alnum:]]*", "", Corpus_1$text)
Corpus_1$text <- gsub("[^[:alpha:][:space:]]*", "", Corpus_1$text)

Corpus_1$text <- Corpus_1$text %>%
  tolower() %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

Corpus_1[Corpus_1==" "] <- NA
Corpus_1[Corpus_1==""] <- NA

Corpus_1 <- Corpus_1[complete.cases(Corpus_1), ]

#Filtering the languages of the column text

text_cat <- textcat(Corpus_1$text)
Corpus_1 <- cbind(Corpus_1, text_cat)

cldr <- detectLanguage(Corpus_1)
cldr <- (cldr[,c("detectedLanguage")])
Corpus_1 <- cbind(Corpus_1, cldr)
Corpus_1$cldr <- Corpus_1$cldr %>%
  tolower()

#Languages pre-filter
#Regional languages

Corpus_1_all <- Corpus_1[(Corpus_1$text_cat == "spanish") | (Corpus_1$cldr == "spanish") |
                                 (Corpus_1$text_cat == "catalan") | (Corpus_1$cldr == "catalan") |
                                 (Corpus_1$text_cat == "galician") | (Corpus_1$cldr == "galician") |
                                 (Corpus_1$text_cat == "basque") | (Corpus_1$cldr == "basque"), ]

#Spanish only

Corpus_1_ESP <- Corpus_1[(Corpus_1$text_cat == "spanish") | (Corpus_1$cldr == "spanish"), ]

#Deleting repeated tweets

Corpus_1_ESP <- Corpus_1_ESP[!duplicated(Corpus_1_ESP$text), ]

Corpus_1_ESP <- select(Corpus_1_ESP, c("text"))

stemCorpusESP <- stemDocument(Corpus_1_ESP, language="spanish")

save(Corpus_1_ESP, file = "Objects/Corpus/Corpus_1_ESP.RData")

#Making the labeling

Corpus <- Corpus(VectorSource(Corpus_1_ESP$text))

Corpustdm <- Corpus %>%  
  tm_map(removeWords, stopwords("spanish"))

Corpuscopia <- tm_map(Corpus, stemDocument, language="spanish")

Corpuscopia <- Corpuscopia %>%  
  tm_map(removeWords, stopwords("spanish"))

CorpuscopiaDF <- data.frame(text = sapply(Corpuscopia, as.character), stringsAsFactors = FALSE)

#Sentiment Analysis

C1sentiment <- sentiment(Corpus_1_ESP$text)

table(sentiments$polarity)
subset(sentiments$text, sentiments$polarity == "positive")

write.csv(CorpuscopiaDF, file = "Objects/Corpus/Corpus_1_ESP_labeled.csv", row.names=FALSE)






