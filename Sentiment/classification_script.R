
#Sentiment Analysis

#Libraries

library("tidyverse")
library("xlsx")
library("readxl")
library("data.table")
library("xtable")
library("foreign")
library("rvest")
library("tm")
library("textcat")
library("cldr")
library("SnowballC")
library("wordcloud")
library("factoextra")
library("graph")
library("Rgraphviz")
library("topicmodels")
require(devtools)
library("sentiment")

#Import the twitts from the source

tweetssample <- read.csv2("~/Desktop/TFG/Datasets/Tweetssample.csv")

tweetssample2 <- select(Tweetssample, c("username", "date", "text"))

#Cleaning the text (regular expresions)

tweetssample2 <- tweetssample2[!(is.na(tweetssample2$text) | tweetssample2$text==""), ]
tweetssample2$text <- gsub("http://www. ", "http://www.", tweetssample2$text)
tweetssample2$text <- gsub("https://www. ", "http://www.", tweetssample2$text)
tweetssample2$text <- gsub("http:// ", "http://www.", tweetssample2$text)
tweetssample2$text <- gsub("https:// ", "http://www.", tweetssample2$text)
tweetssample2$text <- gsub("pic.twitter.com/", "http://www.", tweetssample2$text)
tweetssample2$text <- gsub(" …", ".html", tweetssample2$text)
tweetssample2$text <- gsub("/s tatus/", "", tweetssample2$text)
tweetssample2$text <- gsub(".0", "", tweetssample2$text)
tweetssample2$text <- gsub("http[[:alnum:][:punct:]]*", "", tweetssample2$text)
tweetssample2$text <- gsub("[[:alnum:][:punct:]]*.html", "", tweetssample2$text)
tweetssample2$text <- gsub("rt [[:alnum:][:punct:]]*", "", tweetssample2$text)
tweetssample2$text <- gsub("(RT)((?:\\b\\W*@\\w+)+)", "", tweetssample2$text)
tweetssample2$text <- gsub("#[[:alpha:][:alnum:]]*", "", tweetssample2$text)
tweetssample2$text <- gsub("@[[:alpha:][:alnum:]]*", "", tweetssample2$text)
tweetssample2$text <- gsub("[^[:alpha:][:space:]]*", "", tweetssample2$text)

tweetssample3 <- tweetssample2

tweetssample2$text <- tweetssample2$text %>%
  tolower() %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

#Filtering the languages of the column text

idiomatextcat <- textcat(tweetssample2$text)
tweetssample2 <- cbind(tweetssample2, idioma)
idiomacldr <- results<-detectLanguage(tweetssample2)
idiomacldr <- (idiomacldr[,c("detectedLanguage")])
tweetssample2 <- cbind(tweetssample2, idiomacldr)
tweetssample2$idiomacldr <- tweetssample2$idiomacldr %>%
  tolower()

tweetssample3 <- tweetssample2 

#Language pre-filter

tweetssample3 <- tweetssample3[(tweetssample3$idioma == "spanish") | (tweetssample3$idiomacldr == "spanish") |
                                 (tweetssample3$idioma == "catalan") | (tweetssample3$idiomacldr == "catalan") |
                                 (tweetssample3$idioma == "galician") | (tweetssample3$idiomacldr == "galician") |
                                 (tweetssample3$idioma == "basque") | (tweetssample3$idiomacldr == "basque") |
                                 (tweetssample3$idioma == "portuguese") | (tweetssample3$idiomacldr == "portuguese"), ]

tweetssample3 <- tweetssample3[complete.cases(tweetssample3), ]

#Spanish only

tweetssample4 <- tweetssample3[(tweetssample3$idioma == "spanish") | (tweetssample3$idiomacldr == "spanish"), ]

#Deleting repeated tweets

tweetssample4 <- tweetssample4[!duplicated(tweetssample4$text), ]

save(tweetssample4, file = "Objects/tweetssample4.RData")

#Loading the corpus

myCorpusCopy <- myCorpusc

#root words/stemming

myCorpusd <- tm_map(myCorpusc, stemDocument, language="spanish")

#Sentiments analysis

sentiments <- sentiment(corpustassclean$content)
table(sentiments$polarity)

subset(sentiments$text, sentiments$polarity == "positive")
subset(sentiments$text, sentiments$polarity == "negative")
