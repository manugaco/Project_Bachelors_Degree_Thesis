

#Script to clean the twitts
#Libraries

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

#Example

load("Objects/Tweets/Series_7/Dirty/DF_2017_12_7.RData")

dataset <- DF_2017_12_7

#Cleaning the text (regular expresions)

dataset <- select(dataset, "text")

dataset$text <- gsub("http://www. ", "http://www.", dataset$text)
dataset$text <- gsub("https://www. ", "http://www.", dataset$text)
dataset$text <- gsub("http:// ", "http://www.", dataset$text)
dataset$text <- gsub("https:// ", "http://www.", dataset$text)
dataset$text <- gsub("pic.twitter.com/", "http://www.", dataset$text)
dataset$text <- gsub(" …", ".html", dataset$text)
dataset$text <- gsub("/s tatus/", "", dataset$text)
dataset$text <- gsub(".0", "", dataset$text)
dataset$text <- gsub("http[[:alnum:][:punct:]]*", "", dataset$text)
dataset$text <- gsub("[[:alnum:][:punct:]]*.html", "", dataset$text)
dataset$text <- gsub("rt [[:alnum:][:punct:]]*", "", dataset$text)
dataset$text <- gsub("RT [[:alnum:][:punct:]]*", "", dataset$text)
dataset$text <- gsub("#[[:alpha:][:alnum:]]*", "", dataset$text)
dataset$text <- gsub("@[[:alpha:][:alnum:]]*", "", dataset$text)
dataset$text <- gsub("[^[:alpha:][:space:]]*", "", dataset$text)

dataset$text <- dataset$text %>%
  tolower() %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

dataset[dataset==" "] <- NA
dataset[dataset==""] <- NA

dataset <- as.data.frame(dataset[complete.cases(dataset), ])

#Filtering the languages of the column text

colnames(dataset) <- c("text")

text_cat <- textcat(dataset$text)
dataset <- cbind(dataset, text_cat)

cldr <- detectLanguage(dataset)
cldr <- (cldr[,c("detectedLanguage")])
dataset <- cbind(dataset, cldr)
dataset$cldr <- dataset$cldr %>%
  tolower()

#Languages pre-filter
#Regional languages

dataset_all <- dataset[(dataset$text_cat == "spanish") | (dataset$cldr == "spanish") |
                           (dataset$text_cat == "catalan") | (dataset$cldr == "catalan") |
                           (dataset$text_cat == "galician") | (dataset$cldr == "galician") |
                           (dataset$text_cat == "basque") | (dataset$cldr == "basque"), ]

#Spanish only

dataset_ESP <- dataset[(dataset$text_cat == "spanish") | (dataset$cldr == "spanish"), ]

#Deleting repeated tweets

dataset_ESP <- dataset_ESP[!duplicated(dataset_ESP$text), ]

dataset_ESP <- select(dataset_ESP, c("text"))

#Stemming words

dataset_ESP <- as.data.frame(stemDocument(dataset_ESP$text, language="spanish"))

colnames(dataset_ESP) <- c("text")

DF_2017_12_7_cleanREG2 <- dataset_all
DF_2017_12_7_cleanESP2 <- dataset_ESP

#Saving the dataframes to R objects

save(DF_2017_12_7_cleanREG, file = "Objects/Tweets/Series_7/Clean/DF_2017_12_7_cleanREG.RData")
save(DF_2017_12_7_cleanESP, file = "Objects/Tweets/Series_7/Clean/DF_2017_12_7_cleanESP.RData")

#Removing the objects to get a tidy enviorement

rm(S1_2017_12_7, S2_2017_12_7, S3_2017_12_7, DF_2017_12_7, DF_2017_12_7_cleanREG, 
   clean_all, DF_2017_12_7_cleanESP, clean_ESP, vector)


