#Libraries

library("tidyverse")
library("xlsx")
library("readxl")
library("data.table")
library("xtable")
library("splitstackshape")
library("textcat")
library("cldr")

#Merging the Tweets datasets

#Clean text function

fun_txtcleaner <- function(x){
  
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
  
  vec <- prueba
  
  #Cleaning the text (regular expresions)
  
  vec <- vec[!is.na(vec)]
  vec <- gsub("http://www. ", "http://www.", vec)
  vec <- gsub("https://www. ", "http://www.", vec)
  vec <- gsub("http:// ", "http://www.", vec)
  vec <- gsub("https:// ", "http://www.", vec)
  vec <- gsub("pic.twitter.com/", "http://www.", vec)
  vec <- gsub(" …", ".html", vec)
  vec <- gsub("/s tatus/", "", vec)
  vec <- gsub(".0", "", vec)
  vec <- gsub("http[[:alnum:][:punct:]]*", "", vec)
  vec <- gsub("[[:alnum:][:punct:]]*.html", "", vec)
  vec <- gsub("rt [[:alnum:][:punct:]]*", "", vec)
  vec <- gsub("RT [[:alnum:][:punct:]]*", "", vec)
  vec <- gsub("#[[:alpha:][:alnum:]]*", "", vec)
  vec <- gsub("@[[:alpha:][:alnum:]]*", "", vec)
  vec <- gsub("[^[:alpha:][:space:]]*", "", vec)
  
  vec <- vec %>%
    tolower() %>%
    removePunctuation() %>%
    removeNumbers() %>%
    stripWhitespace()
  
  vec[vec==" "] <- NA
  vec[vec==""] <- NA
  
  vec <- vec[!is.na(vec)]
  
  #Filtering the languages of the column text
  
  df <- as.data.frame(vec)
  
  colnames(df) <- c("text")
  
  text_cat <- textcat(df$text)
  df <- cbind(df, text_cat)
  
  cldr <- detectLanguage(df$text)
  cldr <- (cldr[,c("detectedLanguage")])
  df <- cbind(df, cldr)
  df$cldr <- df$cldr %>%
    tolower()
  
  #Languages pre-filter
  #Regional languages
  
  df_all <- df[(df$text_cat == "spanish") | (df$cldr == "spanish") |
                 (df$text_cat == "catalan") | (df$cldr == "catalan") |
                 (df$text_cat == "galician") | (df$cldr == "galician"), ]
  
  df_all <- df_all[!duplicated(df_all$text), ]
  
  df_all <- as.data.frame(stemDocument(df_all$text, language="spanish"))
  
  colnames(df_all) <- c("text")
  
  clean_all <<- select(df_all, c("text"))
  
  #Spanish only
  
  df_ESP <- df[(df$text_cat == "spanish") | (df$cldr == "spanish"), ]
  
  #Deleting repeated tweets
  
  df_ESP <- df_ESP[!duplicated(df_ESP$text), ]
  
  df_ESP <- as.data.frame(stemDocument(df_ESP$text, language="spanish"))
  
  colnames(df_ESP) <- c("text")
  
  clean_ESP <<- select(df_ESP, c("text"))
}

#Series 7

#---2017---

#December

S1_2017_12_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/12/S1_2017_12_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_12_7 <- select(S1_2017_12_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_12_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/12/S2_2017_12_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_12_7 <- select(S2_2017_12_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_12_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/12/S3_2017_12_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_12_7 <- select(S3_2017_12_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_12_7 <- rbind(S1_2017_12_7, S2_2017_12_7, S3_2017_12_7)

save(DF_2017_12_7, file = "Objects/Tweets/Series_7/Dirty/DF_2017_12_7.RData")

vector <- as.vector(DF_2017_12_7$text)

fun_txtcleaner(vector)

DF_2017_12_7_cleanREG <- clean_all
DF_2017_12_7_cleanESP <- clean_ESP

save(DF_2017_12_7_cleanREG, file = "Objects/Tweets/Series_7/Clean/REG/DF_2017_12_7.RData")
save(DF_2017_12_7_cleanESP, file = "Objects/Tweets/Series_7/Clean/ESP/DF_2017_12_7.RData")

rm(S1_2017_12_7, S2_2017_12_7, S3_2017_12_7, DF_2017_12_7, DF_2017_12_7_cleanREG, clean_all, DF_2017_12_7_cleanESP, clean_ESP, vector)

#November

S1_2017_11_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/11/S1_2017_11_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_11_7 <- select(S1_2017_11_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_11_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/11/S2_2017_11_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_11_7 <- select(S2_2017_11_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_11_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/11/S3_2017_11_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_11_7 <- select(S3_2017_11_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_11_7 <- rbind(S1_2017_11_7, S2_2017_11_7, S3_2017_11_7)

save(DF_2017_11_7, file = "Objects/Tweets/Series_7/DF_2017_11_7.RData")

vector <- as.vector(DF_2017_11_7$text)

fun_txtcleaner(vector)

DF_2017_11_7_cleanREG <- clean_all
DF_2017_11_7_cleanESP <- clean_ESP

save(DF_2017_11_7_cleanREG, file = "Objects/Tweets/Series_7/Clean/REG/DF_2017_11_7.RData")
save(DF_2017_11_7_cleanESP, file = "Objects/Tweets/Series_7/Clean/ESP/DF_2017_11_7.RData")

rm(S1_2017_11_7, S2_2017_11_7, S3_2017_11_7, DF_2017_11_7, DF_2017_11_7_cleanREG, clean_all, DF_2017_11_7_cleanESP, clean_ESP, vector)

#October

S1_2017_10_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/10/S1_2017_10_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_10_7 <- select(S1_2017_10_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_10_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/10/S2_2017_10_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_10_7 <- select(S2_2017_10_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_10_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/10/S3_2017_10_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_10_7 <- select(S3_2017_10_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_10_7 <- rbind(S1_2017_10_7, S2_2017_10_7, S3_2017_10_7)

save(DF_2017_10_7, file = "Objects/Tweets/Series_7/DF_2017_10_7.RData")

vector <- as.vector(DF_2017_10_7$text)

fun_txtcleaner(vector)

DF_2017_10_7_cleanREG <- clean_all
DF_2017_10_7_cleanESP <- clean_ESP

save(DF_2017_10_7_cleanREG, file = "Objects/Tweets/Series_7/Clean/REG/DF_2017_10_7.RData")
save(DF_2017_10_7_cleanESP, file = "Objects/Tweets/Series_7/Clean/ESP/DF_2017_10_7.RData")

rm(S1_2017_10_7, S2_2017_10_7, S3_2017_10_7, DF_2017_10_7, DF_2017_10_7_cleanREG, clean_all, DF_2017_10_7_cleanESP, clean_ESP, vector)

#September

S1_2017_9_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/9/S1_2017_9_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_9_7 <- select(S1_2017_9_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_9_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/9/S2_2017_9_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_9_7 <- select(S2_2017_9_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_9_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/9/S3_2017_9_7.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_9_7 <- select(S3_2017_9_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_9_7 <- rbind(S1_2017_9_7, S2_2017_9_7, S3_2017_9_7)

save(DF_2017_9_7, file = "Objects/Tweets/Series_7/DF_2017_9_7.RData")

vector <- as.vector(DF_2017_9_7$text)

fun_txtcleaner(vector)

DF_2017_9_7_cleanREG <- clean_all
DF_2017_9_7_cleanESP <- clean_ESP

save(DF_2017_9_7_cleanREG, file = "Objects/Tweets/Series_7/Clean/REG/DF_2017_9_7.RData")
save(DF_2017_9_7_cleanESP, file = "Objects/Tweets/Series_7/Clean/ESP/DF_2017_9_7.RData")

rm(S1_2017_9_7, S2_2017_9_7, S3_2017_9_7, DF_2017_9_7, DF_2017_9_7_cleanREG, clean_all, DF_2017_9_7_cleanESP, clean_ESP, vector)

#August

S1_2017_8_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/8/S1_2017_8_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_8_7 <- select(S1_2017_8_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_8_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/8/S2_2017_8_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_8_7 <- select(S2_2017_8_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_8_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/8/S3_2017_8_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_8_7 <- select(S3_2017_8_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_8_7 <- rbind(S1_2017_8_7, S2_2017_8_7, S3_2017_8_7)

save(DF_2017_8_7, file = "Objects/Tweets/Series_7/DF_2017_8_7.RData")

vector <- as.vector(DF_2017_8_7$text)

fun_txtcleaner(vector)

DF_2017_8_7_cleanREG <- clean_all
DF_2017_8_7_cleanESP <- clean_ESP

save(DF_2017_8_7_cleanREG, file = "Objects/Tweets/Series_7/Clean/REG/DF_2017_8_7.RData")
save(DF_2017_8_7_cleanESP, file = "Objects/Tweets/Series_7/Clean/ESP/DF_2017_8_7.RData")

rm(S1_2017_8_7, S2_2017_8_7, S3_2017_8_7, DF_2017_8_7, DF_2017_8_7_cleanREG, clean_all, DF_2017_8_7_cleanESP, clean_ESP, vector)

#July

S1_2017_7_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/7/S1_2017_7_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_7_7 <- select(S1_2017_7_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_7_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/7/S2_2017_7_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_7_7 <- select(S2_2017_7_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_7_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/7/S3_2017_7_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_7_7 <- select(S3_2017_7_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_7_7 <- rbind(S1_2017_7_7, S2_2017_7_7, S3_2017_7_7)

save(DF_2017_7_7, file = "Objects/Tweets/Series_7/DF_2017_7_7.RData")

vector <- as.vector(DF_2017_7_7$text)

fun_txtcleaner(vector)

DF_2017_7_7_cleanREG <- clean_all
DF_2017_7_7_cleanESP <- clean_ESP

save(DF_2017_7_7_cleanREG, file = "Objects/Tweets/Series_7/Clean/REG/DF_2017_7_7.RData")
save(DF_2017_7_7_cleanESP, file = "Objects/Tweets/Series_7/Clean/ESP/DF_2017_7_7.RData")

rm(S1_2017_7_7, S2_2017_7_7, S3_2017_7_7, DF_2017_7_7, DF_2017_7_7_cleanREG, clean_all, DF_2017_7_7_cleanESP, clean_ESP, vector)

#June

S1_2017_6_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/6/S1_2017_6_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_6_7 <- select(S1_2017_6_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_6_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/6/S2_2017_6_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_6_7 <- select(S2_2017_6_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_6_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/6/S3_2017_6_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_6_7 <- select(S3_2017_6_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_6_7 <- rbind(S1_2017_6_7, S2_2017_6_7, S3_2017_6_7)

save(DF_2017_6_7, file = "Objects/Tweets/Series_7/DF_2017_6_7.RData")



#May

S1_2017_5_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/5/S1_2017_5_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_5_7 <- select(S1_2017_5_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_5_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/5/S2_2017_5_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_5_7 <- select(S2_2017_5_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_5_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/5/S3_2017_5_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_5_7 <- select(S3_2017_5_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_5_7 <- rbind(S1_2017_5_7, S2_2017_5_7, S3_2017_5_7)

save(DF_2017_5_7, file = "Objects/Tweets/Series_7/DF_2017_5_7.RData")



#April

S1_2017_4_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/4/S1_2017_4_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_4_7 <- select(S1_2017_4_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_4_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/4/S2_2017_4_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_4_7 <- select(S2_2017_4_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_4_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/4/S3_2017_4_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_4_7 <- select(S3_2017_4_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_4_7 <- rbind(S1_2017_4_7, S2_2017_4_7, S3_2017_4_7)

save(DF_2017_4_7, file = "Objects/Tweets/Series_7/DF_2017_4_7.RData")



#March

S1_2017_3_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/3/S1_2017_3_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_3_7 <- select(S1_2017_3_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_3_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/3/S2_2017_3_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_3_7 <- select(S2_2017_3_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_3_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/3/S3_2017_3_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_3_7 <- select(S3_2017_3_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_3_7 <- rbind(S1_2017_3_7, S2_2017_3_7, S3_2017_3_7)

save(DF_2017_3_7, file = "Objects/Tweets/Series_7/DF_2017_3_7.RData")



#February

S1_2017_2_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/2/S1_2017_2_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_2_7 <- select(S1_2017_2_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_2_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/2/S2_2017_2_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_2_7 <- select(S2_2017_2_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_2_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/2/S3_2017_2_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_2_7 <- select(S3_2017_2_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_2_7 <- rbind(S1_2017_2_7, S2_2017_2_7, S3_2017_2_7)

save(DF_2017_2_7, file = "Objects/Tweets/Series_7/DF_2017_2_7.RData")



#January

S1_2017_1_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/1/S1_2017_1_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S1_2017_1_7 <- select(S1_2017_1_7, c("username", "date", "retweets", "favorites", "text"))

S2_2017_1_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/1/S2_2017_1_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S2_2017_1_7 <- select(S2_2017_1_7, c("username", "date", "retweets", "favorites", "text"))

S3_2017_1_7 <- read_delim("~/Desktop/TFG/Datasets/Tweets/Series_7/2017/1/S3_2017_1_7.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

S3_2017_1_7 <- select(S3_2017_3_7, c("username", "date", "retweets", "favorites", "text"))

DF_2017_1_7 <- rbind(S1_2017_1_7, S2_2017_1_7, S3_2017_1_7)

save(DF_2017_1_7, file = "Objects/Tweets/Series_7/DF_2017_1_7.RData")



