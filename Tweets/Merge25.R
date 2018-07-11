

library("tidyverse")

#Funtion

fun_txtcleaner <- function(x){
  t1 <- Sys.time()
  print("starting function...")
  
  #Libraries
  
  print("loading libraries...")
  library("tidyverse")
  library("splitstackshape")
  library("textcat")
  library("cldr")
  library("RTextTools")
  library("lubridate")
  library("tm")
  print("...libraries loaded")
  
  #function input
  
  vec <- x
  
  #Cleaning the text (regular expresions)
  
  print("regex filter...")
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
  print("regex filter, done")
  
  #Filtering the languages of the column text
  
  print("language filter...")
  df <- as.data.frame(vec)
  
  colnames(df) <- c("text")
  
  t2 <- Sys.time()
  print("textcat...")
  
  text_cat <- textcat(df$text)
  df <- cbind(df, text_cat)
  
  print(difftime(Sys.time(), t2, units = 'mins'))
  print("textcat done")
  
  cldr <- detectLanguage(df$text)
  cldr <- (cldr[,c("detectedLanguage")])
  df <- cbind(df, cldr)
  df$cldr <- df$cldr %>%
    tolower()
  print("language filter, done")
  
  #Languages pre-filter
  #Regional languages
  
  df_all <- df[(df$text_cat == "spanish") | (df$cldr == "spanish") |
                 (df$text_cat == "catalan") | (df$cldr == "catalan") |
                 (df$text_cat == "galician") | (df$cldr == "galician"), ]
  
  df_all <- df_all[!duplicated(df_all$text), ]
  
  df_all <- as.data.frame(stemDocument(as.character(df_all$text), language="spanish"))
  
  colnames(df_all) <- c("text")
  
  clean_REG <<- select(df_all, c("text"))
  
  #Spanish only
  
  df_ESP <- df[(df$text_cat == "spanish") | (df$cldr == "spanish"), ]
  
  #Deleting repeated tweets
  
  df_ESP <- df_ESP[!duplicated(df_ESP$text), ]
  
  df_ESP <- as.data.frame(stemDocument(as.character(df_ESP$text), language="spanish"))
  
  colnames(df_ESP) <- c("text")
  
  clean_ESP <<- select(df_ESP, c("text"))
  print(difftime(Sys.time(), t1, units = 'mins'))
  print("...function finished")
}

#Merging the Tweets datasets

#Series 25

#---2017---

#December

print("December2017, cleaning...")
print(Sys.time())

S1_2017_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/12/S1_2017_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_12_25 <- select(S1_2017_12_25, c("username", "date", "text"))

S2_2017_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/12/S2_2017_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_12_25 <- select(S2_2017_12_25, c("username", "date", "text"))

S3_2017_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/12/S3_2017_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_12_25 <- select(S3_2017_12_25, c("username", "date", "text"))

DF_2017_12_25 <- rbind(S1_2017_12_25, S2_2017_12_25, S3_2017_12_25)

save(DF_2017_12_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_12_25.RData")

vector <- as.vector(DF_2017_12_25$text)

fun_txtcleaner(vector)

DF_2017_12_25_cleanREG <- clean_REG
DF_2017_12_25_cleanESP <- clean_ESP

save(DF_2017_12_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_12_25.RData")
save(DF_2017_12_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_12_25.RData")

rm(S1_2017_12_25, S2_2017_12_25, S3_2017_12_25, DF_2017_12_25, DF_2017_12_25_cleanREG, DF_2017_12_25_cleanESP, clean_ESP, clean_REG, vector)

print("December2017, ...done")
print(Sys.time())

#November

print("November2017, cleaning...")
print(Sys.time())

S1_2017_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/11/S1_2017_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_11_25 <- select(S1_2017_11_25, c("username", "date", "retweets", "favorites", "text"))

S2_2017_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/11/S2_2017_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_11_25 <- select(S2_2017_11_25, c("username", "date", "retweets", "favorites", "text"))

S3_2017_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/11/S3_2017_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_11_25 <- select(S3_2017_11_25, c("username", "date", "retweets", "favorites", "text"))

DF_2017_11_25 <- rbind(S1_2017_11_25, S2_2017_11_25, S3_2017_11_25)

save(DF_2017_11_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_11_25.RData")

vector <- as.vector(DF_2017_11_25$text)

fun_txtcleaner(vector)

DF_2017_11_25_cleanREG <- clean_REG
DF_2017_11_25_cleanESP <- clean_ESP

save(DF_2017_11_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_11_25.RData")
save(DF_2017_11_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_11_25.RData")

rm(S1_2017_11_25, S2_2017_11_25, S3_2017_11_25, DF_2017_11_25, DF_2017_11_25_cleanREG, DF_2017_11_25_cleanESP, clean_ESP, clean_REG, vector)

print("November2017, ...done")
print(Sys.time())

#October

print("October2017, cleaning...")
print(Sys.time())

S1_2017_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/10/S1_2017_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_10_25 <- select(S1_2017_10_25, c("username", "date", "retweets", "favorites", "text"))

S2_2017_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/10/S2_2017_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_10_25 <- select(S2_2017_10_25, c("username", "date", "retweets", "favorites", "text"))

S3_2017_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/10/S3_2017_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_10_25 <- select(S3_2017_10_25, c("username", "date", "retweets", "favorites", "text"))

DF_2017_10_25 <- rbind(S1_2017_10_25, S2_2017_10_25, S3_2017_10_25)

save(DF_2017_10_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_10_25.RData")

vector <- as.vector(DF_2017_10_25$text)

fun_txtcleaner(vector)

DF_2017_10_25_cleanREG <- clean_REG
DF_2017_10_25_cleanESP <- clean_ESP

save(DF_2017_10_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_10_25.RData")
save(DF_2017_10_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_10_25.RData")

rm(S1_2017_10_25, S2_2017_10_25, S3_2017_10_25, DF_2017_10_25, DF_2017_10_25_cleanREG, DF_2017_10_25_cleanESP, clean_ESP, clean_REG, vector)

print("October2017, ...done")
print(Sys.time())

#September

print("September2017, cleaning...")
print(Sys.time())

S1_2017_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/9/S1_2017_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_9_25 <- select(S1_2017_9_25, c("username", "date", "retweets", "favorites", "text"))

S2_2017_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/9/S2_2017_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_9_25 <- select(S2_2017_9_25, c("username", "date", "retweets", "favorites", "text"))

S3_2017_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/9/S3_2017_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_9_25 <- select(S3_2017_9_25, c("username", "date", "retweets", "favorites", "text"))

DF_2017_9_25 <- rbind(S1_2017_9_25, S2_2017_9_25, S3_2017_9_25)

save(DF_2017_9_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_9_25.RData")

vector <- as.vector(DF_2017_9_25$text)

fun_txtcleaner(vector)

DF_2017_9_25_cleanREG <- clean_REG
DF_2017_9_25_cleanESP <- clean_ESP

save(DF_2017_9_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_9_25.RData")
save(DF_2017_9_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_9_25.RData")

rm(S1_2017_9_25, S2_2017_9_25, S3_2017_9_25, DF_2017_9_25, DF_2017_9_25_cleanREG, DF_2017_9_25_cleanESP, clean_ESP, clean_REG, vector)

print("September2017, ...done")
print(Sys.time())

#August

print("August2017, cleaning...")
print(Sys.time())

S1_2017_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/8/S1_2017_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_8_25 <- select(S1_2017_8_25, c("username", "date", "retweets", "favorites", "text"))

S2_2017_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/8/S2_2017_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_8_25 <- select(S2_2017_8_25, c("username", "date", "retweets", "favorites", "text"))

S3_2017_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/8/S3_2017_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_8_25 <- select(S3_2017_8_25, c("username", "date", "retweets", "favorites", "text"))

DF_2017_8_25 <- rbind(S1_2017_8_25, S2_2017_8_25, S3_2017_8_25)

save(DF_2017_8_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_8_25.RData")

vector <- as.vector(DF_2017_8_25$text)

fun_txtcleaner(vector)

DF_2017_8_25_cleanREG <- clean_REG
DF_2017_8_25_cleanESP <- clean_ESP

save(DF_2017_8_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_8_25.RData")
save(DF_2017_8_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_8_25.RData")

rm(S1_2017_8_25, S2_2017_8_25, S3_2017_8_25, DF_2017_8_25, DF_2017_8_25_cleanREG, DF_2017_8_25_cleanESP, clean_ESP, clean_REG, vector)

print("August2017, ...done")
print(Sys.time())

#July

print("July2017, cleaning...")
print(Sys.time())

S1_2017_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/7/S1_2017_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_7_25 <- select(S1_2017_7_25, c("username", "date", "retweets", "favorites", "text"))

S2_2017_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/7/S2_2017_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_7_25 <- select(S2_2017_7_25, c("username", "date", "retweets", "favorites", "text"))

S3_2017_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/7/S3_2017_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_7_25 <- select(S3_2017_7_25, c("username", "date", "retweets", "favorites", "text"))

DF_2017_7_25 <- rbind(S1_2017_7_25, S2_2017_7_25, S3_2017_7_25)

save(DF_2017_7_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_7_25.RData")

vector <- as.vector(DF_2017_7_25$text)

fun_txtcleaner(vector)

DF_2017_7_25_cleanREG <- clean_REG
DF_2017_7_25_cleanESP <- clean_ESP

save(DF_2017_7_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_7_25.RData")
save(DF_2017_7_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_7_25.RData")

rm(S1_2017_7_25, S2_2017_7_25, S3_2017_7_25, DF_2017_7_25, DF_2017_7_25_cleanREG, DF_2017_7_25_cleanESP, clean_ESP, clean_REG, vector)

print("July2017, ...done")
print(Sys.time())

#June

print("June2017, cleaning...")
print(Sys.time())

S1_2017_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/6/S1_2017_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_6_25 <- select(S1_2017_6_25, c("username", "date", "retweets", "favorites", "text"))

S2_2017_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/6/S2_2017_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_6_25 <- select(S2_2017_6_25, c("username", "date", "retweets", "favorites", "text"))

S3_2017_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/6/S3_2017_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_6_25 <- select(S3_2017_6_25, c("username", "date", "retweets", "favorites", "text"))

DF_2017_6_25 <- rbind(S1_2017_6_25, S2_2017_6_25, S3_2017_6_25)

save(DF_2017_6_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_6_25.RData")

vector <- as.vector(DF_2017_6_25$text)

fun_txtcleaner(vector)

DF_2017_6_25_cleanREG <- clean_REG
DF_2017_6_25_cleanESP <- clean_ESP

save(DF_2017_6_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_6_25.RData")
save(DF_2017_6_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_6_25.RData")

rm(S1_2017_6_25, S2_2017_6_25, S3_2017_6_25, DF_2017_6_25, DF_2017_6_25_cleanREG, DF_2017_6_25_cleanESP, clean_ESP, clean_REG, vector)

print("June2017, ...done")
print(Sys.time())

#May

print("May2017, cleaning...")
print(Sys.time())

S1_2017_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/5/S1_2017_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_5_25 <- select(S1_2017_5_25, c("username", "date", "retweets", "favorites", "text"))

S2_2017_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/5/S2_2017_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_5_25 <- select(S2_2017_5_25, c("username", "date", "retweets", "favorites", "text"))

S3_2017_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/5/S3_2017_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_5_25 <- select(S3_2017_5_25, c("username", "date", "retweets", "favorites", "text"))

DF_2017_5_25 <- rbind(S1_2017_5_25, S2_2017_5_25, S3_2017_5_25)

save(DF_2017_5_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_5_25.RData")

vector <- as.vector(DF_2017_5_25$text)

fun_txtcleaner(vector)

DF_2017_5_25_cleanREG <- clean_REG
DF_2017_5_25_cleanESP <- clean_ESP

save(DF_2017_5_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_5_25.RData")
save(DF_2017_5_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_5_25.RData")

rm(S1_2017_5_25, S2_2017_5_25, S3_2017_5_25, DF_2017_5_25, DF_2017_5_25_cleanREG, DF_2017_5_25_cleanESP, clean_ESP, clean_REG, vector)

print("May2017, ...done")
print(Sys.time())

#April

print("April2017, cleaning...")
print(Sys.time())

S1_2017_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/4/S1_2017_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_4_25 <- select(S1_2017_4_25, c("username", "date", "retweets", "favorites", "text"))

S2_2017_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/4/S2_2017_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_4_25 <- select(S2_2017_4_25, c("username", "date", "retweets", "favorites", "text"))

S3_2017_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/4/S3_2017_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_4_25 <- select(S3_2017_4_25, c("username", "date", "retweets", "favorites", "text"))

DF_2017_4_25 <- rbind(S1_2017_4_25, S2_2017_4_25, S3_2017_4_25)

save(DF_2017_4_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_4_25.RData")

vector <- as.vector(DF_2017_4_25$text)

fun_txtcleaner(vector)

DF_2017_4_25_cleanREG <- clean_REG
DF_2017_4_25_cleanESP <- clean_ESP

save(DF_2017_4_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_4_25.RData")
save(DF_2017_4_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_4_25.RData")

rm(S1_2017_4_25, S2_2017_4_25, S3_2017_4_25, DF_2017_4_25, DF_2017_4_25_cleanREG, DF_2017_4_25_cleanESP, clean_ESP, clean_REG, vector)

print("April2017, ...done")
print(Sys.time())

#March

print("March2017, cleaning...")
print(Sys.time())

S1_2017_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/3/S1_2017_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_3_25 <- select(S1_2017_3_25, c("username", "date", "retweets", "favorites", "text"))

S2_2017_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/3/S2_2017_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_3_25 <- select(S2_2017_3_25, c("username", "date", "retweets", "favorites", "text"))

S3_2017_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/3/S3_2017_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_3_25 <- select(S3_2017_3_25, c("username", "date", "retweets", "favorites", "text"))

DF_2017_3_25 <- rbind(S1_2017_3_25, S2_2017_3_25, S3_2017_3_25)

save(DF_2017_3_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_3_25.RData")

vector <- as.vector(DF_2017_3_25$text)

fun_txtcleaner(vector)

DF_2017_3_25_cleanREG <- clean_REG
DF_2017_3_25_cleanESP <- clean_ESP

save(DF_2017_3_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_3_25.RData")
save(DF_2017_3_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_3_25.RData")

rm(S1_2017_3_25, S2_2017_3_25, S3_2017_3_25, DF_2017_3_25, DF_2017_3_25_cleanREG, DF_2017_3_25_cleanESP, clean_ESP, clean_REG, vector)

print("March2017, ...done")
print(Sys.time())

#February

print("February2017, cleaning...")
print(Sys.time())

S1_2017_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/2/S1_2017_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_2_25 <- select(S1_2017_2_25, c("username", "date", "retweets", "favorites", "text"))

S2_2017_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/2/S2_2017_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_2_25 <- select(S2_2017_2_25, c("username", "date", "retweets", "favorites", "text"))

S3_2017_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/2/S3_2017_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_2_25 <- select(S3_2017_2_25, c("username", "date", "retweets", "favorites", "text"))

DF_2017_2_25 <- rbind(S1_2017_2_25, S2_2017_2_25, S3_2017_2_25)

save(DF_2017_2_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_2_25.RData")

vector <- as.vector(DF_2017_2_25$text)

fun_txtcleaner(vector)

DF_2017_2_25_cleanREG <- clean_REG
DF_2017_2_25_cleanESP <- clean_ESP

save(DF_2017_2_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_2_25.RData")
save(DF_2017_2_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_2_25.RData")

rm(S1_2017_2_25, S2_2017_2_25, S3_2017_2_25, DF_2017_2_25, DF_2017_2_25_cleanREG, DF_2017_2_25_cleanESP, clean_ESP, clean_REG, vector)

print("February2017, ...done")
print(Sys.time())

#January

print("January2017, cleaning...")
print(Sys.time())

S1_2017_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/1/S1_2017_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2017_1_25 <- select(S1_2017_1_25, c("username", "date", "retweets", "favorites", "text"))

S2_2017_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/1/S2_2017_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2017_1_25 <- select(S2_2017_1_25, c("username", "date", "retweets", "favorites", "text"))

S3_2017_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2017/1/S3_2017_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2017_1_25 <- select(S3_2017_1_25, c("username", "date", "retweets", "favorites", "text"))

DF_2017_1_25 <- rbind(S1_2017_1_25, S2_2017_1_25, S3_2017_1_25)

save(DF_2017_1_25, file = "Objects/Tweets/Series_25/Dirty/DF_2017_1_25.RData")

vector <- as.vector(DF_2017_1_25$text)

fun_txtcleaner(vector)

DF_2017_1_25_cleanREG <- clean_REG
DF_2017_1_25_cleanESP <- clean_ESP

save(DF_2017_1_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2017_1_25.RData")
save(DF_2017_1_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2017_1_25.RData")

rm(S1_2017_1_25, S2_2017_1_25, S3_2017_1_25, DF_2017_1_25, DF_2017_1_25_cleanREG, DF_2017_1_25_cleanESP, clean_ESP, clean_REG, vector)

print("January2017, ...done")
print(Sys.time())

#---2016---

#December

print("December2016, cleaning...")
print(Sys.time())

S1_2016_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/12/S1_2016_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_12_25 <- select(S1_2016_12_25, c("username", "date", "text"))

S2_2016_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/12/S2_2016_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_12_25 <- select(S2_2016_12_25, c("username", "date", "text"))

S3_2016_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/12/S3_2016_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_12_25 <- select(S3_2016_12_25, c("username", "date", "text"))

DF_2016_12_25 <- rbind(S1_2016_12_25, S2_2016_12_25, S3_2016_12_25)

save(DF_2016_12_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_12_25.RData")

vector <- as.vector(DF_2016_12_25$text)

fun_txtcleaner(vector)

DF_2016_12_25_cleanREG <- clean_REG
DF_2016_12_25_cleanESP <- clean_ESP

save(DF_2016_12_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_12_25.RData")
save(DF_2016_12_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_12_25.RData")

rm(S1_2016_12_25, S2_2016_12_25, S3_2016_12_25, DF_2016_12_25, DF_2016_12_25_cleanREG, DF_2016_12_25_cleanESP, clean_ESP, clean_REG, vector)

print("December2016, ...done")
print(Sys.time())

#November

print("November2016, cleaning...")
print(Sys.time())

S1_2016_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/11/S1_2016_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_11_25 <- select(S1_2016_11_25, c("username", "date", "retweets", "favorites", "text"))

S2_2016_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/11/S2_2016_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_11_25 <- select(S2_2016_11_25, c("username", "date", "retweets", "favorites", "text"))

S3_2016_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/11/S3_2016_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_11_25 <- select(S3_2016_11_25, c("username", "date", "retweets", "favorites", "text"))

DF_2016_11_25 <- rbind(S1_2016_11_25, S2_2016_11_25, S3_2016_11_25)

save(DF_2016_11_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_11_25.RData")

vector <- as.vector(DF_2016_11_25$text)

fun_txtcleaner(vector)

DF_2016_11_25_cleanREG <- clean_REG
DF_2016_11_25_cleanESP <- clean_ESP

save(DF_2016_11_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_11_25.RData")
save(DF_2016_11_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_11_25.RData")

rm(S1_2016_11_25, S2_2016_11_25, S3_2016_11_25, DF_2016_11_25, DF_2016_11_25_cleanREG, DF_2016_11_25_cleanESP, clean_ESP, clean_REG, vector)

print("November2016, ...done")
print(Sys.time())

#October

print("October2016, cleaning...")
print(Sys.time())

S1_2016_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/10/S1_2016_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_10_25 <- select(S1_2016_10_25, c("username", "date", "retweets", "favorites", "text"))

S2_2016_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/10/S2_2016_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_10_25 <- select(S2_2016_10_25, c("username", "date", "retweets", "favorites", "text"))

S3_2016_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/10/S3_2016_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_10_25 <- select(S3_2016_10_25, c("username", "date", "retweets", "favorites", "text"))

DF_2016_10_25 <- rbind(S1_2016_10_25, S2_2016_10_25, S3_2016_10_25)

save(DF_2016_10_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_10_25.RData")

vector <- as.vector(DF_2016_10_25$text)

fun_txtcleaner(vector)

DF_2016_10_25_cleanREG <- clean_REG
DF_2016_10_25_cleanESP <- clean_ESP

save(DF_2016_10_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_10_25.RData")
save(DF_2016_10_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_10_25.RData")

rm(S1_2016_10_25, S2_2016_10_25, S3_2016_10_25, DF_2016_10_25, DF_2016_10_25_cleanREG, DF_2016_10_25_cleanESP, clean_ESP, clean_REG, vector)

print("October2016, ...done")
print(Sys.time())

#September

print("September2016, cleaning...")
print(Sys.time())

S1_2016_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/9/S1_2016_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_9_25 <- select(S1_2016_9_25, c("username", "date", "retweets", "favorites", "text"))

S2_2016_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/9/S2_2016_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_9_25 <- select(S2_2016_9_25, c("username", "date", "retweets", "favorites", "text"))

S3_2016_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/9/S3_2016_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_9_25 <- select(S3_2016_9_25, c("username", "date", "retweets", "favorites", "text"))

DF_2016_9_25 <- rbind(S1_2016_9_25, S2_2016_9_25, S3_2016_9_25)

save(DF_2016_9_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_9_25.RData")

vector <- as.vector(DF_2016_9_25$text)

fun_txtcleaner(vector)

DF_2016_9_25_cleanREG <- clean_REG
DF_2016_9_25_cleanESP <- clean_ESP

save(DF_2016_9_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_9_25.RData")
save(DF_2016_9_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_9_25.RData")

rm(S1_2016_9_25, S2_2016_9_25, S3_2016_9_25, DF_2016_9_25, DF_2016_9_25_cleanREG, DF_2016_9_25_cleanESP, clean_ESP, clean_REG, vector)

print("September2016, ...done")
print(Sys.time())

#August

print("August2016, cleaning...")
print(Sys.time())

S1_2016_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/8/S1_2016_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_8_25 <- select(S1_2016_8_25, c("username", "date", "retweets", "favorites", "text"))

S2_2016_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/8/S2_2016_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_8_25 <- select(S2_2016_8_25, c("username", "date", "retweets", "favorites", "text"))

S3_2016_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/8/S3_2016_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_8_25 <- select(S3_2016_8_25, c("username", "date", "retweets", "favorites", "text"))

DF_2016_8_25 <- rbind(S1_2016_8_25, S2_2016_8_25, S3_2016_8_25)

save(DF_2016_8_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_8_25.RData")

vector <- as.vector(DF_2016_8_25$text)

fun_txtcleaner(vector)

DF_2016_8_25_cleanREG <- clean_REG
DF_2016_8_25_cleanESP <- clean_ESP

save(DF_2016_8_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_8_25.RData")
save(DF_2016_8_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_8_25.RData")

rm(S1_2016_8_25, S2_2016_8_25, S3_2016_8_25, DF_2016_8_25, DF_2016_8_25_cleanREG, DF_2016_8_25_cleanESP, clean_ESP, clean_REG, vector)

print("August2016, ...done")
print(Sys.time())

#July

print("July2016, cleaning...")
print(Sys.time())

S1_2016_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/7/S1_2016_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_7_25 <- select(S1_2016_7_25, c("username", "date", "retweets", "favorites", "text"))

S2_2016_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/7/S2_2016_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_7_25 <- select(S2_2016_7_25, c("username", "date", "retweets", "favorites", "text"))

S3_2016_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/7/S3_2016_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_7_25 <- select(S3_2016_7_25, c("username", "date", "retweets", "favorites", "text"))

DF_2016_7_25 <- rbind(S1_2016_7_25, S2_2016_7_25, S3_2016_7_25)

save(DF_2016_7_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_7_25.RData")

vector <- as.vector(DF_2016_7_25$text)

fun_txtcleaner(vector)

DF_2016_7_25_cleanREG <- clean_REG
DF_2016_7_25_cleanESP <- clean_ESP

save(DF_2016_7_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_7_25.RData")
save(DF_2016_7_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_7_25.RData")

rm(S1_2016_7_25, S2_2016_7_25, S3_2016_7_25, DF_2016_7_25, DF_2016_7_25_cleanREG, DF_2016_7_25_cleanESP, clean_ESP, clean_REG, vector)

print("July2016, ...done")
print(Sys.time())

#June

print("June2016, cleaning...")
print(Sys.time())

S1_2016_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/6/S1_2016_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_6_25 <- select(S1_2016_6_25, c("username", "date", "retweets", "favorites", "text"))

S2_2016_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/6/S2_2016_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_6_25 <- select(S2_2016_6_25, c("username", "date", "retweets", "favorites", "text"))

S3_2016_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/6/S3_2016_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_6_25 <- select(S3_2016_6_25, c("username", "date", "retweets", "favorites", "text"))

DF_2016_6_25 <- rbind(S1_2016_6_25, S2_2016_6_25, S3_2016_6_25)

save(DF_2016_6_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_6_25.RData")

vector <- as.vector(DF_2016_6_25$text)

fun_txtcleaner(vector)

DF_2016_6_25_cleanREG <- clean_REG
DF_2016_6_25_cleanESP <- clean_ESP

save(DF_2016_6_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_6_25.RData")
save(DF_2016_6_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_6_25.RData")

rm(S1_2016_6_25, S2_2016_6_25, S3_2016_6_25, DF_2016_6_25, DF_2016_6_25_cleanREG, DF_2016_6_25_cleanESP, clean_ESP, clean_REG, vector)

print("June2016, ...done")
print(Sys.time())

#May

print("May2016, cleaning...")
print(Sys.time())

S1_2016_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/5/S1_2016_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_5_25 <- select(S1_2016_5_25, c("username", "date", "retweets", "favorites", "text"))

S2_2016_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/5/S2_2016_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_5_25 <- select(S2_2016_5_25, c("username", "date", "retweets", "favorites", "text"))

S3_2016_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/5/S3_2016_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_5_25 <- select(S3_2016_5_25, c("username", "date", "retweets", "favorites", "text"))

DF_2016_5_25 <- rbind(S1_2016_5_25, S2_2016_5_25, S3_2016_5_25)

save(DF_2016_5_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_5_25.RData")

vector <- as.vector(DF_2016_5_25$text)

fun_txtcleaner(vector)

DF_2016_5_25_cleanREG <- clean_REG
DF_2016_5_25_cleanESP <- clean_ESP

save(DF_2016_5_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_5_25.RData")
save(DF_2016_5_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_5_25.RData")

rm(S1_2016_5_25, S2_2016_5_25, S3_2016_5_25, DF_2016_5_25, DF_2016_5_25_cleanREG, DF_2016_5_25_cleanESP, clean_ESP, clean_REG, vector)

print("May2016, ...done")
print(Sys.time())

#April

print("April2016, cleaning...")
print(Sys.time())

S1_2016_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/4/S1_2016_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_4_25 <- select(S1_2016_4_25, c("username", "date", "retweets", "favorites", "text"))

S2_2016_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/4/S2_2016_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_4_25 <- select(S2_2016_4_25, c("username", "date", "retweets", "favorites", "text"))

S3_2016_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/4/S3_2016_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_4_25 <- select(S3_2016_4_25, c("username", "date", "retweets", "favorites", "text"))

DF_2016_4_25 <- rbind(S1_2016_4_25, S2_2016_4_25, S3_2016_4_25)

save(DF_2016_4_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_4_25.RData")

vector <- as.vector(DF_2016_4_25$text)

fun_txtcleaner(vector)

DF_2016_4_25_cleanREG <- clean_REG
DF_2016_4_25_cleanESP <- clean_ESP

save(DF_2016_4_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_4_25.RData")
save(DF_2016_4_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_4_25.RData")

rm(S1_2016_4_25, S2_2016_4_25, S3_2016_4_25, DF_2016_4_25, DF_2016_4_25_cleanREG, DF_2016_4_25_cleanESP, clean_ESP, clean_REG, vector)

print("April2016, ...done")
print(Sys.time())

#March

print("March2016, cleaning...")
print(Sys.time())

S1_2016_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/3/S1_2016_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_3_25 <- select(S1_2016_3_25, c("username", "date", "retweets", "favorites", "text"))

S2_2016_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/3/S2_2016_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_3_25 <- select(S2_2016_3_25, c("username", "date", "retweets", "favorites", "text"))

S3_2016_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/3/S3_2016_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_3_25 <- select(S3_2016_3_25, c("username", "date", "retweets", "favorites", "text"))

DF_2016_3_25 <- rbind(S1_2016_3_25, S2_2016_3_25, S3_2016_3_25)

save(DF_2016_3_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_3_25.RData")

vector <- as.vector(DF_2016_3_25$text)

fun_txtcleaner(vector)

DF_2016_3_25_cleanREG <- clean_REG
DF_2016_3_25_cleanESP <- clean_ESP

save(DF_2016_3_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_3_25.RData")
save(DF_2016_3_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_3_25.RData")

rm(S1_2016_3_25, S2_2016_3_25, S3_2016_3_25, DF_2016_3_25, DF_2016_3_25_cleanREG, DF_2016_3_25_cleanESP, clean_ESP, clean_REG, vector)

print("March2016, ...done")
print(Sys.time())

#February

print("February2016, cleaning...")
print(Sys.time())

S1_2016_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/2/S1_2016_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_2_25 <- select(S1_2016_2_25, c("username", "date", "retweets", "favorites", "text"))

S2_2016_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/2/S2_2016_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_2_25 <- select(S2_2016_2_25, c("username", "date", "retweets", "favorites", "text"))

S3_2016_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/2/S3_2016_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_2_25 <- select(S3_2016_2_25, c("username", "date", "retweets", "favorites", "text"))

DF_2016_2_25 <- rbind(S1_2016_2_25, S2_2016_2_25, S3_2016_2_25)

save(DF_2016_2_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_2_25.RData")

vector <- as.vector(DF_2016_2_25$text)

fun_txtcleaner(vector)

DF_2016_2_25_cleanREG <- clean_REG
DF_2016_2_25_cleanESP <- clean_ESP

save(DF_2016_2_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_2_25.RData")
save(DF_2016_2_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_2_25.RData")

rm(S1_2016_2_25, S2_2016_2_25, S3_2016_2_25, DF_2016_2_25, DF_2016_2_25_cleanREG, DF_2016_2_25_cleanESP, clean_ESP, clean_REG, vector)

print("February2016, ...done")
print(Sys.time())

#January

print("January2016, cleaning...")
print(Sys.time())

S1_2016_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/1/S1_2016_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2016_1_25 <- select(S1_2016_1_25, c("username", "date", "retweets", "favorites", "text"))

S2_2016_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/1/S2_2016_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2016_1_25 <- select(S2_2016_1_25, c("username", "date", "retweets", "favorites", "text"))

S3_2016_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2016/1/S3_2016_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2016_1_25 <- select(S3_2016_1_25, c("username", "date", "retweets", "favorites", "text"))

DF_2016_1_25 <- rbind(S1_2016_1_25, S2_2016_1_25, S3_2016_1_25)

save(DF_2016_1_25, file = "Objects/Tweets/Series_25/Dirty/DF_2016_1_25.RData")

vector <- as.vector(DF_2016_1_25$text)

fun_txtcleaner(vector)

DF_2016_1_25_cleanREG <- clean_REG
DF_2016_1_25_cleanESP <- clean_ESP

save(DF_2016_1_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2016_1_25.RData")
save(DF_2016_1_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2016_1_25.RData")

rm(S1_2016_1_25, S2_2016_1_25, S3_2016_1_25, DF_2016_1_25, DF_2016_1_25_cleanREG, DF_2016_1_25_cleanESP, clean_ESP, clean_REG, vector)

print("January2016, ...done")
print(Sys.time())

#---2015---

#December

print("December2015, cleaning...")
print(Sys.time())

S1_2015_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/12/S1_2015_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_12_25 <- select(S1_2015_12_25, c("username", "date", "text"))

S2_2015_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/12/S2_2015_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_12_25 <- select(S2_2015_12_25, c("username", "date", "text"))

S3_2015_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/12/S3_2015_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_12_25 <- select(S3_2015_12_25, c("username", "date", "text"))

DF_2015_12_25 <- rbind(S1_2015_12_25, S2_2015_12_25, S3_2015_12_25)

save(DF_2015_12_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_12_25.RData")

vector <- as.vector(DF_2015_12_25$text)

fun_txtcleaner(vector)

DF_2015_12_25_cleanREG <- clean_REG
DF_2015_12_25_cleanESP <- clean_ESP

save(DF_2015_12_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_12_25.RData")
save(DF_2015_12_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_12_25.RData")

rm(S1_2015_12_25, S2_2015_12_25, S3_2015_12_25, DF_2015_12_25, DF_2015_12_25_cleanREG, DF_2015_12_25_cleanESP, clean_ESP, clean_REG, vector)

print("December2015, ...done")
print(Sys.time())

#November

print("November2015, cleaning...")
print(Sys.time())

S1_2015_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/11/S1_2015_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_11_25 <- select(S1_2015_11_25, c("username", "date", "retweets", "favorites", "text"))

S2_2015_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/11/S2_2015_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_11_25 <- select(S2_2015_11_25, c("username", "date", "retweets", "favorites", "text"))

S3_2015_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/11/S3_2015_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_11_25 <- select(S3_2015_11_25, c("username", "date", "retweets", "favorites", "text"))

DF_2015_11_25 <- rbind(S1_2015_11_25, S2_2015_11_25, S3_2015_11_25)

save(DF_2015_11_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_11_25.RData")

vector <- as.vector(DF_2015_11_25$text)

fun_txtcleaner(vector)

DF_2015_11_25_cleanREG <- clean_REG
DF_2015_11_25_cleanESP <- clean_ESP

save(DF_2015_11_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_11_25.RData")
save(DF_2015_11_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_11_25.RData")

rm(S1_2015_11_25, S2_2015_11_25, S3_2015_11_25, DF_2015_11_25, DF_2015_11_25_cleanREG, DF_2015_11_25_cleanESP, clean_ESP, clean_REG, vector)

print("November2015, ...done")
print(Sys.time())

#October

print("October2015, cleaning...")
print(Sys.time())

S1_2015_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/10/S1_2015_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_10_25 <- select(S1_2015_10_25, c("username", "date", "retweets", "favorites", "text"))

S2_2015_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/10/S2_2015_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_10_25 <- select(S2_2015_10_25, c("username", "date", "retweets", "favorites", "text"))

S3_2015_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/10/S3_2015_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_10_25 <- select(S3_2015_10_25, c("username", "date", "retweets", "favorites", "text"))

DF_2015_10_25 <- rbind(S1_2015_10_25, S2_2015_10_25, S3_2015_10_25)

save(DF_2015_10_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_10_25.RData")

vector <- as.vector(DF_2015_10_25$text)

fun_txtcleaner(vector)

DF_2015_10_25_cleanREG <- clean_REG
DF_2015_10_25_cleanESP <- clean_ESP

save(DF_2015_10_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_10_25.RData")
save(DF_2015_10_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_10_25.RData")

rm(S1_2015_10_25, S2_2015_10_25, S3_2015_10_25, DF_2015_10_25, DF_2015_10_25_cleanREG, DF_2015_10_25_cleanESP, clean_ESP, clean_REG, vector)

print("October2015, ...done")
print(Sys.time())

#September

print("September2015, cleaning...")
print(Sys.time())

S1_2015_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/9/S1_2015_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_9_25 <- select(S1_2015_9_25, c("username", "date", "retweets", "favorites", "text"))

S2_2015_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/9/S2_2015_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_9_25 <- select(S2_2015_9_25, c("username", "date", "retweets", "favorites", "text"))

S3_2015_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/9/S3_2015_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_9_25 <- select(S3_2015_9_25, c("username", "date", "retweets", "favorites", "text"))

DF_2015_9_25 <- rbind(S1_2015_9_25, S2_2015_9_25, S3_2015_9_25)

save(DF_2015_9_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_9_25.RData")

vector <- as.vector(DF_2015_9_25$text)

fun_txtcleaner(vector)

DF_2015_9_25_cleanREG <- clean_REG
DF_2015_9_25_cleanESP <- clean_ESP

save(DF_2015_9_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_9_25.RData")
save(DF_2015_9_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_9_25.RData")

rm(S1_2015_9_25, S2_2015_9_25, S3_2015_9_25, DF_2015_9_25, DF_2015_9_25_cleanREG, DF_2015_9_25_cleanESP, clean_ESP, clean_REG, vector)

print("September2015, ...done")
print(Sys.time())

#August

print("August2015, cleaning...")
print(Sys.time())

S1_2015_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/8/S1_2015_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_8_25 <- select(S1_2015_8_25, c("username", "date", "retweets", "favorites", "text"))

S2_2015_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/8/S2_2015_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_8_25 <- select(S2_2015_8_25, c("username", "date", "retweets", "favorites", "text"))

S3_2015_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/8/S3_2015_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_8_25 <- select(S3_2015_8_25, c("username", "date", "retweets", "favorites", "text"))

DF_2015_8_25 <- rbind(S1_2015_8_25, S2_2015_8_25, S3_2015_8_25)

save(DF_2015_8_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_8_25.RData")

vector <- as.vector(DF_2015_8_25$text)

fun_txtcleaner(vector)

DF_2015_8_25_cleanREG <- clean_REG
DF_2015_8_25_cleanESP <- clean_ESP

save(DF_2015_8_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_8_25.RData")
save(DF_2015_8_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_8_25.RData")

rm(S1_2015_8_25, S2_2015_8_25, S3_2015_8_25, DF_2015_8_25, DF_2015_8_25_cleanREG, DF_2015_8_25_cleanESP, clean_ESP, clean_REG, vector)

print("August2015, ...done")
print(Sys.time())

#July

print("July2015, cleaning...")
print(Sys.time())

S1_2015_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/7/S1_2015_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_7_25 <- select(S1_2015_7_25, c("username", "date", "retweets", "favorites", "text"))

S2_2015_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/7/S2_2015_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_7_25 <- select(S2_2015_7_25, c("username", "date", "retweets", "favorites", "text"))

S3_2015_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/7/S3_2015_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_7_25 <- select(S3_2015_7_25, c("username", "date", "retweets", "favorites", "text"))

DF_2015_7_25 <- rbind(S1_2015_7_25, S2_2015_7_25, S3_2015_7_25)

save(DF_2015_7_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_7_25.RData")

vector <- as.vector(DF_2015_7_25$text)

fun_txtcleaner(vector)

DF_2015_7_25_cleanREG <- clean_REG
DF_2015_7_25_cleanESP <- clean_ESP

save(DF_2015_7_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_7_25.RData")
save(DF_2015_7_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_7_25.RData")

rm(S1_2015_7_25, S2_2015_7_25, S3_2015_7_25, DF_2015_7_25, DF_2015_7_25_cleanREG, DF_2015_7_25_cleanESP, clean_ESP, clean_REG, vector)

print("July2015, ...done")
print(Sys.time())

#June

print("June2015, cleaning...")
print(Sys.time())

S1_2015_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/6/S1_2015_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_6_25 <- select(S1_2015_6_25, c("username", "date", "retweets", "favorites", "text"))

S2_2015_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/6/S2_2015_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_6_25 <- select(S2_2015_6_25, c("username", "date", "retweets", "favorites", "text"))

S3_2015_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/6/S3_2015_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_6_25 <- select(S3_2015_6_25, c("username", "date", "retweets", "favorites", "text"))

DF_2015_6_25 <- rbind(S1_2015_6_25, S2_2015_6_25, S3_2015_6_25)

save(DF_2015_6_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_6_25.RData")

vector <- as.vector(DF_2015_6_25$text)

fun_txtcleaner(vector)

DF_2015_6_25_cleanREG <- clean_REG
DF_2015_6_25_cleanESP <- clean_ESP

save(DF_2015_6_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_6_25.RData")
save(DF_2015_6_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_6_25.RData")

rm(S1_2015_6_25, S2_2015_6_25, S3_2015_6_25, DF_2015_6_25, DF_2015_6_25_cleanREG, DF_2015_6_25_cleanESP, clean_ESP, clean_REG, vector)

print("June2015, ...done")
print(Sys.time())

#May

print("May2015, cleaning...")
print(Sys.time())

S1_2015_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/5/S1_2015_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_5_25 <- select(S1_2015_5_25, c("username", "date", "retweets", "favorites", "text"))

S2_2015_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/5/S2_2015_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_5_25 <- select(S2_2015_5_25, c("username", "date", "retweets", "favorites", "text"))

S3_2015_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/5/S3_2015_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_5_25 <- select(S3_2015_5_25, c("username", "date", "retweets", "favorites", "text"))

DF_2015_5_25 <- rbind(S1_2015_5_25, S2_2015_5_25, S3_2015_5_25)

save(DF_2015_5_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_5_25.RData")

vector <- as.vector(DF_2015_5_25$text)

fun_txtcleaner(vector)

DF_2015_5_25_cleanREG <- clean_REG
DF_2015_5_25_cleanESP <- clean_ESP

save(DF_2015_5_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_5_25.RData")
save(DF_2015_5_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_5_25.RData")

rm(S1_2015_5_25, S2_2015_5_25, S3_2015_5_25, DF_2015_5_25, DF_2015_5_25_cleanREG, DF_2015_5_25_cleanESP, clean_ESP, clean_REG, vector)

print("May2015, ...done")
print(Sys.time())

#April

print("April2015, cleaning...")
print(Sys.time())

S1_2015_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/4/S1_2015_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_4_25 <- select(S1_2015_4_25, c("username", "date", "retweets", "favorites", "text"))

S2_2015_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/4/S2_2015_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_4_25 <- select(S2_2015_4_25, c("username", "date", "retweets", "favorites", "text"))

S3_2015_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/4/S3_2015_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_4_25 <- select(S3_2015_4_25, c("username", "date", "retweets", "favorites", "text"))

DF_2015_4_25 <- rbind(S1_2015_4_25, S2_2015_4_25, S3_2015_4_25)

save(DF_2015_4_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_4_25.RData")

vector <- as.vector(DF_2015_4_25$text)

fun_txtcleaner(vector)

DF_2015_4_25_cleanREG <- clean_REG
DF_2015_4_25_cleanESP <- clean_ESP

save(DF_2015_4_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_4_25.RData")
save(DF_2015_4_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_4_25.RData")

rm(S1_2015_4_25, S2_2015_4_25, S3_2015_4_25, DF_2015_4_25, DF_2015_4_25_cleanREG, DF_2015_4_25_cleanESP, clean_ESP, clean_REG, vector)

print("April2015, ...done")
print(Sys.time())

#March

print("March2015, cleaning...")
print(Sys.time())

S1_2015_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/3/S1_2015_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_3_25 <- select(S1_2015_3_25, c("username", "date", "retweets", "favorites", "text"))

S2_2015_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/3/S2_2015_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_3_25 <- select(S2_2015_3_25, c("username", "date", "retweets", "favorites", "text"))

S3_2015_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/3/S3_2015_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_3_25 <- select(S3_2015_3_25, c("username", "date", "retweets", "favorites", "text"))

DF_2015_3_25 <- rbind(S1_2015_3_25, S2_2015_3_25, S3_2015_3_25)

save(DF_2015_3_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_3_25.RData")

vector <- as.vector(DF_2015_3_25$text)

fun_txtcleaner(vector)

DF_2015_3_25_cleanREG <- clean_REG
DF_2015_3_25_cleanESP <- clean_ESP

save(DF_2015_3_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_3_25.RData")
save(DF_2015_3_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_3_25.RData")

rm(S1_2015_3_25, S2_2015_3_25, S3_2015_3_25, DF_2015_3_25, DF_2015_3_25_cleanREG, DF_2015_3_25_cleanESP, clean_ESP, clean_REG, vector)

print("March2015, ...done")
print(Sys.time())

#February

print("February2015, cleaning...")
print(Sys.time())

S1_2015_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/2/S1_2015_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_2_25 <- select(S1_2015_2_25, c("username", "date", "retweets", "favorites", "text"))

S2_2015_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/2/S2_2015_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_2_25 <- select(S2_2015_2_25, c("username", "date", "retweets", "favorites", "text"))

S3_2015_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/2/S3_2015_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_2_25 <- select(S3_2015_2_25, c("username", "date", "retweets", "favorites", "text"))

DF_2015_2_25 <- rbind(S1_2015_2_25, S2_2015_2_25, S3_2015_2_25)

save(DF_2015_2_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_2_25.RData")

vector <- as.vector(DF_2015_2_25$text)

fun_txtcleaner(vector)

DF_2015_2_25_cleanREG <- clean_REG
DF_2015_2_25_cleanESP <- clean_ESP

save(DF_2015_2_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_2_25.RData")
save(DF_2015_2_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_2_25.RData")

rm(S1_2015_2_25, S2_2015_2_25, S3_2015_2_25, DF_2015_2_25, DF_2015_2_25_cleanREG, DF_2015_2_25_cleanESP, clean_ESP, clean_REG, vector)

print("February2015, ...done")
print(Sys.time())

#January

print("January2015, cleaning...")
print(Sys.time())

S1_2015_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/1/S1_2015_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2015_1_25 <- select(S1_2015_1_25, c("username", "date", "retweets", "favorites", "text"))

S2_2015_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/1/S2_2015_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2015_1_25 <- select(S2_2015_1_25, c("username", "date", "retweets", "favorites", "text"))

S3_2015_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2015/1/S3_2015_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2015_1_25 <- select(S3_2015_1_25, c("username", "date", "retweets", "favorites", "text"))

DF_2015_1_25 <- rbind(S1_2015_1_25, S2_2015_1_25, S3_2015_1_25)

save(DF_2015_1_25, file = "Objects/Tweets/Series_25/Dirty/DF_2015_1_25.RData")

vector <- as.vector(DF_2015_1_25$text)

fun_txtcleaner(vector)

DF_2015_1_25_cleanREG <- clean_REG
DF_2015_1_25_cleanESP <- clean_ESP

save(DF_2015_1_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2015_1_25.RData")
save(DF_2015_1_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2015_1_25.RData")

rm(S1_2015_1_25, S2_2015_1_25, S3_2015_1_25, DF_2015_1_25, DF_2015_1_25_cleanREG, DF_2015_1_25_cleanESP, clean_ESP, clean_REG, vector)

print("January2015, ...done")
print(Sys.time())

#---2014---

#December

print("December2014, cleaning...")
print(Sys.time())

S1_2014_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/12/S1_2014_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_12_25 <- select(S1_2014_12_25, c("username", "date", "text"))

S2_2014_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/12/S2_2014_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_12_25 <- select(S2_2014_12_25, c("username", "date", "text"))

S3_2014_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/12/S3_2014_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_12_25 <- select(S3_2014_12_25, c("username", "date", "text"))

DF_2014_12_25 <- rbind(S1_2014_12_25, S2_2014_12_25, S3_2014_12_25)

save(DF_2014_12_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_12_25.RData")

vector <- as.vector(DF_2014_12_25$text)

fun_txtcleaner(vector)

DF_2014_12_25_cleanREG <- clean_REG
DF_2014_12_25_cleanESP <- clean_ESP

save(DF_2014_12_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_12_25.RData")
save(DF_2014_12_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_12_25.RData")

rm(S1_2014_12_25, S2_2014_12_25, S3_2014_12_25, DF_2014_12_25, DF_2014_12_25_cleanREG, DF_2014_12_25_cleanESP, clean_ESP, clean_REG, vector)

print("December2014, ...done")
print(Sys.time())

#November

print("November2014, cleaning...")
print(Sys.time())

S1_2014_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/11/S1_2014_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_11_25 <- select(S1_2014_11_25, c("username", "date", "retweets", "favorites", "text"))

S2_2014_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/11/S2_2014_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_11_25 <- select(S2_2014_11_25, c("username", "date", "retweets", "favorites", "text"))

S3_2014_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/11/S3_2014_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_11_25 <- select(S3_2014_11_25, c("username", "date", "retweets", "favorites", "text"))

DF_2014_11_25 <- rbind(S1_2014_11_25, S2_2014_11_25, S3_2014_11_25)

save(DF_2014_11_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_11_25.RData")

vector <- as.vector(DF_2014_11_25$text)

fun_txtcleaner(vector)

DF_2014_11_25_cleanREG <- clean_REG
DF_2014_11_25_cleanESP <- clean_ESP

save(DF_2014_11_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_11_25.RData")
save(DF_2014_11_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_11_25.RData")

rm(S1_2014_11_25, S2_2014_11_25, S3_2014_11_25, DF_2014_11_25, DF_2014_11_25_cleanREG, DF_2014_11_25_cleanESP, clean_ESP, clean_REG, vector)

print("November2014, ...done")
print(Sys.time())

#October

print("October2014, cleaning...")
print(Sys.time())

S1_2014_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/10/S1_2014_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_10_25 <- select(S1_2014_10_25, c("username", "date", "retweets", "favorites", "text"))

S2_2014_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/10/S2_2014_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_10_25 <- select(S2_2014_10_25, c("username", "date", "retweets", "favorites", "text"))

S3_2014_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/10/S3_2014_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_10_25 <- select(S3_2014_10_25, c("username", "date", "retweets", "favorites", "text"))

DF_2014_10_25 <- rbind(S1_2014_10_25, S2_2014_10_25, S3_2014_10_25)

save(DF_2014_10_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_10_25.RData")

vector <- as.vector(DF_2014_10_25$text)

fun_txtcleaner(vector)

DF_2014_10_25_cleanREG <- clean_REG
DF_2014_10_25_cleanESP <- clean_ESP

save(DF_2014_10_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_10_25.RData")
save(DF_2014_10_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_10_25.RData")

rm(S1_2014_10_25, S2_2014_10_25, S3_2014_10_25, DF_2014_10_25, DF_2014_10_25_cleanREG, DF_2014_10_25_cleanESP, clean_ESP, clean_REG, vector)

print("October2014, ...done")
print(Sys.time())

#September

print("September2014, cleaning...")
print(Sys.time())

S1_2014_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/9/S1_2014_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_9_25 <- select(S1_2014_9_25, c("username", "date", "retweets", "favorites", "text"))

S2_2014_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/9/S2_2014_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_9_25 <- select(S2_2014_9_25, c("username", "date", "retweets", "favorites", "text"))

S3_2014_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/9/S3_2014_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_9_25 <- select(S3_2014_9_25, c("username", "date", "retweets", "favorites", "text"))

DF_2014_9_25 <- rbind(S1_2014_9_25, S2_2014_9_25, S3_2014_9_25)

save(DF_2014_9_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_9_25.RData")

vector <- as.vector(DF_2014_9_25$text)

fun_txtcleaner(vector)

DF_2014_9_25_cleanREG <- clean_REG
DF_2014_9_25_cleanESP <- clean_ESP

save(DF_2014_9_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_9_25.RData")
save(DF_2014_9_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_9_25.RData")

rm(S1_2014_9_25, S2_2014_9_25, S3_2014_9_25, DF_2014_9_25, DF_2014_9_25_cleanREG, DF_2014_9_25_cleanESP, clean_ESP, clean_REG, vector)

print("September2014, ...done")
print(Sys.time())

#August

print("August2014, cleaning...")
print(Sys.time())

S1_2014_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/8/S1_2014_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_8_25 <- select(S1_2014_8_25, c("username", "date", "retweets", "favorites", "text"))

S2_2014_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/8/S2_2014_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_8_25 <- select(S2_2014_8_25, c("username", "date", "retweets", "favorites", "text"))

S3_2014_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/8/S3_2014_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_8_25 <- select(S3_2014_8_25, c("username", "date", "retweets", "favorites", "text"))

DF_2014_8_25 <- rbind(S1_2014_8_25, S2_2014_8_25, S3_2014_8_25)

save(DF_2014_8_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_8_25.RData")

vector <- as.vector(DF_2014_8_25$text)

fun_txtcleaner(vector)

DF_2014_8_25_cleanREG <- clean_REG
DF_2014_8_25_cleanESP <- clean_ESP

save(DF_2014_8_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_8_25.RData")
save(DF_2014_8_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_8_25.RData")

rm(S1_2014_8_25, S2_2014_8_25, S3_2014_8_25, DF_2014_8_25, DF_2014_8_25_cleanREG, DF_2014_8_25_cleanESP, clean_ESP, clean_REG, vector)

print("August2014, ...done")
print(Sys.time())

#July

print("July2014, cleaning...")
print(Sys.time())

S1_2014_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/7/S1_2014_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_7_25 <- select(S1_2014_7_25, c("username", "date", "retweets", "favorites", "text"))

S2_2014_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/7/S2_2014_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_7_25 <- select(S2_2014_7_25, c("username", "date", "retweets", "favorites", "text"))

S3_2014_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/7/S3_2014_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_7_25 <- select(S3_2014_7_25, c("username", "date", "retweets", "favorites", "text"))

DF_2014_7_25 <- rbind(S1_2014_7_25, S2_2014_7_25, S3_2014_7_25)

save(DF_2014_7_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_7_25.RData")

vector <- as.vector(DF_2014_7_25$text)

fun_txtcleaner(vector)

DF_2014_7_25_cleanREG <- clean_REG
DF_2014_7_25_cleanESP <- clean_ESP

save(DF_2014_7_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_7_25.RData")
save(DF_2014_7_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_7_25.RData")

rm(S1_2014_7_25, S2_2014_7_25, S3_2014_7_25, DF_2014_7_25, DF_2014_7_25_cleanREG, DF_2014_7_25_cleanESP, clean_ESP, clean_REG, vector)

print("July2014, ...done")
print(Sys.time())

#June

print("June2014, cleaning...")
print(Sys.time())

S1_2014_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/6/S1_2014_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_6_25 <- select(S1_2014_6_25, c("username", "date", "retweets", "favorites", "text"))

S2_2014_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/6/S2_2014_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_6_25 <- select(S2_2014_6_25, c("username", "date", "retweets", "favorites", "text"))

S3_2014_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/6/S3_2014_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_6_25 <- select(S3_2014_6_25, c("username", "date", "retweets", "favorites", "text"))

DF_2014_6_25 <- rbind(S1_2014_6_25, S2_2014_6_25, S3_2014_6_25)

save(DF_2014_6_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_6_25.RData")

vector <- as.vector(DF_2014_6_25$text)

fun_txtcleaner(vector)

DF_2014_6_25_cleanREG <- clean_REG
DF_2014_6_25_cleanESP <- clean_ESP

save(DF_2014_6_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_6_25.RData")
save(DF_2014_6_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_6_25.RData")

rm(S1_2014_6_25, S2_2014_6_25, S3_2014_6_25, DF_2014_6_25, DF_2014_6_25_cleanREG, DF_2014_6_25_cleanESP, clean_ESP, clean_REG, vector)

print("June2014, ...done")
print(Sys.time())

#May

print("May2014, cleaning...")
print(Sys.time())

S1_2014_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/5/S1_2014_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_5_25 <- select(S1_2014_5_25, c("username", "date", "retweets", "favorites", "text"))

S2_2014_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/5/S2_2014_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_5_25 <- select(S2_2014_5_25, c("username", "date", "retweets", "favorites", "text"))

S3_2014_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/5/S3_2014_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_5_25 <- select(S3_2014_5_25, c("username", "date", "retweets", "favorites", "text"))

DF_2014_5_25 <- rbind(S1_2014_5_25, S2_2014_5_25, S3_2014_5_25)

save(DF_2014_5_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_5_25.RData")

vector <- as.vector(DF_2014_5_25$text)

fun_txtcleaner(vector)

DF_2014_5_25_cleanREG <- clean_REG
DF_2014_5_25_cleanESP <- clean_ESP

save(DF_2014_5_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_5_25.RData")
save(DF_2014_5_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_5_25.RData")

rm(S1_2014_5_25, S2_2014_5_25, S3_2014_5_25, DF_2014_5_25, DF_2014_5_25_cleanREG, DF_2014_5_25_cleanESP, clean_ESP, clean_REG, vector)

print("May2014, ...done")
print(Sys.time())

#April

print("April2014, cleaning...")
print(Sys.time())

S1_2014_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/4/S1_2014_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_4_25 <- select(S1_2014_4_25, c("username", "date", "retweets", "favorites", "text"))

S2_2014_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/4/S2_2014_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_4_25 <- select(S2_2014_4_25, c("username", "date", "retweets", "favorites", "text"))

S3_2014_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/4/S3_2014_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_4_25 <- select(S3_2014_4_25, c("username", "date", "retweets", "favorites", "text"))

DF_2014_4_25 <- rbind(S1_2014_4_25, S2_2014_4_25, S3_2014_4_25)

save(DF_2014_4_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_4_25.RData")

vector <- as.vector(DF_2014_4_25$text)

fun_txtcleaner(vector)

DF_2014_4_25_cleanREG <- clean_REG
DF_2014_4_25_cleanESP <- clean_ESP

save(DF_2014_4_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_4_25.RData")
save(DF_2014_4_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_4_25.RData")

rm(S1_2014_4_25, S2_2014_4_25, S3_2014_4_25, DF_2014_4_25, DF_2014_4_25_cleanREG, DF_2014_4_25_cleanESP, clean_ESP, clean_REG, vector)

print("April2014, ...done")
print(Sys.time())

#March

print("March2014, cleaning...")
print(Sys.time())

S1_2014_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/3/S1_2014_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_3_25 <- select(S1_2014_3_25, c("username", "date", "retweets", "favorites", "text"))

S2_2014_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/3/S2_2014_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_3_25 <- select(S2_2014_3_25, c("username", "date", "retweets", "favorites", "text"))

S3_2014_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/3/S3_2014_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_3_25 <- select(S3_2014_3_25, c("username", "date", "retweets", "favorites", "text"))

DF_2014_3_25 <- rbind(S1_2014_3_25, S2_2014_3_25, S3_2014_3_25)

save(DF_2014_3_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_3_25.RData")

vector <- as.vector(DF_2014_3_25$text)

fun_txtcleaner(vector)

DF_2014_3_25_cleanREG <- clean_REG
DF_2014_3_25_cleanESP <- clean_ESP

save(DF_2014_3_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_3_25.RData")
save(DF_2014_3_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_3_25.RData")

rm(S1_2014_3_25, S2_2014_3_25, S3_2014_3_25, DF_2014_3_25, DF_2014_3_25_cleanREG, DF_2014_3_25_cleanESP, clean_ESP, clean_REG, vector)

print("March2014, ...done")
print(Sys.time())

#February

print("February2014, cleaning...")
print(Sys.time())

S1_2014_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/2/S1_2014_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_2_25 <- select(S1_2014_2_25, c("username", "date", "retweets", "favorites", "text"))

S2_2014_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/2/S2_2014_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_2_25 <- select(S2_2014_2_25, c("username", "date", "retweets", "favorites", "text"))

S3_2014_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/2/S3_2014_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_2_25 <- select(S3_2014_2_25, c("username", "date", "retweets", "favorites", "text"))

DF_2014_2_25 <- rbind(S1_2014_2_25, S2_2014_2_25, S3_2014_2_25)

save(DF_2014_2_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_2_25.RData")

vector <- as.vector(DF_2014_2_25$text)

fun_txtcleaner(vector)

DF_2014_2_25_cleanREG <- clean_REG
DF_2014_2_25_cleanESP <- clean_ESP

save(DF_2014_2_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_2_25.RData")
save(DF_2014_2_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_2_25.RData")

rm(S1_2014_2_25, S2_2014_2_25, S3_2014_2_25, DF_2014_2_25, DF_2014_2_25_cleanREG, DF_2014_2_25_cleanESP, clean_ESP, clean_REG, vector)

print("February2014, ...done")
print(Sys.time())

#January

print("January2014, cleaning...")
print(Sys.time())

S1_2014_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/1/S1_2014_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2014_1_25 <- select(S1_2014_1_25, c("username", "date", "retweets", "favorites", "text"))

S2_2014_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/1/S2_2014_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2014_1_25 <- select(S2_2014_1_25, c("username", "date", "retweets", "favorites", "text"))

S3_2014_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2014/1/S3_2014_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2014_1_25 <- select(S3_2014_1_25, c("username", "date", "retweets", "favorites", "text"))

DF_2014_1_25 <- rbind(S1_2014_1_25, S2_2014_1_25, S3_2014_1_25)

save(DF_2014_1_25, file = "Objects/Tweets/Series_25/Dirty/DF_2014_1_25.RData")

vector <- as.vector(DF_2014_1_25$text)

fun_txtcleaner(vector)

DF_2014_1_25_cleanREG <- clean_REG
DF_2014_1_25_cleanESP <- clean_ESP

save(DF_2014_1_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2014_1_25.RData")
save(DF_2014_1_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2014_1_25.RData")

rm(S1_2014_1_25, S2_2014_1_25, S3_2014_1_25, DF_2014_1_25, DF_2014_1_25_cleanREG, DF_2014_1_25_cleanESP, clean_ESP, clean_REG, vector)

print("January2014, ...done")
print(Sys.time())

#---2013---

#December

print("December2013, cleaning...")
print(Sys.time())

S1_2013_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/12/S1_2013_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_12_25 <- select(S1_2013_12_25, c("username", "date", "text"))

S2_2013_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/12/S2_2013_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_12_25 <- select(S2_2013_12_25, c("username", "date", "text"))

S3_2013_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/12/S3_2013_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_12_25 <- select(S3_2013_12_25, c("username", "date", "text"))

DF_2013_12_25 <- rbind(S1_2013_12_25, S2_2013_12_25, S3_2013_12_25)

save(DF_2013_12_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_12_25.RData")

vector <- as.vector(DF_2013_12_25$text)

fun_txtcleaner(vector)

DF_2013_12_25_cleanREG <- clean_REG
DF_2013_12_25_cleanESP <- clean_ESP

save(DF_2013_12_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_12_25.RData")
save(DF_2013_12_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_12_25.RData")

rm(S1_2013_12_25, S2_2013_12_25, S3_2013_12_25, DF_2013_12_25, DF_2013_12_25_cleanREG, DF_2013_12_25_cleanESP, clean_ESP, clean_REG, vector)

print("December2013, ...done")
print(Sys.time())

#November

print("November2013, cleaning...")
print(Sys.time())

S1_2013_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/11/S1_2013_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_11_25 <- select(S1_2013_11_25, c("username", "date", "retweets", "favorites", "text"))

S2_2013_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/11/S2_2013_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_11_25 <- select(S2_2013_11_25, c("username", "date", "retweets", "favorites", "text"))

S3_2013_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/11/S3_2013_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_11_25 <- select(S3_2013_11_25, c("username", "date", "retweets", "favorites", "text"))

DF_2013_11_25 <- rbind(S1_2013_11_25, S2_2013_11_25, S3_2013_11_25)

save(DF_2013_11_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_11_25.RData")

vector <- as.vector(DF_2013_11_25$text)

fun_txtcleaner(vector)

DF_2013_11_25_cleanREG <- clean_REG
DF_2013_11_25_cleanESP <- clean_ESP

save(DF_2013_11_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_11_25.RData")
save(DF_2013_11_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_11_25.RData")

rm(S1_2013_11_25, S2_2013_11_25, S3_2013_11_25, DF_2013_11_25, DF_2013_11_25_cleanREG, DF_2013_11_25_cleanESP, clean_ESP, clean_REG, vector)

print("November2013, ...done")
print(Sys.time())

#October

print("October2013, cleaning...")
print(Sys.time())

S1_2013_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/10/S1_2013_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_10_25 <- select(S1_2013_10_25, c("username", "date", "retweets", "favorites", "text"))

S2_2013_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/10/S2_2013_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_10_25 <- select(S2_2013_10_25, c("username", "date", "retweets", "favorites", "text"))

S3_2013_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/10/S3_2013_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_10_25 <- select(S3_2013_10_25, c("username", "date", "retweets", "favorites", "text"))

DF_2013_10_25 <- rbind(S1_2013_10_25, S2_2013_10_25, S3_2013_10_25)

save(DF_2013_10_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_10_25.RData")

vector <- as.vector(DF_2013_10_25$text)

fun_txtcleaner(vector)

DF_2013_10_25_cleanREG <- clean_REG
DF_2013_10_25_cleanESP <- clean_ESP

save(DF_2013_10_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_10_25.RData")
save(DF_2013_10_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_10_25.RData")

rm(S1_2013_10_25, S2_2013_10_25, S3_2013_10_25, DF_2013_10_25, DF_2013_10_25_cleanREG, DF_2013_10_25_cleanESP, clean_ESP, clean_REG, vector)

print("October2013, ...done")
print(Sys.time())

#September

print("September2013, cleaning...")
print(Sys.time())

S1_2013_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/9/S1_2013_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_9_25 <- select(S1_2013_9_25, c("username", "date", "retweets", "favorites", "text"))

S2_2013_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/9/S2_2013_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_9_25 <- select(S2_2013_9_25, c("username", "date", "retweets", "favorites", "text"))

S3_2013_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/9/S3_2013_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_9_25 <- select(S3_2013_9_25, c("username", "date", "retweets", "favorites", "text"))

DF_2013_9_25 <- rbind(S1_2013_9_25, S2_2013_9_25, S3_2013_9_25)

save(DF_2013_9_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_9_25.RData")

vector <- as.vector(DF_2013_9_25$text)

fun_txtcleaner(vector)

DF_2013_9_25_cleanREG <- clean_REG
DF_2013_9_25_cleanESP <- clean_ESP

save(DF_2013_9_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_9_25.RData")
save(DF_2013_9_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_9_25.RData")

rm(S1_2013_9_25, S2_2013_9_25, S3_2013_9_25, DF_2013_9_25, DF_2013_9_25_cleanREG, DF_2013_9_25_cleanESP, clean_ESP, clean_REG, vector)

print("September2013, ...done")
print(Sys.time())

#August

print("August2013, cleaning...")
print(Sys.time())

S1_2013_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/8/S1_2013_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_8_25 <- select(S1_2013_8_25, c("username", "date", "retweets", "favorites", "text"))

S2_2013_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/8/S2_2013_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_8_25 <- select(S2_2013_8_25, c("username", "date", "retweets", "favorites", "text"))

S3_2013_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/8/S3_2013_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_8_25 <- select(S3_2013_8_25, c("username", "date", "retweets", "favorites", "text"))

DF_2013_8_25 <- rbind(S1_2013_8_25, S2_2013_8_25, S3_2013_8_25)

save(DF_2013_8_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_8_25.RData")

vector <- as.vector(DF_2013_8_25$text)

fun_txtcleaner(vector)

DF_2013_8_25_cleanREG <- clean_REG
DF_2013_8_25_cleanESP <- clean_ESP

save(DF_2013_8_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_8_25.RData")
save(DF_2013_8_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_8_25.RData")

rm(S1_2013_8_25, S2_2013_8_25, S3_2013_8_25, DF_2013_8_25, DF_2013_8_25_cleanREG, DF_2013_8_25_cleanESP, clean_ESP, clean_REG, vector)

print("August2013, ...done")
print(Sys.time())

#July

print("July2013, cleaning...")
print(Sys.time())

S1_2013_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/7/S1_2013_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_7_25 <- select(S1_2013_7_25, c("username", "date", "retweets", "favorites", "text"))

S2_2013_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/7/S2_2013_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_7_25 <- select(S2_2013_7_25, c("username", "date", "retweets", "favorites", "text"))

S3_2013_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/7/S3_2013_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_7_25 <- select(S3_2013_7_25, c("username", "date", "retweets", "favorites", "text"))

DF_2013_7_25 <- rbind(S1_2013_7_25, S2_2013_7_25, S3_2013_7_25)

save(DF_2013_7_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_7_25.RData")

vector <- as.vector(DF_2013_7_25$text)

fun_txtcleaner(vector)

DF_2013_7_25_cleanREG <- clean_REG
DF_2013_7_25_cleanESP <- clean_ESP

save(DF_2013_7_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_7_25.RData")
save(DF_2013_7_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_7_25.RData")

rm(S1_2013_7_25, S2_2013_7_25, S3_2013_7_25, DF_2013_7_25, DF_2013_7_25_cleanREG, DF_2013_7_25_cleanESP, clean_ESP, clean_REG, vector)

print("July2013, ...done")
print(Sys.time())

#June

print("June2013, cleaning...")
print(Sys.time())

S1_2013_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/6/S1_2013_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_6_25 <- select(S1_2013_6_25, c("username", "date", "retweets", "favorites", "text"))

S2_2013_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/6/S2_2013_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_6_25 <- select(S2_2013_6_25, c("username", "date", "retweets", "favorites", "text"))

S3_2013_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/6/S3_2013_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_6_25 <- select(S3_2013_6_25, c("username", "date", "retweets", "favorites", "text"))

DF_2013_6_25 <- rbind(S1_2013_6_25, S2_2013_6_25, S3_2013_6_25)

save(DF_2013_6_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_6_25.RData")

vector <- as.vector(DF_2013_6_25$text)

fun_txtcleaner(vector)

DF_2013_6_25_cleanREG <- clean_REG
DF_2013_6_25_cleanESP <- clean_ESP

save(DF_2013_6_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_6_25.RData")
save(DF_2013_6_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_6_25.RData")

rm(S1_2013_6_25, S2_2013_6_25, S3_2013_6_25, DF_2013_6_25, DF_2013_6_25_cleanREG, DF_2013_6_25_cleanESP, clean_ESP, clean_REG, vector)

print("June2013, ...done")
print(Sys.time())

#May

print("May2013, cleaning...")
print(Sys.time())

S1_2013_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/5/S1_2013_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_5_25 <- select(S1_2013_5_25, c("username", "date", "retweets", "favorites", "text"))

S2_2013_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/5/S2_2013_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_5_25 <- select(S2_2013_5_25, c("username", "date", "retweets", "favorites", "text"))

S3_2013_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/5/S3_2013_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_5_25 <- select(S3_2013_5_25, c("username", "date", "retweets", "favorites", "text"))

DF_2013_5_25 <- rbind(S1_2013_5_25, S2_2013_5_25, S3_2013_5_25)

save(DF_2013_5_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_5_25.RData")

vector <- as.vector(DF_2013_5_25$text)

fun_txtcleaner(vector)

DF_2013_5_25_cleanREG <- clean_REG
DF_2013_5_25_cleanESP <- clean_ESP

save(DF_2013_5_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_5_25.RData")
save(DF_2013_5_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_5_25.RData")

rm(S1_2013_5_25, S2_2013_5_25, S3_2013_5_25, DF_2013_5_25, DF_2013_5_25_cleanREG, DF_2013_5_25_cleanESP, clean_ESP, clean_REG, vector)

print("May2013, ...done")
print(Sys.time())

#April

print("April2013, cleaning...")
print(Sys.time())

S1_2013_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/4/S1_2013_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_4_25 <- select(S1_2013_4_25, c("username", "date", "retweets", "favorites", "text"))

S2_2013_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/4/S2_2013_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_4_25 <- select(S2_2013_4_25, c("username", "date", "retweets", "favorites", "text"))

S3_2013_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/4/S3_2013_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_4_25 <- select(S3_2013_4_25, c("username", "date", "retweets", "favorites", "text"))

DF_2013_4_25 <- rbind(S1_2013_4_25, S2_2013_4_25, S3_2013_4_25)

save(DF_2013_4_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_4_25.RData")

vector <- as.vector(DF_2013_4_25$text)

fun_txtcleaner(vector)

DF_2013_4_25_cleanREG <- clean_REG
DF_2013_4_25_cleanESP <- clean_ESP

save(DF_2013_4_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_4_25.RData")
save(DF_2013_4_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_4_25.RData")

rm(S1_2013_4_25, S2_2013_4_25, S3_2013_4_25, DF_2013_4_25, DF_2013_4_25_cleanREG, DF_2013_4_25_cleanESP, clean_ESP, clean_REG, vector)

print("April2013, ...done")
print(Sys.time())

#March

print("March2013, cleaning...")
print(Sys.time())

S1_2013_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/3/S1_2013_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_3_25 <- select(S1_2013_3_25, c("username", "date", "retweets", "favorites", "text"))

S2_2013_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/3/S2_2013_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_3_25 <- select(S2_2013_3_25, c("username", "date", "retweets", "favorites", "text"))

S3_2013_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/3/S3_2013_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_3_25 <- select(S3_2013_3_25, c("username", "date", "retweets", "favorites", "text"))

DF_2013_3_25 <- rbind(S1_2013_3_25, S2_2013_3_25, S3_2013_3_25)

save(DF_2013_3_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_3_25.RData")

vector <- as.vector(DF_2013_3_25$text)

fun_txtcleaner(vector)

DF_2013_3_25_cleanREG <- clean_REG
DF_2013_3_25_cleanESP <- clean_ESP

save(DF_2013_3_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_3_25.RData")
save(DF_2013_3_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_3_25.RData")

rm(S1_2013_3_25, S2_2013_3_25, S3_2013_3_25, DF_2013_3_25, DF_2013_3_25_cleanREG, DF_2013_3_25_cleanESP, clean_ESP, clean_REG, vector)

print("March2013, ...done")
print(Sys.time())

#February

print("February2013, cleaning...")
print(Sys.time())

S1_2013_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/2/S1_2013_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_2_25 <- select(S1_2013_2_25, c("username", "date", "retweets", "favorites", "text"))

S2_2013_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/2/S2_2013_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_2_25 <- select(S2_2013_2_25, c("username", "date", "retweets", "favorites", "text"))

S3_2013_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/2/S3_2013_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_2_25 <- select(S3_2013_2_25, c("username", "date", "retweets", "favorites", "text"))

DF_2013_2_25 <- rbind(S1_2013_2_25, S2_2013_2_25, S3_2013_2_25)

save(DF_2013_2_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_2_25.RData")

vector <- as.vector(DF_2013_2_25$text)

fun_txtcleaner(vector)

DF_2013_2_25_cleanREG <- clean_REG
DF_2013_2_25_cleanESP <- clean_ESP

save(DF_2013_2_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_2_25.RData")
save(DF_2013_2_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_2_25.RData")

rm(S1_2013_2_25, S2_2013_2_25, S3_2013_2_25, DF_2013_2_25, DF_2013_2_25_cleanREG, DF_2013_2_25_cleanESP, clean_ESP, clean_REG, vector)

print("February2013, ...done")
print(Sys.time())

#January

print("January2013, cleaning...")
print(Sys.time())

S1_2013_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/1/S1_2013_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2013_1_25 <- select(S1_2013_1_25, c("username", "date", "retweets", "favorites", "text"))

S2_2013_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/1/S2_2013_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2013_1_25 <- select(S2_2013_1_25, c("username", "date", "retweets", "favorites", "text"))

S3_2013_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2013/1/S3_2013_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2013_1_25 <- select(S3_2013_1_25, c("username", "date", "retweets", "favorites", "text"))

DF_2013_1_25 <- rbind(S1_2013_1_25, S2_2013_1_25, S3_2013_1_25)

save(DF_2013_1_25, file = "Objects/Tweets/Series_25/Dirty/DF_2013_1_25.RData")

vector <- as.vector(DF_2013_1_25$text)

fun_txtcleaner(vector)

DF_2013_1_25_cleanREG <- clean_REG
DF_2013_1_25_cleanESP <- clean_ESP

save(DF_2013_1_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2013_1_25.RData")
save(DF_2013_1_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2013_1_25.RData")

rm(S1_2013_1_25, S2_2013_1_25, S3_2013_1_25, DF_2013_1_25, DF_2013_1_25_cleanREG, DF_2013_1_25_cleanESP, clean_ESP, clean_REG, vector)

print("January2013, ...done")
print(Sys.time())

#---2012---

#December

print("December2012, cleaning...")
print(Sys.time())

S1_2012_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/12/S1_2012_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_12_25 <- select(S1_2012_12_25, c("username", "date", "text"))

S2_2012_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/12/S2_2012_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_12_25 <- select(S2_2012_12_25, c("username", "date", "text"))

S3_2012_12_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/12/S3_2012_12_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_12_25 <- select(S3_2012_12_25, c("username", "date", "text"))

DF_2012_12_25 <- rbind(S1_2012_12_25, S2_2012_12_25, S3_2012_12_25)

save(DF_2012_12_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_12_25.RData")

vector <- as.vector(DF_2012_12_25$text)

fun_txtcleaner(vector)

DF_2012_12_25_cleanREG <- clean_REG
DF_2012_12_25_cleanESP <- clean_ESP

save(DF_2012_12_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_12_25.RData")
save(DF_2012_12_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_12_25.RData")

rm(S1_2012_12_25, S2_2012_12_25, S3_2012_12_25, DF_2012_12_25, DF_2012_12_25_cleanREG, DF_2012_12_25_cleanESP, clean_ESP, clean_REG, vector)

print("December2012, ...done")
print(Sys.time())

#November

print("November2012, cleaning...")
print(Sys.time())

S1_2012_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/11/S1_2012_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_11_25 <- select(S1_2012_11_25, c("username", "date", "retweets", "favorites", "text"))

S2_2012_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/11/S2_2012_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_11_25 <- select(S2_2012_11_25, c("username", "date", "retweets", "favorites", "text"))

S3_2012_11_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/11/S3_2012_11_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_11_25 <- select(S3_2012_11_25, c("username", "date", "retweets", "favorites", "text"))

DF_2012_11_25 <- rbind(S1_2012_11_25, S2_2012_11_25, S3_2012_11_25)

save(DF_2012_11_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_11_25.RData")

vector <- as.vector(DF_2012_11_25$text)

fun_txtcleaner(vector)

DF_2012_11_25_cleanREG <- clean_REG
DF_2012_11_25_cleanESP <- clean_ESP

save(DF_2012_11_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_11_25.RData")
save(DF_2012_11_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_11_25.RData")

rm(S1_2012_11_25, S2_2012_11_25, S3_2012_11_25, DF_2012_11_25, DF_2012_11_25_cleanREG, DF_2012_11_25_cleanESP, clean_ESP, clean_REG, vector)

print("November2012, ...done")
print(Sys.time())

#October

print("October2012, cleaning...")
print(Sys.time())

S1_2012_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/10/S1_2012_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_10_25 <- select(S1_2012_10_25, c("username", "date", "retweets", "favorites", "text"))

S2_2012_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/10/S2_2012_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_10_25 <- select(S2_2012_10_25, c("username", "date", "retweets", "favorites", "text"))

S3_2012_10_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/10/S3_2012_10_25.csv", 
                                                              ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_10_25 <- select(S3_2012_10_25, c("username", "date", "retweets", "favorites", "text"))

DF_2012_10_25 <- rbind(S1_2012_10_25, S2_2012_10_25, S3_2012_10_25)

save(DF_2012_10_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_10_25.RData")

vector <- as.vector(DF_2012_10_25$text)

fun_txtcleaner(vector)

DF_2012_10_25_cleanREG <- clean_REG
DF_2012_10_25_cleanESP <- clean_ESP

save(DF_2012_10_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_10_25.RData")
save(DF_2012_10_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_10_25.RData")

rm(S1_2012_10_25, S2_2012_10_25, S3_2012_10_25, DF_2012_10_25, DF_2012_10_25_cleanREG, DF_2012_10_25_cleanESP, clean_ESP, clean_REG, vector)

print("October2012, ...done")
print(Sys.time())

#September

print("September2012, cleaning...")
print(Sys.time())

S1_2012_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/9/S1_2012_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_9_25 <- select(S1_2012_9_25, c("username", "date", "retweets", "favorites", "text"))

S2_2012_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/9/S2_2012_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_9_25 <- select(S2_2012_9_25, c("username", "date", "retweets", "favorites", "text"))

S3_2012_9_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/9/S3_2012_9_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_9_25 <- select(S3_2012_9_25, c("username", "date", "retweets", "favorites", "text"))

DF_2012_9_25 <- rbind(S1_2012_9_25, S2_2012_9_25, S3_2012_9_25)

save(DF_2012_9_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_9_25.RData")

vector <- as.vector(DF_2012_9_25$text)

fun_txtcleaner(vector)

DF_2012_9_25_cleanREG <- clean_REG
DF_2012_9_25_cleanESP <- clean_ESP

save(DF_2012_9_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_9_25.RData")
save(DF_2012_9_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_9_25.RData")

rm(S1_2012_9_25, S2_2012_9_25, S3_2012_9_25, DF_2012_9_25, DF_2012_9_25_cleanREG, DF_2012_9_25_cleanESP, clean_ESP, clean_REG, vector)

print("September2012, ...done")
print(Sys.time())

#August

print("August2012, cleaning...")
print(Sys.time())

S1_2012_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/8/S1_2012_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_8_25 <- select(S1_2012_8_25, c("username", "date", "retweets", "favorites", "text"))

S2_2012_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/8/S2_2012_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_8_25 <- select(S2_2012_8_25, c("username", "date", "retweets", "favorites", "text"))

S3_2012_8_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/8/S3_2012_8_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_8_25 <- select(S3_2012_8_25, c("username", "date", "retweets", "favorites", "text"))

DF_2012_8_25 <- rbind(S1_2012_8_25, S2_2012_8_25, S3_2012_8_25)

save(DF_2012_8_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_8_25.RData")

vector <- as.vector(DF_2012_8_25$text)

fun_txtcleaner(vector)

DF_2012_8_25_cleanREG <- clean_REG
DF_2012_8_25_cleanESP <- clean_ESP

save(DF_2012_8_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_8_25.RData")
save(DF_2012_8_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_8_25.RData")

rm(S1_2012_8_25, S2_2012_8_25, S3_2012_8_25, DF_2012_8_25, DF_2012_8_25_cleanREG, DF_2012_8_25_cleanESP, clean_ESP, clean_REG, vector)

print("August2012, ...done")
print(Sys.time())

#July

print("July2012, cleaning...")
print(Sys.time())

S1_2012_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/7/S1_2012_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_7_25 <- select(S1_2012_7_25, c("username", "date", "retweets", "favorites", "text"))

S2_2012_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/7/S2_2012_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_7_25 <- select(S2_2012_7_25, c("username", "date", "retweets", "favorites", "text"))

S3_2012_7_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/7/S3_2012_7_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_7_25 <- select(S3_2012_7_25, c("username", "date", "retweets", "favorites", "text"))

DF_2012_7_25 <- rbind(S1_2012_7_25, S2_2012_7_25, S3_2012_7_25)

save(DF_2012_7_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_7_25.RData")

vector <- as.vector(DF_2012_7_25$text)

fun_txtcleaner(vector)

DF_2012_7_25_cleanREG <- clean_REG
DF_2012_7_25_cleanESP <- clean_ESP

save(DF_2012_7_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_7_25.RData")
save(DF_2012_7_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_7_25.RData")

rm(S1_2012_7_25, S2_2012_7_25, S3_2012_7_25, DF_2012_7_25, DF_2012_7_25_cleanREG, DF_2012_7_25_cleanESP, clean_ESP, clean_REG, vector)

print("July2012, ...done")
print(Sys.time())

#June

print("June2012, cleaning...")
print(Sys.time())

S1_2012_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/6/S1_2012_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_6_25 <- select(S1_2012_6_25, c("username", "date", "retweets", "favorites", "text"))

S2_2012_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/6/S2_2012_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_6_25 <- select(S2_2012_6_25, c("username", "date", "retweets", "favorites", "text"))

S3_2012_6_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/6/S3_2012_6_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_6_25 <- select(S3_2012_6_25, c("username", "date", "retweets", "favorites", "text"))

DF_2012_6_25 <- rbind(S1_2012_6_25, S2_2012_6_25, S3_2012_6_25)

save(DF_2012_6_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_6_25.RData")

vector <- as.vector(DF_2012_6_25$text)

fun_txtcleaner(vector)

DF_2012_6_25_cleanREG <- clean_REG
DF_2012_6_25_cleanESP <- clean_ESP

save(DF_2012_6_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_6_25.RData")
save(DF_2012_6_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_6_25.RData")

rm(S1_2012_6_25, S2_2012_6_25, S3_2012_6_25, DF_2012_6_25, DF_2012_6_25_cleanREG, DF_2012_6_25_cleanESP, clean_ESP, clean_REG, vector)

print("June2012, ...done")
print(Sys.time())

#May

print("May2012, cleaning...")
print(Sys.time())

S1_2012_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/5/S1_2012_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_5_25 <- select(S1_2012_5_25, c("username", "date", "retweets", "favorites", "text"))

S2_2012_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/5/S2_2012_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_5_25 <- select(S2_2012_5_25, c("username", "date", "retweets", "favorites", "text"))

S3_2012_5_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/5/S3_2012_5_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_5_25 <- select(S3_2012_5_25, c("username", "date", "retweets", "favorites", "text"))

DF_2012_5_25 <- rbind(S1_2012_5_25, S2_2012_5_25, S3_2012_5_25)

save(DF_2012_5_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_5_25.RData")

vector <- as.vector(DF_2012_5_25$text)

fun_txtcleaner(vector)

DF_2012_5_25_cleanREG <- clean_REG
DF_2012_5_25_cleanESP <- clean_ESP

save(DF_2012_5_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_5_25.RData")
save(DF_2012_5_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_5_25.RData")

rm(S1_2012_5_25, S2_2012_5_25, S3_2012_5_25, DF_2012_5_25, DF_2012_5_25_cleanREG, DF_2012_5_25_cleanESP, clean_ESP, clean_REG, vector)

print("May2012, ...done")
print(Sys.time())

#April

print("April2012, cleaning...")
print(Sys.time())

S1_2012_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/4/S1_2012_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_4_25 <- select(S1_2012_4_25, c("username", "date", "retweets", "favorites", "text"))

S2_2012_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/4/S2_2012_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_4_25 <- select(S2_2012_4_25, c("username", "date", "retweets", "favorites", "text"))

S3_2012_4_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/4/S3_2012_4_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_4_25 <- select(S3_2012_4_25, c("username", "date", "retweets", "favorites", "text"))

DF_2012_4_25 <- rbind(S1_2012_4_25, S2_2012_4_25, S3_2012_4_25)

save(DF_2012_4_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_4_25.RData")

vector <- as.vector(DF_2012_4_25$text)

fun_txtcleaner(vector)

DF_2012_4_25_cleanREG <- clean_REG
DF_2012_4_25_cleanESP <- clean_ESP

save(DF_2012_4_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_4_25.RData")
save(DF_2012_4_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_4_25.RData")

rm(S1_2012_4_25, S2_2012_4_25, S3_2012_4_25, DF_2012_4_25, DF_2012_4_25_cleanREG, DF_2012_4_25_cleanESP, clean_ESP, clean_REG, vector)

print("April2012, ...done")
print(Sys.time())

#March

print("March2012, cleaning...")
print(Sys.time())

S1_2012_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/3/S1_2012_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_3_25 <- select(S1_2012_3_25, c("username", "date", "retweets", "favorites", "text"))

S2_2012_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/3/S2_2012_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_3_25 <- select(S2_2012_3_25, c("username", "date", "retweets", "favorites", "text"))

S3_2012_3_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/3/S3_2012_3_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_3_25 <- select(S3_2012_3_25, c("username", "date", "retweets", "favorites", "text"))

DF_2012_3_25 <- rbind(S1_2012_3_25, S2_2012_3_25, S3_2012_3_25)

save(DF_2012_3_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_3_25.RData")

vector <- as.vector(DF_2012_3_25$text)

fun_txtcleaner(vector)

DF_2012_3_25_cleanREG <- clean_REG
DF_2012_3_25_cleanESP <- clean_ESP

save(DF_2012_3_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_3_25.RData")
save(DF_2012_3_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_3_25.RData")

rm(S1_2012_3_25, S2_2012_3_25, S3_2012_3_25, DF_2012_3_25, DF_2012_3_25_cleanREG, DF_2012_3_25_cleanESP, clean_ESP, clean_REG, vector)

print("March2012, ...done")
print(Sys.time())

#February

print("February2012, cleaning...")
print(Sys.time())

S1_2012_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/2/S1_2012_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_2_25 <- select(S1_2012_2_25, c("username", "date", "retweets", "favorites", "text"))

S2_2012_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/2/S2_2012_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_2_25 <- select(S2_2012_2_25, c("username", "date", "retweets", "favorites", "text"))

S3_2012_2_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/2/S3_2012_2_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_2_25 <- select(S3_2012_2_25, c("username", "date", "retweets", "favorites", "text"))

DF_2012_2_25 <- rbind(S1_2012_2_25, S2_2012_2_25, S3_2012_2_25)

save(DF_2012_2_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_2_25.RData")

vector <- as.vector(DF_2012_2_25$text)

fun_txtcleaner(vector)

DF_2012_2_25_cleanREG <- clean_REG
DF_2012_2_25_cleanESP <- clean_ESP

save(DF_2012_2_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_2_25.RData")
save(DF_2012_2_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_2_25.RData")

rm(S1_2012_2_25, S2_2012_2_25, S3_2012_2_25, DF_2012_2_25, DF_2012_2_25_cleanREG, DF_2012_2_25_cleanESP, clean_ESP, clean_REG, vector)

print("February2012, ...done")
print(Sys.time())

#January

print("January2012, cleaning...")
print(Sys.time())

S1_2012_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/1/S1_2012_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S1_2012_1_25 <- select(S1_2012_1_25, c("username", "date", "retweets", "favorites", "text"))

S2_2012_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/1/S2_2012_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S2_2012_1_25 <- select(S2_2012_1_25, c("username", "date", "retweets", "favorites", "text"))

S3_2012_1_25 <- suppressMessages(suppressWarnings(read_delim("Datasets/Tweets/Series_25/2012/1/S3_2012_1_25.csv", 
                                                             ";", escape_double = FALSE, trim_ws = TRUE)))

S3_2012_1_25 <- select(S3_2012_1_25, c("username", "date", "retweets", "favorites", "text"))

DF_2012_1_25 <- rbind(S1_2012_1_25, S2_2012_1_25, S3_2012_1_25)

save(DF_2012_1_25, file = "Objects/Tweets/Series_25/Dirty/DF_2012_1_25.RData")

vector <- as.vector(DF_2012_1_25$text)

fun_txtcleaner(vector)

DF_2012_1_25_cleanREG <- clean_REG
DF_2012_1_25_cleanESP <- clean_ESP

save(DF_2012_1_25_cleanREG, file = "Objects/Tweets/Series_25/Clean/REG/DF_2012_1_25.RData")
save(DF_2012_1_25_cleanESP, file = "Objects/Tweets/Series_25/Clean/ESP/DF_2012_1_25.RData")

rm(S1_2012_1_25, S2_2012_1_25, S3_2012_1_25, DF_2012_1_25, DF_2012_1_25_cleanREG, DF_2012_1_25_cleanESP, clean_ESP, clean_REG, vector)

print("January2012, ...done")
print(Sys.time())
