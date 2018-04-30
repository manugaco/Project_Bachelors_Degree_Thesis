

#Clean text funtion: clean with regular expressions, language filtering and stemming

vector <- as.vector(sample(DF_2017_12_7$text, 10000)) #Input has to be a vector and ourput will be a dataframe

fun_txtcleaner <- function(x){
  
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
  
  dataset <- x
  
  text <- select(dataset, "text")
  
  vec <- as.vector(text)
  
  #Regular Expressions
  
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
  
  #Language filtering with two libraries (textcat and cldr)
  
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
  #Regional languages (Spanish, Catalan and Galician)
  
  df_all <- df[(df$text_cat == "spanish") | (df$cldr == "spanish") |
                           (df$text_cat == "catalan") | (df$cldr == "catalan") |
                           (df$text_cat == "galician") | (df$cldr == "galician"), ]
  
  df_all <- as.data.frame(df_all[!duplicated(df_all$text), ])
  
  df_all <- data.frame(lapply(df_all, as.character), stringsAsFactors=FALSE)
  
  df_all <- as.data.frame(stemDocument(df_all$text, language="spanish"))
  
  colnames(df_all) <- c("text")
  
  clean_REG <<- select(df_all, c("text"))
  
  #Only Spanish tweets
  
  df_ESP <- df[(df$text_cat == "spanish") | (df$cldr == "spanish"), ]
  
  #Deleting repeated tweets
  
  df_ESP <- as.data.frame(df_ESP[!duplicated(df_ESP$text), ])
  
  df_ESP <- data.frame(lapply(df_ESP, as.character), stringsAsFactors=FALSE)
  
  df_ESP <- as.data.frame(stemDocument(df_ESP$text, language="spanish"))
  
  colnames(df_ESP) <- c("text")
  
  clean_ESP <<- select(df_ESP, c("text"))
} #works

fun_txtcleaner(vector) #PROBAR ESTO
