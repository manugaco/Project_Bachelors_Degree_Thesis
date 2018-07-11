

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

#Analisis de frecuencia de palabras

tdm <- TermDocumentMatrix(myCorpusCopy, control = list(wordLengths = c(1, Inf)))
inspect(tdm[10:25,2:12])


#Words and its frequency inside a DF

freq.terms <- findFreqTerms(tdm, lowfreq = 5)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 5)
df <- data.frame(term = names(term.freq), freq = term.freq)


#Histogram

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Términos") + ylab("Cantidad") + coord_flip()

#Wordcloud

m <- as.matrix(tdm)

  #Sort by freq

word.freq <- sort(rowSums(m), decreasing = T)

  #Color palette

pal <- brewer.pal(9, "BuGn")[-(1:4)]

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 7,random.order = F, colors = pal)

#grouping words with clustering

# delete low usage terms (sparse)
tdm2 <- removeSparseTerms(tdm, sparse = 0.98) 
m2 <- as.matrix(tdm2)

# clusters

distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")

plot(fit)
rect.hclust(fit, k = 4) 

#Kmeans grouping

#traspose the matrix

m3 <- t(m2)
set.seed(122) 
fviz_nbclust(m3, kmeans, method = "wss")

#clúster numbers
k <- 8 
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
}
  
  #Command to show all twitts inside a cluster
  #print(tweets[which(kmeansResult$cluster==i)])

#Group and find associations

findAssocs(tdm, "opinion", 0.2)
findAssocs(tdm, "economia", 0.2)

#Terms net

plot(tdm, term = freq.terms, corThreshold = 0.4, weighting = T)
plot(tdm, term = freq.terms, corThreshold = 0.3, weighting = T)

#Topic analysis

dtm <- as.DocumentTermMatrix(tdm)

lda <- LDA(dtm, k = 8)
term <- terms(lda, 7)
term

topics <- topics(lda) 
topics <- data.frame(date=as.Date(tweets.df$created), topic=topics)
ggplot(topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")

# Sentiment plot

sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.Date(tweets.df$created)
result <- aggregate(score ~ date, data = sentiments, sum)
ggplot(data=result, aes(x=date,y=score)) + geom_line()


