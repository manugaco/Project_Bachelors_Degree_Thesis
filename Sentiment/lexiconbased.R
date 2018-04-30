
#merging and labeling the lexicon

#connect all libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(tidyverse)

#tweets evaluation function

score.sentiment <- function(sentences, valence, .progress='none') {
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, valence){
    sentence <- gsub('[[:punct:]]', '', sentence) #cleaning tweets
    sentence <- gsub('[[:cntrl:]]', '', sentence) #cleaning tweets
    sentence <- gsub('\\d+', '', sentence) #cleaning tweets
    sentence <- tolower(sentence) #cleaning tweets
    word.list <- str_split(sentence, '\\s+') #separating words
    words <- unlist(word.list)
    val.matches <- match(words, valence$Word) #find words from tweet in "Word" column of dictionary
    val.match <- valence$Rating[val.matches] #evaluating words which were found (suppose rating is in "Rating" column of dictionary).
    val.match <- na.omit(val.match)
    val.match <- as.numeric(val.match)
    score <- sum(val.match)/length(val.match) #rating of tweet (average value of evaluated words)
    return(score)
  }, valence, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences) #save results to the data frame
  return(scores.df)
}

valence1 <- read.csv('Datasets/Words/dictionary.csv', sep=',' , header=TRUE) #load dictionary from .csv file

valencewords <- as.data.frame(wordStem(valence$Word, language = "spanish"))

colnames(valencewords) <- c("Word")

valence <- data.frame(cbind(valencewords, valence1$ValenceMean))

colnames(valence) <- c("Word", "Rating")

save(valence, file = "Objects/Models/valence.RData")

Dataset <- TASScorpus_clean
Dataset$content <- as.factor(Dataset$content)
scores <- score.sentiment(Dataset$content, valence, .progress='text') #start score function

score2 <- scores

score2$score[scores$score < "5.5555"] <- "Neg"
score2$score[scores$score == "5.5555"] <- "Neu"
score2$score[scores$score > "5.5555"] <- "Pos"

#Testing the accuracy of this method

resultsLexicon <- table(observed = TASScorpus_clean$sentiment, predicted = score2$score)

resultsLexicon

accuracyLexicon <- (((results[1,1]+results[2,2])/43966)*100)

error <- (100-accuracy)

print(accuracyLexicon) #77,04%
print(error) #22,96%

save(resultsLexicon, file = "resultsLexicon.RData")
save(accuracyLexicon, file = "accuracyLexiconLexicon.RData")

#modify evaluation

stat <- scores
stat$created <- stack$created
stat$created <- as.Date(stat$created)
stat <- na.omit(stat) #delete unvalued tweets
write.csv(stat, file=paste(searchterm, '_opin_val.csv'), row.names=TRUE)

#chart

ggplot(stat, aes(created, score)) + geom_point(size=1) +
  stat_summary(fun.data = 'mean_cl_normal', mult = 1, geom = 'smooth') +
  ggtitle(searchterm)
ggsave(file=paste(searchterm, '_plot_val.jpeg'))



