
library("tidyverse")
library("ggplot2")
library("readxl")
library("xlsx")
library("ggpubr")

#results/dataset conditioning

dataset21 <- read_excel("Datasets/SSMI.xlsx")
load(file = "Objects/Models/results21.RData")

DF_results <- cbind(dataset21, results21)

colnames(DF_results) <- c("date", "total", "positive",  "negative", "sentiment")

DF_results <- dplyr::select(DF_results, date, positive, negative, total)

DF_results <- DF_results  %>%
  mutate(((positive/total)*100)+100)

DF_results$`((positive/total) * 100) + 100` <- round(DF_results$`((positive/total) * 100) + 100`, digits = 3)

DF_results <- DF_results  %>%
  mutate(((negative/total)*100)+100)

DF_results$`((negative/total) * 100) + 100` <- round(DF_results$`((negative/total) * 100) + 100`, digits = 3)

DF_results <- DF_results  %>%
  mutate((((positive/total)*100)-((negative/total)*100))+100)

DF_results$`(((positive/total) * 100) - ((negative/total) * 100)) + 100` <- round(DF_results$`(((positive/total) * 100) - ((negative/total) * 100)) + 100`, digits = 3)

colnames(DF_results) <- c("date", "positive", "negative", "total", "%pos", "%neg", "sentiment")

write.xlsx(DF_results, file="Datasets/Series/Test21P.xlsx")

#results/dataset conditioning (economic content filtered and neutral range added)

dataset21 <- read_excel("Datasets/SSMI.xlsx")
load(file = "Objects/Models/results21f.RData")

DF_results <- cbind(dataset21, results21f)

colnames(DF_results) <- c("date", "total", "positive",  "negative", "sentiment")

DF_results <- dplyr::select(DF_results, date, positive, negative, total)

DF_results <- DF_results  %>%
  mutate(((positive/total)*100)+100)

DF_results$`((positive/total) * 100) + 100` <- round(DF_results$`((positive/total) * 100) + 100`, digits = 3)

DF_results <- DF_results  %>%
  mutate(((negative/total)*100)+100)

DF_results$`((negative/total) * 100) + 100` <- round(DF_results$`((negative/total) * 100) + 100`, digits = 3)

DF_results <- DF_results  %>%
  mutate((((positive/total)*100)-((negative/total)*100))+100)

DF_results$`(((positive/total) * 100) - ((negative/total) * 100)) + 100` <- round(DF_results$`(((positive/total) * 100) - ((negative/total) * 100)) + 100`, digits = 3)

colnames(DF_results) <- c("date", "positive", "negative", "total", "%pos", "%neg", "sentiment")

write.xlsx(DF_results, file="Datasets/Series/Test21FP.xlsx")

#results/dataset conditioning (economic content filtered and neutral range added)

dataset21 <- read_excel("Datasets/SSMI.xlsx")
load(file = "Objects/Models/results21_RFDEF.RData")

DF_results <- cbind(dataset21, results21f)

colnames(DF_results) <- c("date", "total", "positive", "neutral",  "negative", "sentiment")

DF_results <- dplyr::select(DF_results, date, positive, negative, total)

DF_results <- DF_results  %>%
  mutate(((positive/total)*100)+100)

DF_results$`((positive/total) * 100) + 100` <- round(DF_results$`((positive/total) * 100) + 100`, digits = 3)

DF_results <- DF_results  %>%
  mutate(((negative/total)*100)+100)

DF_results$`((negative/total) * 100) + 100` <- round(DF_results$`((negative/total) * 100) + 100`, digits = 3)

DF_results <- DF_results  %>%
  mutate((((positive/total)*100)-((negative/total)*100))+100)

DF_results$`(((positive/total) * 100) - ((negative/total) * 100)) + 100` <- round(DF_results$`(((positive/total) * 100) - ((negative/total) * 100)) + 100`, digits = 3)

colnames(DF_results) <- c("date", "positive", "negative", "total", "%pos", "%neg", "sentiment")

write.xlsx(DF_results, file="Datasets/Series/Test21FNP.xlsx")

#plotting all words series

alltweetschart <- ggplot(DF_results, aes(x=date, y=sentiment))+
  geom_line(colour = "red") +
  labs(x = "Time Series 2012 - 2017") +
  labs(y = "Sentiment") +
  labs(caption = "(21th day only series)") +
  geom_smooth(colour = "blue") +
  theme_bw() +
  ggtitle("Spanish Social Media Index, all tweets")

alltweetschart

#plotting the CIS official statistics series

ICCtweetschart <- ggplot(icc_12_17, aes(x=Date, y=ICC))+
  geom_line(colour = "red") +
  labs(x = "Time Series 2012 - 2017") +
  labs(y = "Consumption Confidence") +
  labs(caption = "(CIS official statistic)") +
  geom_smooth(colour = "blue") +
  theme_bw() +
  ggtitle("Consumtion Confidence, reference index")

alltweetschart
ecotweetschart
ICCtweetschart

#Compute descriptive statistics in a table

library("pastecs")

icc_12_17n <- icc_12_17
icc_12_17_n <- icc_12_17n[order(icc_12_17n$Date , decreasing = TRUE ),]

DF_mainstat <- as.data.frame(cbind(dataset21f, icc_12_17_n$ICC, DF_results$sentiment, DF_resultsf$sentimentf))
colnames(DF_mainstat) <- c("date", "ICC", "all", "eco")

stableICC <- stat.desc(data.frame(DF_mainstat$ICC))
stableall <- stat.desc(data.frame(DF_mainstat$all))
stableeco <- stat.desc(data.frame(DF_mainstat$eco))

stable <- cbind(stableICC, stableall, stableeco)
colnames(stable) <- c("ICC", "all", "eco")

stable <- round(stable, digits = 3)

stable <- stable[c("min", "max", "range", "median", "mean", "var", "std.dev", "coef.var"),]

#Statistics summary table plot

stable.p <- ggtexttable(stable, rows = row.names(stable), 
                        theme = ttheme("mBlue"))
stable.p

togethercharts <- ggarrange(alltweetschart, ecotweetschart, ICCtweetschart, stable.p,  
                            labels = c("A", "B", "C", "D Main statistics"),
                            ncol = 2, nrow = 2)

togethercharts

write.xlsx(DF_results, file = "Datasets/Series/21.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

#Correlation matrix

library("ggcorrplot")

DF_maincorr <- as.data.frame(cbind(icc_12_17_n$ICC, DF_results$sentiment,DF_resultsf$sentimentf))
colnames(DF_maincorr) <- c("ICC","sentiment","sentimentf")

corr <- round(cor(DF_maincorr), 3)
head(corr[, 1:3])

#computing p-values

p.mat <- cor_pmat(DF_maincorr)
head(p.mat[, 1:3])

#plotting the correlation matrix

corplot <- ggcorrplot(corr, method = "circle", hc.order = TRUE, type = "lower",
           insig = "blank",
           outline.col = "white",
           colors = c("#6D9EC1", "white", "#E46726"))

corplot

DF_new <- as.data.frame(cbind(dataset21, icc_12_17_n$ICC, DF_results$sentiment, DF_resultsf$sentimentf))
colnames(DF_new) <- c("date", "ICC", "sentiment", "sentimentf")

plot1 <- ggplot(DF_new, aes(date, ICC)) +
  geom_line(colour = "red") +
  geom_smooth(method = "loess") +
  theme_bw() +
  ggtitle("Consumer Confidence Index")

plot2 <- ggplot(DF_new, aes(date, sentiment)) +
  geom_line(colour = "red") +
  geom_smooth(method = "loess") +
  theme_bw() +
  ggtitle("Sentiment Index, all tweets")

plot3 <- ggplot(DF_new, aes(date, sentimentf)) +
  geom_line(colour = "red") +
  geom_smooth(method = "loess") +
  theme_bw() +
  ggtitle("Sentiment Index, filtered tweets")

finalchart <- ggarrange(plot1, plot2, plot3,
                            labels = c("A", "B", "C", "D"),
                            ncol = 2, nrow = 2)
finalchart

save(DF_new, file = "Objects/TimeSeries/Results2.RData")
