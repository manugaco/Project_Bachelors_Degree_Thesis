


library("tidyverse")
library("ggplot2")
library("readxl")
library("xlsx")
library("ggpubr")

#Joining the results

#Series21

Time21 <- read_excel("Datasets/Series/Time21.xlsx")
load(file = "Objects/Models/results21.RData")

DF_results21 <- cbind(Time21, results21)

plot(DF_results21$date, DF_results21$Score)

load(file = "Objects/Models/results21F.RData")
results21F <- results21f

DF_results21F <- cbind(Time21, results21F)

plot(DF_results21F$date, DF_results21F$Score)

load(file = "Objects/Models/results21FN.RData")
results21FN <- results21f

DF_results21FN <- cbind(Time21, results21FN)

plot(DF_results21FN$date, DF_results21FN$Score)

#Series22

Time22 <- read_excel("Datasets/Series/Time22.xlsx")
load(file = "Objects/Models/results22.RData")

DF_results22 <- cbind(Time22, results22)

plot(DF_results22$date, DF_results22$Score)

load(file = "Objects/Models/results22F.RData")

DF_results22F <- cbind(Time22, results22F)

plot(DF_results22F$date, DF_results22F$Score)

load(file = "Objects/Models/results22FN.RData")

DF_results22FN <- cbind(Time22, results22FN)

plot(DF_results22FN$date, DF_results22FN$Score)

#Series23

Time23 <- read_excel("Datasets/Series/Time23.xlsx")
load(file = "Objects/Models/results23.RData")

DF_results23 <- cbind(Time23, results23)

plot(DF_results23$date, DF_results23$Score)

load(file = "Objects/Models/results23F.RData")

DF_results23F <- cbind(Time23, results23F)

plot(DF_results23F$date, DF_results23F$Score)

load(file = "Objects/Models/results23FN.RData")

DF_results23FN <- cbind(Time23, results23FN)

plot(DF_results23FN$date, DF_results23FN$Score)

#Merging the datasets
#Unfirtered tweets, regional, pos/neg

DF_results <- rbind(DF_results21, DF_results22, DF_results23)
DF_results <- DF_results[order(as.Date(DF_results$date, format="%Y/%m/%d")),]

plot1 <- ggplot(DF_results, aes(x=date, y=Score)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All tweets")

save(DF_results, file = "Objects/TimeSeries/DF_results.RData")
write.xlsx(DF_results, file="Datasets/Series/DF_results.xlsx")

#Filtered tweets, regional, pos/neg

DF_resultsF <- rbind(DF_results21F, DF_results22F, DF_results23F)
DF_resultsF <- DF_resultsF[order(as.Date(DF_resultsF$date, format="%Y/%m/%d")),]

plot2 <- ggplot(DF_resultsF, aes(x=date, y=Score)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Economic content filter")

save(DF_resultsF, file = "Objects/TimeSeries/DF_resultsF.RData")
write.xlsx(DF_resultsF, file="Datasets/Series/DF_resultsF.xlsx")

#Filtered tweets, regional, pos/neu/neg

DF_resultsFN <- rbind(DF_results21FN, DF_results22FN, DF_results23FN)
DF_resultsFN <- DF_resultsFN[order(as.Date(DF_resultsFN$date, format="%Y/%m/%d")),]

plot3 <- ggplot(DF_resultsFN, aes(x=date, y=Score)) +
  geom_point() +
  geom_smooth()  +
  ggtitle("Economic content and neutral category filter")

save(DF_resultsFN, file = "Objects/TimeSeries/DF_resultsFN.RData")
write.xlsx(DF_resultsFN, file="Datasets/Series/DF_resultsFN.xlsx")

finalchart <- ggarrange(plot1, plot2, plot3,
                        labels = c("A.", "B.", "C."),
                        ncol = 2, nrow = 2)
finalchart

#Positive, negative and total classification levels

plot4 <- ggplot(DF_results, aes(x=date, y=poss)) +
  geom_line() +
  geom_line(aes(x=date, y=negs), colour="red") +
  geom_line(aes(x=date, y=tots), colour="blue")
plot4

plot5 <- ggplot(DF_resultsF, aes(x=date, y=poss)) +
  geom_line() +
  geom_line(aes(x=date, y=negs), colour="red") +
  geom_line(aes(x=date, y=tots), colour="blue")
plot5

plot6 <- ggplot(DF_resultsFN, aes(x=date, y=poss)) +
  geom_line() +
  geom_line(aes(x=date, y=negs), colour="red") +
  geom_line(aes(x=date, y=tots), colour="blue")
plot6

