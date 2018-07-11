


library("tidyverse")
library("ggplot2")
library("readxl")
library("xlsx")
library("ggpubr")

#Joining the results

#Series21L

load(file = "Objects/Backup/results21LF.RData")

plot(results21LF$date, results21LF$Score)

#Series22L

load(file = "Objects/Backup/results22LF.RData")

plot(results22LF$date, results22LF$Score)

#Series23L

load(file = "Objects/Backup/results23LF.RData")

plot(results23LF$date, results23LF$Score)

#Merging the datasets
#Unfirtered tweets, regional, pos/neg

DF_resultsLF <- rbind(results21LF, results22LF, results23LF)
DF_resultsLF <- DF_resultsLF[order(as.Date(DF_resultsLF$date, format="%Y/%m/%d")),]

plot1 <- ggplot(DF_resultsLF, aes(x=date, y=Score)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All tweets")

plot1

save(DF_resultsLF, file = "Objects/TimeSeries/DF_resultsLF.RData")
write.xlsx(DF_resultsLF, file="Datasets/Series/DF_resultsLF.xlsx")
