


library("tidyverse")
library("ggplot2")
library("readxl")
library("xlsx")
library("ggpubr")

#Joining the results

#Series21L2

load(file = "Objects/Models/results21L2F.RData")

plot(results21L2F$date, results21L2F$Score)

#Series22L2

load(file = "Objects/Models/results22L2F.RData")

plot(results22L2F$date, results22L2F$Score)

#Series23L2

load(file = "Objects/Models/results23L2F.RData")

plot(results23L2F$date, results23L2F$Score)

#Merging the datasets
#Unfirtered tweets, regional, pos/neg

DF_resultsL2F <- rbind(results21L2F, results22L2F, results23L2F)
DF_resultsL2F <- DF_resultsL2F[rev(order(as.Date(DF_resultsL2F$date, format="%Y/%m/%d"))),]

plot1 <- ggplot(DF_resultsL2F, aes(x=date, y=Score)) +
  geom_point() +
  geom_smooth() +
  ggtitle("All tweets")

plot1

save(DF_resultsL2F, file = "Objects/TimeSeries/DF_resultsL2F.RData")
write.xlsx(DF_resultsL2F, file="Datasets/Series/DF_resultsL2F.xlsx")
