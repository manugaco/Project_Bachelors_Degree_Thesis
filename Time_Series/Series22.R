
library("tidyverse")
library("ggplot2")
library("readxl")
library("xlsx")
library("ggpubr")

#results/dataset conditioning

dataset22 <- read_excel("Datasets/Series/SSMI.xlsx")
load(file = "Objects/Models/resultsF22.RData")

DF_results <- cbind(dataset22, results22)

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

write.xlsx(DF_results, file="Datasets/Series/DF_results22.xlsx")
