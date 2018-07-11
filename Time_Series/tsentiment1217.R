

#Time series analysis and correlation
#Main statistics

#Compute descriptive statistics in a table

library("tidyverse")
library("pastecs")
library("ggcorrplot")
library('reshape2')
library('ggplot2')
library("readxl")
library("xlsx")
library("ggpubr")

Test3 <- testDF

stableICC <- stat.desc(data.frame(Test3$ICC))
stableISA <- stat.desc(data.frame(Test3$ISA))
stableIEE <- stat.desc(data.frame(Test3$IEE))

stableICC1 <- stat.desc(data.frame(Test3$`ICC (t-1)`))
stableISA1 <- stat.desc(data.frame(Test3$`ISA (t-1)`))
stableIEE1 <- stat.desc(data.frame(Test3$`IEE (t-1)`))

stableICC2 <- stat.desc(data.frame(Test3$`ICC (t-2)`))
stableISA2 <- stat.desc(data.frame(Test3$`ISA (t-2)`))
stableIEE2 <- stat.desc(data.frame(Test3$`IEE (t-2)`))

stableall1 <- stat.desc(data.frame(Test3$sentiment))
stableall2 <- stat.desc(data.frame(Test3$`sentiment(t-1)`))
stableall3 <- stat.desc(data.frame(Test3$`sentiment(t-2)`))

stableeco1 <- stat.desc(data.frame(Test3$sentimentf))
stableeco2 <- stat.desc(data.frame(Test3$`sentimentf(t-1)`))
stableeco3 <- stat.desc(data.frame(Test3$`sentimentf(t-2)`))

stablen1 <- stat.desc(data.frame(Test3$sentimentN))
stablen2 <- stat.desc(data.frame(Test3$`sentimentN(t-1)`))
stablen3 <- stat.desc(data.frame(Test3$`sentimentN(t-2)`))

stable0 <- cbind(stableall1, stableall2, stableall3, stableeco1, stableeco2, stableeco3, 
                 stablen1, stablen2, stablen3, stableICC, stableISA, stableIEE, stableICC1, stableISA1, 
                 stableIEE1, stableICC2, stableISA2, stableIEE2)

colnames(stable0) <- c("SSI", "SSI1", "SSI2", "SSIF", "SSIF1", "SSIF2", "SSIN", 
                       "SSIN1", "SSIN2", "ICC", "ISA", "IEE", "ICC1", "ISA1", "IEE1", "ICC2", "ISA2", "IEE2")

stable0 <- round(stable0, digits = 2)

stable0 <- stable0[c("min", "max", "range", "median", "mean", "var", "std.dev", "coef.var"),]

#Statistics summary table plot

stable.p0 <- ggtexttable(stable0, rows = row.names(stable0), 
                        theme = ttheme("mBlue"))
stable.p0

#Correlation matrix

DF_maincorr <- as.data.frame(cbind(Test3$sentiment, Test3$`sentiment(t-1)`, Test3$`sentiment(t-2)`, 
                                   Test3$sentimentf, Test3$`sentimentf(t-1)`, Test3$`sentimentf(t-2)`,
                                   Test3$sentimentN, Test3$`sentimentN(t-1)`, Test3$`sentimentN(t-2)`,
                                   Test3$ICC, Test3$ISA, Test3$IEE, Test3$`ICC (t-1)`, Test3$`ISA (t-1)`, Test3$`IEE (t-1)`,
                                   Test3$`ICC (t-2)`, Test3$`ISA (t-2)`, Test3$`IEE (t-2)`))

colnames(DF_maincorr) <- c("SSI", "SSI1", "SSI2", "SSIF", "SSIF1", "SSIF2", "SSIN", 
                           "SSIN1", "SSIN2", "ICC", "ISA", "IEE", "ICC1", "ISA1", "IEE1", "ICC2", "ISA2", "IEE2")

corr <- round(cor(DF_maincorr), 2)
head(corr[, 1:3])

cortab <- ggtexttable(corr, theme = ttheme("mBlue"))

cortab

#computing p-values

p.mat <- round(cor_pmat(DF_maincorr), 2)
head(p.mat[, 1:3])

pval <- ggtexttable(p.mat, theme = ttheme("mBlue"))

pval

#plotting the correlation matrix

corplot <- ggcorrplot(corr, hc.order = TRUE, insig = "blank", outline.col = "white", lab = TRUE,
                    colors = c("#6D9EC1", "white", "#E46726"))

corplot

#Charts

Test31 <- dplyr::select(Test3, date, `sentimentf(t-2)`, ICC, ISA, IEE)

melted1 <- melt(Test31, id.vars="date")

Test32 <- dplyr::select(Test3, date, sentiment, `ICC (t-1)`, `ISA (t-1)`, `IEE (t-1)`)

melted2 <- melt(Test32, id.vars="date")

Test33 <- dplyr::select(Test3, date, sentimentf, `ICC (t-2)`, `ISA (t-2)`, `IEE (t-2)`)

melted3 <- melt(Test33, id.vars="date")

#plotting all words series

chart1 <- ggplot(data = melted1, aes(x = date, y = value, color = variable)) +
  geom_line() +
  xlab("Time Series 2012 - 2017 (t)") +
  ylab("Index") +
  theme(legend.position="top")

chart1

chart2 <- ggplot(data = melted2, aes(x = date, y = value, color = variable)) +
  geom_line() +
  xlab("Time Series 2012 - 2017 (t-1)") +
  ylab("Index") +
  theme(legend.position="top")

chart2

chart3 <- ggplot(data = melted3, aes(x = date, y = value, color = variable)) +
  geom_line() +
  xlab("Time Series 2012 - 2017 (t-2)") +
  ylab("Index") +
  theme(legend.position="top")

chart3

#combine all interesting plots

finalchart <- ggarrange(chart1, chart2, chart3,
                        labels = c("A. Sentiment Series Filtered compared with ICC, ISA and IEE",
                                   "B. Sentiment Series Filtered compared with ICC, ISA and IEE in t-1",
                                   "C. Sentiment Series Filtered compared with ICC, ISA and IEE in t-2"),
                        hjust = 0, vjust = 1.25,
                        font.label = list(size = 10, face = "bold"),
                        ncol = 2, nrow = 2)

finalchart

#combine statistics tables

finaltabs <- ggarrange(cortab, pval, stable.p0,
                       labels = c("A. Correlation Matrix", "B. p-values", "C. Main Statistics"),
                       hjust = 0, vjust = 1.5,
                       ncol = 1, nrow = 3)
finaltabs

