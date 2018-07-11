

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

Test4 <- Pruebas5

stableICC <- stat.desc(data.frame(Test4$ICC))
stableISA <- stat.desc(data.frame(Test4$ISA))
stableIEE <- stat.desc(data.frame(Test4$IEE))

stableICC1 <- stat.desc(data.frame(Test4$`ICC (t-1)`))
stableISA1 <- stat.desc(data.frame(Test4$`ISA (t-1)`))
stableIEE1 <- stat.desc(data.frame(Test4$`IEE (t-1)`))

stableICC2 <- stat.desc(data.frame(Test4$`ICC (t-2)`))
stableISA2 <- stat.desc(data.frame(Test4$`ISA (t-2)`))
stableIEE2 <- stat.desc(data.frame(Test4$`IEE (t-2)`))

stableall <- stat.desc(data.frame(Test4$sentiment))
stableeco <- stat.desc(data.frame(Test4$sentimentf))

stable0 <- cbind(stableall, stableeco, stableICC, stableISA, stableIEE, stableICC1, stableISA1, 
                 stableIEE1, stableICC2, stableISA2, stableIEE2)

colnames(stable0) <- c("SSI", "SSIF", "ICC", "ISA", "IEE", "ICC1", "ISA1", "IEE1", "ICC2", "ISA2", "IEE2")

stable0 <- round(stable0, digits = 3)

stable0 <- stable0[c("min", "max", "range", "median", "mean", "var", "std.dev", "coef.var"),]

#Statistics summary table plot

stable.p0 <- ggtexttable(stable0, rows = row.names(stable0), 
                        theme = ttheme("mBlue"))
stable.p0

stable.p1 <- ggtexttable(stable1, rows = row.names(stable1), 
                         theme = ttheme("mBlue"))
stable.p1

stable.p2 <- ggtexttable(stable2, rows = row.names(stable2), 
                         theme = ttheme("mBlue"))
stable.p2

#Correlation matrix

DF_maincorr <- as.data.frame(cbind(Test4$sentiment, Test4$sentimentf, Test4$ICC, Test4$ISA,
                                   Test4$IEE, Test4$`ICC (t-1)`, Test4$`ISA (t-1)`, Test4$`IEE (t-1)`,
                                   Test4$`ICC (t-2)`, Test4$`ISA (t-2)`, Test4$`IEE (t-2)`))

colnames(DF_maincorr) <- c("sentiment","sentimentf", "ICC", "ISA", "IEE", "ICC(t-1)", "ISA(t-1)",
                           "IEE(t-1)", "ICC(t-2)", "ISA(t-2)", "IEE(t-2)")

corr <- round(cor(DF_maincorr), 3)
head(corr[, 1:3])

cortab <- ggtexttable(corr, theme = ttheme("mBlue"))
cortab <- table_cell_font(cortab, row = 3, column = 6,
                       face = "bold")
cortab <- table_cell_bg(cortab, row = 3, column = 6, linewidth = 5,
                     fill="darkolivegreen1", color = "darkolivegreen4")

cortab <- table_cell_font(cortab, row = 6, column = 3,
                          face = "bold")
cortab <- table_cell_bg(cortab, row = 6, column = 3, linewidth = 5,
                        fill="darkolivegreen1", color = "darkolivegreen4")

cortab <- table_cell_font(cortab, row = 3, column = 4,
                          face = "bold")

cortab <- table_cell_font(cortab, row = 4, column = 3,
                          face = "bold")

cortab

#computing p-values

p.mat <- cor_pmat(DF_maincorr)
head(p.mat[, 1:3])

#plotting the correlation matrix

corplot <- ggcorrplot(corr, hc.order = TRUE, insig = "blank", outline.col = "white", lab = TRUE,
                    colors = c("#6D9EC1", "white", "#E46726"))

corplot

#Charts

Test41 <- dplyr::select(Test4, date, sentimentf, ICC, ISA, IEE)

melted1 <- melt(Test41, id.vars="date")

Test42 <- dplyr::select(Test4, date, sentimentf, `ICC (t-1)`, `ISA (t-1)`, `IEE (t-1)`)

melted2 <- melt(Test42, id.vars="date")

Test43 <- dplyr::select(Test4, date, sentimentf, `ICC (t-2)`, `ISA (t-2)`, `IEE (t-2)`)

melted3 <- melt(Test43, id.vars="date")

#plotting all words series

chart1 <- ggplot(data = melted1, aes(x = date, y = value, color = variable)) +
  geom_line() +
  xlab("Time Series 2015 - 2017 (t)") +
  ylab("Index") +
  theme(legend.position="top")

chart1

chart2 <- ggplot(data = melted2, aes(x = date, y = value, color = variable)) +
  geom_line() +
  xlab("Time Series 2015 - 2017 (t-1)") +
  ylab("Index") +
  theme(legend.position="top")

chart2

chart3 <- ggplot(data = melted3, aes(x = date, y = value, color = variable)) +
  geom_line() +
  xlab("Time Series 2015 - 2017 (t-2)") +
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

finaltabs <- ggarrange(cortab, stable.p0,
                        labels = c("A. Correlation Matrix", "B. Main Statistics"),
                        hjust = 0, vjust = 1.5,
                        ncol = 1, nrow = 2)
finaltabs

