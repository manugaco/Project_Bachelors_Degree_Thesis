
library("readxl")
library("readtext")
library("TTR")
library("forecast")
library("ggplot2")
library("ggfortify")
library("vars")
library("xlsx")
library("astsa")
library("stats")

#Time series object and plot

DF_resultsF_U <- read_excel("~/Desktop/TFG/Datasets/Series/DF_results.xlsx", 
                            sheet = "Sent")

ICC <- (DF_resultsF_U$ICC)
ISA <- (DF_resultsF_U$ISA)
IEE <- (DF_resultsF_U$IEE)
sent <- (DF_resultsF_U$Sentiment*100)+100
sentch <- as.character(sent)

plot(sent)

ICCts <- ts(ICC, frequency = 12, start = 2012/1/1)
ISAts <- ts(ISA, frequency = 12, start = 2012/1/1)
IEEts <- ts(IEE, frequency = 12, start = 2012/1/1)
sentts <- ts(sent, frequency = 12, start = 2012/1/1) 

autoplot(ICCts)

autoplot(sentts)

sentdec <- decompose(sentts)

autoplot(sentdec) +
  ggtitle("Additive model decomposition")

#Correlogram

x11()
acf(sent, lag.max=40) #this suggest seasonality of 12 periods

#Holt-Winters simple smooth forecast

senttsforecasts <- HoltWinters(sentts, alpha = 0.01444122, beta=FALSE, gamma=FALSE, l.start=99.45)

autoplot(senttsforecasts) +
  ggtitle("Holt-Winters Smoothing Forecast")

senttsforecasts$fitted

#split

library(dygraphs)
library(magrittr)

hw <- HoltWinters(window(sentts, 2012, c(2015,12)))        #subset timeseries to first 4 years (train data)
p <- predict(hw, n.ahead = 24, prediction.interval = TRUE)  #predict for the last 2 years (test data)
all <- cbind(sentts, p) #we add full ldeaths into graph to see test data (2016-2017) against the prediction

dygraph(all, "Sentiment Index") %>%
  dySeries("sentts", label = "Actual") %>%
  dySeries(c("p.lwr", "p.fit", "p.upr"), label = "Predicted")

hw$alpha

#Structural brake tests

##Strucchange

library(strucchange)
library(xlsx)
library(forecast)
library(tseries)
library(ggplot2)

ts <- senttsforecasts$fitted

bp_ts <- breakpoints(ts ~ 1)

summary(bp_ts)
breakdates(bp_ts)

plot(bp_ts)
plot(ts)
lines(bp_ts)

ci_ts <- confint(bp_ts)
ci_ts
lines(ci_ts)

##Fstats (Chow's test F Statistics)

fs.ts <- strucchange:::Fstats(senttsforecasts$x ~ 1)
plot(fs.ts)
strucchange:::sctest(fs.ts)

plot(ts)
lines(strucchange:::breakpoints(fs.ts))

#Future forecast

senttsforecasts2 <- forecast(senttsforecasts, h=2)

accuracy(senttsforecasts2)

autoplot(senttsforecasts2) +
  ggtitle("Holt-Winters Smoothing Forecast")

x11()
plot(senttsforecasts2)

x11()
acf(na.omit(senttsforecasts2$residuals), lag.max=20)

Box.test(senttsforecasts2$residuals, lag=20, type="Ljung-Box")

x11()
autoplot(senttsforecasts2$residuals)

plotForecastErrors <- function(forecasterrors){
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

x11()
forecasterrors <- plotForecastErrors(na.omit(senttsforecasts2$residuals))

#Saving fitted values

sentmat <- data.frame(as.matrix(senttsforecasts$fitted))

sentNA <- c("", "")
sentmat <- rbind(sentNA, sentmat)

DF_HW <- cbind(DF_resultsF_U, sentmat$xhat)

colnames(DF_HW) <- c("date", "ICC", "ISA", "IEE", "Sent", "holtWinters")

write.xlsx(DF_HW, file = "Datasets/Series/DF_HW2.xlsx")

#Correlations

library("ggcorrplot")

#2012 - 2017

DF_maincorr <- data.frame(cbind(as.numeric(DF_HW$ICC), as.numeric(DF_HW$ISA), as.numeric(DF_HW$IEE), as.numeric(DF_HW$Sentimen)
                                , as.numeric(na.omit(sentmat$xhat))))
colnames(DF_maincorr) <- c("ICC", "ISA", "IEE", "Sent", "HoltWinters")

corr1217 <- cor(DF_maincorr, use = "complete.obs")

corplot1217 <- ggcorrplot(corr1217, hc.order = TRUE,
                      insig = "blank", lab = TRUE,
                      outline.col = "white",
                      colors = c("#6D9EC1", "white", "#E46726"))

corplot1217

plot1217 <- ggplot(DF_HW, aes(x = date)) + 
  geom_line(aes(y = ICC), colour = "blue") +
  geom_line(aes(y = holtWinters), colour = "red")

plot1217

#2012 - 2014

EPL2011_12FirstHalf <- subset(EPL2011_12, Date2 > as.Date("2012-01-13") )



