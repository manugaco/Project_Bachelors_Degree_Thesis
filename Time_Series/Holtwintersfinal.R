
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

DF_resultsF_U <- read_excel("~/Desktop/TFG/Datasets/Series/DF_resultsF_U.xlsx", 
                            sheet = "wm2")

ICC <- (DF_resultsF_U$ICC)
ISA <- (DF_resultsF_U$ISA)
IEE <- (DF_resultsF_U$IEE)
sent <- (DF_resultsF_U$sentimen_F*100)+100
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

acf(sent, lag.max=40) #this suggest seasonality of 12 periods

#Holt-Winters simple smooth forecast

senttsforecasts <- HoltWinters(sentts, alpha = 0.04838908, beta=FALSE, gamma=FALSE, l.start=98.4)

autoplot(senttsforecasts) +
  ggtitle("Holt-Winters Smoothing Forecast")

senttsforecasts$fitted

#Cross validation

library(dygraphs)
library(magrittr)

hw <- HoltWinters(window(sentts, 2012, c(2015,12)))        #subset timeseries to first 4 years (train data)
p <- predict(hw, n.ahead = 24, prediction.interval = TRUE)  #predict for the last 2 years (test data)
all <- cbind(sentts, p) #we add full ldeaths into graph to see test data (2016-2017) against the prediction

dygraph(all, "Sentiment Index") %>%
  dySeries("sentts", label = "Actual") %>%
  dySeries(c("p.lwr", "p.fit", "p.upr"), label = "Predicted")

hw$alpha

#Structural brake tests for ICC and Sentiment index

library(strucchange)
library(xlsx)
library(forecast)
library(tseries)
library(ggplot2)

#Consumer Index

ICCts

bp_ICCts <- breakpoints(ICCts ~ 1)

summary(bp_ICCts)
breakdates(bp_ICCts)

plot(bp_ICCts)
plot(ICCts)
lines(bp_ICCts)

ci_ICCts <- confint(bp_ICCts)
ci_ICCts
lines(ci_ICCts)

##Sentiment Index

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

#Structural change of the time series without filtering, try different h parameters

autoplot(sentts)

sentts_ts <- breakpoints(sentts ~ 1)
summary(sentts_ts)
breakdates(sentts_ts)


plot(sentts)
lines(sentts_ts)
plot(sentts_ts)

#No breackpoints detected

#Split non-filtered time serie before and after the structural change
##Split before make a ts object

sent1214 <- DF_resultsF_U[c(1:36),5]
sent1214 <- (sent1214*100)+100
sentts1214 <- ts(sent1214, frequency = 12, start = 2012/1/1) 
autoplot(sentts1214)

sts1214 <- HoltWinters(window(sentts1214, 2012, c(2014,12)))

sts1214$alpha #0.03794197

autoplot(sts1214)

sts1214F <- HoltWinters(sentts1214, alpha = 0.03794197, beta=FALSE, gamma=FALSE, l.start=98)

autoplot(sts1214F)

sent1517 <- DF_resultsF_U[c(36:72),5]
sent1517 <- (sent1517*100)+100

sentts1517 <- ts(sent1517, frequency = 12, start = 2015/1/1) 
autoplot(sentts1517)

sts1517 <- HoltWinters(window(sentts1517, 2015, c(2017,9)))

autoplot(sts1517)

sts1517$alpha #0.1839732

sts1517F <- HoltWinters(sentts1517, alpha = 0.1610238, beta=FALSE, gamma=FALSE, l.start=100.32054)

autoplot(sts1517F)

#Saving fitted values

mat1 <- data.frame(as.matrix(sts1214F$fitted))
mat2 <- data.frame(as.matrix(sts1517F$fitted))
sentNA <- c("", "")
mat3 <- rbind(sentNA, mat1,  sentNA, mat2)

sentNA <- c("", "")
sentmat <- rbind(sentNA, sentmat)

DF_HW <- cbind(DF_resultsF_U, sentmat$xhat, mat3$xhat)

colnames(DF_HW) <- c("date", "ICC", "ISA", "IEE", "Sent", "Filter1", "Filter2")

write.xlsx(DF_HW, file = "Datasets/Series/DF_HW5.xlsx")


#Future forecast

senttsforecasts2 <- forecast(senttsforecasts, h = 0.5)

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

write.xlsx(DF_HW, file = "Datasets/Series/DF_HW3.xlsx")

#Correlations

library("ggcorrplot")

#2012 - 2017

DF_maincorr <- data.frame(cbind(as.numeric(DF_HW$ICC), as.numeric(DF_HW$ISA), as.numeric(DF_HW$IEE), as.numeric(DF_HW$sentimen_F)
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



