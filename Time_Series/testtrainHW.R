

#Holt winters split test/train

library(dygraphs)
library(magrittr)

hw <- HoltWinters(window(sentts, 2012, c(2015,12)))        #subset timeseries to first 4 years (train data)
p <- predict(hw, n.ahead = 24, prediction.interval = TRUE)  #predict for the last 2 years (test data)
all <- cbind(sentts, p) #we add full ldeaths into graph to see test data (2016-2017) against the prediction

dygraph(all, "Sentiment Index") %>%
  dySeries("sentts", label = "Actual") %>%
  dySeries(c("p.lwr", "p.fit", "p.upr"), label = "Predicted")



#several algorithm prediction

sent2 <- window(sentts,start=2012,end=c(2015,12))
meanf <- meanf(sent2,h=24)
rwf <- rwf(sent2,h=24)
snaive <- snaive(sent2,h=24)
hw <- hw(sent2,h=24)
arima <- auto.arima(sent2)

library("ggfortify")

autoplot(window(sentts, start=2012)) +
  autolayer(meanf, series="Mean", PI=FALSE) +
  autolayer(rwf, series="Naïve", PI=FALSE) +
  autolayer(snaive, series="Seasonal naïve", PI=FALSE) +
  autolayer(dshw, series="HoltWinters", PI=FALSE) +
  autolayer(arima, series="Arima", PI=FALSE) +
  xlab("Year") + ylab("Sentiment") +
  ggtitle("Forecasts SSI") +
  guides(colour=guide_legend(title="Forecast"))

sent3 <- window(sentts, start=2016)
accuracy(meanf, sent3)
accuracy(rwf, sent3)
accuracy(snaive, sent3)
accuracy(hw, sent3)
accuracy(arima, sent3)


