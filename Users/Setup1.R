
#Libraries

library("tidyverse")
library("lubridate")
library("scales")
library("xlsx")
library("readxl")
library("data.table")
library("xtable")
library("foreign")
library("rvest")
library("twitteR")
library("magrittr")
library("readtext")
library("knitr")
library("stargazer")
library("png")
library("readtext")
library("TTR")
library("forecast")
library("ggfortify")
library("vars")
library("tm")
library("SnowballC")
library("wordcloud")
library("factoextra")
library("graph")
library("Rgraphviz")
library("topicmodels")
require(devtools)
library("sentiment")

#API key

setup_twitter_oauth(consumer_key = "u2UthjbK6YHyQSp4sPk6yjsuV",
                    consumer_secret = "sC4mjd2WME5nH1FoWeSTuSy7JCP5DHjNtTYU1X6BwQ1vPZ0j3v", 
                    access_token = "1365606414-7vPfPxStYNq6kWEATQlT8HZBd4G83BBcX4VoS9T",
                    access_secret = "0hJq9KYC3eBRuZzJqSacmtJ4PNJ7tNLkGrQrVl00JHirs")

#API key 2

setup_twitter_oauth(consumer_key = "FByTAFwGz2uS0Hd0HbShiXmON",
                    consumer_secret = "hN6v71falrydNUYni6WOLvrvebJ2a8Hku7wZXC0a2KGgDA1n4K", 
                    access_token = "960999979761504257-OqPalTobXYfRnc36q13AzZA4mpioJPx",
                    access_secret = "y8NrguNvL0XU9xeme245tptZ1DFbdWMPT4MMqMWQ4FJis")
