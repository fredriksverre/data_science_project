library(gtrendsR)
library(ggplot2)
library(tidyverse)
library(httr)
library(readr)
library(quantmod)

#define the keywords
#keywords=c("BTC","Bitcoin","Buy bitcoin")

#
#BTC <- getSymbols("BTC-USD", auto.assign=FALSE, from="2015-01-04", src='yahoo')
#
#BTC <- zoo::fortify.zoo(BTC)
#BTC <- BTC %>% select("Index","BTC-USD.Close") %>% rename(Date ="Index", Price = "BTC-USD.Close") %>% mutate(Asset = "BitcoinPrice")

#define the keywords
keywords=c("Bitcoin Search")
#set the geographic area: DE = Germany
country=c('')
#set the time window
#time=("2015-01-01 2020-12-03")
#time=("2015-01-01 today")
time = ("today+5-y")
#set channels 
channel='web'

trends = gtrends(keywords, gprop =channel,geo=country, time = time )
#select only interst over time 
time_trend=trends$interest_over_time
head(time_trend)

time_trend <- time_trend %>% select(date,hits,keyword) %>% rename(Date ="date", Price = "hits", Asset = "keyword") %>% filter(Date>= "2016-01-04")


#####################################################################

BTC2<-BTC %>% mutate(Asset="Bitcoin Price")
BTC2$week <- as.POSIXct(paste(BTC2$week, BTC2$Time), format="%Y-%m-%d")



time_trend<-time_trend %>%mutate(Price= Price*150 )


names(time_trend)[names(time_trend) == 'Date'] <- 'week' #Endrer navn på week data 

test26<- rbind(BTC2,time_trend)

vs<-test26 %>% 
  ggplot(aes(x=week, y=Price, group=Asset)) +
  geom_line(aes(color=Asset))+
  ylab(expression("Bitcoin Price dollar")) +
  xlab("Date") +
  labs(title = "Bitcoin Price vs Searches at Google",
       subtitle = "",
       caption = "")+theme(plot.title = element_text(hjust = 0.5))
vs<-vs+ scale_y_continuous(sec.axis = sec_axis(~./200, name = ""))
vs



