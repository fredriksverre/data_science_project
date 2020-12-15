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
keywords=c("Bitcoin")
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


#ggplot(data=time_trend, aes(x=Date, y=Price,group=Asset,col=Asset))+
 # geom_line() + facet_wrap(~Asset) + xlab('Time') + ylab('Relative Interest')  +
 # theme(legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=12))+ggtitle("Google Search Volume")
#plot

ggplot(data=time_trend, aes(x=Date, y=Price,group=Asset,col=Asset))+
  geom_line() + facet_wrap(~Asset) + xlab('Time') + ylab('Relative Interest')  +
ggtitle("Google Search Volume")
