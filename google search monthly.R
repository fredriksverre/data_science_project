library(gtrendsR)
library(ggplot2)
library(tidyverse)
library(httr)
library(readr)
library(quantmod)

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


#####################################################################

BTC2<-BTC %>% mutate(Asset="Bitcoin Price")
BTC2$week <- as.POSIXct(paste(BTC2$week, BTC2$Time), format="%Y-%m-%d")

#
BTC2$month <- floor_date(BTC2$week, "month")

# Grouping then new variable week and taking the mean to get weekly price
BTC2 <- BTC2 %>%
  group_by(month) %>%
  summarize(mean = mean(Price))
#
#
time_trend$Price <- as.numeric(time_trend$Price)

time_trend$month <- floor_date(time_trend$Date, "month")


# Grouping then new variable week and taking the mean to get weekly price
time_trend <- time_trend %>%
  group_by(month) %>%
  summarize(mean = mean(Price))

#
time_trend<-time_trend %>%mutate(mean= mean*200 )

time_trend <- time_trend %>% mutate(Asset = "Google Search ")

BTC2 <- BTC2 %>% mutate(Asset = "Bitcoin Price")

#
names(time_trend)[names(time_trend) == 'Date'] <- 'week' #Endrer navn på week data 

test26<- rbind(BTC2,time_trend)

vs<-test26 %>% 
  ggplot(aes(x=month, y=mean, group=Asset)) +
  geom_line(aes(color=Asset))+
  ylab(expression("Bitcoin Price $")) +
  xlab("Date") +
  labs(title = "Bitcoin Price vs Searches at Google",
       subtitle = "",
       caption = "")+theme(plot.title = element_text(hjust = 0.5))
vs<-vs+ scale_y_continuous(sec.axis = sec_axis(~./200, name = ""))
vs



