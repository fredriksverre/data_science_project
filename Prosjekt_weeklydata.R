#Linker Kilder

#Bitcoin
#https://finance.yahoo.com/quote/BTC-USD/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

#Ethereum
#https://finance.yahoo.com/quote/ETH-USD/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

#Gull
#https://finance.yahoo.com/quote/GC%3DF/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

#Vix
#https://finance.yahoo.com/quote/%5EVIX/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

#Brent olje 
#https://finance.yahoo.com/quote/BZ%3DF/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

#S&P500
#https://finance.yahoo.com/quote/%5EGSPC/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

library(tidyverse)
library(httr)
library(readr)
library(quantmod)
library(lubridate)
library(Quandl)
#library(raster)

# Getting data from API 
Gold <-Quandl("LBMA/GOLD", api_key="4TmA83fLoY_UpQJVjVJu", start_date="2016-01-04", end_date="2021-01-01")

Gold <- Gold %>% select("Date", "USD (PM)") %>% rename(Price = "USD (PM)")

# Getting data from yahoo
ETH <- getSymbols("ETH-USD", auto.assign=FALSE, from="2016-01-04", src='yahoo')
BTC <- getSymbols("BTC-USD", auto.assign=FALSE, from="2016-01-04", src='yahoo')
SP500 <- getSymbols("^GSPC", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Datar starter 04.01.2016
#BrentOil <- getSymbols("BZ=F", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Data fra 04.12.2018
VIX <- getSymbols("^VIX", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Data starter fra 04.01.2016. Mangler observasjoner i volum, men vi har ikke behov for dette
#Gold <- getSymbols("GC=F", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Data starter fra 04.01.2016

# Making data from xts to normal df. Picking columns and renameing.
ETH <- zoo::fortify.zoo(ETH)
ETH <- ETH %>% select("Index","ETH-USD.Close") %>% rename(Date ="Index", Price = "ETH-USD.Close")

BTC <- zoo::fortify.zoo(BTC)
BTC <- BTC %>% select("Index","BTC-USD.Close") %>% rename(Date ="Index", Price = "BTC-USD.Close")

SP500 <- zoo::fortify.zoo(SP500)
SP500 <- SP500 %>% select("Index","GSPC.Close") %>% rename(Date ="Index", Price = "GSPC.Close")

#Gold <- zoo::fortify.zoo(Gold)
#Gold <- Gold %>% select("Index","GC=F.Close") %>% mutate(Asset = "Gold") %>% rename(Date ="Index", Price = "GC=F.Close")

VIX <- zoo::fortify.zoo(VIX)
VIX <- VIX %>% select("Index","VIX.Close") %>% rename(Date ="Index", Price = "VIX.Close")

BrentOil <- zoo::fortify.zoo(BrentOil)
BrentOil <- BrentOil %>% select("Index","BZ=F.Close") %>% rename(Date ="Index", Price = "BZ=F.Close")

# Lager ny variabel som sorterer ukentlig
BTC$week <- floor_date(BTC$Date, "week")
# Grouping then new variable week and taking the mean to get weekly price
BTC <- BTC %>%
  group_by(week) %>%
  summarize(mean = mean(Price))
#
BTC <- BTC %>% rename(Price = "mean") %>% mutate(Asset = "Bitcoin")


# 
ETH$week <- floor_date(ETH$Date, "week")
# 
ETH <- ETH %>%
  group_by(week) %>%
  summarize(mean = mean(Price))
#
ETH <- ETH %>% rename(Price = "mean") %>% mutate(Asset = "Ethereum")


# 
SP500$week <- floor_date(SP500$Date, "week")
#
SP500 <- SP500 %>%
  group_by(week) %>%
  summarize(mean = mean(Price))
#
SP500 <- SP500 %>% rename(Price = "mean") %>% mutate(Asset = "SP500")


# Fjerner N/A verdier
Gold <- na.omit(Gold)
# 
Gold$week <- floor_date(Gold$Date, "week")
#
Gold <- Gold %>%
  group_by(week) %>%
  summarize(mean = mean(Price))
#
Gold <- Gold %>% rename(Price = "mean") %>% mutate(Asset = "Gold")

# 
VIX$week <- floor_date(VIX$Date, "week")
#
VIX <- VIX %>%
  group_by(week) %>%
  summarize(mean = mean(Price))
#
VIX <- VIX %>% rename(Price = "mean") %>% mutate(Asset = "VIX")


# Making LOG percentage change from day to day
log_returns <- diff(log(ETH$Price), lag=1)
# Making df og LOG
log_returns <- cbind(log_returns)
# Putting 0 at the top of LOG, to make it same length as other variables
log_returns <- rbind(0,log_returns)
#Binding LOG togheter with main df
ETH <- cbind(ETH, log_returns)

#
log_returns <- diff(log(BTC$Price), lag=1)

log_returns <- cbind(log_returns)

log_returns <- rbind(0,log_returns)

BTC <- cbind(BTC, log_returns)

#
log_returns <- diff(log(SP500$Price), lag=1)

log_returns <- cbind(log_returns)

log_returns <- rbind(0,log_returns)

SP500 <- cbind(SP500, log_returns)

#
log_returns <- diff(log(Gold$Price), lag=1)

log_returns <- cbind(log_returns)

log_returns <- rbind(0,log_returns)

Gold <- cbind(Gold, log_returns)

#
log_returns <- diff(log(VIX$Price), lag=1)

log_returns <- cbind(log_returns)

log_returns <- rbind(0,log_returns)

VIX <- cbind(VIX, log_returns)

# Making data long
df <- bind_rows(ETH,BTC,SP500,Gold,VIX) # For store hull i datasettet til gull.

#df <- df %>%
 # arrange(Asset,Date) 

# Making log_returns cumulative
df <- df %>% group_by(Asset)%>% mutate(cumulative = cumsum(log_returns))

# Plotting! Usikker på om grafen er riktig. Synes nedgangen er for liten i bitcoin og ETH
df %>% 
  ggplot(aes(x=week, y=cumulative, group=Asset)) +
  geom_line(aes(color=Asset))+
  ylab(expression("Vekst på kumulativ form")) +
  xlab("Daily") +
  labs(title = "",
       subtitle = "",
       caption = "")

# Coefficient of variations (relative standard deviation)
raster::cv(BTC$Price)    # BTC 72.41231
raster::cv(ETH$Price)    # ETH 96.86965
raster::cv(Gold$Price)   # Gold 15.16687
raster::cv(SP500$Price)  # SP500 15.20827


# ETH CV/return of asset (historical)
96.54888/61670 # 0.001565573
# BTC 
72.8754/ 4100 # 0.01777449
# Gold
15.20132 / 61 # 0.249202
#SP500
15.3343 / 83 # 0.1847506

#Korrelasjon 2016-2020
cor(BTC$Price, ETH$Price) # BTC/ETH på 0.7109
cor(BTC$Price, Gold$Price) #BTC/Gold 0.6299536
cor(BTC$Price, SP500$Price) #BTC/SP500 0.8419065
cor(Gold$Price,SP500$Price)

# Korrelasjon 2018
ETH2018 <- ETH %>% filter(week>= "2018-01-01" & week <"2019-01-01")
BTC2018 <- BTC %>% filter(week>= "2018-01-01" & week <"2019-01-01")
Gold2018 <- Gold %>% filter(week>= "2018-01-01" & week <"2019-01-01")
SP500_2018 <- SP500 %>% filter(week>= "2018-01-01" & week <"2019-01-01")

cor(BTC2018$Price,ETH2018$Price) #BTC/ETH 
cor(BTC2018$Price,Gold2018$Price) #BTC/Gold 
cor(BTC2018$Price,SP500_2018$Price) #BTC/SP500 
cor(Gold2018$Price,SP500_2018$Price)

# Korrelasjon 2019
ETH2019 <- ETH %>% filter(week>= "2019-01-01" & week <"2020-01-01")
BTC2019 <- BTC %>% filter(week>= "2019-01-01" & week <"2020-01-01")
Gold2019 <- Gold %>% filter(week>= "2019-01-01" & week <"2020-01-01")
SP500_2019 <- SP500 %>% filter(week>= "2019-01-01" & week <"2020-01-01")

cor(BTC2019$Price,ETH2019$Price) #BTC/ETH
cor(BTC2019$Price,Gold2019$Price) #BTC/Gold 
cor(BTC2019$Price,SP500_2019$Price) #BTC/SP500 
cor(Gold2019$Price,SP500_2019$Price)

# Korrelasjon 2020
ETH2020 <- ETH %>% filter(week>= "2020-01-01")
BTC2020 <- BTC %>% filter(week>= "2020-01-01")
Gold2020 <- Gold %>% filter(week>= "2020-01-01")
SP500_2020 <- SP500 %>% filter(week>= "2020-01-01")

cor(BTC2020$Price,ETH2020$Price) #BTC/ETH
cor(BTC2020$Price,Gold2020$Price) #BTC/Gold 
cor(BTC2020$Price,SP500_2020$Price) #BTC/SP500 
cor(Gold2020$Price,SP500_2020$Price) #Gold /SP500

# Taking log of df
log_returns2 <- diff(log(BTC2020$Price), lag=1)

log_returns2 <- cbind(log_returns2)

log_returns2 <- rbind(0,log_returns2)

BTC2020 <- cbind(BTC2020, log_returns2)
# 
log_returns2 <- diff(log(Gold2020$Price), lag=1)

log_returns2 <- cbind(log_returns2)

log_returns2 <- rbind(0,log_returns2)

Gold2020 <- cbind(Gold2020, log_returns2)
<<<<<<< HEAD

# Making data long
df2<-bind_rows(BTC2020,Gold2020)
=======
# 
log_returns2 <- diff(log(SP500_2020$Price), lag=1)

log_returns2 <- cbind(log_returns2)

log_returns2 <- rbind(0,log_returns2)

SP500_2020 <- cbind(SP500_2020, log_returns2)

# Making data long
df2<-bind_rows(BTC2020,Gold2020,SP500_2020)
>>>>>>> 6dd7e0656a1e6b10977891c2421eb33bd2a8eb31
# Making cumulative count
df2 <- df2 %>% select(week,Price,Asset,log_returns2) %>% group_by(Asset)%>% mutate(cumulative = cumsum(log_returns2))
# Plotting. Ser på avkastning til Gull og Bitcoin i 2020
df2 %>% 
  ggplot(aes(x=week, y=cumulative, group=Asset)) +
  geom_line(aes(color=Asset))+
  ylab(expression("Vekst på kumulativ form")) +
  xlab("Daily") +
<<<<<<< HEAD
  labs(title = "",
       subtitle = "",
       caption = "")
=======
  labs(title = "Logaritmisk avkastning på kumulativ form",
       subtitle = "",
       caption = "")+theme(plot.title = element_text(hjust = 0.5))
>>>>>>> 6dd7e0656a1e6b10977891c2421eb33bd2a8eb31

#
gp = group_by(Asset)
dplyr::summarize(gp, cor(a, b))


df_regresjon <- cbind(BTC,ETH$Price)
df_regresjon <- df_regresjon %>% select(Price,`ETH$Price`)

regresjon1 <- lm(Price ~ ETH$Price, data= df_regresjon)
plot(df_regresjon$Price,df_regresjon$`ETH$Price`, pch = 16, col = "blue") 
abline(regresjon1)

summary(regresjon1)




# Sliter med å finne Olje data.

# Relativ varians (Risiko), coefficient of variation. Hva sier den CV vi har? 
# CV = sd/mean*100, den sier noe om hvor store svingninger det er i aksjen men ikke noe om avkastning(potential return).
# Bruke sharp ratio? Hvordan beregne antatt avkastning?



#lmTemp = lm(Pressure~Temperature, data = pressure) #Create the linear regression
#plot(pressure, pch = 10, col = "blue") #Plot the results
#abline(lmTemp) #Add a regression line


summary(BTC)
summary(ETH)


# Making a function to calculate coefficient of variation
#CV <- function(mean, sd){
  #(sd/mean)*100
#}

# Coefficient of variations




#CV(mean = 200.6097, sd = 227.2656) 

#CV(mean = 1379.591, sd = 209.2408) 

#CV(mean = 2665.762, sd = 405.4164)



