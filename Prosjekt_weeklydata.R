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
library(dplyr)
library(Quandl)
library(raster)

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
ETH <- ETH %>% select("Index","ETH-USD.Close") %>% mutate(Asset = "Ethereum") %>% rename(Date ="Index", Price = "ETH-USD.Close")

BTC <- zoo::fortify.zoo(BTC)
BTC <- BTC %>% select("Index","BTC-USD.Close") %>% mutate(Asset = "Bitcoin") %>% rename(Date ="Index", Price = "BTC-USD.Close")

SP500 <- zoo::fortify.zoo(SP500)
SP500 <- SP500 %>% select("Index","GSPC.Close") %>% mutate(Asset = "S&P500") %>% rename(Date ="Index", Price = "GSPC.Close")

#Gold <- zoo::fortify.zoo(Gold)
#Gold <- Gold %>% select("Index","GC=F.Close") %>% mutate(Asset = "Gold") %>% rename(Date ="Index", Price = "GC=F.Close")

VIX <- zoo::fortify.zoo(VIX)
VIX <- VIX %>% select("Index","VIX.Close") %>% mutate(Asset = "VixIndex") %>% rename(Date ="Index", Price = "VIX.Close")

BrentOil <- zoo::fortify.zoo(BrentOil)
BrentOil <- BrentOil %>% select("Index","BZ=F.Close") %>% mutate(Asset = "BrentOil") %>% rename(Date ="Index", Price = "BZ=F.Close")

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

# Plotting
df %>% 
  ggplot(aes(x=week, y=cumulative, group=Asset)) +
  geom_line(aes(color=Asset))+
  ylab(expression("Vekst på kumulativ form")) +
  xlab("Daily") +
  labs(title = "",
       subtitle = "",
       caption = "")

# Coefficient of variations
cv(BTC$Price)    # BTC 72.41231
cv(ETH$Price)    # ETH 96.86965
cv(Gold$Price)   # Gold 15.16687
cv(SP500$Price)  # SP500 15.20827


#Mulig vi må ha ukentlig pris
cor(BTC$Price, ETH$Price) # Cor på 0.7109
cor(BTC$Price, SP500$Price) # Incompatible. Why ?? 
cor(BTC$Price, Gold$Price) # Incompatible. Why ?? 

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

#coefficient of variation
#mean(BTC$Price)
#sd(BTC$Price)
#
#mean(ETH$Price)
#sd(ETH$Price)
#
#mean(Gold$Price)
#sd(Gold$Price)
#
#mean(SP500$Price)
#sd(SP500$Price)

# Making a function to calculate coefficient of variation
#CV <- function(mean, sd){
#  (sd/mean)*100
#}

# Coefficient of variations
#CV(mean = 5851.609, sd = 4237.285) 

#CV(mean = 234.6097, sd = 227.2656) 

#CV(mean = 1379.591, sd = 209.2408) 

#CV(mean = 2665.762, sd = 405.4164)



