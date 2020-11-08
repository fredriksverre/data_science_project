#Linker Kilder

#Bitcoin
#https://finance.yahoo.com/quote/BTC-USD/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

#Ethereum
#https://finance.yahoo.com/quote/ETH-USD/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

#gull
#https://finance.yahoo.com/quote/GC%3DF/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

#natural gas
#https://finance.yahoo.com/quote/NG%3DF/history?period1=1573032651&period2=1604655051&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

#vix
#https://finance.yahoo.com/quote/%5EVIX/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

#brent olje 
#https://finance.yahoo.com/quote/BZ%3DF/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true

#S&P500
#https://finance.yahoo.com/quote/%5EGSPC/history?period1=1446768000&period2=1604620800&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true


library(tidyverse)
library(httr)
library(readr)
library(quantmod)

# Henter inn finans data fra yahoo

ETH <- getSymbols("ETH-USD", auto.assign=FALSE, from="2016-01-04", src='yahoo')
BTC <- getSymbols("BTC-USD", auto.assign=FALSE, from="2016-01-04", src='yahoo')
SP500 <- getSymbols("^GSPC", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Datar starter 04.01.2016
BrentOil <- getSymbols("BZ=F", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Data fra 04.12.2018
VIX <- getSymbols("^VIX", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Data starter fra 04.01.2016. Mangler observasjoner i volum, men vi har ikke behov for dette
Gold <- getSymbols("GC=F", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Data starter fra 04.01.2016

# Gjør om fra xts til "normal" datasett. Velger kolonner og endrer navn.
ETH <- zoo::fortify.zoo(ETH)
ETH <- ETH %>% select("Index","ETH-USD.Close") %>% mutate(Asset = "Ethereum") %>% rename(Date ="Index", Price = "ETH-USD.Close")

BTC <- zoo::fortify.zoo(BTC)
BTC <- BTC %>% select("Index","BTC-USD.Close") %>% mutate(Asset = "Bitcoin") %>% rename(Date ="Index", Price = "BTC-USD.Close")

SP500 <- zoo::fortify.zoo(SP500)
SP500 <- SP500 %>% select("Index","GSPC.Close") %>% mutate(Asset = "S&P500") %>% rename(Date ="Index", Price = "GSPC.Close")

Gold <- zoo::fortify.zoo(Gold)
Gold <- Gold %>% select("Index","GC=F.Close") %>% mutate(Asset = "Gold") %>% rename(Date ="Index", Price = "GC=F.Close")

VIX <- zoo::fortify.zoo(VIX)
VIX <- VIX %>% select("Index","VIX.Close") %>% mutate(Asset = "VixIndex") %>% rename(Date ="Index", Price = "VIX.Close")

BrentOil <- zoo::fortify.zoo(BrentOil)
BrentOil <- BrentOil %>% select("Index","BZ=F.Close") %>% mutate(Asset = "BrentOil") %>% rename(Date ="Index", Price = "BZ=F.Close")

# Making data long
Alldata<- bind_rows(BTC,ETH,BrentOil,Gold,VIX,SP500) 

# Arranging data
Alldata <- Alldata %>%
  arrange(Asset,Date) 

# Making cumulative
Alldata<-Alldata%>% group_by(Asset)%>% mutate(cumulative = cumsum(Price))

# Ser ingen forskjell når jeg plotter med log10, log eller uten log. Usikker på om den egentlig tar log 
Alldata %>% 
  ggplot(aes(x=Date, y=Price, group=Asset), log10(y)) +
  geom_line(aes(color=Asset))+
  ylab(expression("Price in USD")) +
  xlab("Daily") +
  labs(title = "",
       subtitle = "",
       caption = "")

ETH %>% 
  ggplot(aes(x=Date, y=Price, group=Asset), log10(y)) +
  geom_line(aes(color=Asset))+
  ylab(expression("Price in USD")) +
  xlab("Daily") +
  labs(title = "",
       subtitle = "",
       caption = "")

## Mulig vi må ha ukentlig pris

cor(BTC$Price, ETH$Price) # Cor på 0.7109
cor(BTC$Price, SP500$Price) # Incompatible. Why ?? 
cor(BTC$Price, Gold$Price) # Incompatible. Why ?? 








