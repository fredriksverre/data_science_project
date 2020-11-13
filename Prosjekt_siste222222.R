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

# Getting data from yahoo
ETH <- getSymbols("ETH-USD", auto.assign=FALSE, from="2016-01-04", src='yahoo')
BTC <- getSymbols("BTC-USD", auto.assign=FALSE, from="2016-01-04", src='yahoo')
SP500 <- getSymbols("^GSPC", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Datar starter 04.01.2016
BrentOil <- getSymbols("BZ=F", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Data fra 04.12.2018
VIX <- getSymbols("^VIX", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Data starter fra 04.01.2016. Mangler observasjoner i volum, men vi har ikke behov for dette
Gold <- getSymbols("GC=F", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Data starter fra 04.01.2016

# Making data from xts to normal df. Picking columns and renameing.
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

# Making data long
df <- bind_rows(ETH,BTC,SP500,Gold) # For store hull i datasettet til gull.

#df <- df %>%
 # arrange(Asset,Date) 

# Making log_returns cumulative
df <- df %>% group_by(Asset)%>% mutate(cumulative = cumsum(log_returns))

# Plotting
df %>% 
  ggplot(aes(x=Date, y=cumulative, group=Asset)) +
  geom_line(aes(color=Asset))+
  ylab(expression("Vekst på kumulativ form")) +
  xlab("Daily") +
  labs(title = "",
       subtitle = "",
       caption = "")

#Mulig vi må ha ukentlig pris
cor(BTC$Price, ETH$Price) # Cor på 0.7109
cor(BTC$Price, SP500$Price) # Incompatible. Why ?? 
cor(BTC$Price, Gold$Price) # Incompatible. Why ?? 


# Finner gull og olje data fra en annen side som er brukt i en forelesning.

# Ta log(logaritmen) av dataen, så gjøre det cumulativt for å se prosentvis endring.

# Relativ varians (Risiko), coefficient of variation. Standardavvik






