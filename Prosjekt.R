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

# Alle data settene er på "månedelige intervaller". Tror vi får en mer presis graf om vi tar daglige observasjoner?
BTC_USD <- read_csv("BTC-USD.csv",
                    col_types = cols_only(Close = col_number(),
                                          Date = col_date(format = "%Y-%m-%d")))
#NG_F <- read_csv("NG=F (1).csv",
               #  col_types = cols_only(Close = col_number(),
                                      # Date = col_date(format = "%Y-%m-%d")))

GC_F <- read_csv("GC=F.csv",col_types = cols_only(Close = col_number(),
                                                  Date = col_date(format = "%Y-%m-%d")))
BZ_F <- read_csv("BZ=F.csv",col_types = cols_only(Close = col_number(),
                                                  Date = col_date(format = "%Y-%m-%d")))
X_VIX <- read_csv("^VIX.csv", col_types = cols_only(Close = col_number(),
                                                    Date = col_date(format = "%Y-%m-%d")))
ETH_USD <- read_csv("ETH-USD (3).csv",col_types = cols_only(Close = col_guess(), 
                                             Date = col_date(format = "%Y-%m-%d")))
SP500 <- read_csv("^GSPC.csv",col_types = cols_only(Close = col_guess(), 
                                                            Date = col_date(format = "%Y-%m-%d")))
BTC_USD <- BTC_USD %>%mutate(Asset ="Bitcoin")
#NG_F<- NG_F %>%mutate(Stock="Natural_gas")
GC_F <- GC_F %>%mutate(Asset ="Gold")
BZ_F <- BZ_F %>%mutate(Asset ="Brent_oil")
X_VIX  <- X_VIX  %>%mutate(Asset ="VIX")
ETH_USD <- ETH_USD %>%mutate(Asset ="Ethereum")
SP500 <- SP500 %>%mutate(Asset ="S&P_500")

Alldata<- bind_rows(BTC_USD,GC_F,BZ_F,X_VIX,ETH_USD,SP500) 

Alldata <- Alldata %>%
  arrange(Asset,Date,) 

Alldata<-Alldata%>% group_by(Asset)%>% mutate(cumulative = cumsum(Close))


# Ser ingen forskjell når jeg plotter med log10, log eller uten log. Usikker på om den egentlig tar log 
Alldata %>% 
  ggplot(aes(x=Date, y=Close, group=Asset), log10("y")) +
  geom_line(aes(color=Asset))+
  ylab(expression("Tissen til Alf er stor")) +
  xlab("Daglige kurser") +
  labs(title = "",
       subtitle = "",
       caption = "")

