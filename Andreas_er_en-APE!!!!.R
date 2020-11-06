#Andreas er en treig ape!!!!!

#oppdatert....

#link: https://ourworldindata.org/



# https://metals-api.com/api/latest

library(tidyverse)
library(httr)
library(readr)


NG_F <- read_csv("NG=F (1).csv",
                 col_types = cols_only(Close = col_number(),
                                       Date = col_date(format = "%Y-%m-%d")))
GC_F <- read_csv("GC=F.csv",col_types = cols_only(Close = col_number(),
                                                  Date = col_date(format = "%Y-%m-%d")))
BZ_F <- read_csv("BZ=F.csv",col_types = cols_only(Close = col_number(),
                                                  Date = col_date(format = "%Y-%m-%d")))
X_VIX <- read_csv("^VIX.csv", col_types = cols_only(Close = col_number(),
                                                    Date = col_date(format = "%Y-%m-%d")))
ETH_USD <- read_csv("ETH-USD (3).csv",col_types = cols_only(Close = col_guess(), 
                                             Date = col_date(format = "%Y-%m-%d")))

NG_F<- NG_F %>%mutate(Stock="Natural_gas")
GC_F <- GC_F %>%mutate(Stock="Gold")
BZ_F <- BZ_F %>%mutate(Stock="Natural_gas")
X_VIX  <- X_VIX  %>%mutate(Stock="Brent_olje")
ETH_USD <- ETH_USD %>%mutate(Stock="Ethereum")


Alldata<- bind_rows(NG_F,GC_F,BZ_F,X_VIX,ETH_USD ) 
