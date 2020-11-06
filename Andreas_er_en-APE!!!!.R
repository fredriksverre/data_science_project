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

