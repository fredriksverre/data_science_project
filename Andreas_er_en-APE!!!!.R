#Andreas er en treig ape!!!!!

#oppdatert....

#link: https://ourworldindata.org/



# https://metals-api.com/api/latest

library(tidyverse)
library(httr)
library(jsonlite)

Apigull<-GET("https://metals-api.com/api/latest?access_key=okrh96bifqusv5w65rpj6w3cs223gf570bs57akec4j4r03aucdze0yjlqkl")


{
  access_key = "okrh96bifqusv5w65rpj6w3cs223gf570bs57akec4j4r03aucdze0yjlqkl"
  start_date = 2000-01-01
  end_date = 2001-01-01
  base = "USD"
  symbols = "XAU"
}

headers(Apigull)
str(content(Apigull))

datagull = fromJSON(rawToChar(Apigull$content))
names(datagull)
datagull[["rates"]][["XAU"]]
datagull

<<<<<<< HEAD

=======
>>>>>>> 8ec1b61e6d28a261efd20d58d87e245b2a41c051
