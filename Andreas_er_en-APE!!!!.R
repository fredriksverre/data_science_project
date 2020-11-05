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

##her

library(httr)

key = "a6a6478d2dmsha4f18475fb193c4p183012jsna7dfda1107f2" 

url = "https://gold-price1.p.rapidapi.com/get_price/USD" 

result = GET(url, add_headers("X-RapidAPI-Key" = key)) 

content(result)

headers(result)
str(content(result))

datagull = fromJSON(rawToChar(result$content))
names(datagull)
ji<-as.data.frame(datagull)
str(ji)
