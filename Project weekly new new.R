library(tidyverse)
library(httr)
library(readr)
library(quantmod)
library(lubridate)
library(Quandl)
library(PerformanceAnalytics)
library(xts)
library(tidyquant)
library(cranlogs)   # For inspecting package downloads over time
library(corrr)      # Tidy correlation tables and correlation plottings
library(cowplot) 
library(gtrendsR) #laste inn data google
library(shiny)

### Data
# Getting data from API 
Gold <-Quandl("LBMA/GOLD", api_key="4TmA83fLoY_UpQJVjVJu", start_date="2016-01-04", end_date="2022-01-01")
Gold <- Gold %>% select("Date", "USD (PM)") %>% rename(Price = "USD (PM)")

# Getting data from yahoo
ETH <- getSymbols("ETH-USD", auto.assign=FALSE, from="2016-01-04", src='yahoo')
BTC <- getSymbols("BTC-USD", auto.assign=FALSE, from="2016-01-04", src='yahoo')
SP500 <- getSymbols("^GSPC", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Datar 
VIX <- getSymbols("^VIX", auto.assign=FALSE, from="2016-01-04", src='yahoo') # Data starter 
# Making data from xts to normal df. Picking columns and renameing.
ETH <- zoo::fortify.zoo(ETH)
ETH <- ETH %>% select("Index","ETH-USD.Close") %>% mutate(Asset = "Ethereum") %>% rename(Date ="Index", Price = "ETH-USD.Close")

BTC <- zoo::fortify.zoo(BTC)
BTC <- BTC %>% select("Index","BTC-USD.Close") %>% mutate(Asset = "Bitcoin") %>% rename(Date ="Index", Price = "BTC-USD.Close")

SP500 <- zoo::fortify.zoo(SP500)
SP500 <- SP500 %>% select("Index","GSPC.Close") %>% mutate(Asset = "S&P500") %>% rename(Date ="Index", Price = "GSPC.Close")

VIX <- zoo::fortify.zoo(VIX)
VIX <- VIX %>% select("Index","VIX.Close") %>% mutate(Asset = "VixIndex") %>% rename(Date ="Index", Price = "VIX.Close")

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


# Removing N/A values
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
df <- bind_rows(ETH,BTC,SP500,Gold,VIX)

# Making log_returns cumulative
df <- df %>% group_by(Asset)%>% mutate(cumulative = cumsum(log_returns))

# Plotting
p<-df %>% 
  ggplot(aes(x=week, y=cumulative, group=Asset)) +
  geom_line(aes(color=Asset))+
  ylab(expression("cumulative form")) +
  xlab("Date") +
  labs(title = "Logaritmic return on cumulativ form from 2016-2020",
       subtitle = "",
       caption = "")+theme(plot.title = element_text(hjust = 0.5))

p


########### Shiny
server <- function(input,output){
  
  #dat <- reactive({df[df$week %in% seq(from=min(df$week),to=max(df$week),by=1),]})
  dat <- reactive({
    test <- df[df$week %in% seq(from=min(input$df),to=max(input$df),by=1),]
    print(test)
    test
  })
  
  output$plot2<-renderPlot({
    req(dat())
    ggplot(data=dat()%>%group_by(Asset)%>% mutate(culinteractive=cumsum(log_returns)),aes(x=week,y=culinteractive),group=Asset)+geom_line(aes(color=Asset))},height = 350,width = 500)}
shinyApp(ui, server)

#
ui <- fluidPage(
  titlePanel(title=h4("When did you invest?", align="center")),
  sidebarLayout(
    sidebarPanel( 
      sliderInput("df", "Dates:",min = as.Date("2016-01-01","%Y-%m-%d"), max = as.Date(Sys.time(),"%Y-%m-%d"),value=c(as.Date("2016-01-01"),as.Date("2018-12-01")),timeFormat="%Y-%m-%d")),mainPanel(plotOutput("plot2"))))

############ Making cumulative plot for 2020

# Filter for year 2020
ETH2020 <- ETH %>% filter(week>= "2020-01-01")
BTC2020 <- BTC %>% filter(week>= "2020-01-01")
Gold2020 <- Gold %>% filter(week>= "2020-01-01")
SP500_2020 <- SP500 %>% filter(week>= "2020-01-01")

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
# 
log_returns2 <- diff(log(SP500_2020$Price), lag=1)

log_returns2 <- cbind(log_returns2)

log_returns2 <- rbind(0,log_returns2)

SP500_2020 <- cbind(SP500_2020, log_returns2)

# Making data long
df2<-bind_rows(BTC2020,Gold2020,SP500_2020)
# Making cumulative count
df2 <- df2 %>% select(week,Price,Asset,log_returns2) %>% group_by(Asset)%>% mutate(cumulative = cumsum(log_returns2))
# Plotting. 
df2 %>% 
  ggplot(aes(x=week, y=cumulative, group=Asset)) +
  geom_line(aes(color=Asset))+
  ylab(expression("Return on cumulativ form")) +
  xlab("Date") +
  labs(title = "Logaritmic return on cumulative form 2020",
       subtitle = "",
       caption = "")+theme(plot.title = element_text(hjust = 0.5))


########### Rolling correlation for BTC

# Making data long
df2 <- bind_rows(ETH,SP500,Gold,VIX)

df2 <- df2 %>% select(week,Price,Asset) %>% rename(Asset2 = "Asset")

BTC1 <- BTC %>% select(week,Price) %>% tibble::as_tibble()

# Asset Returns
df2 <- df2 %>%
  group_by(Asset2) %>%
  tq_transmute(select     = Price, 
               mutate_fun = periodReturn,
               period     = "weekly")

# Baseline Returns
BTC1 <- BTC1 %>%
  tq_transmute(select     = Price, 
               mutate_fun = periodReturn,
               period     = "weekly")

# Correlation table
tidyverse_static_correlations <- df2 %>%
  # Data wrangling
  spread(key = Asset2, value = weekly.returns) %>%
  left_join(BTC1, by = "week") %>%
  rename(Bitcoin = weekly.returns) %>%
  select(-week) %>%
  # Correlation and formating
  correlate() 


# Get rolling correlations
tidyverse_rolling_corr <- df2 %>%
  # Data wrangling
  left_join(BTC1, by = "week") %>%
  select(week,Asset2, weekly.returns.x, weekly.returns.y ) %>%
  # Mutation
  tq_mutate_xy(
    x          = weekly.returns.x,
    y          = weekly.returns.y,
    mutate_fun = runCor, 
    # runCor args
    n          = 8,
    use        = "pairwise.complete.obs",
    # tq_mutate args
    col_rename = "rolling_corr"
  )


# Join static correlations with rolling correlations
tidyverse_static_correlations <- tidyverse_static_correlations %>%
  select(term, Bitcoin) %>%
  rename(Asset2 = term)

tidyverse_rolling_corr2 <- tidyverse_rolling_corr %>%
  left_join(tidyverse_static_correlations, by = "Asset2") %>%
  rename(static_corr = Bitcoin)

# Plot
tidyverse_rolling_corr2 %>%
  ggplot(aes(x = week, color = Asset2)) +
  # Data
  geom_line(aes(y = static_corr), color = "red") +
  geom_point(aes(y = rolling_corr), alpha = 0.5) +
  facet_wrap(~ Asset2, ncol = 2, scales = "free_y") +
  # Aesthetics
  scale_color_tq() +
  labs(
    title = "Bitcoin:8-Weeks Rolling Correlations, Bitcoin vs assets",
    subtitle = "Relationships are dynamic vs static correlation (red line)",
    x = "", y = "Correlation"
  ) +
  theme_tq() +
  theme(legend.position="none")

################# Rolling correlation SP500

# Making data long
df3 <- bind_rows(ETH,BTC,Gold,VIX)

df3 <- df3 %>% select(week,Price,Asset)

SP500_1 <- SP500 %>% select(week,Price) %>% tibble::as_tibble()

# Asset Returns
df3 <- df3 %>%
  group_by(Asset) %>%
  tq_transmute(select     = Price, 
               mutate_fun = periodReturn,
               period     = "weekly")

# Baseline Returns
SP500_1 <- SP500_1 %>%
  tq_transmute(select     = Price, 
               mutate_fun = periodReturn,
               period     = "weekly")

# Correlation table
tidyverse_static_correlations2 <- df3 %>%
  # Data wrangling
  spread(key = Asset, value = weekly.returns) %>%
  left_join(SP500_1, by = "week") %>%
  rename(SP500 = weekly.returns) %>%
  select(-week) %>%
  # Correlation and formating
  correlate() 

# Get rolling correlations
tidyverse_rolling_corr2 <- df3 %>%
  # Data wrangling
  left_join(SP500_1, by = "week") %>%
  select(week,Asset, weekly.returns.x, weekly.returns.y ) %>%
  # Mutation
  tq_mutate_xy(
    x          = weekly.returns.x,
    y          = weekly.returns.y,
    mutate_fun = runCor, 
    # runCor args
    n          = 12,
    use        = "pairwise.complete.obs",
    # tq_mutate args
    col_rename = "rolling_corr"
  )

# Join static correlations with rolling correlations
tidyverse_static_correlations2 <- tidyverse_static_correlations2 %>%
  select(term, SP500) %>%
  rename(Asset = term)

tidyverse_rolling_corr2 <- tidyverse_rolling_corr2 %>%
  left_join(tidyverse_static_correlations2, by = "Asset") %>%
  rename(static_corr = SP500)

# Plot
tidyverse_rolling_corr2 %>%
  ggplot(aes(x = week, color = Asset)) +
  # Data
  geom_line(aes(y = static_corr), color = "red") +
  geom_point(aes(y = rolling_corr), alpha = 0.5) +
  facet_wrap(~ Asset, ncol = 2, scales = "free_y") +
  # Aesthetics
  scale_color_tq() +
  labs(
    title = " S&P500: 12-Weeks Rolling Correlations, SP500 vs assets",
    subtitle = "Relationships are dynamic vs static correlation (red line)",
    x = "", y = "Correlation"
  ) +
  theme_tq() +
  theme(legend.position="none")

################## Rolling correlation SP500 for 2020

# Correlation table
tidyverse_static_correlations3 <- df3 %>% filter(week>= "2020-01-01") %>%
  # Data wrangling
  spread(key = Asset, value = weekly.returns) %>%
  left_join(SP500_1, by = "week") %>%
  rename(SP500 = weekly.returns) %>%
  select(-week) %>%
  # Correlation and formating
  correlate() 

# Get rolling correlations
tidyverse_rolling_corr3 <- df3 %>% filter(week>= "2020-01-01") %>%
  # Data wrangling
  left_join(SP500_1, by = "week") %>%
  select(week,Asset, weekly.returns.x, weekly.returns.y ) %>%
  # Mutation
  tq_mutate_xy(
    x          = weekly.returns.x,
    y          = weekly.returns.y,
    mutate_fun = runCor, 
    # runCor args
    n          = 4,
    use        = "pairwise.complete.obs",
    # tq_mutate args
    col_rename = "rolling_corr"
  )

# Join static correlations with rolling correlations
tidyverse_static_correlations3 <- tidyverse_static_correlations3 %>%
  select(term, SP500) %>%
  rename(Asset = term)

tidyverse_rolling_corr3 <- tidyverse_rolling_corr3 %>%
  left_join(tidyverse_static_correlations3, by = "Asset") %>%
  rename(static_corr = SP500)

# Plot
tidyverse_rolling_corr3 %>%
  ggplot(aes(x = week, color = Asset)) +
  # Data
  geom_line(aes(y = static_corr), color = "red") +
  geom_line(aes(y = rolling_corr), alpha = 0.5) +
  facet_wrap(~ Asset, ncol = 2, scales = "free_y") +
  # Aesthetics
  scale_color_tq() +
  labs(
    title = "S&P500: 4-Weeks Rolling Correlations, S&P500 vs different assets",
    subtitle = "Relationships are dynamic vs static correlation (red line)",
    x = "", y = "Correlation"
  ) +
  theme_tq() +
  theme(legend.position="none")


################## Google search monthly data

#define the keywords
keywords=c("Bitcoin")
#set the geographic area: DE = Germany
country=c('')
# Setting time frame
time = ("today+5-y")
#set channels 
channel='web'
# Pulling the data from google
trends = gtrends(keywords, gprop =channel,geo=country, time = time )
#select only interest over time 
time_trend=trends$interest_over_time
#
time_trend <- time_trend %>% select(date,hits,keyword) %>% rename(Date ="date", Price = "hits", Asset = "keyword") %>% filter(Date>= "2016-01-04")

#
BTC2<-BTC %>% mutate(Asset="Bitcoin Price")
BTC2$week <- as.POSIXct(paste(BTC2$week, BTC2$Time), format="%Y-%m-%d")

# Making monthly data
BTC2$month <- floor_date(BTC2$week, "month")
# Grouping then new variable month and taking the mean to get monthly price
BTC2 <- BTC2 %>%
  group_by(month) %>%
  summarize(mean = mean(Price))

# Making numeric
time_trend$Price <- as.numeric(time_trend$Price)

# Making monthly data
time_trend$month <- floor_date(time_trend$Date, "month")

# Grouping then new variable week and taking the mean to get monthly price
time_trend <- time_trend %>%
  group_by(month) %>%
  summarize(mean = mean(Price))

# Making google search fit the scale on the graph
time_trend<-time_trend %>%mutate(mean= mean*200 )
# Naming assets
time_trend <- time_trend %>% mutate(Asset = "Google Search ")
# Naming assets
BTC2 <- BTC2 %>% mutate(Asset = "Bitcoin Price")

# Changing name on time_trend from date to week
names(time_trend)[names(time_trend) == 'Date'] <- 'week' 
# Making data long
test26<- rbind(BTC2,time_trend)
# Plotting
vs<-test26 %>% 
  ggplot(aes(x=month, y=mean, group=Asset)) +
  geom_line(aes(color=Asset))+
  ylab(expression("Bitcoin Price $")) +
  xlab("Date") +
  labs(title = "Bitcoin Price vs Searches at Google",
       subtitle = "Graph:Monthly data",
       caption = "")+theme(plot.title = element_text(hjust = 0.5))
vs<-vs+ scale_y_continuous(sec.axis = sec_axis(~./200, name = ""))
vs
