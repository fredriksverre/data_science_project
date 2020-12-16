library(shiny)

ui <- fluidPage(
  titlePanel(title=h4("ASSET", align="center")),
  sidebarLayout(
    sidebarPanel( 
      sliderInput("df", "Dates:",min = as.Date("2016-01-01","%Y-%m-%d"), max = as.Date(Sys.time(),"%Y-%m-%d"),value=c(as.Date("2016-01-01"),as.Date("2018-12-01")),timeFormat="%Y-%m-%d")),mainPanel(plotOutput("plot2"))))


server <- function(input,output){
  
  #dat <- reactive({df[df$week %in% seq(from=min(df$week),to=max(df$week),by=1),]})
  dat <- reactive({
    test <- df[df$week %in% seq(from=min(input$df),to=max(input$df),by=1),]
    print(test)
    test
  })
  
  output$plot2<-renderPlot({
    req(dat())
    ggplot(data=dat()%>%group_by(Asset)%>% mutate(culinteractive=cumsum(log_returns)- lag(log_returns, default = first(log_returns))),aes(x=week,y=culinteractive),group=Asset)+geom_line(aes(color=Asset))},height = 300,width = 500)}
shinyApp(ui, server)

