library(shiny)
require(quantmod)
require(PerformanceAnalytics)

ui <- pageWithSidebar(
  headerPanel("Investment Strategy Tool", title = span(
    tagList(icon("line-chart"), "Investment Strategy Tool")
  )),
  
  sidebarPanel(
    helpText("You can select maximum 3 ETF"),
    textInput('symbol', label="Enter ticker symbol", value="GOOG;FB"),
    helpText("Examples: 'SPY', 'GOOG', 'FB' or 'SPY;GOOG;FB' (separate by semi-colon)"),
    dateRangeInput(
      "dates",
      "Compare to historic returns from",
      start = "2012-01-01",
      end = Sys.Date(),
      format = 'yyyy-mm-dd'
    ),
    actionButton("get", "Get Stock"),
    hr()
  ),
  mainPanel(tabsetPanel(
    tabPanel("Performance", verbatimTextOutput("result"), plotOutput("plot")),
    id = "tab"
  ))
)

server <- function(input, output) {
  stockData <- new.env()
  
  dataInput <- reactive({
    if (input$get == 0)
      return(NULL)
    return(isolate({
      getSymbols(input$symbol, src = "yahoo",env=stockData, from=input$dates[1])
    }))
  })
  
  datesInput <- reactive({
    if (input$get == 0)
      return(NULL)
    
    return(isolate({
      paste0(input$dates[1], "::",  input$dates[2])
    }))
  })
  
  returns <- reactive({
    if (input$get == 0)
      return(NULL)
    
    dailyReturn(dataInput())
  })
  
  configs <- reactive({
    if (input$get == 0)
      return(NULL)
    
    m_configs <- list()
    for(i in 1:21) {
      weight1 <- (i-1)*.05
      weight2 <- 1-weight1
      config <- Return.portfolio(R = returns, weights=c(weight1, weight2), rebalance_on = "months")
      m_configs[[i]] <- config
    }
    m_configs <- do.call(cbind, m_configs)
  })
  
  roll72CumAnn <- reactive({
    if (input$get == 0)
      return(NULL)
    
    cumRets <- cumprod(1+configs())
    period <- 72
    
    (cumRets/lag(cumRets, period))^(252/period) - 1
  })
  
  roll72SD <- reactive({
    if (input$get == 0)
      return(NULL)
    sapply(X = configs(), runSD, n=period)*sqrt(252)
  })
  
  monthlyModSharpe <- reactive({
    if (input$get == 0)
      return(NULL)
    sd_f_factor <- 2.5
    modSharpe <- roll72CumAnn()/roll72SD()^sd_f_factor
    modSharpe[endpoints(modSharpe, on="months"),]
  })
  
  findMax <- function(data) {
    return(data==max(data))
  }
  
  
  output$result <- renderPrint({
    if (input$get == 0)
      return(NULL)
    length(returns())
    }
  )
}

shinyApp(ui = ui, server = server)
