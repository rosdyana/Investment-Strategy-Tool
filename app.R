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
  
  symbols <- function(symbol) getSymbols(symbol,env=stockData, from=input$dates[1])
  apacoba <- NULL
  mainfuncIST <- function(){
    #if(input$get==0){return()} #confirming button click
    #isolate({
    if (input$get == 0)
      return(NULL)
      sim <- input$symbol
      getSymbols(sim,env=stockData, from=input$dates[1])
      Data <- NULL
      #get(sim[2], pos=stockData)[,6]
      #diff(log(Cl(get(sim[1], pos=stockData))))
      validate(need(sim != "", label = "stock"))
      for (i in 1:length(sim)) {
        #x[[i]] <-   # get data from stockData environment  
        Data <- cbind(Data,Return.calculate((get(sim[i], pos=stockData)[,6])))
      }
      returns <- Data[-1,]
      configs <- list()
      for(i in 1:21) {
        weightSPY <- (i-1)*.05
        weightTLT <- 1-weightSPY
        config <- Return.portfolio(R = returns, weights=c(weightSPY, weightTLT), rebalance_on = "months")
        configs[[i]] <- config
      }
      configs <- do.call(cbind, configs)
      cumRets <- cumprod(1+configs)
      period <- 72
      
      roll72CumAnn <- (cumRets/lag(cumRets, period))^(252/period) - 1
      roll72SD <- sapply(X = configs, runSD, n=period)*sqrt(252)
      
      
      sd_f_factor <- 2.5
      modSharpe <- roll72CumAnn/roll72SD^sd_f_factor
      monthlyModSharpe <- modSharpe[endpoints(modSharpe, on="months"),]
      
      findMax <- function(data) {
        return(data==max(data))
      }
      
      weights <- t(apply(monthlyModSharpe, 1, findMax))
      weights <- weights*1
      weights <- xts(weights, order.by=as.Date(rownames(weights)))
      weights[is.na(weights)] <- 0
      weights$zeroes <- 1-rowSums(weights)
      configs$zeroes <- 0
      
      
      stratRets <- Return.portfolio(R = configs, weights = weights)
      rbind(table.AnnualizedReturns(stratRets), maxDrawdown(stratRets))
      charts.PerformanceSummary(stratRets)
      
      
      stratAndComponents <- merge(returns, stratRets, join='inner')
      charts.PerformanceSummary(stratAndComponents)
      rbind(table.AnnualizedReturns(stratAndComponents), maxDrawdown(stratAndComponents))
      apply.yearly(stratAndComponents, Return.cumulative)
      
      
      weightSPY <- apply(monthlyModSharpe, 1, which.max)
      weightSPY <- do.call(rbind, weightSPY)
      weightSPY <- (weightSPY-1)*.05
      align <- cbind(weightSPY, stratRets)
      align <- na.locf(align)
      apacoba <- chart.TimeSeries(align[,1], date.format="%Y", ylab="Weight SPY", main="Weight of SPY in SPY-TLT pair")
        
    #})

  }
  
  output$plot <- renderPlot({
    input$get
    apacoba
  })


  output$result <- renderPrint({
    if (input$get == 0)
      return(NULL)
    symbols(input$symbol)
  }

    )
}

shinyApp(ui = ui, server = server)
