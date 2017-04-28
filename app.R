library(shiny)
require(quantmod)
require(PerformanceAnalytics)

ui <- pageWithSidebar(
  headerPanel("Investment Strategy Tool", title = span(
    tagList(icon("line-chart"), "Investment Strategy Tool")
  )),
  
  sidebarPanel(
    helpText("You can select maximum 2 ETF"),
    textInput('symbol', label="Enter ticker symbol", value="SPY;TLT"),
    helpText("Examples: SPY;TLT (separate by semi-colon)"),
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
  mainPanel(  tabsetPanel(
    tabPanel("Returns Performance", plotOutput("plot1")),
    tabPanel("Adjusted Performance", plotOutput("plot2")),
    tabPanel("Weight", plotOutput("plot3"))
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
 
  
  findMax <- function(data) {
    return(data==max(data))
  }
  
  mainfunc <- function(x){
    #stockData <- new.env()
    sim <- x
    #symbols <- getSymbols(sim,env=stockData, from="2013-01-01")
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
    output$plot1 <- renderPlot({
      charts.PerformanceSummary(stratRets)      
    })

    
    
    stratAndComponents <- merge(returns, stratRets, join='inner')
    output$plot2 <- renderPlot({
      charts.PerformanceSummary(stratAndComponents)
    })
    rbind(table.AnnualizedReturns(stratAndComponents), maxDrawdown(stratAndComponents))
    apply.yearly(stratAndComponents, Return.cumulative)
    
    
    weightSPY <- apply(monthlyModSharpe, 1, which.max)
    weightSPY <- do.call(rbind, weightSPY)
    weightSPY <- (weightSPY-1)*.05
    align <- cbind(weightSPY, stratRets)
    align <- na.locf(align)
    output$plot3 <- renderPlot({
      chart.TimeSeries(align[,1], date.format="%Y", ylab=paste("Weight ",dataInput()[1]), main=paste("Weight  in ",input$symbol," pair"))
      
    })
    
  }
  
  
  observeEvent(input$get, {
    mainfunc(dataInput())
  })
}

shinyApp(ui = ui, server = server)
