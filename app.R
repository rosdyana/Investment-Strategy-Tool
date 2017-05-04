library(shiny)
require(quantmod)
require(PerformanceAnalytics)

ui <- pageWithSidebar(
  headerPanel("Investment Strategy Tool", title = span(
    tagList(icon("line-chart"), "Investment Strategy Tool")
  )),

  sidebarPanel(
    width = 3,
    helpText("Select two ETF to compare"),
    textInput('symbol', label="Enter ticker symbol", value="SPY;TLT"),
    helpText("Examples: SPY;TLT (separate by semi-colon)"),
    dateRangeInput(
      "dates",
      "Compare to historic returns from",
      start = "2012-01-01",
      end = Sys.Date(),
      format = 'yyyy-mm-dd'
    ),
    textInput('weight',label = "Enter weight factor", value="0.05"),
    actionButton("get", "Run", icon("check-circle")),
    actionButton("about", "About", icon("info-circle")),
    hr()
  ),
  mainPanel(  tabsetPanel(
    tabPanel("Returns Performance", plotOutput("plot1")),
    tabPanel("Adjusted Performance", plotOutput("plot2")),
    tabPanel("Weight", plotOutput("plot3"))
  ),
  downloadButton("downloadPDF", "Download Report"))
)

server <- function(input, output) {
  stockData <- new.env()

  #*****************************************************************
  # get data
  #******************************************************************
  # symbols
  dataInput <- reactive({
    if (input$get == 0)
      return(NULL)
    return(isolate({
      getSymbols(input$symbol, src = "yahoo",env=stockData, from=input$dates[1])
    }))
  })

  # dates
  datesInput <- reactive({
    if (input$get == 0)
      return(NULL)

    return(isolate({
      paste0(input$dates[1], "::",  input$dates[2])
    }))
  })

  # weight
  getWeight <- reactive({
    if (input$get == 0)
      return(NULL)
    return(isolate({
      as.numeric(input$weight)
    }))
  })


  findMax <- function(data) {
    return(data==max(data))
  }

  #*****************************************************************
  # MAIN FUNCTION
  #******************************************************************
  mainfunc <- function(x){
    print("Do the main function\n")
    sim <- x
    Data <- NULL
    validate(need(sim != "", label = "stock"))
    for (i in 1:length(sim)) {
      Data <- cbind(Data,Return.calculate((get(sim[i], pos=stockData)[,6])))
    }
    returns <- Data[-1,]
    configs <- list()
    for(i in 1:21) {
      weight1 <- (i-1)*getWeight()
      weight2 <- 1-weight1
      config <- Return.portfolio(R = returns, weights=c(weight1, weight2), rebalance_on = "months")
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

    weight1 <- apply(monthlyModSharpe, 1, which.max)
    weight1 <- do.call(rbind, weight1)
    weight1 <- (weight1-1)*.05
    align <- cbind(weight1, stratRets)
    align <- na.locf(align)
    output$plot3 <- renderPlot({
      chart.TimeSeries(align[,1], date.format="%Y", ylab=paste("Weight ",dataInput()[1]), main=paste("Weight  in ",input$symbol," pair"))
    })
    # Download pdf report
    output$downloadPDF <- downloadHandler(
      filename = 'report.pdf',
      content = function(file) {
        pdf(file = file, width=8.5, height=11)

        charts.PerformanceSummary(stratRets)
        charts.PerformanceSummary(stratAndComponents)
        chart.TimeSeries(align[,1], date.format="%Y", ylab=paste("Weight ",dataInput()[1]), main=paste("Weight  in ",input$symbol," pair"))
        dev.off()
      }
    )
    outputOptions(output, "downloadPDF", suspendWhenHidden=FALSE)
    print("end of main function\n")
  }


  observeEvent(input$get, {
    mainfunc(dataInput())
  })

  #*****************************************************************
  # About
  #******************************************************************
  observeEvent(input$about, {
    showModal(modalDialog(
      title = span(tagList(icon("info-circle"), "About")),
      tags$div(
        HTML(
          "<img src='https://avatars1.githubusercontent.com/u/4516635?v=3&s=460' width=150><br/><br/>",
          "<p>Developer : <a href='https://github.com/rosdyana' target=blank>Rosdyana Kusuma</a></br>Email : <a href=mailto:rosdyana.kusuma@gmail.com>rosdyana.kusuma@gmail.com</a></br>linkedin : <a href='https://www.linkedin.com/in/rosdyanakusuma/' target=blank>Open me</a></p>"
        )
      ),
      easyClose = TRUE
    ))
  })


}

shinyApp(ui = ui, server = server)
