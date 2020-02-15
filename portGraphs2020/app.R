library(shiny)
library(rhandsontable)
library(DT)
library(tidyverse)
library(tidyquant)
library(magrittr)
library(corrplot)
library(ggrepel)

options(stringsAsFactors = F)

source("functions.R")

# Define Constants

portfolio = data.frame(symbol= c("AAPL", "AMZN", "SFIX", "F", "DIS", "TGT"),
                       shares = c (10, 5, 100, 500, 100, 50))

index = data.frame(symbol=c("VT","FWDB","USCI"), 
                   weight = c(.7,.2,.1))

date1=Sys.Date()-365
date2=Sys.Date()
marketSharpe = 0.6 #long-term average
libor=.02 #1-year libor around Feb 2020
tradingDaysPerYear = 251
lambda = 0.94 #from RiskMetrics


## Index
computeIndexReturns(index, date1, date2) -> indexReturns 
# could move this to on-run os it doesn't crash app if it doesn't work. 
# Adds < 1/2 second to run

###### START ACTUAL APP

ui <- fluidPage(
    
    titlePanel("CAPM Portfolio Analytics"),
    
    sidebarLayout(
        sidebarPanel(width=3,
            rHandsontableOutput('portfolio'),
            helpText("You can edit the above table, including adding and subtracting rows."),
            helpText("Enter symbols that have quotes every day of the last year. Enter a non-0 number for shares."),
            actionButton("Run", "Run"),
            br(),
            textOutput("portValue"),
            br(),
            textOutput("portEReturn"),
            textOutput("portSD"),
            textOutput("portSharpe"),
            br(),
            textOutput("indexEReturn"),
            textOutput("sd.index"),
            textOutput("indexSharpe")
            
            ),
    
    mainPanel(
        tabsetPanel(
        tabPanel("Charts",
                 plotOutput("mispricing"),
                 plotOutput("backtest"),
                 plotOutput("volatility"),
                 plotOutput("ef"),
                 plotOutput("correlation"),
                 plotOutput("angle"),
                 plotOutput("weights")
                 ),
        tabPanel("Interpretation", includeMarkdown("interpretation.Rmd")),
        tabPanel("Statistics", DTOutput("answer")),
        tabPanel("Glossary", includeMarkdown("explanation.Rmd"))
        ) # end tabsetPanel
        ) # end MainPanel
    ) # end sidebarLayout
    ) # end fluidPage

server = function(input, output, session) {
    
    values <- reactiveValues()
    
    output$portfolio <- renderRHandsontable({
        rhandsontable(portfolio)
    })
    
    observeEvent(input$Run, {
        
        values$table1 <-  hot_to_r(input$portfolio)
        #### GET Portfolio data
        quotes = tq_get(values$table1$symbol, from=date1, to= date2) 
        ## Compute returns
        computePortfolioReturns(values$table1, quotes, indexReturns) -> returns
        
        # Now we have one tidy dataframe, called returns, with daily and cumulative returns for every symbol in the portfolio as well as the portfolio itself, and the index
        
        ### Compute summary statistics
        
        sd.index = returns %>% filter(symbol=="INDEX") %>% summarise(sdi=sd(daily.returns) * sqrt(tradingDaysPerYear)) %>% pull(sdi)
        day0 = returns %>% filter(date==min(date)) %>% select(symbol, value)
        dayN = returns %>% filter(date==max(date)) %>% select(symbol, value)
        
        firstStats(returns, tradingDaysPerYear, sd.index, marketSharpe, libor) -> stats
        
        portEReturn = stats %>% filter(symbol == "PORT") %>% pull(eReturn)
        portSD = stats %>% filter(symbol=="PORT") %>% pull(sd)
        portValueN = dayN %>% filter(symbol=="PORT") %>% pull(value)
        portSharpe = stats %>% filter(symbol=="PORT") %>% pull(sharpe)
        indexEReturn = stats %>% filter(symbol == "INDEX") %>% pull(eReturn)
        indexSharpe <- stats %>% filter(symbol=="INDEX") %>% pull(sharpe)
        
        finalStats(stats, dayN, day0, portSD, portValueN) -> stats
        
        # Present results
        
        output$portValue <- renderText(paste("Portfolio Value:", round(portValueN, 0)))
        output$portEReturn <- renderText(paste("Portfolio E[Return]:", scales::percent(portEReturn,.1)))
        output$portSD <- renderText(paste("Portfolio Risk:", scales::percent(portSD,.1)))
        output$portSharpe <- renderText(paste("Portfolio Sharpe:", round(portSharpe, 3)))
        output$indexEReturn <- renderText(paste("Market E[Return]:", scales::percent(indexEReturn,.1)))
        output$sd.index <- renderText(paste("Market Risk:", scales::percent(sd.index,.1)))
        output$indexSharpe <- renderText(paste("Market Sharpe:", round(indexSharpe, 3)))
        
        ## Pretty table of stats
        output$answer <- renderDT({
          DT::datatable(
            stats %>% 
              mutate_if(is.numeric, round, 2) %>%
              select(symbol, value, expweights, ytd, sd, corr2port, corr2index, beta, eReturn, riskContribution, riskWeight, retRisk, sharpe), 
            options=list(pageLength=25))
          })
        
        ## Plots
        output$mispricing <- renderPlot({
          mispricingPlot(stats, portEReturn, libor, portSD)
        })
        
        
        output$backtest <-   renderPlot({
          backtestPlot(returns)
        })
        
        output$volatility <- renderPlot({
          indexVolPlot(indexReturns, sd.index, tradingDaysPerYear)
        })
        
        output$weights <- renderPlot({
           weightsPlot(stats)
        })
        
        output$correlation <- renderPlot({
          corPlot(returns)
        })
        
        output$angle <- renderPlot({
          anglePlot(stats, portSD, portValueN)
        })
        
        output$ef <- renderPlot({
          efficientFrontierPlot(portSD, portEReturn, sd.index, indexEReturn, marketSharpe, libor)
        })
    })
    
}

shinyApp(ui, server)
