library(shiny)
library(rhandsontable)
library(DT)
library(tidyverse)
library(tidyquant)
library(magrittr)
library(corrplot)
library(ggrepel)

options(stringsAsFactors = F)

# portfolio = data.frame(symbol = c('AMZN', 'EQAL', 'SCHA', 'RPG', 'AMAL',
#                                   "VWO", "IXJ", "SCHF", "LIT", "CQQQ", 
#                                   "BOTZ", "SHOP", "HYD", "UST", "IBND",
#                                   "GCC", "VNQ", "DJP", "AGGY"),
#                        shares = c(4,650,200,100,200,
#                                   300, 250, 625, 150, 75,
#                                   400, 10, 50, 50, 200,
#                                   250, 100, 300, 125)
#)

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
indexReturns = index$symbol %>% 
    tq_get(from = date1, to=date2) 

index %<>% 
    left_join(indexReturns %>% 
                  filter(date==max(date)) %>% 
                  select(symbol, adjusted),
              by='symbol') %>%
    mutate(shares = weight / adjusted )


indexReturns %<>% 
    left_join(index %>%
                  select(symbol, shares)) %>%
    mutate(value = adjusted * shares) %>%
    group_by(date) %>%
    summarise(adjusted = sum(value), symbol="INDEX") %>%
    tq_mutate(select=adjusted,
              mutate_fun = periodReturn,
              period='daily',
              type='log')


###### START ACTUAL APP

ui <- fluidPage(
    
    titlePanel("CAPM Portfolio Analytics"),
    
    sidebarLayout(
        sidebarPanel(width=3,
            rHandsontableOutput('portfolio'),
            helpText("You can edit the above table, including adding and subtracting rows. Enter symbols that have quotes every day of the last year."),
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
                 plotOutput("correlation"),
                 plotOutput("angle"),
                 plotOutput("weights")
                 ),
        tabPanel("Interpretation", includeMarkdown("interpretation.Rmd")),
        tabPanel("Statistics", DTOutput("answer")),
        tabPanel("Glossary", includeMarkdown("explanation.Rmd"))
        
        )
    )
    
    )
)

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
        returns = quotes %>% 
            group_by(symbol) %>%
            left_join(values$table1) %>% 
            mutate(value=shares*adjusted) %>%
            tq_mutate(select=value,
                      mutate_fun = periodReturn,
                      period='daily',
                      type='log')
        
        
        returns %<>% 
            left_join(portfolio, by="symbol") %>% 
            group_by(date) %>%
            summarise(value = sum(value), symbol="PORT") %>% 
            tq_mutate(select=value,
                      mutate_fun = periodReturn,
                      period='daily',
                      type='log') %>% 
            bind_rows(indexReturns %>% 
                          select(date, value=adjusted, symbol, daily.returns)) %>%
            bind_rows(returns %>% 
                          select(date, value, symbol, daily.returns)) 
        
        
        returns %<>% 
            group_by(symbol) %>% 
            arrange(symbol, date) %>%
            mutate(cumreturns = exp(cumsum(daily.returns))-1)
        
        # Now we have one tidy dataframe, called returns, with daily and cumulative returns for every symbol in the portfolio as well as the portfolio itself, and the index
        
        ### Compute summary statistics
        
        sd.index = returns %>% filter(symbol=="INDEX") %>% summarise(sdi=sd(daily.returns) * sqrt(tradingDaysPerYear)) %>% pull(sdi)
        
        returns %>% 
            left_join(returns %>% filter(symbol=="PORT") %>% select(date, portReturns = daily.returns), by='date') %>% 
            left_join(returns %>% filter(symbol=="INDEX") %>% select(date, indexReturns = daily.returns), by='date') %>% 
            select(date, value, symbol=symbol.x, daily.returns, cumreturns, portReturns, indexReturns) %>% 
            group_by(symbol) %>%
            summarise(sd = sd(daily.returns) * sqrt(tradingDaysPerYear),
                      corr2port = cor(daily.returns, portReturns),
                      corr2index = cor(daily.returns, indexReturns),
                      riskContribution = sd * corr2port,
                      beta = sd * corr2index / sd.index,
                      eReturn = beta * (sd.index * marketSharpe) + libor,
                      retRisk = (eReturn-libor)/riskContribution,
                      sharpe = (eReturn - libor) / sd
            ) -> stats
        
        
        day0 = returns %>% filter(date==min(date)) %>% select(symbol, value)
        dayN = returns %>% filter(date==max(date)) %>% select(symbol, value)
        
        portEReturn = stats %>% filter(symbol == "PORT") %>% pull(eReturn)
        portSD = stats %>% filter(symbol=="PORT") %>% pull(sd)
        portValueN = dayN %>% filter(symbol=="PORT") %>% pull(value)
        portSharpe = stats %>% filter(symbol=="PORT") %>% pull(sharpe)
        indexEReturn = stats %>% filter(symbol == "INDEX") %>% pull(eReturn)
        indexSharpe <- stats %>% filter(symbol=="INDEX") %>% pull(sharpe)
        
        
        stats %<>% left_join(dayN) %>%              #also adds value to stats
            mutate(riskWeight = riskContribution * value / (portSD * portValueN))   
        
        day0 %>% 
            left_join(dayN, by='symbol') %>% 
            mutate(ytd = value.y/value.x - 1,
                   expweights = value.y / portValueN) %>% 
            left_join(stats) %>% 
            select(-value.y, -value.x) -> stats
        
        # Present results
        
        output$portValue <- renderText(paste("Portfolio Value:", round(portValueN, 0)))
        output$portEReturn <- renderText(paste("Portfolio E[Return]:", scales::percent(portEReturn,.1)))
        output$portSD <- renderText(paste("Portfolio Risk:", scales::percent(portSD,.1)))
        output$portSharpe <- renderText(paste("Portfolio Sharpe:", round(portSharpe, 3)))
        output$indexEReturn <- renderText(paste("Market E[Return]:", scales::percent(indexEReturn,.1)))
        output$sd.index <- renderText(paste("Market Risk:", scales::percent(sd.index,.1)))
        output$indexSharpe <- renderText(paste("Market Sharpe:", round(indexSharpe, 3)))
        
        ## Pretty table of stats
        stats %>% 
            mutate_if(is.numeric, round, 2) %>%
            select(symbol, value, expweights, ytd, sd, corr2port, corr2index, beta, eReturn, riskContribution, riskWeight, retRisk, sharpe) -> tmp
        
      
        
        output$answer <- renderDT({DT::datatable(tmp, options=list(pageLength=25))})
        
        
        output$mispricing <- renderPlot({
            stats %>% filter(! symbol %in% c("PORT", "INDEX")) %>%
                ggplot() +
                coord_cartesian(xlim=c(min(stats$riskContribution,0),max(stats$riskContribution)+.02)) +
                coord_cartesian(ylim=c(0,max(stats$eReturn)+.02)) +
                geom_point(aes(riskContribution, eReturn), color = 'red') +
                geom_text_repel(aes(riskContribution, eReturn, label = symbol)) +
                geom_abline(intercept=libor, slope=(portEReturn -libor) /portSD) + #absolute return
                xlab("Non-Diversified Risk") +
                ylab("E[return]") +
                ggtitle("Mispricing")
        })
        
        
        output$backtest <-   renderPlot({returns %>% 
            filter(symbol %in% c("PORT", "INDEX")) %>%
            bind_rows(returns %>% 
                          filter(symbol %in% c("PORT", "INDEX")) %>%
                          mutate(cumreturns = case_when(symbol=="INDEX" ~ -cumreturns, 
                                                        T ~ cumreturns)) %>%
                          select(symbol, date,cumreturns) %>%
                          group_by(date) %>%
                          summarize_if(is.numeric, sum) %>%
                          mutate(symbol="DIFF") ) %>%
            ggplot(aes (x=date, y =cumreturns, col=symbol)) + 
            geom_line() +
            ggtitle("Back Test of Portfolio vs. Index") +
            ylab("Return") +
            theme(legend.position = c(0.2, 0.8))
        })
        
        mean_roll_10 = tibbletime::rollify(mean, 10)
        
        output$volatility <- renderPlot({indexReturns %>% 
                mutate(AnnualizedTenDayVol = mean_roll_10(abs(daily.returns)) * sqrt(tradingDaysPerYear)) %>%  
                ggplot() + geom_line(aes(x=date, y=AnnualizedTenDayVol)) +
                ggtitle("Annualized 10-day moving average volatilty of Index") +
                ylab("Volatility") +
                geom_hline(yintercept = sd.index)})
        
        output$weights <- renderPlot({
            # Exposure Weight vs. Risk Weight
            
            stats %>% 
                filter(! symbol %in% c("PORT", "INDEX")) %>%
                ggplot() +
                geom_point(aes(x=expweights, y = riskWeight)) +
                geom_text_repel(aes(expweights, riskWeight, label = symbol)) +
                coord_equal() +
                geom_abline(slope=1, intercept=0) +
                ylab("Percent of Portfolio Risk") +
                xlab("Percent of Portfolio Value") +
                ggtitle("Exposure vs. Risk") +
                expand_limits(x = 0, y = 0)
        })
        
        output$correlation <- renderPlot({
            ## Correlation Plot
            returns %>% 
                pivot_wider(id_cols = date, names_from = symbol, values_from = daily.returns) %>%
                select_if(is.numeric) %>% 
                cor %>% 
                corrplot(method="square", order="FPC")
        })
        
        output$angle <- renderPlot({
            #produce angle plot
            
            stats %>% 
                filter(! symbol %in% c('INDEX', 'PORT')) %>%
                mutate(xender = riskContribution * value,
                       theta = acos(corr2port),
                       yender = sin(theta) * sd * value
                ) %>%
                ggplot() + 
                geom_segment(aes(x=0, y = 0, xend=xender, yend=yender, size=sd, color = eReturn)) +
                scale_color_distiller(palette = 'PRGn') +
                coord_equal() +
                xlab("Kept Risk - SD($)") + 
                ylab("Diversified Risk - SD($)") +
                geom_text_repel(aes(label=symbol, x = xender, y = yender)) +
                ggtitle("Portfolio Component Risk/Return")
        })
    })
    
}

shinyApp(ui, server)
