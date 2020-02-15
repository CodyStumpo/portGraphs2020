# Functions for portgraphs2020

computeIndexReturns <- function(index, date1, date2) {
  indexReturns = index$symbol %>% 
    tq_get(from = date1, to=date2) 
  
  index %>% 
    left_join(indexReturns %>% 
                filter(date==max(date)) %>% 
                select(symbol, adjusted),
              by='symbol') %>%
    mutate(shares = weight / adjusted ) -> indexRich
  
  
  indexReturns %>% 
    left_join(indexRich %>%
                select(symbol, shares)) %>%
    mutate(value = adjusted * shares) %>%
    group_by(date) %>%
    summarise(adjusted = sum(value), symbol="INDEX") %>%
    tq_mutate(select=adjusted,
              mutate_fun = periodReturn,
              period='daily',
              type='log')

}

computePortfolioReturns <- function(portfolio, quotes, indexReturns) {
  returns = quotes %>% 
    group_by(symbol) %>%
    left_join(portfolio) %>% 
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
  
}

#stats is more complicated.  Probably need a few functions as their are a lot of steps that depend on other steps, which also need to be explicit outputs to the UI.
firstStats <- function(returns, tradingDaysPerYear, sd.index, marketSharpe, libor) {
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
    )
}

finalStats <- function(stats, dayN, day0, portSD, portValueN) {
  stats %<>% left_join(dayN) %>%              #also adds value to stats
    mutate(riskWeight = riskContribution * value / (portSD * portValueN))   
  
  day0 %>% 
    left_join(dayN, by='symbol') %>% 
    mutate(ytd = value.y/value.x - 1,
           expweights = value.y / portValueN) %>% 
    left_join(stats) %>% 
    select(-value.y, -value.x) 
}


mispricingPlot <- function(stats, portEReturn, libor, portSD){
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
}

backtestPlot <- function(returns){
  returns %>% 
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
    theme(legend.position = c(0.1, 0.8))
}

indexVolPlot <- function(indexReturns, sd.index, tradingDaysPerYear) {
  mean_roll_10 = tibbletime::rollify(mean, 10)
  indexReturns %>% 
    mutate(AnnualizedTenDayVol = mean_roll_10(abs(daily.returns)) * sqrt(tradingDaysPerYear)) %>%  
    ggplot() + geom_line(aes(x=date, y=AnnualizedTenDayVol)) +
    ggtitle("Annualized 10-day moving average volatilty of Index") +
    ylab("Volatility") +
    geom_hline(yintercept = sd.index)
}

weightsPlot <- function(stats) {
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
}

corPlot <- function(returns){
  ## Correlation Plot
  returns %>% 
    pivot_wider(id_cols = date, names_from = symbol, values_from = daily.returns) %>%
    select_if(is.numeric) %>% 
    cor %>% 
    corrplot(method="square", order="FPC")
}

anglePlot <- function(stats, portSD, portValueN) {
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
    labs(title ="Portfolio Component Risk/Return",
         subtitle = paste0("Portfolio SD = $",scales::comma(portSD * portValueN))
         )
}

efficientFrontierPlot <- function (portSD, portEReturn, sd.index, indexEReturn, marketSharpe, libor){
  data.frame(risk=portSD, return=portEReturn, portfolio="you") %>%
    add_row(risk=sd.index, return=indexEReturn, portfolio="current market") %>%
    ggplot()+
    geom_point(aes(x=risk, y=return, color=portfolio, size=5)) +
    geom_abline(intercept = libor, slope = marketSharpe) +
    ggtitle("Efficient Frontier") +
    guides(size=F) + 
    theme(legend.position = c(0.2, 0.8)) +
    expand_limits(x=0, y=c(0, (marketSharpe * portSD) + libor))
}
