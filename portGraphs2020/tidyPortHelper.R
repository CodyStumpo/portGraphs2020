library(tidyverse)
library(tidyquant)
library(magrittr)
library(corrplot)
library(ggrepel)

options(stringsAsFactors = F)

### INPUTS

portfolio = data.frame(shares = c(4,650,200,100,200,
                                  300, 250, 625, 150, 75,
                                  400, 10, 50, 50, 200,
                                  250, 100, 300, 125),
                       symbol = c('AMZN', 'EQAL', 'SCHA', 'RPG', 'AMAL',
                                  "VWO", "IXJ", "SCHF", "LIT", "CQQQ", 
                                  "BOTZ", "SHOP", "HYD", "UST", "IBND",
                                  "GCC", "VNQ", "DJP", "AGGY")
                       )

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


#### GET Portfolio data
quotes = tq_get(portfolio$symbol, from=date1, to= date2) 

## Compute returns
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

stats %<>% left_join(dayN) %>%              #also adds value to stats
  mutate(riskWeight = riskContribution * value / (portSD * portValueN))   

day0 %>% 
  left_join(dayN, by='symbol') %>% 
  mutate(ytd = value.y/value.x - 1,
         expweights = value.y / portValueN) %>% 
  left_join(stats) %>% 
  select(-value.y, -value.x) -> stats

# Present results

## Pretty table of stats
stats %>% 
  mutate_if(is.numeric, round, 2) %>%
  select(symbol, value, expweights, ytd, sd, corr2port, corr2index, beta, eReturn, riskContribution, riskWeight, retRisk, sharpe) %>% View

## Correlation Plot
returns %>% 
  pivot_wider(id_cols = date, names_from = symbol, values_from = daily.returns) %>%
  select_if(is.numeric) %>% 
  cor %>% 
  corrplot(method="square", order="FPC")


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

# mispricing

stats %>% ggplot() +
  coord_cartesian(xlim=c(min(stats$riskContribution,0),max(stats$riskContribution)+.02)) +
  coord_cartesian(ylim=c(0,max(stats$eReturn)+.02)) +
  geom_point(aes(riskContribution, eReturn), color = 'red') +
  geom_text_repel(aes(riskContribution, eReturn, label = symbol)) +
  geom_abline(intercept=libor, slope=(portEReturn -libor) /portSD) + #absolute return
  xlab("Non-Diversified Risk") +
  ylab("E[return]") +
  ggtitle("Mispricing")


# index volatility timeseries

mean_roll_10 = tibbletime::rollify(mean, 10)
indexReturns %>% 
  mutate(AnnualizedTenDayVol = mean_roll_10(abs(daily.returns)) * sqrt(tradingDaysPerYear)) %>%  
  ggplot() + geom_line(aes(x=date, y=AnnualizedTenDayVol)) +
  ggtitle("Annualized 10-day moving average volatilty of Index") +
  ylab("Volatility") +
  geom_hline(yintercept = sd.index)

  # backtest
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
    theme(legend.position = c(0.2, 0.8))
           


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
  
    
    
  
  
  
