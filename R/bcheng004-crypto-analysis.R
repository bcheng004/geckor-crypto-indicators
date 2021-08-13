# created by bcheng004
remove(list=ls())
cat("\014")
# init packrat
packrat::init(getwd())
# load libraries
suppressPackageStartupMessages(library(geckor))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(TTR))
suppressPackageStartupMessages(library(quantmod))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(modeltime))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(timetk))
# check API service available
coin_gecko_api_status <- ping()
# Filter for supported coins on coin gecko
if(coin_gecko_api_status){
  coin_tbl <- supported_coins()
  check_coin_tbl_df <- FALSE
  if (check_coin_tbl_df) {
    is.data.frame(coin_tbl)
  }
}
chosen_coin_id <-"cardano"
crypto_ticker <- coin_tbl %>% filter(str_detect(coin_id, glue("^{chosen_coin_id}$"))) %>% select(symbol)
# Current price and market conditions
current_bool <- FALSE
if (current_bool){
  current_price(coin_ids = c("cardano", "algorand", "polkadot", "bitcoin"), vs_currencies = c("usd"))
  current_market(coin_ids = c("cardano", "algorand", "polkadot"), vs_currency = "usd") %>% glimpse()
}
# a look at cardano
cardano_history <- coin_history(coin_id = glue("{chosen_coin_id}"), 
                                vs_currency = "usd", 
                                days = "max")
cardano_history_ohlc <- coin_history_ohlc(coin_id = glue("{chosen_coin_id}"),
                                          vs_currency = "usd", 
                                          days = "max")
history_merge_cols <- c(colnames(cardano_history)[1:3])
cardano_history_total <- merge(cardano_history,cardano_history_ohlc,by=history_merge_cols)
ADA <- xts(cardano_history_total[,c("price_open","price_high","price_low","price_close","total_volume","price")],
                          cardano_history_total$timestamp)
colnames(ADA) <- c(
  glue("{crypto_ticker}.Open"),
  glue("{crypto_ticker}.High"),
  glue("{crypto_ticker}.Low"),
  glue("{crypto_ticker}.Close"),
  glue("{crypto_ticker}.Volume"),
  glue("{crypto_ticker}.Adjusted")
  )
# cardano_history %>% ggplot(aes(timestamp, price)) + geom_line() + theme_minimal()

# basic technical indicators

# lookback_window <- 14

# Daily Returns

# cardano_price_ts <- xts(cardano_history$price,cardano_history$timestamp)
# cardano_daily_ret <- dailyReturn(cardano_price_ts)
# cardano_daily_ret %>% ggplot(aes(Index,daily.returns)) + geom_line() + theme_minimal()

# Relative Strength Index

# cardano_rsi <- RSI(cardano_history$price, n = lookback_window, maType = "SMA")
# cardano_rsi_ts <- xts(cardano_rsi,cardano_history$timestamp)
# colnames(cardano_rsi_ts) <- c("rsi")
# cardano_rsi_ts %>% ggplot(aes(Index,rsi)) + geom_line() + theme_minimal()

# quantmod chartSeries
lookback_window <- 14
curr_date <- Sys.Date()
start_date <- curr_date %m+% years(x=-1)
chartSeries(ADA, subset=glue('{start_date}::{curr_date}'),
            theme=chartTheme('black',up.col='green',dn.col='red'),
            TA=c(addBBands(n=lookback_window,sd=2,),addSMA(n=lookback_window,col="black"),addRSI(n=lookback_window)))
candleChart(ADA,subset=glue('{start_date}::{curr_date}'),
            theme=chartTheme('black',up.col='green',dn.col='red'),
            type='candles')
# next take a look at modeltime package for forecasting
interactive <- FALSE
cardano_history_total %>% plot_time_series(timestamp,price_close,.interactive=interactive)
splits <- initial_time_split(cardano_history_total, prop = 0.9)
# Model 1: auto_arima
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine="auto_arima") %>% 
  fit(price_close~timestamp,data=training(splits))
#> frequency = 24 observations per 1 quarter

# Model 2: arima_boost
model_fit_arima_boosted <- arima_boost(min_n=2,learn_rate=0.015) %>%
  set_engine(engine="auto_arima_xgboost") %>%
  fit(price_close~timestamp+as.numeric(timestamp)+factor(month(timestamp,label=TRUE),ordered=F),
      data=training(splits))
#> frequency = 24 observations per 1 quarter

## Take a snapshot of installed packages
packrat::snapshot()
