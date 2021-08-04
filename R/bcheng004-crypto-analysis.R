# created by bcheng004
remove(list=ls())
cat("\014")
# init packrat
packrat::init(getwd())
## Take a snapshot of installed packages
packrat::snapshot()
# load libraries
library(geckor)
library(dplyr)
library(ggplot2)
library(stringr)
library(TTR)
library(quantmod)
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
coin_tbl %>% filter(str_detect(symbol, "btc"))
# Current price and market conditions
current_price(coin_ids = c("cardano", "algorand", "polkadot", "bitcoin"), vs_currencies = c("usd"))
current_market(coin_ids = c("cardano", "algorand", "polkadot"), vs_currency = "usd") %>% glimpse()
# a look at cardano
cardano_history <- coin_history(coin_id = "cardano", 
                                vs_currency = "usd", 
                                days = "max")
# head(cardano_history)
cardano_history %>% ggplot(aes(timestamp, price)) + geom_line() + theme_minimal()
# basic technical indicators
lookback_window <- 14
# Daily Returns
cardano_ts <- xts(cardano_history$price,cardano_history$timestamp)
cardano_daily_ret <- dailyReturn(cardano_ts)
cardano_daily_ret %>% ggplot(aes(Index,daily.returns)) + geom_line() + theme_minimal()
# Relative Strength Index
cardano_rsi <- RSI(cardano_history$price, n = lookback_window, maType = "SMA")
cardano_rsi_ts <- xts(cardano_rsi,cardano_history$timestamp)
colnames(cardano_rsi_ts) <- c("rsi")
cardano_rsi_ts %>% ggplot(aes(Index,rsi)) + geom_line() + theme_minimal()
