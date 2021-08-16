# created by bcheng004
remove(list=ls())
cat("\014")
# init packrat
# take a look at renv
renv::init(bare=TRUE)
# load libraries
if (!requireNamespace("glue")){
  install.packages("glue")
} else {
  suppressPackageStartupMessages(library("glue"))
}
packages_no_glue <- c("geckor","dplyr","ggplot2","stringr",
                      "TTR","quantmod","lubridate","xgboost",
                      "tidymodels","modeltime","tidyverse","timetk")
for (p in packages_no_glue){
  if (!requireNamespace(glue('{p}'))){
    install.packages(glue('{p}'))
  }
}
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

# Model 3: Error Trend Season (ets) exponential smoothing state space
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine="ets") %>% fit(price_close~timestamp,data=training(splits))
#> frequency = 24 observations per 1 quarter

# Model 4: prophet
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine="prophet") %>%
  fit(price_close~timestamp,data=training(splits))

# Model 5: Linear Regression (Parsnip)
model_fit_lm <- linear_reg() %>%
  set_engine(engine="lm") %>%
  fit(price_close~as.numeric(timestamp)+factor(month(timestamp,label=TRUE),ordered=FALSE),
      data=training(splits))

# Model 6: MARS (Workflow)
model_spec_mars <- mars(mode="regression") %>%
  set_engine(engine="earth")
recipe_spec <- recipe(price_close~timestamp,data=training(splits)) %>%
  step_date(timestamp,features="month",ordinal=FALSE) %>%
  step_mutate(timestamp_num=as.numeric(timestamp)) %>%
  step_normalize(timestamp_num) %>%
  step_rm(timestamp)
wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))

# Model Table
models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  wflw_fit_mars
)

# Calibrate model to test set
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data=testing(splits))

calibration_tbl %>% modeltime_forecast(
  new_data=testing(splits),
  actual_data=cardano_history_total
) %>%
  plot_modeltime_forecast(
    .legend_max_width=25,
    .interactive=interactive
  )

# metrics
calibration_tbl %>% 
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive=interactive
  )

# refit to full dataset & forecast forward
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data=cardano_history_total)
refit_tbl %>% modeltime_forecast(h="1 month",actual_data=cardano_history_total,conf_interval = 0.95) %>%
  plot_modeltime_forecast(
    .legend_max_width=25,
    .interactive=interactive
  )

# Take snapshot with renv
