library(ggplot2)

# Script Params ----

roll_window <- 36L

tickers <- c(
  "IWB", #R1000
  "IWD", #R1000V
  "IWF", #R1000G
  "IWM", #R2000
  "IWN", #R2000V
  "IWO", #R2000G
  "MTUM", #MSCI Momentum
  "USMV", #MSCI USA Min Vol
  "QUAL" #MSCI USA Quality
)

# Data Pulls ----
all_data <- fetch_adjusted_prices(tickers)

# Data Pre-processing ----
# note - this is a long-form dataframe indexed by `date` and `ticker` and the values are `adjusted_close`
# here we filter for sufficient price history
all_data <- all_data %>% 
  dplyr::arrange(ticker, date) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::add_count() %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n > roll_window) %>% 
  dplyr::select(-n)

# daily log returns
return_data <- all_data %>%
  dplyr::arrange(ticker, date) %>%
  dplyr::group_by(ticker) %>%
  dplyr::mutate(return = log(adjusted_close / dplyr::lag(adjusted_close))) %>%
  dplyr::ungroup()

# isolate market returns and rejoin as 'market_return'
market_returns <- return_data %>%
  dplyr::filter(ticker == "IWB") %>%
  dplyr::select(date, market_return = return)

return_data <- return_data %>%
  dplyr::left_join(market_returns, by = "date")

# Rolling Linear Regression ----

simple_regression <- return_data %>%
  dplyr::arrange(ticker, date) %>%
  dplyr::group_by(ticker) %>%
  dplyr::mutate(
    # Calculate rolling regression per group, not as a list-column
    roll_res = list(roll::roll_lm(
      x = market_return,
      y = return,
      width = roll_window
    )),
    alpha = roll_res[[1]]$coefficients[, "(Intercept)"],
    beta = roll_res[[1]]$coefficients[, "x1"]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    -roll_res,
    -adjusted_close,
    -return,
    -market_return
  ) 