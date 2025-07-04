test_df <- tibble::tribble(
  ~date, ~ticker, ~adjusted_close,
  as.Date("2023-01-01"), "AAPL", 100.0,
  as.Date("2023-01-02"), "AAPL", 105.0,
  as.Date("2023-01-03"), "AAPL", 110.25,
  as.Date("2023-01-01"), "MSFT", 200.0,
  as.Date("2023-01-02"), "MSFT", 210.0,
  as.Date("2023-01-03"), "MSFT", 220.5
)

test_that("calculates log returns with default parameters", {
  actual <- calculate_log_returns(test_df)
  expected_returns <- c(
    NA, log(105.0/100.0), log(110.25/105.0),
    NA, log(210.0/200.0), log(220.5/210.0)
  )
  expect_equal(actual$return, expected_returns, tolerance = 1e-10)
  expect_equal(nrow(actual), 6)
  expect_true("return" %in% names(actual))
})

test_that("calculates log returns with custom column names", {
  custom_df <- test_df %>% 
    dplyr::rename(my_date = date, my_ticker = ticker, my_price = adjusted_close)
  
  actual <- calculate_log_returns(
    custom_df, 
    date_col = "my_date", 
    ticker_col = "my_ticker", 
    price_col = "my_price",
    return_col = "my_return"
  )
  
  expected_returns <- c(
    NA, log(105.0/100.0), log(110.25/105.0),
    NA, log(210.0/200.0), log(220.5/210.0)
  )
  expect_equal(actual$my_return, expected_returns, tolerance = 1e-10)
  expect_true("my_return" %in% names(actual))
})

test_that("preserves original data structure and adds return column", {
  actual <- calculate_log_returns(test_df)
  expected_cols <- c("date", "ticker", "adjusted_close", "return")
  expect_equal(names(actual), expected_cols)
  expect_equal(actual$date, test_df$date)
  expect_equal(actual$ticker, test_df$ticker)
  expect_equal(actual$adjusted_close, test_df$adjusted_close)
})

test_that("groups by ticker correctly", {
  actual <- calculate_log_returns(test_df)
  aapl_returns <- actual[actual$ticker == "AAPL", ]$return
  msft_returns <- actual[actual$ticker == "MSFT", ]$return
  
  expect_true(is.na(aapl_returns[1]))
  expect_true(is.na(msft_returns[1]))
  expect_false(is.na(aapl_returns[2]))
  expect_false(is.na(msft_returns[2]))
})

test_that("stops when date column is not Date type", {
  bad_date_df <- test_df %>% 
    dplyr::mutate(date = as.character(date))
  expect_error(
    calculate_log_returns(bad_date_df),
    "^Column 'date' must be of type Date. Received: character$"
  )
})

test_that("stops when price column is not numeric", {
  bad_price_df <- test_df %>% 
    dplyr::mutate(adjusted_close = as.character(adjusted_close))
  expect_error(
    calculate_log_returns(bad_price_df),
    "^Column 'adjusted_close' must be numeric. Received: character$"
  )
})

test_that("handles single ticker correctly", {
  single_ticker_df <- test_df %>% dplyr::filter(ticker == "AAPL")
  actual <- calculate_log_returns(single_ticker_df)
  expected_returns <- c(NA, log(105.0/100.0), log(110.25/105.0))
  expect_equal(actual$return, expected_returns, tolerance = 1e-10)
  expect_equal(nrow(actual), 3)
})

test_that("handles single observation per ticker correctly", {
  single_obs_df <- test_df %>% dplyr::filter(date == as.Date("2023-01-01"))
  actual <- calculate_log_returns(single_obs_df)
  expect_true(all(is.na(actual$return)))
  expect_equal(nrow(actual), 2)
})