test_df <- tibble::tribble(
  ~date, ~ticker, ~return,
  as.Date("2023-01-01"), "AAPL", 0.01,
  as.Date("2023-01-01"), "GOOGL", 0.02,
  as.Date("2023-01-01"), "MSFT", 0.015,
  as.Date("2023-01-02"), "AAPL", -0.005,
  as.Date("2023-01-02"), "GOOGL", 0.01,
  as.Date("2023-01-02"), "MSFT", 0.02,
  as.Date("2023-01-03"), "AAPL", 0.02,
  as.Date("2023-01-03"), "GOOGL", NA_real_,
  as.Date("2023-01-03"), "MSFT", 0.01
)

test_that("calculates basket returns correctly with complete data", {
  basket_tickers <- c("AAPL", "GOOGL")
  
  actual <- calculate_basket_returns(test_df, basket_tickers)
  
  expected <- tibble::tribble(
    ~date, ~basket_return,
    as.Date("2023-01-01"), 0.015,
    as.Date("2023-01-02"), 0.0025,
    as.Date("2023-01-03"), 0.02
  )
  
  expect_equal(actual, expected)
})

test_that("calculates basket returns correctly with single ticker", {
  basket_tickers <- c("AAPL")
  
  actual <- calculate_basket_returns(test_df, basket_tickers)
  
  expected <- tibble::tribble(
    ~date, ~basket_return,
    as.Date("2023-01-01"), 0.01,
    as.Date("2023-01-02"), -0.005,
    as.Date("2023-01-03"), 0.02
  )
  
  expect_equal(actual, expected)
})

test_that("includes dates with NA values when all tickers are present", {
  basket_tickers <- c("AAPL", "GOOGL", "MSFT")
  
  actual <- calculate_basket_returns(test_df, basket_tickers)
  
  expected <- tibble::tribble(
    ~date, ~basket_return,
    as.Date("2023-01-01"), 0.015,
    as.Date("2023-01-02"), 0.008333333,
    as.Date("2023-01-03"), 0.015
  )
  
  expect_equal(actual, expected, tolerance = 1e-6)
})

test_that("handles NA values correctly by including dates with NA when na.rm=TRUE", {
  basket_tickers <- c("GOOGL", "MSFT")
  
  actual <- calculate_basket_returns(test_df, basket_tickers)
  
  expected <- tibble::tribble(
    ~date, ~basket_return,
    as.Date("2023-01-01"), 0.0175,
    as.Date("2023-01-02"), 0.015,
    as.Date("2023-01-03"), 0.01
  )
  
  expect_equal(actual, expected)
})

test_that("throws error when basket_tickers is not a character vector", {
  basket_tickers <- c(1, 2, 3)
  
  expect_error(
    calculate_basket_returns(test_df, basket_tickers),
    "^basket_tickers must be a character vector, got: numeric$"
  )
})

test_that("throws error when basket_tickers is empty", {
  basket_tickers <- character(0)
  
  expect_error(
    calculate_basket_returns(test_df, basket_tickers),
    "^basket_tickers cannot be empty$"
  )
})

test_that("throws error when basket_tickers not found in return_data", {
  basket_tickers <- c("TSLA", "NVDA")
  
  expect_error(
    calculate_basket_returns(test_df, basket_tickers),
    "^The following basket_tickers are not found in return_data: TSLA, NVDA$"
  )
})

test_that("throws error when some basket_tickers not found in return_data", {
  basket_tickers <- c("AAPL", "TSLA")
  
  expect_error(
    calculate_basket_returns(test_df, basket_tickers),
    "^The following basket_tickers are not found in return_data: TSLA$"
  )
})

test_that("throws error when no return data found for basket_tickers", {
  # Use a dataframe with data but different tickers
  different_tickers_df <- tibble::tribble(
    ~date, ~ticker, ~return,
    as.Date("2023-01-01"), "META", 0.01,
    as.Date("2023-01-02"), "NFLX", 0.02
  )
  basket_tickers <- c("AAPL", "GOOGL")
  
  expect_error(
    calculate_basket_returns(different_tickers_df, basket_tickers),
    "^The following basket_tickers are not found in return_data: AAPL, GOOGL$"
  )
})

test_that("excludes dates when tickers are completely missing", {
  incomplete_df <- tibble::tribble(
    ~date, ~ticker, ~return,
    as.Date("2023-01-01"), "AAPL", 0.01,
    as.Date("2023-01-02"), "GOOGL", 0.02
  )
  basket_tickers <- c("AAPL", "GOOGL")
  
  expect_error(
    calculate_basket_returns(incomplete_df, basket_tickers),
    "^No dates found with complete return data for all 2 basket tickers$"
  )
})

test_that("returns correct column names", {
  basket_tickers <- c("AAPL")
  
  actual <- calculate_basket_returns(test_df, basket_tickers)
  
  expect_equal(names(actual), c("date", "basket_return"))
})

test_that("preserves date class in output", {
  basket_tickers <- c("AAPL")
  
  actual <- calculate_basket_returns(test_df, basket_tickers)
  
  expect_true(inherits(actual$date, "Date"))
})