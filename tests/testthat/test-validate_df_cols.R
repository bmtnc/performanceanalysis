test_df <- tibble::tribble(
  ~date, ~ticker, ~price,
  as.Date("2023-01-01"), "AAPL", 100.0,
  as.Date("2023-01-02"), "AAPL", 105.0,
  as.Date("2023-01-01"), "MSFT", 200.0,
  as.Date("2023-01-02"), "MSFT", 210.0
)

test_that("validates successful operation with valid inputs", {
  required_cols <- c("date", "ticker", "price")
  actual <- validate_df_cols(test_df, required_cols)
  expected <- NULL
  expect_equal(actual, expected)
})

test_that("stops when data is not a data.frame", {
  required_cols <- c("date", "ticker")
  expect_error(
    validate_df_cols(list(a = 1, b = 2), required_cols),
    "^Input data must be a data.frame. Received: list$"
  )
})

test_that("stops when data is matrix", {
  required_cols <- c("date", "ticker")
  expect_error(
    validate_df_cols(matrix(1:4, nrow = 2), required_cols),
    "^Input data must be a data.frame. Received: matrix$"
  )
})

test_that("stops when required_cols is not a character vector", {
  expect_error(
    validate_df_cols(test_df, c(1, 2, 3)),
    "^Input 'required_cols' must be a character vector. Received: numeric$"
  )
})

test_that("stops when data is empty", {
  empty_df <- test_df[0, ]
  required_cols <- c("date", "ticker")
  expect_error(
    validate_df_cols(empty_df, required_cols),
    "^Input data is empty \\(0 rows\\)$"
  )
})

test_that("stops when required columns are missing", {
  required_cols <- c("date", "ticker", "volume", "returns")
  expect_error(
    validate_df_cols(test_df, required_cols),
    "^Required columns missing from data: volume, returns\\. Available columns: date, ticker, price$"
  )
})

test_that("stops when single required column is missing", {
  required_cols <- c("date", "ticker", "volume")
  expect_error(
    validate_df_cols(test_df, required_cols),
    "^Required columns missing from data: volume\\. Available columns: date, ticker, price$"
  )
})

test_that("validates when subset of columns are required", {
  required_cols <- c("date", "ticker")
  actual <- validate_df_cols(test_df, required_cols)
  expected <- NULL
  expect_equal(actual, expected)
})