test_that("basic CTR decomposition identity holds: fund_return = alpha + factor_ctrs + residual", {
  # 5 dates, 2 factors, rolling window of 3 => first valid beta at date 3
  # Lagged: beta from date 3 applies to date 4, beta from date 4 applies to date 5
  dates <- as.Date("2020-01-01") + (0:4) * 31
  factor_cols <- c("f1", "f2")

  # Fake coefficient matrix (5 rows, intercept + 2 factors)
  # First 2 rows are NA (warmup), rows 3-5 have valid betas
  coef_mat <- matrix(NA_real_, nrow = 5, ncol = 3)
  colnames(coef_mat) <- c("(Intercept)", "f1", "f2")
  coef_mat[3, ] <- c(0.001, 0.5, 0.3)
  coef_mat[4, ] <- c(0.002, 0.6, 0.2)
  coef_mat[5, ] <- c(0.001, 0.4, 0.4)

  rolling_fit <- list(coefficients = coef_mat)

  fund_returns <- tibble::tibble(
    date = dates,
    return = c(0.01, 0.02, -0.01, 0.03, 0.015)
  )

  factor_returns <- tibble::tibble(
    date = dates,
    f1 = c(0.005, 0.01, -0.005, 0.02, 0.01),
    f2 = c(0.003, 0.008, 0.002, -0.01, 0.005)
  )

  result <- calculate_ctr(
    rolling_fit, dates, fund_returns, factor_returns, factor_cols
  )

  # Should have 2 rows: beta at date 3 -> date 4, beta at date 4 -> date 5
  expect_equal(nrow(result), 2)
  expect_equal(result$date, dates[4:5])

  # Identity check: fund_return = alpha_ctr + f1_ctr + f2_ctr + residual
  identity <- result$alpha_ctr + result$f1_ctr + result$f2_ctr + result$residual
  expect_equal(identity, result$fund_return, tolerance = 1e-12)

  # total_explained = alpha + factor ctrs
  expect_equal(
    result$total_explained,
    result$alpha_ctr + result$f1_ctr + result$f2_ctr,
    tolerance = 1e-12
  )
})

test_that("lag logic applies previous period betas, not current", {
  dates <- as.Date("2020-01-01") + (0:3) * 31

  # Beta at date 2: intercept=0, f1=1.0, f2=0.0
  # Beta at date 3: intercept=0, f1=0.0, f2=1.0
  coef_mat <- matrix(NA_real_, nrow = 4, ncol = 3)
  colnames(coef_mat) <- c("(Intercept)", "f1", "f2")
  coef_mat[2, ] <- c(0, 1.0, 0.0)
  coef_mat[3, ] <- c(0, 0.0, 1.0)
  coef_mat[4, ] <- c(0, 0.5, 0.5)

  rolling_fit <- list(coefficients = coef_mat)

  fund_returns <- tibble::tibble(
    date = dates,
    return = c(0.01, 0.02, 0.03, 0.04)
  )

  factor_returns <- tibble::tibble(
    date = dates,
    f1 = c(0.01, 0.02, 0.05, 0.03),
    f2 = c(0.01, 0.01, 0.01, 0.07)
  )

  result <- calculate_ctr(
    rolling_fit, dates, fund_returns, factor_returns, c("f1", "f2")
  )

  # Row 1: beta from date 2 (f1=1, f2=0) applied to date 3 returns
  # f1_ctr = 1.0 * 0.05 = 0.05, f2_ctr = 0.0 * 0.01 = 0.0
  expect_equal(result$f1_ctr[1], 0.05, tolerance = 1e-12)
  expect_equal(result$f2_ctr[1], 0.0, tolerance = 1e-12)

  # Row 2: beta from date 3 (f1=0, f2=1) applied to date 4 returns
  # f1_ctr = 0.0 * 0.03 = 0.0, f2_ctr = 1.0 * 0.07 = 0.07
  expect_equal(result$f1_ctr[2], 0.0, tolerance = 1e-12)
  expect_equal(result$f2_ctr[2], 0.07, tolerance = 1e-12)
})

test_that("no intercept case sets alpha_ctr to zero", {
  dates <- as.Date("2020-01-01") + (0:3) * 31

  coef_mat <- matrix(NA_real_, nrow = 4, ncol = 1)
  colnames(coef_mat) <- "f1"
  coef_mat[2, ] <- 0.8
  coef_mat[3, ] <- 1.2
  coef_mat[4, ] <- 0.5

  rolling_fit <- list(coefficients = coef_mat)

  fund_returns <- tibble::tibble(
    date = dates,
    return = c(0.01, 0.02, 0.03, 0.04)
  )

  factor_returns <- tibble::tibble(
    date = dates,
    f1 = c(0.01, 0.02, 0.03, 0.04)
  )

  result <- calculate_ctr(
    rolling_fit, dates, fund_returns, factor_returns, "f1"
  )

  expect_true(all(result$alpha_ctr == 0))
  expect_equal(result$f1_ctr[1], 0.8 * 0.03, tolerance = 1e-12)
})

test_that("single factor model works", {
  dates <- as.Date("2020-01-01") + (0:2) * 31

  coef_mat <- matrix(NA_real_, nrow = 3, ncol = 2)
  colnames(coef_mat) <- c("(Intercept)", "mkt")
  coef_mat[1, ] <- c(0.001, 0.9)
  coef_mat[2, ] <- c(0.002, 1.1)
  coef_mat[3, ] <- c(0.001, 0.8)

  rolling_fit <- list(coefficients = coef_mat)

  fund_returns <- tibble::tibble(
    date = dates,
    return = c(0.05, -0.03, 0.02)
  )

  factor_returns <- tibble::tibble(
    date = dates,
    mkt = c(0.04, -0.02, 0.015)
  )

  result <- calculate_ctr(
    rolling_fit, dates, fund_returns, factor_returns, "mkt"
  )

  expect_equal(nrow(result), 2)
  expect_true("mkt_ctr" %in% colnames(result))

  # Row 1: beta from date 1 applied to date 2
  expect_equal(result$alpha_ctr[1], 0.001, tolerance = 1e-12)
  expect_equal(result$mkt_ctr[1], 0.9 * -0.02, tolerance = 1e-12)

  identity <- result$alpha_ctr + result$mkt_ctr + result$residual
  expect_equal(identity, result$fund_return, tolerance = 1e-12)
})

test_that("input validation catches bad inputs", {
  dates <- as.Date("2020-01-01") + 0:2 * 31
  coef_mat <- matrix(1, nrow = 3, ncol = 2)
  colnames(coef_mat) <- c("(Intercept)", "f1")
  rolling_fit <- list(coefficients = coef_mat)
  fund_returns <- tibble::tibble(date = dates, return = c(0.01, 0.02, 0.03))
  factor_returns <- tibble::tibble(date = dates, f1 = c(0.01, 0.02, 0.03))

  # Wrong dates length
  expect_error(
    calculate_ctr(
      rolling_fit, dates[1:2], fund_returns, factor_returns, "f1"
    ),
    "length"
  )

  # Missing factor column
  expect_error(
    calculate_ctr(
      rolling_fit, dates, fund_returns, factor_returns, "f_missing"
    ),
    "missing"
  )

  # Not a list
  expect_error(
    calculate_ctr(
      "not_a_fit", dates, fund_returns, factor_returns, "f1"
    ),
    "list"
  )
})
