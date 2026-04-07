test_that("enriches df with cumulative return and cone columns", {
  returns <- tibble::tibble(
    date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
    return = rep(0.01, 12)
  )
  result <- calculate_performance_cone(returns, sharpe_ratio = 0.5,
                                       volatility = 0.10)
  expected_cols <- c("date", "return", "cumulative_return", "t_years",
                     "center", "upper_1", "lower_1", "upper_2", "lower_2",
                     "upper_3", "lower_3")
  expect_true(all(expected_cols %in% names(result)))
  expect_equal(nrow(result), 12)
})

test_that("cumulative_return matches cumulate_returns", {
  returns <- tibble::tibble(
    date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 4),
    return = c(0.02, -0.01, 0.03, 0.005)
  )
  result <- calculate_performance_cone(returns, sharpe_ratio = 0.5,
                                       volatility = 0.10)
  # Manual: (1.02)(0.99) - 1, (1.02)(0.99)(1.03) - 1, ...
  expected <- c(
    0.02,
    1.02 * 0.99 - 1,
    1.02 * 0.99 * 1.03 - 1,
    1.02 * 0.99 * 1.03 * 1.005 - 1
  )
  expect_equal(result$cumulative_return, expected, tolerance = 1e-12)
})

test_that("cone starts at zero at t = 0", {
  returns <- tibble::tibble(
    date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    return = rep(0.01, 24)
  )
  result <- calculate_performance_cone(returns, sharpe_ratio = 0.5,
                                       volatility = 0.15)
  # First row has t_years = 0
  expect_equal(result$t_years[1], 0)
  expect_equal(result$center[1], 0)
  expect_equal(result$upper_1[1], 0)
  expect_equal(result$lower_1[1], 0)
  expect_equal(result$upper_3[1], 0)
  expect_equal(result$lower_3[1], 0)
})

test_that("center line matches geometric growth formula", {
  sr <- 0.5
  vol <- 0.15
  mu_g <- sr * vol - (vol^2) / 2
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = 61)
  returns <- tibble::tibble(date = dates, return = rep(0, 61))
  result <- calculate_performance_cone(returns, sharpe_ratio = sr,
                                       volatility = vol)
  # Verify at last observation using actual t_years value
  t_end <- result$t_years[61]
  expect_equal(result$center[61], exp(mu_g * t_end) - 1, tolerance = 1e-10)
})

test_that("bands are symmetric in log space and match vol * sqrt(t)", {
  sr <- 0.8
  vol <- 0.12
  mu_g <- sr * vol - (vol^2) / 2
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = 36)
  returns <- tibble::tibble(date = dates, return = rep(0, 36))
  result <- calculate_performance_cone(returns, sharpe_ratio = sr,
                                       volatility = vol)
  # Pick a row well into the series (row 25)
  t <- result$t_years[25]
  log_upper <- log(1 + result$upper_1[25])
  log_center <- log(1 + result$center[25])
  log_lower <- log(1 + result$lower_1[25])
  # Symmetric around center
  expect_equal(log_upper - log_center, log_center - log_lower,
               tolerance = 1e-12)
  # Distance equals vol * sqrt(t)
  expect_equal(log_upper - log_center, vol * sqrt(t), tolerance = 1e-10)
})

test_that("attributes carry correct derived assumptions", {
  sr <- 0.6
  vol <- 0.20
  returns <- tibble::tibble(
    date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 12),
    return = rep(0, 12)
  )
  result <- calculate_performance_cone(returns, sharpe_ratio = sr,
                                       volatility = vol)
  # mu_a = 0.6 * 0.20 = 0.12
  # mu_g = 0.12 - 0.04/2 = 0.10
  # variance_drain = 0.04/2 = 0.02
  expect_equal(attr(result, "sharpe_ratio"), 0.6)
  expect_equal(attr(result, "volatility"), 0.20)
  expect_equal(attr(result, "mu_arithmetic"), 0.12, tolerance = 1e-12)
  expect_equal(attr(result, "mu_geometric"), 0.10, tolerance = 1e-12)
  expect_equal(attr(result, "variance_drain"), 0.02, tolerance = 1e-12)
})

test_that("negative Sharpe produces declining center line", {
  sr <- -0.3
  vol <- 0.15
  mu_g <- sr * vol - (vol^2) / 2  # -0.045 - 0.01125 = -0.05625
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = 61)
  returns <- tibble::tibble(date = dates, return = rep(0, 61))
  result <- calculate_performance_cone(returns, sharpe_ratio = sr,
                                       volatility = vol)
  t_end <- result$t_years[61]
  expected <- exp(mu_g * t_end) - 1
  expect_equal(result$center[61], expected, tolerance = 1e-10)
  expect_true(result$center[61] < 0)
})
