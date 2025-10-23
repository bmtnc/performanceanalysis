test_that("attribution identity: factor + selection = excess", {
  # nolint start
  # fmt: skip
  target_weights <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    F1   = c(0.6, 0.7, 0.5),
    F2   = c(0.4, 0.3, 0.5)
  )
  # nolint end

  # nolint start
  # fmt: skip
  benchmark_weights <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    F1   = c(0.5, 0.5, 0.5),
    F2   = c(0.5, 0.5, 0.5)
  )
  # nolint end

  # nolint start
  # fmt: skip
  factor_returns <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    F1   = c(0.01, 0.02, -0.01, 0.015),
    F2   = c(-0.01, 0.01, 0.02, -0.005)
  )
  # nolint end

  # nolint start
  # fmt: skip
  target_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    return = c(0.005, 0.012, 0.008, 0.003)
  )
  # nolint end

  # nolint start
  # fmt: skip
  benchmark_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    return = c(0.000, 0.015, 0.005, 0.005)
  )
  # nolint end

  result <- calculate_factor_attribution(
    target_weights = target_weights,
    benchmark_weights = benchmark_weights,
    factor_returns = factor_returns,
    target_returns = target_returns,
    benchmark_returns = benchmark_returns,
    factor_cols = c("F1", "F2")
  )

  expect_true(all(abs(result$excess_return - (result$factor_contribution + result$selection_effect)) < 1e-10))
})

test_that("perfect replication: identical weights yield zero factor contribution", {
  # nolint start
  # fmt: skip
  identical_weights <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02")),
    F1   = c(0.6, 0.7),
    F2   = c(0.4, 0.3)
  )
  # nolint end

  # nolint start
  # fmt: skip
  factor_returns <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    F1   = c(0.01, 0.02, -0.01),
    F2   = c(-0.01, 0.01, 0.02)
  )
  # nolint end

  # nolint start
  # fmt: skip
  target_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    return = c(0.005, 0.012, 0.008)
  )
  # nolint end

  # nolint start
  # fmt: skip
  benchmark_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    return = c(0.003, 0.010, 0.006)
  )
  # nolint end

  result <- calculate_factor_attribution(
    target_weights = identical_weights,
    benchmark_weights = identical_weights,
    factor_returns = factor_returns,
    target_returns = target_returns,
    benchmark_returns = benchmark_returns,
    factor_cols = c("F1", "F2")
  )

  expect_true(all(abs(result$factor_contribution) < 1e-10))
  expect_true(all(abs(result$selection_effect - result$excess_return) < 1e-10))
})

test_that("out-of-sample lag logic: weights from t applied to returns at t+1", {
  # nolint start
  # fmt: skip
  target_weights <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    F1   = c(1.0, 0.0, 0.5),
    F2   = c(0.0, 1.0, 0.5)
  )
  # nolint end

  # nolint start
  # fmt: skip
  benchmark_weights <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    F1   = c(0.5, 0.5, 0.5),
    F2   = c(0.5, 0.5, 0.5)
  )
  # nolint end

  # nolint start
  # fmt: skip
  factor_returns <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    F1   = c(0.00, 0.10, 0.20, 0.30),
    F2   = c(0.00, 0.05, 0.10, 0.15)
  )
  # nolint end

  # nolint start
  # fmt: skip
  target_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    return = c(0.00, 0.12, 0.08, 0.20)
  )
  # nolint end

  # nolint start
  # fmt: skip
  benchmark_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    return = c(0.00, 0.075, 0.15, 0.225)
  )
  # nolint end

  result <- calculate_factor_attribution(
    target_weights = target_weights,
    benchmark_weights = benchmark_weights,
    factor_returns = factor_returns,
    target_returns = target_returns,
    benchmark_returns = benchmark_returns,
    factor_cols = c("F1", "F2")
  )

  # nolint start
  # fmt: skip
  expected <- tibble::tibble(
    date                = as.Date(c("2020-01-02", "2020-01-03")),
    excess_return       = c(0.045, -0.070),
    factor_contribution = c(0.025, -0.050),
    selection_effect    = c(0.020, -0.020)
  )
  # nolint end

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("pure factor tilt with zero selection effect", {
  # nolint start
  # fmt: skip
  target_weights <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    F1   = c(1.0, 1.0, 1.0, 1.0),
    F2   = c(0.0, 0.0, 0.0, 0.0)
  )
  # nolint end

  # nolint start
  # fmt: skip
  benchmark_weights <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    F1   = c(0.0, 0.0, 0.0, 0.0),
    F2   = c(1.0, 1.0, 1.0, 1.0)
  )
  # nolint end

  # nolint start
  # fmt: skip
  factor_returns <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
    F1   = c(0.00, 0.05, 0.04, 0.06, 0.05),
    F2   = c(0.00, 0.02, 0.01, 0.03, 0.02)
  )
  # nolint end

  # nolint start
  # fmt: skip
  target_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
    return = c(0.00, 0.05, 0.04, 0.06, 0.05)
  )
  # nolint end

  # nolint start
  # fmt: skip
  benchmark_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
    return = c(0.00, 0.02, 0.01, 0.03, 0.02)
  )
  # nolint end

  result <- calculate_factor_attribution(
    target_weights = target_weights,
    benchmark_weights = benchmark_weights,
    factor_returns = factor_returns,
    target_returns = target_returns,
    benchmark_returns = benchmark_returns,
    factor_cols = c("F1", "F2")
  )

  # nolint start
  # fmt: skip
  expected <- tibble::tibble(
    date                = as.Date(c("2020-01-02", "2020-01-03", "2020-01-04")),
    excess_return       = c(0.03, 0.03, 0.03),
    factor_contribution = c(0.03, 0.03, 0.03),
    selection_effect    = c(0.00, 0.00, 0.00)
  )
  # nolint end

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("single factor model simplifies correctly", {
  # nolint start
  # fmt: skip
  target_weights <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    F1   = c(1.0, 1.0, 1.0, 1.0)
  )
  # nolint end

  # nolint start
  # fmt: skip
  benchmark_weights <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    F1   = c(0.6, 0.6, 0.6, 0.6)
  )
  # nolint end

  # nolint start
  # fmt: skip
  factor_returns <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
    F1   = c(0.00, 0.10, 0.08, 0.07, 0.09)
  )
  # nolint end

  # nolint start
  # fmt: skip
  target_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
    return = c(0.00, 0.12, 0.09, 0.08, 0.10)
  )
  # nolint end

  # nolint start
  # fmt: skip
  benchmark_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04", "2020-01-05")),
    return = c(0.00, 0.07, 0.05, 0.04, 0.05)
  )
  # nolint end

  result <- calculate_factor_attribution(
    target_weights = target_weights,
    benchmark_weights = benchmark_weights,
    factor_returns = factor_returns,
    target_returns = target_returns,
    benchmark_returns = benchmark_returns,
    factor_cols = c("F1")
  )

  # nolint start
  # fmt: skip
  expected <- tibble::tibble(
    date                = as.Date(c("2020-01-02", "2020-01-03", "2020-01-04")),
    excess_return       = c(0.05, 0.04, 0.04),
    factor_contribution = c(0.04, 0.032, 0.028),
    selection_effect    = c(0.01, 0.008, 0.012)
  )
  # nolint end

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("join integrity: no missing or duplicate dates", {
  # nolint start
  # fmt: skip
  target_weights <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    F1   = c(0.6, 0.7, 0.5),
    F2   = c(0.4, 0.3, 0.5)
  )
  # nolint end

  # nolint start
  # fmt: skip
  benchmark_weights <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    F1   = c(0.5, 0.5, 0.5),
    F2   = c(0.5, 0.5, 0.5)
  )
  # nolint end

  # nolint start
  # fmt: skip
  factor_returns <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    F1   = c(0.01, 0.02, -0.01, 0.015),
    F2   = c(-0.01, 0.01, 0.02, -0.005)
  )
  # nolint end

  # nolint start
  # fmt: skip
  target_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    return = c(0.005, 0.012, 0.008, 0.003)
  )
  # nolint end

  # nolint start
  # fmt: skip
  benchmark_returns <- tibble::tibble(
    date   = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    return = c(0.000, 0.015, 0.005, 0.005)
  )
  # nolint end

  result <- calculate_factor_attribution(
    target_weights = target_weights,
    benchmark_weights = benchmark_weights,
    factor_returns = factor_returns,
    target_returns = target_returns,
    benchmark_returns = benchmark_returns,
    factor_cols = c("F1", "F2")
  )

  expect_equal(nrow(result), 2)
  expect_true(all(!duplicated(result$date)))
  expect_equal(result$date, as.Date(c("2020-01-02", "2020-01-03")))
})
