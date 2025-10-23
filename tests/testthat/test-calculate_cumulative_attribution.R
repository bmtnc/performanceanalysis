test_that("carino linking maintains additive identity at all points", {
  # nolint start
  # fmt: skip
  daily_attribution <- tibble::tibble(
    date                = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    excess_return       = c(0.01, 0.02, -0.01),
    factor_contribution = c(0.005, 0.015, -0.005),
    selection_effect    = c(0.005, 0.005, -0.005)
  )
  # nolint end

  result <- calculate_cumulative_attribution(daily_attribution)

  expect_equal(nrow(result), 3)
  expect_true(all(c("cumulative_excess", "cumulative_factor", "cumulative_selection") %in% colnames(result)))

  identity_check <- result$cumulative_factor + result$cumulative_selection - result$cumulative_excess
  expect_true(all(abs(identity_check) < 1e-10))

  expect_equal(result$cumulative_excess[1], 0.01, tolerance = 1e-10)
  expect_equal(result$cumulative_excess[2], 0.0302, tolerance = 1e-10)
  expect_equal(result$cumulative_excess[3], 0.019898, tolerance = 1e-10)
})

test_that("zero returns edge case: all cumulative values are zero", {
  # nolint start
  # fmt: skip
  daily_attribution <- tibble::tibble(
    date                = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    excess_return       = c(0.0, 0.0, 0.0),
    factor_contribution = c(0.0, 0.0, 0.0),
    selection_effect    = c(0.0, 0.0, 0.0)
  )
  # nolint end

  result <- calculate_cumulative_attribution(daily_attribution)

  # nolint start
  # fmt: skip
  expected <- tibble::tibble(
    date                  = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    excess_return         = c(0.0, 0.0, 0.0),
    factor_contribution   = c(0.0, 0.0, 0.0),
    selection_effect      = c(0.0, 0.0, 0.0),
    cumulative_excess     = c(0.0, 0.0, 0.0),
    cumulative_factor     = c(0.0, 0.0, 0.0),
    cumulative_selection  = c(0.0, 0.0, 0.0)
  )
  # nolint end

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("large positive/negative swings: carino linking handles volatility correctly", {
  # nolint start
  # fmt: skip
  daily_attribution <- tibble::tibble(
    date                = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04")),
    excess_return       = c(0.10, -0.08, 0.05, -0.03),
    factor_contribution = c(0.08, -0.05, 0.03, -0.01),
    selection_effect    = c(0.02, -0.03, 0.02, -0.02)
  )
  # nolint end

  result <- calculate_cumulative_attribution(daily_attribution)

  identity_check <- result$cumulative_factor + result$cumulative_selection - result$cumulative_excess
  expect_true(all(abs(identity_check) < 1e-10))

  expect_equal(result$cumulative_excess[1], 0.10, tolerance = 1e-10)
  expect_equal(result$cumulative_excess[2], 0.012, tolerance = 1e-10)
  expect_equal(result$cumulative_excess[3], 0.0626, tolerance = 1e-10)
  expect_equal(result$cumulative_excess[4], 0.030722, tolerance = 1e-10)
})

test_that("single observation: cumulative equals daily", {
  # nolint start
  # fmt: skip
  daily_attribution <- tibble::tibble(
    date                = as.Date(c("2020-01-01")),
    excess_return       = c(0.05),
    factor_contribution = c(0.03),
    selection_effect    = c(0.02)
  )
  # nolint end

  result <- calculate_cumulative_attribution(daily_attribution)

  # nolint start
  # fmt: skip
  expected <- tibble::tibble(
    date                  = as.Date(c("2020-01-01")),
    excess_return         = c(0.05),
    factor_contribution   = c(0.03),
    selection_effect      = c(0.02),
    cumulative_excess     = c(0.05),
    cumulative_factor     = c(0.03),
    cumulative_selection  = c(0.02)
  )
  # nolint end

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("date ordering dependency: function sorts before compounding", {
  # nolint start
  # fmt: skip
  daily_attribution_unsorted <- tibble::tibble(
    date                = as.Date(c("2020-01-03", "2020-01-01", "2020-01-02")),
    excess_return       = c(-0.01, 0.01, 0.02),
    factor_contribution = c(-0.005, 0.005, 0.015),
    selection_effect    = c(-0.005, 0.005, 0.005)
  )
  # nolint end

  result <- calculate_cumulative_attribution(daily_attribution_unsorted)

  expect_equal(result$date, as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")))

  identity_check <- result$cumulative_factor + result$cumulative_selection - result$cumulative_excess
  expect_true(all(abs(identity_check) < 1e-10))

  expect_equal(result$cumulative_excess, c(0.01, 0.0302, 0.019898), tolerance = 1e-10)
})

test_that("equal daily contributions maintain equal cumulative shares", {
  # nolint start
  # fmt: skip
  daily_attribution <- tibble::tibble(
    date                = as.Date(c("2020-01-01", "2020-01-02")),
    excess_return       = c(0.10, 0.10),
    factor_contribution = c(0.05, 0.05),
    selection_effect    = c(0.05, 0.05)
  )
  # nolint end

  result <- calculate_cumulative_attribution(daily_attribution)

  identity_check <- result$cumulative_factor + result$cumulative_selection - result$cumulative_excess
  expect_true(all(abs(identity_check) < 1e-10))

  expect_equal(result$cumulative_excess, c(0.10, 0.21), tolerance = 1e-10)

  expect_equal(result$cumulative_factor[2] / result$cumulative_excess[2], 0.5, tolerance = 1e-6)
  expect_equal(result$cumulative_selection[2] / result$cumulative_excess[2], 0.5, tolerance = 1e-6)
})

test_that("negative returns: carino linking maintains identity with negative values", {
  # nolint start
  # fmt: skip
  daily_attribution <- tibble::tibble(
    date                = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    excess_return       = c(-0.05, -0.03, 0.02),
    factor_contribution = c(-0.03, -0.02, 0.01),
    selection_effect    = c(-0.02, -0.01, 0.01)
  )
  # nolint end

  result <- calculate_cumulative_attribution(daily_attribution)

  identity_check <- result$cumulative_factor + result$cumulative_selection - result$cumulative_excess
  expect_true(all(abs(identity_check) < 1e-10))

  expect_equal(result$cumulative_excess, c(-0.05, -0.0785, -0.06007), tolerance = 1e-10)
})
