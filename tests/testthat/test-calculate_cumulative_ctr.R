test_that("carino linking preserves additive identity at every point", {
  # nolint start
  # fmt: skip
  ctr_data <- tibble::tibble(
    date            = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01")),
    fund_return     = c(0.02, -0.01, 0.03, 0.005),
    alpha_ctr       = c(0.001, 0.001, 0.002, 0.001),
    f1_ctr          = c(0.010, -0.008, 0.015, 0.003),
    f2_ctr          = c(0.005, 0.002, 0.008, -0.001),
    total_explained = c(0.016, -0.005, 0.025, 0.003),
    residual        = c(0.004, -0.005, 0.005, 0.002)
  )
  # nolint end

  result <- calculate_cumulative_ctr(ctr_data, c("f1_ctr", "f2_ctr"))

  expect_true(all(c(
    "cumulative_fund_return",
    "cumulative_alpha_ctr",
    "cumulative_f1_ctr",
    "cumulative_f2_ctr",
    "cumulative_residual"
  ) %in% colnames(result)))

  # Additive identity at every row
  identity <- result$cumulative_alpha_ctr +
    result$cumulative_f1_ctr +
    result$cumulative_f2_ctr +
    result$cumulative_residual
  expect_equal(identity, result$cumulative_fund_return, tolerance = 1e-10)
})

test_that("single period: cumulative equals period values", {
  # nolint start
  # fmt: skip
  ctr_data <- tibble::tibble(
    date            = as.Date("2020-01-01"),
    fund_return     = 0.05,
    alpha_ctr       = 0.01,
    f1_ctr          = 0.03,
    total_explained = 0.04,
    residual        = 0.01
  )
  # nolint end

  result <- calculate_cumulative_ctr(ctr_data, "f1_ctr")

  expect_equal(result$cumulative_fund_return, 0.05, tolerance = 1e-12)
  identity <- unname(
    result$cumulative_alpha_ctr + result$cumulative_f1_ctr + result$cumulative_residual
  )
  expect_equal(identity, 0.05, tolerance = 1e-12)
})

test_that("zero returns produce zero cumulative values", {
  # nolint start
  # fmt: skip
  ctr_data <- tibble::tibble(
    date            = as.Date("2020-01-01") + (0:2) * 31,
    fund_return     = c(0, 0, 0),
    alpha_ctr       = c(0, 0, 0),
    f1_ctr          = c(0, 0, 0),
    total_explained = c(0, 0, 0),
    residual        = c(0, 0, 0)
  )
  # nolint end

  result <- calculate_cumulative_ctr(ctr_data, "f1_ctr")

  expect_equal(result$cumulative_fund_return, c(0, 0, 0))
  expect_equal(result$cumulative_alpha_ctr, c(0, 0, 0))
  expect_equal(result$cumulative_f1_ctr, c(0, 0, 0))
  expect_equal(result$cumulative_residual, c(0, 0, 0))
})

test_that("unsorted input gets sorted by date", {
  # nolint start
  # fmt: skip
  ctr_data <- tibble::tibble(
    date            = as.Date(c("2020-03-01", "2020-01-01", "2020-02-01")),
    fund_return     = c(0.03, 0.01, 0.02),
    alpha_ctr       = c(0.01, 0.005, 0.008),
    f1_ctr          = c(0.015, 0.003, 0.01),
    total_explained = c(0.025, 0.008, 0.018),
    residual        = c(0.005, 0.002, 0.002)
  )
  # nolint end

  result <- calculate_cumulative_ctr(ctr_data, "f1_ctr")

  # Should be sorted by date
  expect_equal(result$date, as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")))

  # First cumulative fund return should be from the earliest date
  expect_equal(result$cumulative_fund_return[1], 0.01, tolerance = 1e-12)
})

test_that("volatile returns with sign changes maintain identity", {
  # nolint start
  # fmt: skip
  ctr_data <- tibble::tibble(
    date            = as.Date("2020-01-01") + (0:4) * 31,
    fund_return     = c(0.10, -0.08, 0.05, -0.12, 0.07),
    alpha_ctr       = c(0.002, 0.002, 0.002, 0.002, 0.002),
    f1_ctr          = c(0.06, -0.05, 0.03, -0.08, 0.04),
    f2_ctr          = c(0.02, -0.01, 0.01, -0.03, 0.02),
    total_explained = c(0.082, -0.058, 0.042, -0.108, 0.062),
    residual        = c(0.018, -0.022, 0.008, -0.012, 0.008)
  )
  # nolint end

  result <- calculate_cumulative_ctr(ctr_data, c("f1_ctr", "f2_ctr"))

  # Identity at every point
  identity <- result$cumulative_alpha_ctr +
    result$cumulative_f1_ctr +
    result$cumulative_f2_ctr +
    result$cumulative_residual
  expect_equal(identity, result$cumulative_fund_return, tolerance = 1e-10)

  # Cumulative fund return should match geometric compounding
  expected_cum <- cumprod(1 + ctr_data$fund_return) - 1
  expect_equal(result$cumulative_fund_return, expected_cum, tolerance = 1e-10)
})

test_that("many factors: identity holds with 5 components", {
  # nolint start
  # fmt: skip
  ctr_data <- tibble::tibble(
    date            = as.Date("2020-01-01") + (0:2) * 31,
    fund_return     = c(0.03, 0.02, -0.01),
    alpha_ctr       = c(0.001, 0.001, 0.001),
    f1_ctr          = c(0.005, 0.003, -0.002),
    f2_ctr          = c(0.008, 0.006, -0.003),
    f3_ctr          = c(0.004, 0.002, -0.001),
    f4_ctr          = c(0.006, 0.004, -0.002),
    f5_ctr          = c(0.003, 0.001, -0.001),
    total_explained = c(0.027, 0.017, -0.008),
    residual        = c(0.003, 0.003, -0.002)
  )
  # nolint end

  fac_cols <- paste0("f", 1:5, "_ctr")
  result <- calculate_cumulative_ctr(ctr_data, fac_cols)

  identity <- result$cumulative_alpha_ctr +
    result$cumulative_f1_ctr +
    result$cumulative_f2_ctr +
    result$cumulative_f3_ctr +
    result$cumulative_f4_ctr +
    result$cumulative_f5_ctr +
    result$cumulative_residual
  expect_equal(identity, result$cumulative_fund_return, tolerance = 1e-10)
})

test_that("input validation catches bad inputs", {
  good_data <- tibble::tibble(
    date = as.Date("2020-01-01"),
    fund_return = 0.01,
    alpha_ctr = 0.005,
    f1_ctr = 0.003,
    total_explained = 0.008,
    residual = 0.002
  )

  # Not a data frame
  expect_error(
    calculate_cumulative_ctr("not_a_df", "f1_ctr"),
    "data.frame"
  )

  # Missing required column
  expect_error(
    calculate_cumulative_ctr(good_data[, -2], "f1_ctr"),
    "missing"
  )

  # Missing factor CTR column
  expect_error(
    calculate_cumulative_ctr(good_data, "f_missing_ctr"),
    "missing"
  )
})
