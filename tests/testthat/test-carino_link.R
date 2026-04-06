test_that("additive identity holds at every point with 2 components", {
  total <- c(0.01, 0.02, -0.01, 0.03)
  components <- tibble::tibble(
    factor = c(0.006, 0.015, -0.004, 0.020),
    selection = c(0.004, 0.005, -0.006, 0.010)
  )

  result <- carino_link(total, components)

  expect_equal(nrow(result), 4)
  expect_true(all(c("cumulative_total", "cumulative_factor", "cumulative_selection") %in% colnames(result)))

  identity <- result$cumulative_factor + result$cumulative_selection
  expect_equal(identity, result$cumulative_total, tolerance = 1e-10)

  # Cumulative total should match geometric compounding
  expected_cum <- cumprod(1 + total) - 1
  expect_equal(result$cumulative_total, expected_cum, tolerance = 1e-10)
})

test_that("additive identity holds with many components", {
  total <- c(0.03, 0.02, -0.01)
  components <- tibble::tibble(
    alpha = c(0.001, 0.001, 0.001),
    f1 = c(0.010, 0.008, -0.004),
    f2 = c(0.008, 0.006, -0.003),
    f3 = c(0.005, 0.002, -0.001),
    residual = c(0.006, 0.003, -0.003)
  )

  result <- carino_link(total, components)

  identity <- result$cumulative_alpha + result$cumulative_f1 +
    result$cumulative_f2 + result$cumulative_f3 + result$cumulative_residual
  expect_equal(identity, result$cumulative_total, tolerance = 1e-10)
})

test_that("single period: cumulative equals period return", {
  total <- 0.05
  components <- tibble::tibble(f1 = 0.03, f2 = 0.02)

  result <- carino_link(total, components)

  expect_equal(result$cumulative_total, 0.05, tolerance = 1e-12)
  expect_equal(
    result$cumulative_f1 + result$cumulative_f2,
    0.05,
    tolerance = 1e-12
  )
})

test_that("zero returns produce zero cumulative values", {
  total <- c(0, 0, 0)
  components <- tibble::tibble(f1 = c(0, 0, 0), f2 = c(0, 0, 0))

  result <- carino_link(total, components)

  expect_equal(result$cumulative_total, c(0, 0, 0))
  expect_equal(result$cumulative_f1, c(0, 0, 0))
  expect_equal(result$cumulative_f2, c(0, 0, 0))
})

test_that("single component works", {
  total <- c(0.02, -0.01, 0.03)
  components <- tibble::tibble(mkt = total)

  result <- carino_link(total, components)

  expect_equal(result$cumulative_mkt, result$cumulative_total, tolerance = 1e-10)
})

test_that("volatile returns with sign changes maintain identity", {
  total <- c(0.10, -0.08, 0.05, -0.12, 0.07)
  components <- tibble::tibble(
    f1 = c(0.06, -0.05, 0.03, -0.08, 0.04),
    f2 = c(0.02, -0.01, 0.01, -0.03, 0.02),
    resid = c(0.02, -0.02, 0.01, -0.01, 0.01)
  )

  result <- carino_link(total, components)

  identity <- result$cumulative_f1 + result$cumulative_f2 + result$cumulative_resid
  expect_equal(identity, result$cumulative_total, tolerance = 1e-10)

  expected_cum <- cumprod(1 + total) - 1
  expect_equal(result$cumulative_total, expected_cum, tolerance = 1e-10)
})

test_that("matches old calculate_cumulative_attribution for 2 components", {
  # nolint start
  # fmt: skip
  daily_attr <- tibble::tibble(
    date                = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    excess_return       = c(0.01, 0.02, -0.01),
    factor_contribution = c(0.005, 0.015, -0.005),
    selection_effect    = c(0.005, 0.005, -0.005)
  )
  # nolint end

  old_result <- calculate_cumulative_attribution(daily_attr)

  new_result <- carino_link(
    daily_attr$excess_return,
    tibble::tibble(
      factor = daily_attr$factor_contribution,
      selection = daily_attr$selection_effect
    )
  )

  expect_equal(new_result$cumulative_total, old_result$cumulative_excess, tolerance = 1e-10)
  expect_equal(new_result$cumulative_factor, old_result$cumulative_factor, tolerance = 1e-10)
  expect_equal(new_result$cumulative_selection, old_result$cumulative_selection, tolerance = 1e-10)
})

test_that("input validation catches bad inputs", {
  expect_error(
    carino_link("not_numeric", tibble::tibble(f1 = 1)),
    "numeric"
  )

  expect_error(
    carino_link(c(0.01, 0.02), tibble::tibble(f1 = 0.01)),
    "nrow"
  )

  expect_error(
    carino_link(0.01, "not_a_df"),
    "data.frame"
  )
})
