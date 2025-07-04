test_that("returns correct constraints when both non_negative and sum_to_one are TRUE", {
  actual <- construct_linear_regression_constraints(k = 3, non_negative = TRUE, sum_to_one = TRUE)
  expected <- list(
    Amat = rbind(rep(1, 3), diag(3)),
    bvec = c(1, 0, 0, 0),
    meq = 1L
  )
  expect_equal(actual, expected)
})

test_that("returns correct constraints when non_negative is TRUE and sum_to_one is FALSE", {
  actual <- construct_linear_regression_constraints(k = 3, non_negative = TRUE, sum_to_one = FALSE)
  expected <- list(
    Amat = diag(3),
    bvec = c(0, 0, 0),
    meq = 0L
  )
  expect_equal(actual, expected)
})

test_that("returns correct constraints when non_negative is FALSE and sum_to_one is TRUE", {
  actual <- construct_linear_regression_constraints(k = 3, non_negative = FALSE, sum_to_one = TRUE)
  expected <- list(
    Amat = matrix(rep(1, 3), nrow = 1),
    bvec = 1,
    meq = 1L
  )
  expect_equal(actual, expected)
})

test_that("returns correct constraints when both non_negative and sum_to_one are FALSE", {
  actual <- construct_linear_regression_constraints(k = 3, non_negative = FALSE, sum_to_one = FALSE)
  expected <- list(
    Amat = matrix(0, nrow = 0, ncol = 3),
    bvec = numeric(0),
    meq = 0L
  )
  expect_equal(actual, expected)
})

test_that("works with k = 1", {
  actual <- construct_linear_regression_constraints(k = 1, non_negative = TRUE, sum_to_one = TRUE)
  expected <- list(
    Amat = rbind(1, 1),
    bvec = c(1, 0),
    meq = 1L
  )
  expect_equal(actual, expected)
})

test_that("works with larger k values", {
  actual <- construct_linear_regression_constraints(k = 5, non_negative = TRUE, sum_to_one = FALSE)
  expected <- list(
    Amat = diag(5),
    bvec = rep(0, 5),
    meq = 0L
  )
  expect_equal(actual, expected)
})

test_that("throws error when k is not numeric", {
  expect_error(
    construct_linear_regression_constraints(k = "3", non_negative = TRUE, sum_to_one = TRUE),
    "^`k` must be a positive integer; got 3\\.$"
  )
})

test_that("throws error when k is not a single value", {
  expect_error(
    construct_linear_regression_constraints(k = c(2, 3), non_negative = TRUE, sum_to_one = TRUE),
    "^`k` must be a positive integer; got 2\\.`k` must be a positive integer; got 3\\.$"
  )
})

test_that("throws error when k is not positive", {
  expect_error(
    construct_linear_regression_constraints(k = 0, non_negative = TRUE, sum_to_one = TRUE),
    "^`k` must be a positive integer; got 0\\.$"
  )
})

test_that("throws error when k is negative", {
  expect_error(
    construct_linear_regression_constraints(k = -1, non_negative = TRUE, sum_to_one = TRUE),
    "^`k` must be a positive integer; got -1\\.$"
  )
})

test_that("throws error when k is not an integer", {
  expect_error(
    construct_linear_regression_constraints(k = 3.5, non_negative = TRUE, sum_to_one = TRUE),
    "^`k` must be a positive integer; got 3\\.5\\.$"
  )
})

test_that("throws error when non_negative is not logical", {
  expect_error(
    construct_linear_regression_constraints(k = 3, non_negative = "TRUE", sum_to_one = TRUE),
    "^`non_negative` must be a single logical value\\.$"
  )
})

test_that("throws error when non_negative is not a single value", {
  expect_error(
    construct_linear_regression_constraints(k = 3, non_negative = c(TRUE, FALSE), sum_to_one = TRUE),
    "^`non_negative` must be a single logical value\\.$"
  )
})

test_that("throws error when sum_to_one is not logical", {
  expect_error(
    construct_linear_regression_constraints(k = 3, non_negative = TRUE, sum_to_one = 1),
    "^`sum_to_one` must be a single logical value\\.$"
  )
})

test_that("throws error when sum_to_one is not a single value", {
  expect_error(
    construct_linear_regression_constraints(k = 3, non_negative = TRUE, sum_to_one = c(TRUE, FALSE)),
    "^`sum_to_one` must be a single logical value\\.$"
  )
})