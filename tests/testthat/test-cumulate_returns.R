test_that("single return equals itself", {
  expect_equal(cumulate_returns(0.05), 0.05)
})

test_that("cumulative values match running product", {
  r <- c(0.02, 0.03, -0.01)
  # t1: 1.02 - 1 = 0.02
  # t2: 1.02 * 1.03 - 1 = 1.0506 - 1 = 0.0506
  # t3: 1.02 * 1.03 * 0.99 - 1 = 1.040094 - 1 = 0.040094
  expected <- c(0.02, 0.0506, 1.02 * 1.03 * 0.99 - 1)
  expect_equal(cumulate_returns(r), expected, tolerance = 1e-12)
})

test_that("last element equals compound_returns of full vector", {
  r <- c(0.01, -0.02, 0.03, 0.005, -0.01)
  cum <- cumulate_returns(r)
  expect_equal(cum[length(cum)], compound_returns(r), tolerance = 1e-12)
})

test_that("zero returns stay at zero", {
  expect_equal(cumulate_returns(c(0, 0, 0)), c(0, 0, 0))
})

test_that("large drawdown followed by recovery", {
  # Down 50%, then up 100% => back to even
  r <- c(-0.50, 1.00)
  # t1: 0.50 - 1 = -0.50
  # t2: 0.50 * 2.00 - 1 = 0
  expect_equal(cumulate_returns(r), c(-0.50, 0.0), tolerance = 1e-12)
})
