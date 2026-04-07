test_that("single return passes through unchanged", {
  expect_equal(compound_returns(0.05), 0.05)
})

test_that("two positive returns compound geometrically", {
  # (1.02)(1.03) - 1 = 1.0506 - 1 = 0.0506
  expect_equal(compound_returns(c(0.02, 0.03)), 0.0506, tolerance = 1e-12)
})

test_that("offsetting returns do not cancel arithmetically", {
  # (1.10)(0.90) - 1 = 0.99 - 1 = -0.01
  # Arithmetic sum would be 0, but geometric is -1%
  expect_equal(compound_returns(c(0.10, -0.10)), -0.01, tolerance = 1e-12)
})

test_that("zero returns compound to zero", {
  expect_equal(compound_returns(c(0, 0, 0)), 0)
})

test_that("multi-period compounding matches manual calculation", {
  r <- c(0.01, -0.02, 0.03, 0.005)
  # (1.01)(0.98)(1.03)(1.005) - 1
  expected <- 1.01 * 0.98 * 1.03 * 1.005 - 1
  expect_equal(compound_returns(r), expected, tolerance = 1e-12)
})
