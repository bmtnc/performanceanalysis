test_that("first-of-month maps to last day of same month", {
  expect_equal(
    to_month_end(as.Date("2025-04-01")),
    as.Date("2025-04-30")
  )
  expect_equal(
    to_month_end(as.Date("2025-01-01")),
    as.Date("2025-01-31")
  )
})

test_that("last-of-month stays unchanged", {
  expect_equal(
    to_month_end(as.Date("2025-03-31")),
    as.Date("2025-03-31")
  )
  expect_equal(
    to_month_end(as.Date("2024-02-29")),
    as.Date("2024-02-29")
  )
})

test_that("mid-month dates snap to month end", {
  expect_equal(
    to_month_end(as.Date("2025-06-15")),
    as.Date("2025-06-30")
  )
})

test_that("february handles leap years correctly", {
  expect_equal(
    to_month_end(as.Date("2024-02-01")),
    as.Date("2024-02-29")
  )
  expect_equal(
    to_month_end(as.Date("2025-02-01")),
    as.Date("2025-02-28")
  )
})

test_that("vectorized input returns vectorized output", {
  input <- as.Date(c("2025-01-01", "2025-02-01", "2025-03-01"))
  expected <- as.Date(c("2025-01-31", "2025-02-28", "2025-03-31"))
  expect_equal(to_month_end(input), expected)
})

test_that("works with character input", {
  expect_equal(
    to_month_end("2025-04-01"),
    as.Date("2025-04-30")
  )
})

test_that("december rolls to Dec 31, not next year", {
  expect_equal(
    to_month_end(as.Date("2025-12-01")),
    as.Date("2025-12-31")
  )
})
