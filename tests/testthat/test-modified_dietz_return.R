test_that("no cash flows gives simple return", {
  # 100 grows to 110 = 10%
  expect_equal(modified_dietz_return(100, 110), 0.10)
  # 100 drops to 90 = -10%
  expect_equal(modified_dietz_return(100, 90), -0.10)
  # no change
  expect_equal(modified_dietz_return(100, 100), 0.0)
})

test_that("zero bmv with no cash flows returns NA", {
  expect_true(is.na(modified_dietz_return(0, 100)))
})

test_that("cash flow at start of period gets full weight", {
  # BMV = 100, contribute 50 on day 0, EMV = 157.5
  # weight = (30 - 0) / 30 = 1.0 (full weight)
  # denominator = 100 + 50*1 = 150
  # return = (157.5 - 100 - 50) / 150 = 7.5/150 = 5%
  expect_equal(
    modified_dietz_return(100, 157.5, cf = 50, cf_days = 0, total_days = 30),
    0.05
  )
})

test_that("cash flow at end of period gets zero weight", {
  # BMV = 100, contribute 50 on day 30 (last day), EMV = 155
  # weight = (30 - 30) / 30 = 0 (no weight)
  # denominator = 100 + 50*0 = 100
  # return = (155 - 100 - 50) / 100 = 5/100 = 5%
  expect_equal(
    modified_dietz_return(100, 155, cf = 50, cf_days = 30, total_days = 30),
    0.05
  )
})

test_that("mid-period cash flow gets half weight", {
  # BMV = 100, contribute 50 on day 15 of 30-day period, EMV = 131.25
  # weight = (30 - 15) / 30 = 0.5
  # denominator = 100 + 50*0.5 = 125
  # return = (131.25 - 100 - 50) / 125 = -18.75/125 = -15%
  expect_equal(
    modified_dietz_return(100, 131.25, cf = 50, cf_days = 15, total_days = 30),
    -0.15
  )
})

test_that("distribution (negative cf) works correctly", {
  # BMV = 200, distribute 50 on day 0, EMV = 157.5
  # weight = 1.0
  # denominator = 200 + (-50)*1 = 150
  # return = (157.5 - 200 - (-50)) / 150 = 7.5/150 = 5%
  expect_equal(
    modified_dietz_return(200, 157.5, cf = -50, cf_days = 0, total_days = 30),
    0.05
  )
})

test_that("multiple cash flows are handled", {
  # BMV = 100
  # Day 0: contribute 20 (weight = 1.0)
  # Day 15: distribute -10 (weight = 0.5)
  # EMV = 115.5
  # net_cf = 20 + (-10) = 10
  # weighted_cf = 20*1.0 + (-10)*0.5 = 15
  # denominator = 100 + 15 = 115
  # return = (115.5 - 100 - 10) / 115 = 5.5/115
  expected <- 5.5 / 115
  expect_equal(
    modified_dietz_return(100, 115.5, cf = c(20, -10), cf_days = c(0, 15),
                          total_days = 30),
    expected
  )
})

test_that("zero denominator returns NA", {
  # BMV = 100, distribute entire value at start
  # denominator = 100 + (-100)*1 = 0
  expect_true(is.na(
    modified_dietz_return(100, 0, cf = -100, cf_days = 0, total_days = 30)
  ))
})

test_that("new investment with zero bmv works", {
  # BMV = 0, contribute 100 on day 0, grows to 105
  # weight = 1.0, denominator = 0 + 100*1 = 100
  # return = (105 - 0 - 100) / 100 = 5%
  expect_equal(
    modified_dietz_return(0, 105, cf = 100, cf_days = 0, total_days = 30),
    0.05
  )
})
