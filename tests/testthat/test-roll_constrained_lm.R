test_df <- tibble::tribble(
  ~date, ~x1, ~x2, ~y,
  1,     1,   4,   10,
  2,     2,   3,   14,
  3,     3,   7,   18,
  4,     4,   2,   22,
  5,     5,   6,   26,
  6,     6,   8,   30,
  7,     7,   1,   34,
  8,     8,   5,   38
)

testthat::test_that("returns correct structure with single predictor", {
  x <- as.matrix(test_df[, "x1"])
  y <- test_df$y
  
  testthat::expect_warning(
    actual <- roll_constrained_lm(x = x, y = y, width = 3L, intercept = TRUE),
    "^First 2 observations have NA coefficients due to insufficient window size \\(width = 3\\)\\.$"
  )
  
  testthat::expect_s3_class(actual, "roll_constrained_lm")
  testthat::expect_named(actual, c("coefficients", "width", "call"))
  testthat::expect_equal(actual$width, 3L)
  testthat::expect_equal(ncol(actual$coefficients), 2L)
  testthat::expect_equal(colnames(actual$coefficients), c("(Intercept)", "x1"))
})

testthat::test_that("returns correct structure with multiple predictors", {
  x <- as.matrix(test_df[, c("x1", "x2")])
  y <- test_df$y
  
  testthat::expect_warning(
    actual <- roll_constrained_lm(x = x, y = y, width = 3L, intercept = TRUE),
    "^First 2 observations have NA coefficients due to insufficient window size \\(width = 3\\)\\.$"
  )
  
  testthat::expect_s3_class(actual, "roll_constrained_lm")
  testthat::expect_equal(ncol(actual$coefficients), 3L)
  testthat::expect_equal(colnames(actual$coefficients), c("(Intercept)", "x1", "x2"))
})

testthat::test_that("produces correct number of rolling windows", {
  x <- as.matrix(test_df[, "x1"])
  y <- test_df$y
  
  testthat::expect_warning(
    actual <- roll_constrained_lm(x = x, y = y, width = 3L),
    "^First 2 observations have NA coefficients due to insufficient window size \\(width = 3\\)\\.$"
  )
  
  # Should return same number of rows as input data (like roll::roll_lm)
  testthat::expect_equal(nrow(actual$coefficients), length(y))
})

testthat::test_that("passes through sum_to_one constraint", {
  x <- as.matrix(test_df[, c("x1", "x2")])
  y <- test_df$y
  
  testthat::expect_warning(
    actual <- roll_constrained_lm(x = x, y = y, width = 3L, sum_to_one = TRUE),
    "^First 2 observations have NA coefficients due to insufficient window size \\(width = 3\\)\\.$"
  )
  
  predictor_coefs <- actual$coefficients[, c("x1", "x2")]
  # Filter out incomplete windows (first width-1 rows are NA)
  complete_rows <- complete.cases(predictor_coefs)
  row_sums <- rowSums(predictor_coefs[complete_rows, , drop = FALSE])
  
  testthat::expect_equal(row_sums, rep(1, sum(complete_rows)), tolerance = 1e-8)
})

testthat::test_that("passes through non_negative constraint", {
  x <- as.matrix(test_df[, c("x1", "x2")])
  y <- test_df$y
  
  testthat::expect_warning(
    actual <- roll_constrained_lm(x = x, y = y, width = 3L, non_negative = TRUE),
    "^First 2 observations have NA coefficients due to insufficient window size \\(width = 3\\)\\.$"
  )
  
  predictor_coefs <- actual$coefficients[, c("x1", "x2")]
  # Filter out incomplete windows
  complete_rows <- complete.cases(predictor_coefs)
  
  testthat::expect_true(all(predictor_coefs[complete_rows, ] >= -1e-10))
})

testthat::test_that("passes through both constraints", {
  x <- as.matrix(test_df[, c("x1", "x2")])
  y <- test_df$y
  
  testthat::expect_warning(
    actual <- roll_constrained_lm(x = x, y = y, width = 3L, non_negative = TRUE, sum_to_one = TRUE),
    "^First 2 observations have NA coefficients due to insufficient window size \\(width = 3\\)\\.$"
  )
  
  predictor_coefs <- actual$coefficients[, c("x1", "x2")]
  # Filter out incomplete windows
  complete_rows <- complete.cases(predictor_coefs)
  
  testthat::expect_true(all(predictor_coefs[complete_rows, ] >= -1e-10))
  
  row_sums <- rowSums(predictor_coefs[complete_rows, , drop = FALSE])
  testthat::expect_equal(row_sums, rep(1, sum(complete_rows)), tolerance = 1e-8)
})

testthat::test_that("handles insufficient data correctly", {
  x <- as.matrix(test_df[1:2, "x1"])
  y <- test_df$y[1:2]
  
  testthat::expect_error(
    roll_constrained_lm(x = x, y = y, width = 5L, intercept = TRUE),
    "Insufficient observations: need at least 5 but got 2\\."
  )
})

testthat::test_that("produces NA values for incomplete windows", {
  x <- as.matrix(test_df[, "x1"])
  y <- test_df$y
  
  testthat::expect_warning(
    actual <- roll_constrained_lm(x = x, y = y, width = 4L, intercept = TRUE),
    "^First 3 observations have NA coefficients due to insufficient window size \\(width = 4\\)\\.$"
  )
  
  testthat::expect_true(all(is.na(actual$coefficients[1, ])))
})

testthat::test_that("matches roll::roll_lm when constraints are disabled", {
  x <- as.matrix(test_df[, "x1"])
  y <- test_df$y
  
  testthat::expect_warning(
    actual <- roll_constrained_lm(x = x, y = y, width = 3L, non_negative = FALSE, sum_to_one = FALSE, intercept = TRUE),
    "^First 2 observations have NA coefficients due to insufficient window size \\(width = 3\\)\\.$"
  )
  expected <- roll::roll_lm(x = x, y = y, width = 3L)
  
  testthat::expect_equal(actual$coefficients, expected$coefficients, tolerance = 1e-8)
})

testthat::test_that("handles data.frame input for x", {
  x <- test_df[, c("x1", "x2")]
  y <- test_df$y
  
  testthat::expect_warning(
    actual <- roll_constrained_lm(x = x, y = y, width = 3L, intercept = TRUE),
    "^First 2 observations have NA coefficients due to insufficient window size \\(width = 3\\)\\.$"
  )
  
  testthat::expect_s3_class(actual, "roll_constrained_lm")
  testthat::expect_equal(colnames(actual$coefficients), c("(Intercept)", "x1", "x2"))
})

testthat::test_that("error when width is not positive integer", {
  x <- as.matrix(test_df[, "x1"])
  y <- test_df$y
  
  testthat::expect_error(
    roll_constrained_lm(x = x, y = y, width = 0L),
    "width"
  )
})

testthat::test_that("error when x and y have different lengths", {
  x <- as.matrix(test_df[1:5, "x1"])
  y <- test_df$y
  
  testthat::expect_error(
    roll_constrained_lm(x = x, y = y, width = 3L),
    "^Length of `y` must match number of rows in `x`\\.$"
  )
})

testthat::test_that("error when y is not numeric", {
  x <- as.matrix(test_df[, "x1"])
  y <- as.character(test_df$y)

  testthat::expect_error(
    roll_constrained_lm(x = x, y = y, width = 3L),
    "numeric"
  )
})

testthat::test_that("preserves column names from x", {
  x <- as.matrix(test_df[, c("x1", "x2")])
  colnames(x) <- c("factor1", "factor2")
  y <- test_df$y
  
  testthat::expect_warning(
    actual <- roll_constrained_lm(x = x, y = y, width = 3L, intercept = TRUE),
    "^First 2 observations have NA coefficients due to insufficient window size \\(width = 3\\)\\.$"
  )
  
  testthat::expect_equal(colnames(actual$coefficients), c("(Intercept)", "factor1", "factor2"))
})
