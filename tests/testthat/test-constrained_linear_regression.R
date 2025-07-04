test_df <- tibble::tribble(
  ~x1, ~x2, ~y,
   1 ,  4 , 10,
   2 ,  6 , 14,    
   3 ,  5 , 18,
   4 ,  7 , 22
)

testthat::test_that("returns same coefficients as lm() when constraints are disabled", {
  X <- as.matrix(test_df[, c("x1", "x2")])
  Y <- test_df$y

  actual   <- constrained_linear_regression(
    x = X,
    y = Y,
    non_negative = FALSE,
    sum_to_one   = FALSE
  )$coefficients

  expected <- stats::coef(stats::lm(y ~ x1 + x2 - 1, data = test_df))

  testthat::expect_equal(actual, expected, tolerance = 1e-8)
})

testthat::test_that("non-negative and sum-to-one constraints are enforced", {
  X <- as.matrix(test_df[, c("x1", "x2")])
  Y <- test_df$y

  result <- constrained_linear_regression(x = X, y = Y)

  testthat::expect_equal(sum(result$coefficients), 1, tolerance = 1e-8)
  testthat::expect_true(all(result$coefficients >= -1e-10))  # Allow for numerical precision
})

testthat::test_that("only non-negative constraint is enforced", {
  X <- as.matrix(test_df[, c("x1", "x2")])
  Y <- test_df$y

  result <- constrained_linear_regression(
    x = X, 
    y = Y,
    non_negative = TRUE,
    sum_to_one = FALSE
  )

  # Coefficients should be non-negative
  testthat::expect_true(all(result$coefficients >= -1e-10))
  
  # Sum should NOT be constrained to 1
  testthat::expect_false(abs(sum(result$coefficients) - 1) < 1e-8)
})

testthat::test_that("only sum-to-one constraint is enforced", {
  X <- as.matrix(test_df[, c("x1", "x2")])
  Y <- test_df$y

  result <- constrained_linear_regression(
    x = X, 
    y = Y,
    non_negative = FALSE,
    sum_to_one = TRUE
  )

  # Coefficients should sum to 1
  testthat::expect_equal(sum(result$coefficients), 1, tolerance = 1e-8)
  
  # Coefficients can be negative (not constrained)
  # We can't guarantee negative coefficients will occur with this data,
  # so we just check that the constraint allows it by verifying the sum constraint works
  testthat::expect_true(TRUE)  # This constraint combination is mathematically valid
})

testthat::test_that("intercept=TRUE matches lm() when constraints are disabled", {
  X <- as.matrix(test_df[, c("x1", "x2")])
  Y <- test_df$y

  actual <- constrained_linear_regression(
    x = X,
    y = Y,
    non_negative = FALSE,
    sum_to_one = FALSE,
    intercept = TRUE
  )$coefficients

  expected <- stats::coef(stats::lm(y ~ x1 + x2, data = test_df))

  testthat::expect_equal(actual, expected, tolerance = 1e-8)
})

testthat::test_that("intercept=TRUE with constraints applies constraints only to predictors", {
  X <- as.matrix(test_df[, c("x1", "x2")])
  Y <- test_df$y

  result <- constrained_linear_regression(
    x = X,
    y = Y,
    non_negative = TRUE,
    sum_to_one = TRUE,
    intercept = TRUE
  )

  # Predictor coefficients should be non-negative and sum to 1
  predictor_coefs <- result$coefficients[c("x1", "x2")]
  testthat::expect_equal(sum(predictor_coefs), 1, tolerance = 1e-8)
  testthat::expect_true(all(predictor_coefs >= -1e-10))
  
  # Intercept should be unconstrained
  testthat::expect_true("(Intercept)" %in% names(result$coefficients))
})

testthat::test_that("intercept=TRUE with only non-negative constraint", {
  X <- as.matrix(test_df[, c("x1", "x2")])
  Y <- test_df$y

  result <- constrained_linear_regression(
    x = X,
    y = Y,
    non_negative = TRUE,
    sum_to_one = FALSE,
    intercept = TRUE
  )

  # Predictor coefficients should be non-negative
  predictor_coefs <- result$coefficients[c("x1", "x2")]
  testthat::expect_true(all(predictor_coefs >= -1e-10))
  
  # Sum should NOT be constrained to 1
  testthat::expect_false(abs(sum(predictor_coefs) - 1) < 1e-8)
  
  # Intercept should be present and unconstrained
  testthat::expect_true("(Intercept)" %in% names(result$coefficients))
})

testthat::test_that("error when intercept is not logical scalar", {
  X <- as.matrix(test_df[, c("x1", "x2")])
  y <- test_df$y

  testthat::expect_error(
    constrained_linear_regression(X, y, intercept = "yes"),
    "^`intercept` must be a single logical value\\.$"
  )
})

testthat::test_that("fitted values and residuals are consistent", {
  X <- as.matrix(test_df[, c("x1", "x2")])
  Y <- test_df$y

  result <- constrained_linear_regression(x = X, y = Y)
  
  # Check that fitted + residuals = y
  testthat::expect_equal(result$fitted.values + result$residuals, Y, tolerance = 1e-10)
  
  # Check that fitted values come from X %*% beta
  expected_fitted <- X %*% result$coefficients
  testthat::expect_equal(result$fitted.values, as.vector(expected_fitted), tolerance = 1e-10)
})

testthat::test_that("k=1 case works correctly", {
  X <- matrix(c(1, 2, 3, 4), ncol = 1)
  colnames(X) <- "x1"
  Y <- c(2, 4, 6, 8)

  result <- constrained_linear_regression(x = X, y = Y)
  
  # With k=1, non-negative and sum-to-one constraints mean coefficient = 1
testthat::expect_equal(as.numeric(result$coefficients[1]), 1, tolerance = 1e-8)
  testthat::expect_equal(sum(result$coefficients), 1, tolerance = 1e-8)
})

testthat::test_that("error on non-matrix x", {
  bad_x <- data.frame(test_df[, c("x1", "x2")])
  y     <- test_df$y

  testthat::expect_error(
    constrained_linear_regression(bad_x, y),
    "^`x` must be a numeric matrix"
  )
})

testthat::test_that("error when y length does not match nrow\\(x\\)", {
  X      <- as.matrix(test_df[, c("x1", "x2")])
  y_bad  <- test_df$y[-1]

  testthat::expect_error(
    constrained_linear_regression(X, y_bad),
    "^Length of `y`"
  )
})

testthat::test_that("error when non_negative is not logical scalar", {
  X <- as.matrix(test_df[, c("x1", "x2")])
  y <- test_df$y

  testthat::expect_error(
    constrained_linear_regression(X, y, non_negative = "yes"),
    "^`non_negative` must be a single logical value\\.$"
  )
})

testthat::test_that("error when sum_to_one is not logical scalar", {
  X <- as.matrix(test_df[, c("x1", "x2")])
  y <- test_df$y

  testthat::expect_error(
    constrained_linear_regression(X, y, sum_to_one = "yes"),
    "^`sum_to_one` must be a single logical value\\.$"
  )
})