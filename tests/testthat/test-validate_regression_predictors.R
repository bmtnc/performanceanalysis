test_df <- tibble::tribble(
  ~x1, ~x2, ~x3,
   1 ,  3 ,  2,
   2 ,  1 ,  5,
   4 ,  4 ,  3,
   3 ,  2 ,  6
)

testthat::test_that("passes validation with well-conditioned matrix", {
  X <- as.matrix(test_df)
  
  testthat::expect_silent(validate_regression_predictors(X))
})

testthat::test_that("error when input is not a matrix", {
  bad_input <- test_df
  
  testthat::expect_error(
    validate_regression_predictors(bad_input),
    "^Input must be a numeric matrix\\.$"
  )
})

testthat::test_that("error when input is not numeric", {
  bad_matrix <- matrix(c("a", "b", "c", "d"), nrow = 2)
  
  testthat::expect_error(
    validate_regression_predictors(bad_matrix),
    "^Input must be a numeric matrix\\.$"
  )
})

testthat::test_that("error when more predictors than observations", {
  X <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  
  testthat::expect_error(
    validate_regression_predictors(X),
    paste0("^More predictors \\(3\\) than observations \\(2\\)\\. ",
           "This makes the system unsolvable\\. ",
           "Please reduce the number of predictors or collect more data\\.$")
  )
})

testthat::test_that("error when predictors equal observations", {
  X <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  
  testthat::expect_error(
    validate_regression_predictors(X),
    paste0("^More predictors \\(2\\) than observations \\(2\\)\\. ",
           "This makes the system unsolvable\\. ",
           "Please reduce the number of predictors or collect more data\\.$")
  )
})

testthat::test_that("error when constant column detected with colnames", {
  constant_df <- tibble::tribble(
    ~x1, ~x2, ~x3,
     1 ,  5 ,  2,
     2 ,  5 ,  5,
     4 ,  5 ,  3,
     3 ,  5 ,  6
  )
  X <- as.matrix(constant_df)
  
  testthat::expect_error(
    validate_regression_predictors(X),
    paste0("^Constant columns detected: x2\\. ",
           "These columns have no variation and cannot be used for prediction\\.$")
  )
})

testthat::test_that("error when constant column detected without colnames", {
  X <- matrix(c(1, 2, 4, 3, 5, 5, 5, 5, 2, 5, 3, 6), nrow = 4, ncol = 3)
  
  testthat::expect_error(
    validate_regression_predictors(X),
    paste0("^Constant columns detected: Column 2\\. ",
           "These columns have no variation and cannot be used for prediction\\.$")
  )
})

testthat::test_that("error when multiple constant columns detected", {
  constant_df <- tibble::tribble(
    ~x1, ~x2, ~x3,
     1 ,  5 ,  8,
     2 ,  5 ,  8,
     4 ,  5 ,  8,
     3 ,  5 ,  8
  )
  X <- as.matrix(constant_df)
  
  testthat::expect_error(
    validate_regression_predictors(X),
    paste0("^Constant columns detected: x2, x3\\. ",
           "These columns have no variation and cannot be used for prediction\\.$")
  )
})

testthat::test_that("error when highly correlated columns detected with colnames", {
  corr_df <- tibble::tribble(
    ~x1, ~x2, ~x3,
     1 ,  2 ,  2,
     2 ,  4 ,  5,
     3 ,  6 ,  3,
     4 ,  8 ,  6
  )
  X <- as.matrix(corr_df)
  
  testthat::expect_error(
    validate_regression_predictors(X),
    paste0("^Highly correlated predictors detected: x2 and x1 \\(correlation = 1\\.000\\)\\. ",
           "This can cause unstable coefficient estimates\\. ",
           "Consider removing one of these predictors\\.$")
  )
})

testthat::test_that("error when highly correlated columns detected without colnames", {
  X <- matrix(c(1, 2, 3, 4, 2, 4, 6, 8, 2, 5, 3, 6), nrow = 4, ncol = 3)
  
  testthat::expect_error(
    validate_regression_predictors(X),
    paste0("^Highly correlated predictors detected: Column 2 and Column 1 \\(correlation = 1\\.000\\)\\. ",
           "This can cause unstable coefficient estimates\\. ",
           "Consider removing one of these predictors\\.$")
  )
})

testthat::test_that("passes with correlation below threshold", {
  moderate_corr_df <- tibble::tribble(
    ~x1, ~x2, ~x3,
     1 ,  2 ,  2,
     2 ,  3 ,  5,
     3 ,  5 ,  3,
     4 ,  7 ,  6
  )
  X <- as.matrix(moderate_corr_df)
  
  testthat::expect_silent(validate_regression_predictors(X))
})

testthat::test_that("error when correlation exceeds custom threshold", {
  moderate_corr_df <- tibble::tribble(
    ~x1, ~x2, ~x3,
     1 ,  2 ,  2,
     2 ,  3 ,  5,
     3 ,  5 ,  3,
     4 ,  7 ,  6
  )
  X <- as.matrix(moderate_corr_df)
  
  testthat::expect_error(
    validate_regression_predictors(X, cor_threshold = 0.8),
    paste0("^Highly correlated predictors detected: x2 and x1 \\(correlation = 0\\.990\\)\\. ",
           "This can cause unstable coefficient estimates\\. ",
           "Consider removing one of these predictors\\.$")
  )
})

testthat::test_that("passes with single column matrix", {
  X <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1)
  
  testthat::expect_silent(validate_regression_predictors(X))
})

testthat::test_that("error when single column is constant", {
  X <- matrix(c(5, 5, 5, 5), nrow = 4, ncol = 1)
  
  testthat::expect_error(
    validate_regression_predictors(X),
    paste0("^Constant columns detected: Column 1\\. ",
           "These columns have no variation and cannot be used for prediction\\.$")
  )
})

testthat::test_that("passes with negative correlation", {
  neg_corr_df <- tibble::tribble(
    ~x1, ~x2, ~x3,
     1 ,  3 ,  2,
     2 ,  2 ,  5,
     3 ,  1 ,  3,
     4 ,  2 ,  6
  )
  X <- as.matrix(neg_corr_df)
  
  testthat::expect_silent(validate_regression_predictors(X))
})

testthat::test_that("error when negative correlation exceeds threshold", {
  neg_corr_df <- tibble::tribble(
    ~x1, ~x2, ~x3,
     1 ,  4 ,  2,
     2 ,  3 ,  5,
     3 ,  2 ,  3,
     4 ,  1 ,  6
  )
  X <- as.matrix(neg_corr_df)
  
  testthat::expect_error(
    validate_regression_predictors(X, cor_threshold = 0.8),
    paste0("^Highly correlated predictors detected: x2 and x1 \\(correlation = -1\\.000\\)\\. ",
           "This can cause unstable coefficient estimates\\. ",
           "Consider removing one of these predictors\\.$")
  )
})