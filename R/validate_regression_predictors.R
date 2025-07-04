#' Check for Common Matrix Problems
#'
#' Validates that a design matrix doesn't have obvious problems that would
#' make coefficient estimation unreliable. Checks for highly correlated
#' predictors, constant columns, and insufficient observations.
#'
#' @details
#' This function performs three critical checks:
#'
#' 1. Too Many Predictors vs. Observations
#' If you have more predictors than observations (p >= n), there are infinite 
#' possible solutions. Solution: reduce predictors or collect more data.
#'
#' 2. Constant Columns
#' Columns with no variation provide zero information for prediction.
#' Solution: remove these columns or check your data filtering.
#'
#' 3. Highly Correlated Predictors
#' When predictors are nearly identical (correlation > 0.99), coefficients
#' become unstable and hard to interpret. Solution: remove one of the 
#' correlated predictors or combine them.
#'
#' These checks are especially important for constrained regression because
#' constraints make optimization more sensitive to data quality issues.

#' @param x A numeric matrix (design matrix) to check.
#' @param cor_threshold Numeric scalar; correlation threshold above which to fail (default 0.99).
#'
#' @return NULL (called for side effects - throws error if problems found)
#' 
validate_regression_predictors <- function(x, cor_threshold = 0.99) {
    if (!is.matrix(x) || !is.numeric(x)) {
        stop("Input must be a numeric matrix.")
    }
    
    n <- nrow(x)
    p <- ncol(x)
    
    # Check 1: More predictors than observations
    if (p >= n) {
        stop(paste0(
            "More predictors (", p, ") than observations (", n, "). ",
            "This makes the system unsolvable. ",
            "Please reduce the number of predictors or collect more data."
        ))
    }
    
    # Check 2: Constant columns (no variation)
    col_vars <- apply(x, 2, var)
    constant_cols <- which(col_vars < 1e-10)
    if (length(constant_cols) > 0) {
        col_names <- colnames(x)[constant_cols]
        if (is.null(col_names)) col_names <- paste0("Column ", constant_cols)
        stop(paste0(
            "Constant columns detected: ", paste(col_names, collapse = ", "), ". ",
            "These columns have no variation and cannot be used for prediction."
        ))
    }
    
    # Check 3: Highly correlated columns
    if (p > 1) {
        cor_matrix <- cor(x)
        # Set diagonal to 0 to ignore self-correlations
        diag(cor_matrix) <- 0
        
        # Find pairs with high correlation
        high_cor_pairs <- which(abs(cor_matrix) > cor_threshold, arr.ind = TRUE)
        
        if (nrow(high_cor_pairs) > 0) {
            # Get column names for the first problematic pair
            col1 <- high_cor_pairs[1, 1]
            col2 <- high_cor_pairs[1, 2]
            
            # Handle column names properly
            if (is.null(colnames(x))) {
                col1_name <- paste0("Column ", col1)
                col2_name <- paste0("Column ", col2)
            } else {
                col1_name <- colnames(x)[col1]
                col2_name <- colnames(x)[col2]
            }
            
            correlation <- cor_matrix[col1, col2]
            
            stop(paste0(
                "Highly correlated predictors detected: ",
                col1_name, " and ", col2_name, 
                " (correlation = ", sprintf("%.3f", correlation), "). ",
                "This can cause unstable coefficient estimates. ",
                "Consider removing one of these predictors."
            ))
        }
    }
    
    invisible(NULL)
}