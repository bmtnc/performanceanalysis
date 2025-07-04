#' Rolling constrained linear regression
#'
#' Performs constrained OLS (via `constrained_linear_regression()`) on a
#' sliding window using slider for clean iteration.
#'
#' @param x Numeric matrix / data.frame of predictors (n × k).
#' @param y Numeric response vector (length n).
#' @param width Integer; window width.
#' @param ...  Arguments forwarded to `constrained_linear_regression()`
#'             (e.g. `non_negative`, `sum_to_one`).
#'
#' @return A list with a `coefficients` matrix identical to `roll::roll_lm()`.
#' @export
roll_constrained_lm <- function(x, y, width, ...) {
    
    # Input validation
    if (!is.numeric(width) || length(width) != 1 || width <= 0 || width != as.integer(width)) {
        stop("`width` must be a positive integer.")
    }
    
    if (!is.numeric(y)) {
        stop("`y` must be numeric.")
    }
    
    x <- as.matrix(x)
    y <- as.numeric(y)
    
    if (length(y) != nrow(x)) {
        stop("Length of `y` must match number of rows in `x`.")
    }
    
    # Create a data frame to slide over (since we need to keep x and y together)
    data_df <- data.frame(y = y, x)
    
    # Use slide to create rolling windows
    results <- slider::slide(
        data_df,
        function(window_data) {
            if (nrow(window_data) < width) {
                return(rep(NA_real_, ncol(x) + 1L))
            }
            
            # Extract the last `width` rows if window is larger than needed
            if (nrow(window_data) > width) {
                window_data <- window_data[(nrow(window_data) - width + 1):nrow(window_data), ]
            }
            
            fit <- constrained_linear_regression(
                x = as.matrix(window_data[, -1, drop = FALSE]),  # Remove y column
                y = window_data$y,
                intercept = TRUE,
                ...
            )
            
            fit$coefficients
        },
        .before = width - 1L,  # This creates windows of size `width`
        .complete = FALSE
    )
    
    # Convert to matrix format
    coef_matrix <- do.call(rbind, results)
    colnames(coef_matrix) <- c("(Intercept)", colnames(x))
    
    structure(
        list(coefficients = coef_matrix,
            width        = width,
            call         = match.call()),
            class = "roll_constrained_lm"
        )
}