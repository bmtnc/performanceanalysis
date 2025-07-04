#' Build Linear Regression Constraints for Quadratic Programming
#'
#' Constructs constraint matrix, bounds vector, and equality constraint count
#' for use with quadprog::solve.QP() in constrained linear regression.
#'
#' @param k Integer; number of coefficients (columns in design matrix).
#' @param non_negative Logical scalar; if TRUE coefficients are constrained to be >= 0.
#' @param sum_to_one Logical scalar; if TRUE coefficients are constrained to sum to 1.
#'
#' @return List with components: Amat (constraint matrix), bvec (bounds vector), meq (equality count).
#' @export
construct_linear_regression_constraints <- function(k, non_negative, sum_to_one) {
    
    if (!is.numeric(k) || length(k) != 1 || k <= 0 || k != as.integer(k)) {
        stop(paste0("`k` must be a positive integer; got ", k, "."))
    }
    if (!is.logical(non_negative) || length(non_negative) != 1) {
        stop("`non_negative` must be a single logical value.")
    }
    if (!is.logical(sum_to_one) || length(sum_to_one) != 1) {
        stop("`sum_to_one` must be a single logical value.")
    }
    
    if (non_negative && sum_to_one) {
        # Both constraints: sum-to-one (equality) + non-negativity (inequality)
        Amat <- rbind(rep(1, k), diag(k))
        bvec <- c(1, rep(0, k))
        meq <- 1L
    } else if (non_negative && !sum_to_one) {
        # Only non-negativity constraints (inequality)
        Amat <- diag(k)
        bvec <- rep(0, k)
        meq <- 0L
    } else if (!non_negative && sum_to_one) {
        # Only sum-to-one constraint (equality)
        Amat <- matrix(rep(1, k), nrow = 1)
        bvec <- 1
        meq <- 1L
    } else {
        # No constraints (should not be called in practice for constrained regression)
        Amat <- matrix(0, nrow = 0, ncol = k)
        bvec <- numeric(0)
        meq <- 0L
    }
    
    list(
        Amat = Amat,
        bvec = bvec,
        meq = meq
    )
}
