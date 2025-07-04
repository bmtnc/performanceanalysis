#' Constrained Linear Regression via Quadratic Programming
#'
#' Performs a linear regression subject to optional constraints:
#' (i) coefficients must be non-negative and/or (ii) coefficients must sum to one.
#' If both constraints are \code{FALSE} the function returns the ordinary
#' least-squares (OLS) solution, reproducing \code{lm(y ~ x - 1)} exactly.
#'
#' @param x A numeric matrix whose rows are observations and columns are predictors.
#' @param y A numeric vector of responses; \code{length(y)} must equal \code{nrow(x)}.
#' @param non_negative Logical scalar; if \code{TRUE} (default) coefficients are constrained to be ≥ 0.
#' @param sum_to_one   Logical scalar; if \code{TRUE} (default) coefficients are constrained to sum to 1.
#' @param intercept    Logical scalar; if \code{TRUE} an intercept is included (constraints apply only to non-intercept coefficients).
#'
#' @return List with components \code{coefficients}, \code{fitted.values}, and \code{residuals}.
#' @export
constrained_linear_regression <- function(
    x,
    y,
    non_negative = TRUE,
    sum_to_one   = TRUE,
    intercept    = FALSE
) {
    # ---- input validation ------------------------------------------------------
    if (!is.matrix(x) || !is.numeric(x)) {
        stop(paste0("`x` must be a numeric matrix; got object of class ",
        paste(class(x), collapse = ", "), "."))
    }
    if (!is.numeric(y) || is.matrix(y) || is.data.frame(y)) {
        stop(paste0("`y` must be a numeric vector; got object of class ",
        paste(class(y), collapse = ", "), "."))
    }
    if (length(y) != nrow(x)) {
        stop(paste0("Length of `y` (", length(y),
        ") must equal the number of rows in `x` (", nrow(x), ")."))
    }
    if (!is.logical(non_negative) || length(non_negative) != 1) {
        stop("`non_negative` must be a single logical value.")
    }
    if (!is.logical(sum_to_one) || length(sum_to_one) != 1) {
        stop("`sum_to_one` must be a single logical value.")
    }
    if (!is.logical(intercept) || length(intercept) != 1) {
        stop("`intercept` must be a single logical value.")
    }
    
    # ---- prepare design matrix -------------------------------------------------
    if (intercept) {
        x_design <- cbind(1, x)
        colnames(x_design) <- c("(Intercept)", colnames(x))
    } else {
        x_design <- x
    }
    
    # ---- validate design matrix ------------------------------------------------
    validate_regression_predictors(x)
    
    k <- ncol(x_design)
    k_predictors <- ncol(x)  # number of non-intercept coefficients
    
    # ---- unconstrained case: ordinary least squares ---------------------------
    if (!non_negative && !sum_to_one) {
        betas        <- base::solve(base::crossprod(x_design), base::crossprod(x_design, y))
        fitted_vals  <- x_design %*% betas
        residuals    <- y - fitted_vals
        
        # add names to coefficients
        coef_vector <- as.vector(betas)
        names(coef_vector) <- colnames(x_design)
        
        return(list(
            coefficients  = coef_vector,
            fitted.values = as.vector(fitted_vals),
            residuals     = as.vector(residuals)
        ))
    }
    
    # ---- constrained case: build QP -------------------------------------------
    Dmat <- 2 * base::crossprod(x_design)      # 2 * X'X
    dvec <- 2 * base::crossprod(x_design, y)   # 2 * X'y
    
    # Build constraints for non-intercept coefficients only
    constraints <- construct_linear_regression_constraints(k_predictors, non_negative, sum_to_one)
    
    # If intercept is present, expand constraint matrix to include unconstrained intercept
    if (intercept) {
        # Add column of zeros for intercept (first coefficient is unconstrained)
        if (nrow(constraints$Amat) > 0) {
            constraints$Amat <- cbind(rep(0, nrow(constraints$Amat)), constraints$Amat)
        } else {
            constraints$Amat <- matrix(0, nrow = 0, ncol = k)
        }
    }
    
    qp_solution <- quadprog::solve.QP(
        Dmat = Dmat,
        dvec = dvec,
        Amat = base::t(constraints$Amat),
        bvec = constraints$bvec,
        meq  = constraints$meq
    )
    
    betas       <- qp_solution$solution
    fitted_vals <- x_design %*% betas
    residuals   <- y - fitted_vals
    
    # add names to coefficients
    coef_vector <- as.vector(betas)
    names(coef_vector) <- colnames(x_design)
    
    list(
        coefficients  = coef_vector,
        fitted.values = as.vector(fitted_vals),
        residuals     = as.vector(residuals)
    )
}