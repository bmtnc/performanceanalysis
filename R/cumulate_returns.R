#' Cumulate simple returns
#'
#' Computes the running cumulative return from a vector of simple
#' returns. Designed for use inside dplyr::mutate.
#'
#' @param x Numeric vector of simple returns
#'
#' @return Numeric vector of cumulative returns (same length as input)
#' @export
cumulate_returns <- function(x) {
  exp(cumsum(log(1 + x))) - 1
}
