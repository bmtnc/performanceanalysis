#' Compound simple returns
#'
#' Geometrically compounds a vector of simple returns into a single
#' period return. Designed for use inside dplyr::summarise.
#'
#' @param x Numeric vector of simple returns
#'
#' @return A single numeric value: the compounded return
#' @export
compound_returns <- function(x) {
  exp(sum(log(1 + x))) - 1
}
