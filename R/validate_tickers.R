#' Validate Ticker Vector
#'
#' @param tickers Character vector of symbols.
#'
#' @return Invisibly returns `TRUE` when validation passes.
#' @export
validate_tickers <- function(tickers) {
  if (!is.character(tickers) || length(tickers) == 0) {
    stop(paste0(
      "`tickers` must be a non-empty character vector. ",
      "Got type: ", typeof(tickers), ", length: ", length(tickers), "."
    ))
  }
  invisible(TRUE)
}
