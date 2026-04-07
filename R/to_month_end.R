#' Convert dates to month-end
#'
#' Normalizes dates to the last calendar day of their month. Designed for
#' use inside dplyr::mutate to align monthly time series that use different
#' date conventions (e.g., FRED's first-of-month vs AQR/Bloomberg month-end).
#'
#' @param x Date vector (or coercible to Date)
#'
#' @return Date vector with each date moved to the last day of its month
#' @export
to_month_end <- function(x) {
  x <- as.Date(x)
  lubridate::ceiling_date(x, "month") - 1L
}
