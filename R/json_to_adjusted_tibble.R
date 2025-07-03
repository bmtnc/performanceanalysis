#' Convert Alpha Vantage JSON to Tidy Tibble (Adjusted Close Only)
#'
#' @param json_data Parsed JSON list from `fetch_symbol_json()`.
#' @param symbol Character scalar ticker.
#'
#' @return Tibble with columns `date`, `ticker`, `adjusted_close`, or `NULL`.
#' @export
json_to_adjusted_tibble <- function(json_data, symbol) {
  if (is.null(json_data) || !is.null(json_data[["Error Message"]])) {
    warning("Alpha Vantage returned an error for symbol: ", symbol)
    return(NULL)
  }

  ts <- json_data[["Time Series (Daily)"]]
  if (is.null(ts)) {
    warning("No time-series data found for symbol: ", symbol)
    return(NULL)
  }

  adj_close <- vapply(
    ts,
    function(x) as.numeric(x[["5. adjusted close"]]),
    numeric(1)
  )

  tibble::tibble(
    date           = as.Date(names(ts)),
    ticker         = symbol,
    adjusted_close = adj_close
  )
}
