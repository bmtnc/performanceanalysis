#' Fetch Adjusted-Close Prices for Multiple Tickers
#'
#' @param tickers Character vector of ticker symbols.
#' @param api_key Character scalar or `NULL`. See `get_api_key()`.
#'
#' @return Tibble with columns `date`, `ticker`, `adjusted_close`, sorted by both.
#' @export
fetch_adjusted_prices <- function(tickers, api_key = NULL) {
  validate_tickers(tickers)
  key <- get_api_key(api_key)

  data_list <- lapply(
    tickers,
    function(sym) {
      res <- fetch_single_adjusted(sym, key)
      Sys.sleep(1)
      res
    }
  )

  dplyr::bind_rows(data_list) %>%
    dplyr::arrange(date, ticker)
}