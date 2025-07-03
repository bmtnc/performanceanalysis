#' Fetch Adjusted-Close Prices for One Ticker
#'
#' @param symbol Character scalar ticker.
#' @param api_key Character scalar API key.
#'
#' @return Tibble with columns `date`, `ticker`, `adjusted_close`, or `NULL`.
#' @export
fetch_single_adjusted <- function(symbol, api_key) {
  json_data <- fetch_symbol_json(symbol, api_key)
  json_to_adjusted_tibble(json_data, symbol)
}
