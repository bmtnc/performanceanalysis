#' Download Raw JSON for a Single Symbol
#'
#' @param symbol Character scalar ticker.
#' @param api_key Character scalar API key.
#'
#' @return Parsed JSON (list) or `NULL` on failure.
#' @export
fetch_symbol_json <- function(symbol, api_key) {
  resp <- httr::RETRY(
    verb      = "GET",
    url       = "https://www.alphavantage.co/query",
    query     = list(
      "function"   = "TIME_SERIES_DAILY_ADJUSTED",
      symbol       = symbol,
      outputsize   = "full",
      apikey       = api_key,
      datatype     = "json"
    ),
    times     = 3,
    pause_min = 2
  )

  if (httr::http_error(resp)) {
    warning("HTTP error while retrieving data for symbol: ", symbol)
    return(NULL)
  }

  dat_txt  <- httr::content(resp, "text", encoding = "UTF-8")
  jsonlite::fromJSON(dat_txt, simplifyDataFrame = TRUE)
}
