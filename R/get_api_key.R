#' Get a Valid Alpha Vantage API Key
#'
#' @param api_key Character scalar or `NULL`. If `NULL`, fallback to environment
#'   variable `ALPHA_VANTAGE_API_KEY`.
#'
#' @return Character scalar containing a non-empty API key.
#' @export
get_api_key <- function(api_key = NULL) {
  if (is.null(api_key)) {
    api_key <- Sys.getenv("ALPHA_VANTAGE_API_KEY")
  }
  if (!is.character(api_key) || length(api_key) != 1 || !nzchar(api_key)) {
    stop(paste0(
      "`api_key` must be a non-empty character scalar or be set in ",
      "`ALPHA_VANTAGE_API_KEY`."
    ))
  }
  api_key
}
