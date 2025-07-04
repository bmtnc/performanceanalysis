#' Calculate Log Returns for Panel Data
#'
#' Takes a dataframe indexed by date and ticker and adds a log return column.
#' The function calculates returns as log(price_t / price_{t-1}) grouped by ticker.
#'
#' @param data A data.frame containing date, ticker, and price columns
#' @param date_col Character string specifying the date column name (default: "date")
#' @param ticker_col Character string specifying the ticker column name (default: "ticker")
#' @param price_col Character string specifying the price column name (default: "adjusted_close")
#' @param return_col Character string specifying the name for the new return column (default: "return")
#'
#' @return A data.frame with the same structure as input plus the specified return column
#' @export
calculate_log_returns <- function(
    data, 
    date_col = "date", 
    ticker_col = "ticker", 
    price_col = "adjusted_close",
    return_col = "return"
) {
    
    required_cols <- c(date_col, ticker_col, price_col)
    validate_df_cols(data, required_cols)
    
    # Input validation for column name parameters
    if (!is.character(date_col) || length(date_col) != 1) {
        stop(paste0("Input 'date_col' must be a single character string. Received: ", 
        class(date_col)[1], " of length ", length(date_col)))
    }
    if (!is.character(ticker_col) || length(ticker_col) != 1) {
        stop(paste0("Input 'ticker_col' must be a single character string. Received: ", 
        class(ticker_col)[1], " of length ", length(ticker_col)))
    }
    if (!is.character(price_col) || length(price_col) != 1) {
        stop(paste0("Input 'price_col' must be a single character string. Received: ", 
        class(price_col)[1], " of length ", length(price_col)))
    }
    if (!is.character(return_col) || length(return_col) != 1) {
        stop(paste0("Input 'return_col' must be a single character string. Received: ", 
        class(return_col)[1], " of length ", length(return_col)))
    }
    if (!inherits(data[[date_col]], "Date")) {
        stop(paste0("Column '", date_col, "' must be of type Date. Received: ", 
        class(data[[date_col]])[1]))
    }
    if (!is.numeric(data[[price_col]])) {
        stop(paste0("Column '", price_col, "' must be numeric. Received: ", 
        class(data[[price_col]])[1]))
    }
    
    data %>%
        dplyr::arrange(!!rlang::sym(ticker_col), !!rlang::sym(date_col)) %>%
        dplyr::group_by(!!rlang::sym(ticker_col)) %>%
        dplyr::mutate(
            !!rlang::sym(return_col) := log(!!rlang::sym(price_col) /
            dplyr::lag(!!rlang::sym(price_col)))
        ) %>%
        dplyr::ungroup()
}