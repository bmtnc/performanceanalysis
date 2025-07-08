#' Calculate Equal-Weighted Basket Returns
#'
#' Calculates equal-weighted returns for a basket of tickers from individual ticker return data.
#' Only includes dates where all basket tickers have complete return observations.
#'
#' @param return_data A data frame containing individual ticker returns with columns: date, ticker, return
#' @param basket_tickers A character vector of ticker symbols to include in the basket
#' @return A data frame with columns: date, basket_return
#' @export
calculate_basket_returns <- function(return_data, basket_tickers) {
    
    # Validate data frame and required columns
    validate_df_cols(return_data, c("date", "ticker", "return"))
    
    # Validate basket_tickers
    if (!is.character(basket_tickers)) {
        stop(paste0("basket_tickers must be a character vector, got: ", class(basket_tickers)[1]))
    }
    
    if (length(basket_tickers) == 0) {
        stop("basket_tickers cannot be empty")
    }
    
    # Check if basket tickers exist in data
    available_tickers <- unique(return_data$ticker)
    missing_tickers <- setdiff(basket_tickers, available_tickers)
    if (length(missing_tickers) > 0) {
        stop(paste0("The following basket_tickers are not found in return_data: ", 
                   paste(missing_tickers, collapse = ", ")))
    }
    
    # Check if we have any data for the basket tickers
    basket_data <- return_data %>%
        dplyr::filter(ticker %in% basket_tickers)
    
    if (nrow(basket_data) == 0) {
        stop("No return data found for the specified basket_tickers")
    }
    
    # Calculate basket returns
    result <- basket_data %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(
            basket_return = mean(return, na.rm = TRUE),
            n_tickers = dplyr::n(),
            .groups = "drop"
        ) %>%
        dplyr::filter(n_tickers == length(basket_tickers)) %>%
        dplyr::select(date, basket_return)
    
    # Check if we have any complete observations
    if (nrow(result) == 0) {
        stop(paste0("No dates found with complete return data for all ", length(basket_tickers), 
                   " basket tickers"))
    }
    
    result
}