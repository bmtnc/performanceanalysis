#' Calculate performance cone
#'
#' Enriches a returns data frame with a cone of expected performance paths
#' given a Sharpe ratio and volatility assumption. The center line is the
#' geometric growth rate (arithmetic return minus variance drain). Sigma
#' bands capture the lognormal dispersion of cumulative wealth over time.
#'
#' @param returns Data frame with date and return columns (simple returns)
#' @param sharpe_ratio Assumed Sharpe ratio (annualized, excess return / vol)
#' @param volatility Assumed annualized volatility (e.g. 0.15 for 15 percent)
#' @param n_sigma Number of standard deviation bands to compute (default 3)
#' @param date_col Name of the date column (default "date")
#' @param return_col Name of the return column (default "return")
#'
#' @return The input data frame with added columns: cumulative_return,
#'   t_years, center, upper_1, lower_1, ..., upper_n, lower_n.
#'   Cone values are cumulative returns (starting at 0). The result
#'   carries attributes: sharpe_ratio, volatility, mu_arithmetic,
#'   mu_geometric, variance_drain.
#' @export
calculate_performance_cone <- function(returns, sharpe_ratio, volatility,
                                       n_sigma = 3L, date_col = "date",
                                       return_col = "return") {
  validate_df_cols(returns, c(date_col, return_col))
  stopifnot(
    is.numeric(sharpe_ratio), length(sharpe_ratio) == 1,
    is.numeric(volatility), length(volatility) == 1, volatility > 0,
    is.numeric(n_sigma), n_sigma >= 1
  )

  # Prepend inception row (return = 0) one month before first observation
  # so cumulative series starts at 0 without a duplicate date
  inception <- returns[1, ]
  inception[[date_col]] <- seq.Date(inception[[date_col]], by = "-1 month", length.out = 2)[2]
  inception[[return_col]] <- 0
  returns <- dplyr::bind_rows(inception, returns)

  dates <- returns[[date_col]]
  rets <- returns[[return_col]]

  # Time in years from inception
  t_years <- as.numeric(difftime(dates, min(dates), units = "days")) / 365.25

  # Arithmetic excess return from Sharpe and vol
  mu_a <- sharpe_ratio * volatility

  # Geometric growth rate via variance drain
  mu_g <- mu_a - (volatility^2) / 2

  result <- returns
  result[["cumulative_return"]] <- cumulate_returns(rets)
  result[["t_years"]] <- t_years
  result[["center"]] <- exp(mu_g * t_years) - 1

  for (k in seq_len(n_sigma)) {
    result[[paste0("upper_", k)]] <-
      exp(mu_g * t_years + k * volatility * sqrt(t_years)) - 1
    result[[paste0("lower_", k)]] <-
      exp(mu_g * t_years - k * volatility * sqrt(t_years)) - 1
  }

  attr(result, "sharpe_ratio") <- sharpe_ratio
  attr(result, "volatility") <- volatility
  attr(result, "mu_arithmetic") <- mu_a
  attr(result, "mu_geometric") <- mu_g
  attr(result, "variance_drain") <- (volatility^2) / 2

  result
}
