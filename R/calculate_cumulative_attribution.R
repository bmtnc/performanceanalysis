#' Calculate Cumulative Attribution Using Carino Linking
#'
#' Converts daily attribution components into cumulative values using
#' Carino linking methodology. This ensures that cumulative components
#' maintain the additive identity: cumulative_factor + cumulative_selection = cumulative_excess.
#' Uses geometric weighting to properly account for compounding effects.
#'
#' @param daily_attribution tibble: Daily attribution with columns date, excess_return, factor_contribution, selection_effect
#' @return tibble: Input data with added cumulative columns
#' @keywords internal
calculate_cumulative_attribution <- function(daily_attribution) {
  if (!is.data.frame(daily_attribution)) {
    stop(paste0("calculate_cumulative_attribution(): [daily_attribution] must be a data.frame, not ", class(daily_attribution)[1]))
  }

  required_cols <- c("date", "excess_return", "factor_contribution", "selection_effect")
  if (!all(required_cols %in% colnames(daily_attribution))) {
    stop(paste0("calculate_cumulative_attribution(): [daily_attribution] missing required columns: ", paste(setdiff(required_cols, colnames(daily_attribution)), collapse = ", ")))
  }

  if (!is.numeric(daily_attribution$excess_return)) {
    stop(paste0("calculate_cumulative_attribution(): [excess_return] must be numeric, not ", class(daily_attribution$excess_return)[1]))
  }
  if (!is.numeric(daily_attribution$factor_contribution)) {
    stop(paste0("calculate_cumulative_attribution(): [factor_contribution] must be numeric, not ", class(daily_attribution$factor_contribution)[1]))
  }
  if (!is.numeric(daily_attribution$selection_effect)) {
    stop(paste0("calculate_cumulative_attribution(): [selection_effect] must be numeric, not ", class(daily_attribution$selection_effect)[1]))
  }

  sorted_data <- daily_attribution %>%
    dplyr::arrange(date)

  n <- nrow(sorted_data)
  cumulative_excess <- numeric(n)
  cumulative_factor <- numeric(n)
  cumulative_selection <- numeric(n)

  cum_excess <- 0.0
  sum_weighted_factor <- 0.0
  sum_weighted_selection <- 0.0
  cum_product <- 1.0

  for (i in seq_len(n)) {
    prev_product <- cum_product
    cum_product <- cum_product * (1 + sorted_data$excess_return[i])
    cum_excess <- cum_product - 1
    cumulative_excess[i] <- cum_excess

    if (abs(sorted_data$excess_return[i]) < 1e-10) {
      k_factor <- 1.0
    } else {
      k_factor <- log(1 + sorted_data$excess_return[i]) / sorted_data$excess_return[i]
    }

    sum_weighted_factor <- sum_weighted_factor + sorted_data$factor_contribution[i] * k_factor
    sum_weighted_selection <- sum_weighted_selection + sorted_data$selection_effect[i] * k_factor

    total_weighted <- sum_weighted_factor + sum_weighted_selection

    if (abs(total_weighted) < 1e-12) {
      cumulative_factor[i] <- 0.0
      cumulative_selection[i] <- 0.0
    } else {
      cumulative_factor[i] <- cum_excess * (sum_weighted_factor / total_weighted)
      cumulative_selection[i] <- cum_excess * (sum_weighted_selection / total_weighted)
    }
  }

  sorted_data %>%
    dplyr::mutate(
      cumulative_excess = cumulative_excess,
      cumulative_factor = cumulative_factor,
      cumulative_selection = cumulative_selection
    )
}
