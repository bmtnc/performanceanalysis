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
  avpipeline::validate_df_cols(
    daily_attribution,
    c("date", "excess_return", "factor_contribution", "selection_effect")
  )
  avpipeline::validate_numeric_vector(daily_attribution$excess_return, name = "excess_return")
  avpipeline::validate_numeric_vector(daily_attribution$factor_contribution, name = "factor_contribution")
  avpipeline::validate_numeric_vector(daily_attribution$selection_effect, name = "selection_effect")

  sorted_data <- daily_attribution %>%
    dplyr::arrange(date)

  linked <- carino_link(
    sorted_data$excess_return,
    tibble::tibble(
      factor = sorted_data$factor_contribution,
      selection = sorted_data$selection_effect
    )
  )

  sorted_data %>%
    dplyr::mutate(
      cumulative_excess = linked$cumulative_total,
      cumulative_factor = linked$cumulative_factor,
      cumulative_selection = linked$cumulative_selection
    )
}
