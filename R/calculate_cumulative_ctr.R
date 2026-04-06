#' Calculate Cumulative Contribution to Return Using Carino Linking
#'
#' Cumulates per-period CTR components into running totals using Carino
#' log-linking, preserving the additive identity at every point:
#' cumulative_fund_return = cumulative_alpha_ctr + sum(cumulative_factor_ctrs) + cumulative_residual.
#'
#' @param ctr_data tibble as returned by calculate_ctr(),
#'   with columns date, fund_return, alpha_ctr, factor CTR columns
#'   (ending in "_ctr"), and residual.
#' @param factor_ctr_cols Character vector of the factor CTR column names
#'   (e.g., c("eq_hml_ctr", "fi_carry_ctr")).
#'
#' @return tibble: input data with added cumulative columns
#'   (cumulative_fund_return, cumulative_alpha_ctr,
#'   cumulative_{factor}_ctr for each factor, cumulative_residual).
#' @export
calculate_cumulative_ctr <- function(ctr_data, factor_ctr_cols) {
  avpipeline::validate_df_cols(
    ctr_data,
    c("date", "fund_return", "alpha_ctr", "residual")
  )
  avpipeline::validate_non_empty(factor_ctr_cols, name = "factor_ctr_cols")
  avpipeline::validate_df_cols(ctr_data, factor_ctr_cols)

  component_cols <- c("alpha_ctr", factor_ctr_cols, "residual")
  for (col in component_cols) {
    avpipeline::validate_numeric_vector(ctr_data[[col]], name = col)
  }

  sorted_data <- ctr_data %>%
    dplyr::arrange(date)

  linked <- carino_link(
    sorted_data$fund_return,
    sorted_data %>% dplyr::select(dplyr::all_of(component_cols))
  )

  # Rename cumulative_total -> cumulative_fund_return for clarity
  sorted_data %>%
    dplyr::mutate(
      cumulative_fund_return = linked$cumulative_total
    ) %>%
    dplyr::bind_cols(
      linked %>% dplyr::select(-cumulative_total)
    )
}
