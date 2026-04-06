#' Calculate Contribution to Return (CTR) Decomposition
#'
#' Decomposes fund returns into per-factor contributions using lagged
#' (out-of-sample) rolling regression betas. For each period t, betas
#' estimated from the window ending at t-1 are applied to factor returns
#' at t, producing a genuine out-of-sample decomposition.
#'
#' @param rolling_fit List with a coefficients matrix (as returned by
#'   roll_constrained_lm). Rows correspond to dates; columns are
#'   optionally "(Intercept)" followed by factor names matching
#'   factor_cols.
#' @param dates Date vector of length nrow(rolling_fit$coefficients),
#'   aligned 1:1 with coefficient rows.
#' @param fund_returns tibble with columns date and return.
#' @param factor_returns tibble with columns date and one column per
#'   factor named in factor_cols.
#' @param factor_cols Character vector of factor column names.
#'
#' @return tibble with columns: date, fund_return, alpha_ctr, one
#'   {factor}_ctr column per factor, total_explained, and residual.
#' @export
calculate_ctr <- function(
  rolling_fit,
  dates,
  fund_returns,
  factor_returns,
  factor_cols
) {
  if (!is.list(rolling_fit) || is.null(rolling_fit$coefficients)) {
    stop(
      "calculate_ctr(): [rolling_fit] must be a list with a 'coefficients' matrix."
    )
  }
  coef_mat <- as.matrix(rolling_fit$coefficients)

  avpipeline::validate_date_type(dates, scalar = FALSE, name = "dates")
  if (length(dates) != nrow(coef_mat)) {
    stop(paste0(
      "calculate_ctr(): [dates] must have length ",
      nrow(coef_mat),
      "; got length ",
      length(dates),
      "."
    ))
  }
  avpipeline::validate_df_cols(fund_returns, c("date", "return"))
  avpipeline::validate_df_cols(factor_returns, c("date", factor_cols))
  avpipeline::validate_non_empty(factor_cols, name = "factor_cols")

  missing_in_coefs <- setdiff(factor_cols, colnames(coef_mat))
  if (length(missing_in_coefs) > 0) {
    stop(paste0(
      "calculate_ctr(): [rolling_fit$coefficients] missing columns: ",
      paste(missing_in_coefs, collapse = ", "),
      "."
    ))
  }

  has_intercept <- "(Intercept)" %in% colnames(coef_mat)
  ctr_col_names <- paste0(factor_cols, "_ctr")
  beta_rename <- stats::setNames(factor_cols, paste0("beta_", factor_cols))

  # Lag betas by one period: beta estimated at t applied to returns at t+1
  coef_df <- tibble::as_tibble(as.data.frame(coef_mat)) %>%
    dplyr::mutate(estimation_date = dates) %>%
    dplyr::arrange(estimation_date) %>%
    dplyr::filter(!is.na(.data[[factor_cols[1]]])) %>%
    dplyr::mutate(apply_date = dplyr::lead(estimation_date, 1)) %>%
    dplyr::filter(!is.na(apply_date)) %>%
    dplyr::rename(!!!beta_rename)

  # Pivot betas and factor returns to long, multiply, pivot back
  betas_long <- coef_df %>%
    dplyr::select(apply_date, dplyr::all_of(paste0("beta_", factor_cols))) %>%
    tidyr::pivot_longer(
      cols = -apply_date,
      names_to = "factor",
      names_prefix = "beta_",
      values_to = "beta"
    )

  factors_long <- factor_returns %>%
    dplyr::select(date, dplyr::all_of(factor_cols)) %>%
    tidyr::pivot_longer(
      cols = -date,
      names_to = "factor",
      values_to = "factor_return"
    )

  ctr_wide <- betas_long %>%
    dplyr::inner_join(factors_long, by = c("apply_date" = "date", "factor")) %>%
    dplyr::mutate(ctr = beta * factor_return) %>%
    dplyr::select(apply_date, factor, ctr) %>%
    tidyr::pivot_wider(
      names_from = factor,
      values_from = ctr,
      names_glue = "{factor}_ctr"
    )

  coef_df %>%
    dplyr::select(apply_date, dplyr::any_of("(Intercept)")) %>%
    dplyr::inner_join(
      fund_returns %>% dplyr::select(date, fund_return = return),
      by = c("apply_date" = "date")
    ) %>%
    dplyr::inner_join(ctr_wide, by = "apply_date") %>%
    dplyr::mutate(
      alpha_ctr = if (has_intercept) .data[["(Intercept)"]] else 0,
      total_explained = alpha_ctr +
        rowSums(dplyr::pick(dplyr::all_of(ctr_col_names))),
      residual = fund_return - total_explained
    ) %>%
    dplyr::select(
      date = apply_date,
      fund_return,
      alpha_ctr,
      dplyr::all_of(ctr_col_names),
      total_explained,
      residual
    ) %>%
    dplyr::arrange(date)
}
