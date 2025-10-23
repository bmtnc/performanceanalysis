#' Calculate Factor Attribution for Portfolio Returns
#'
#' Decomposes excess returns between target and benchmark into factor
#' contribution and selection effect using out-of-sample attribution.
#' Uses factor weights from period t to explain returns in period t+1.
#'
#' @param target_weights tibble: Factor weights for target portfolio with columns date, factor columns
#' @param benchmark_weights tibble: Factor weights for benchmark portfolio with columns date, factor columns
#' @param factor_returns tibble: Factor returns with columns date, factor columns
#' @param target_returns tibble: Target actual returns with columns date, return
#' @param benchmark_returns tibble: Benchmark actual returns with columns date, return
#' @param factor_cols character: Vector of factor column names to use
#' @return tibble: Daily attribution with excess_return, factor_contribution, selection_effect columns
#' @keywords internal
calculate_factor_attribution <- function(
  target_weights,
  benchmark_weights,
  factor_returns,
  target_returns,
  benchmark_returns,
  factor_cols
) {
  if (!is.data.frame(target_weights)) {
    stop(paste0("calculate_factor_attribution(): [target_weights] must be a data.frame, not ", class(target_weights)[1]))
  }
  if (!is.data.frame(benchmark_weights)) {
    stop(paste0("calculate_factor_attribution(): [benchmark_weights] must be a data.frame, not ", class(benchmark_weights)[1]))
  }
  if (!is.data.frame(factor_returns)) {
    stop(paste0("calculate_factor_attribution(): [factor_returns] must be a data.frame, not ", class(factor_returns)[1]))
  }
  if (!is.data.frame(target_returns)) {
    stop(paste0("calculate_factor_attribution(): [target_returns] must be a data.frame, not ", class(target_returns)[1]))
  }
  if (!is.data.frame(benchmark_returns)) {
    stop(paste0("calculate_factor_attribution(): [benchmark_returns] must be a data.frame, not ", class(benchmark_returns)[1]))
  }
  if (!is.character(factor_cols) || length(factor_cols) == 0) {
    stop(paste0("calculate_factor_attribution(): [factor_cols] must be a character vector of length > 0, not ", class(factor_cols)[1], " of length ", length(factor_cols)))
  }

  required_cols <- c("date", factor_cols)
  if (!all(required_cols %in% colnames(target_weights))) {
    stop(paste0("calculate_factor_attribution(): [target_weights] missing required columns: ", paste(setdiff(required_cols, colnames(target_weights)), collapse = ", ")))
  }
  if (!all(required_cols %in% colnames(benchmark_weights))) {
    stop(paste0("calculate_factor_attribution(): [benchmark_weights] missing required columns: ", paste(setdiff(required_cols, colnames(benchmark_weights)), collapse = ", ")))
  }
  if (!all(required_cols %in% colnames(factor_returns))) {
    stop(paste0("calculate_factor_attribution(): [factor_returns] missing required columns: ", paste(setdiff(required_cols, colnames(factor_returns)), collapse = ", ")))
  }
  if (!all(c("date", "return") %in% colnames(target_returns))) {
    stop(paste0("calculate_factor_attribution(): [target_returns] must have columns 'date' and 'return'"))
  }
  if (!all(c("date", "return") %in% colnames(benchmark_returns))) {
    stop(paste0("calculate_factor_attribution(): [benchmark_returns] must have columns 'date' and 'return'"))
  }

  target_weights_shifted <- target_weights %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(future_date = dplyr::lead(date, 1)) %>%
    dplyr::filter(!is.na(future_date))

  benchmark_weights_shifted <- benchmark_weights %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(future_date = dplyr::lead(date, 1)) %>%
    dplyr::filter(!is.na(future_date))

  attribution_data <- target_weights_shifted %>%
    dplyr::inner_join(
      benchmark_weights_shifted,
      by = c("date", "future_date"),
      suffix = c("_target", "_benchmark")
    ) %>%
    dplyr::inner_join(
      factor_returns %>% dplyr::rename(future_date = date),
      by = "future_date"
    ) %>%
    dplyr::inner_join(
      target_returns %>% dplyr::rename(future_date = date, actual_target = return),
      by = "future_date"
    ) %>%
    dplyr::inner_join(
      benchmark_returns %>% dplyr::rename(future_date = date, actual_benchmark = return),
      by = "future_date"
    )

  attribution_data <- attribution_data %>%
    dplyr::mutate(
      expected_target = 0,
      expected_benchmark = 0
    )

  for (factor in factor_cols) {
    target_col <- paste0(factor, "_target")
    benchmark_col <- paste0(factor, "_benchmark")

    attribution_data <- attribution_data %>%
      dplyr::mutate(
        expected_target = expected_target + !!as.symbol(target_col) * !!as.symbol(factor),
        expected_benchmark = expected_benchmark + !!as.symbol(benchmark_col) * !!as.symbol(factor)
      )
  }

  attribution_data %>%
    dplyr::mutate(
      excess_return = actual_target - actual_benchmark,
      factor_contribution = expected_target - expected_benchmark,
      selection_effect = (actual_target - expected_target) - (actual_benchmark - expected_benchmark)
    ) %>%
    dplyr::select(
      date = future_date,
      excess_return,
      factor_contribution,
      selection_effect
    )
}
