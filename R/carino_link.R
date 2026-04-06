#' Carino Log-Linking for Cumulative Return Decomposition
#'
#' Converts per-period additive return components into cumulative values
#' that sum to the geometric (compounded) cumulative total return at every
#' point in time. Works with any number of components.
#'
#' @param total_returns Numeric vector of per-period total returns (simple,
#'   not log). The components should sum to this each period.
#' @param components Data frame or tibble whose columns are the per-period
#'   additive components to cumulate. Column names are preserved in output
#'   with a "cumulative_" prefix.
#'
#' @return tibble with columns: cumulative_total plus one
#'   cumulative_{component} column per input component. Number of rows
#'   equals length of total_returns.
#' @export
carino_link <- function(total_returns, components) {
  avpipeline::validate_numeric_vector(total_returns, name = "total_returns")
  avpipeline::validate_df_type(components)

  n <- length(total_returns)
  if (nrow(components) != n) {
    stop(paste0(
      "carino_link(): nrow(components) must equal length(total_returns). ",
      "Got ",
      nrow(components),
      " vs ",
      n,
      "."
    ))
  }

  comp_mat <- as.matrix(components)
  if (!is.numeric(comp_mat)) {
    stop("carino_link(): all component columns must be numeric.")
  }

  k <- dplyr::if_else(
    abs(total_returns) < 1e-10,
    1.0,
    log(1 + total_returns) / total_returns
  )
  cum_geometric <- cumprod(1 + total_returns) - 1

  weighted_comp <- comp_mat * k
  cum_weighted_comp <- apply(weighted_comp, 2, cumsum)
  # apply() drops dimensions on single-row or single-column input
  if (is.null(dim(cum_weighted_comp))) {
    cum_weighted_comp <- matrix(
      cum_weighted_comp,
      nrow = n,
      ncol = ncol(comp_mat)
    )
  }
  cum_weighted_total <- cumsum(k * total_returns)

  safe_total <- dplyr::if_else(
    abs(cum_weighted_total) < 1e-12,
    NA_real_,
    cum_weighted_total
  )
  cum_components <- cum_weighted_comp * (cum_geometric / safe_total)
  cum_components[is.na(cum_components)] <- 0

  tibble::as_tibble(
    stats::setNames(
      as.data.frame(cum_components),
      paste0("cumulative_", colnames(components))
    )
  ) %>%
    dplyr::mutate(cumulative_total = cum_geometric) %>%
    dplyr::select(cumulative_total, dplyr::everything())
}
