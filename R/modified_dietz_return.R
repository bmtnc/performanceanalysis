#' Modified Dietz return
#'
#' Calculates a single-period return using the Modified Dietz method.
#' Approximates time-weighted return by day-weighting cash flows within the
#' period so that the timing of contributions and distributions does not
#' distort the measured return.
#'
#' @param bmv Beginning market value (numeric scalar)
#' @param emv Ending market value (numeric scalar)
#' @param cf Numeric vector of cash flow amounts. Positive = inflow
#'   (contribution/capital call), negative = outflow (distribution/redemption).
#'   Defaults to empty (no cash flows).
#' @param cf_days Integer vector of day offsets for each cash flow, where
#'   0 = start of period and total_days = end of period. Same length as cf.
#' @param total_days Total calendar days in the period (integer scalar)
#'
#' @return Numeric scalar: Modified Dietz return for the period.
#'   Returns NA if the adjusted denominator is zero (e.g., fully liquidated
#'   position with no beginning value).
#' @export
modified_dietz_return <- function(bmv, emv, cf = numeric(0),
                                  cf_days = integer(0), total_days = 1L) {
  avpipeline::validate_numeric_scalar(bmv, "bmv")
  avpipeline::validate_numeric_scalar(emv, "emv")
  stopifnot(length(bmv) == 1L, length(emv) == 1L)
  stopifnot(length(cf) == length(cf_days))
  stopifnot(total_days > 0L)

  net_cf <- sum(cf)

  if (length(cf) == 0L) {
    if (bmv == 0) return(NA_real_)
    return((emv - bmv) / bmv)
  }

  weights <- (total_days - cf_days) / total_days
  weighted_cf <- sum(cf * weights)
  denominator <- bmv + weighted_cf

  if (denominator == 0) return(NA_real_)

  (emv - bmv - net_cf) / denominator
}
