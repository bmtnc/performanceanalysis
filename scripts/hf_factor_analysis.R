# HF Factor Analysis -- Absolute Return Cross-Asset Factor Decomposition
#
# Decomposes hedge fund returns into cross-asset factor exposures using
# unconstrained rolling regression. No benchmark, no sum-to-one, allows
# negative (short) factor exposures.
#
# Factors: FI (4), FX (3), Equity (5) = 12 cross-asset premia

# Script Params ----

target_ticker <- "BIMBX"

roll_window_monthly <- 24L # factor regressions (monthly, ~5 years)

# Date range filtering (NULL = use all available data)
start_date <- as.Date("2018-01-31")
end_date <- NULL

# Equity factor geography: "Global", "USA", "Europe", "Pacific", "Global Ex USA"
equity_geography <- "USA"

# Toggle asset class factor groups
use_fi_factors <- TRUE
use_fx_factors <- TRUE
use_eq_factors <- TRUE

# constraints

non_negative <- FALSE
sum_to_one <- FALSE
intercept <- TRUE


target_lower <- tolower(target_ticker)

# Load package functions
devtools::load_all()

# Data Pulls ----

message("========================================")
message("FETCHING DATA")
message("========================================")

all_data <- fetch_adjusted_prices(target_ticker)

fi_raw <- readRDS("data/fred/fi_factor_returns.rds")
fx_raw <- readRDS("data/fred/fx_factor_returns.rds")
eq_raw <- readRDS("data/aqr/aqr_equity_factors.rds")
mom_raw <- readRDS("data/aqr/aqr_momentum_factors.rds")

# Data Pre-processing ----

message("\nPre-processing data...")

daily_returns <- calculate_log_returns(all_data)

# Compound daily to monthly returns
monthly_returns <- daily_returns %>%
  dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
  dplyr::group_by(ticker, month) %>%
  dplyr::summarise(
    return = exp(sum(log(1 + return))) - 1,
    .groups = "drop"
  ) %>%
  dplyr::rename(date = month)

# Build combined factor matrix (dates normalized to first-of-month)
factor_dfs <- list()

if (use_fi_factors) {
  factor_dfs[["fi"]] <- fi_raw %>%
    dplyr::mutate(date = lubridate::floor_date(date, "month")) %>%
    dplyr::select(
      date,
      fi_carry = carry,
      fi_value = value,
      fi_mom = momentum,
      fi_def = defensive
    )
}

if (use_fx_factors) {
  factor_dfs[["fx"]] <- fx_raw %>%
    dplyr::mutate(date = lubridate::floor_date(date, "month")) %>%
    dplyr::select(
      date,
      fx_carry = carry_return,
      fx_mom = mom_return,
      fx_value = val_return
    )
}

if (use_eq_factors) {
  eq_filtered <- eq_raw %>%
    dplyr::filter(geography == equity_geography)
  if (nrow(eq_filtered) == 0) {
    stop(paste0(
      "No equity data for geography '",
      equity_geography,
      "'. ",
      "Valid values: ",
      paste(unique(eq_raw$geography), collapse = ", ")
    ))
  }
  # Momentum is in a separate file with different structure
  # Map geography to momentum column
  mom_col <- switch(
    equity_geography,
    "USA" = "us_large_cap",
    "Global" = ,
    "Global Ex USA" = ,
    "Europe" = ,
    "Pacific" = "international",
    "us_large_cap"
  )
  mom_monthly <- mom_raw %>%
    dplyr::mutate(date = lubridate::floor_date(date, "month")) %>%
    dplyr::select(date, eq_mom = dplyr::all_of(mom_col))

  factor_dfs[["eq"]] <- eq_filtered %>%
    dplyr::mutate(date = lubridate::floor_date(date, "month")) %>%
    dplyr::select(
      date,
      eq_hml = hml,
      eq_bab = bab,
      eq_qmj = qmj,
      eq_mkt = mkt,
      eq_smb = smb
    ) %>%
    dplyr::full_join(mom_monthly, by = "date")
}

if (length(factor_dfs) == 0) {
  stop("No factor groups selected")
}

factor_data <- Reduce(
  function(x, y) dplyr::full_join(x, y, by = "date"),
  factor_dfs
)

factor_cols <- setdiff(names(factor_data), "date")
factor_labels <- factor_metadata(factor_cols)$labels
message(paste0(
  "Using ",
  length(factor_cols),
  " cross-asset factors: ",
  paste(factor_cols, collapse = ", ")
))

# Build regression dataset. Replace NAs in factor columns with 0 so that
# stale factors (e.g. FI/FX value truncated by CPI lag) contribute zero
# rather than dropping the entire month. This extends the usable date range
# to the latest available factor (typically EQ or FI carry/momentum).
target_monthly <- monthly_returns %>%
  dplyr::filter(ticker == target_ticker) %>%
  dplyr::select(date, target_return = return)

regression_data <- target_monthly %>%
  dplyr::inner_join(factor_data, by = "date") %>%
  dplyr::mutate(dplyr::across(
    dplyr::all_of(factor_cols),
    ~ tidyr::replace_na(.x, 0)
  )) %>%
  dplyr::arrange(date)

# Apply date filtering
if (!is.null(start_date)) {
  start_dt <- lubridate::ymd(start_date)
  regression_data <- regression_data %>% dplyr::filter(date >= start_dt)
  message(paste0("Filtering: start_date >= ", start_date))
}
if (!is.null(end_date)) {
  end_dt <- lubridate::ymd(end_date)
  regression_data <- regression_data %>% dplyr::filter(date <= end_dt)
  message(paste0("Filtering: end_date <= ", end_date))
}

message(paste0("Regression dataset: ", nrow(regression_data), " months"))
message(paste0(
  "Date range: ",
  min(regression_data$date),
  " to ",
  max(regression_data$date)
))

# ============================================================================
# ANALYSIS 1: ROLLING FACTOR EXPOSURES (UNCONSTRAINED)
# ============================================================================

message("\n========================================")
message("ANALYSIS 1: ROLLING FACTOR EXPOSURES")
message("========================================")

x_mat <- as.matrix(regression_data[, factor_cols])

message(paste0(
  "Running unconstrained rolling regression (",
  roll_window_monthly,
  "-month window)..."
))

target_fit <- roll_constrained_lm(
  x = x_mat,
  y = regression_data$target_return,
  width = roll_window_monthly,
  non_negative = non_negative,
  sum_to_one = sum_to_one,
  intercept = intercept
)

target_coefs <- as.data.frame(target_fit$coefficients)
names(target_coefs)[1] <- "alpha"

target_exposure <- tibble::as_tibble(target_coefs) %>%
  dplyr::mutate(date = regression_data$date) %>%
  dplyr::filter(!is.na(alpha))

# Summary stats
latest <- target_exposure %>% dplyr::slice_tail(n = 1)
message(paste0(
  "\nLatest exposures (",
  format(latest$date),
  "):"
))
for (fc in factor_cols) {
  message(sprintf("  %-12s: %+.3f", factor_labels[fc], latest[[fc]]))
}
message(sprintf(
  "  %-12s: %+.4f (%.1f%% annualized)",
  "Alpha",
  latest$alpha,
  latest$alpha * 12 * 100
))
message(sprintf("  %-12s: %.3f", "Sum of betas", sum(latest[factor_cols])))

p1 <- plot_rolling_exposures(
  target_exposure, factor_cols, target_ticker, roll_window_monthly
)

print(p1)

# ============================================================================
# ANALYSIS 2: ROLLING ALPHA
# ============================================================================

message("\n========================================")
message("ANALYSIS 2: ROLLING ALPHA")
message("========================================")

p2 <- plot_rolling_alpha(target_exposure, target_ticker, roll_window_monthly)

print(p2)

# ============================================================================
# ANALYSIS 3: FULL-SAMPLE DECOMPOSITION
# ============================================================================

message("\n========================================")
message("ANALYSIS 3: FULL-SAMPLE DECOMPOSITION")
message("========================================")

full_fit <- constrained_linear_regression(
  x = x_mat,
  y = regression_data$target_return,
  non_negative = FALSE,
  sum_to_one = FALSE,
  intercept = TRUE
)

full_coefs <- full_fit$coefficients
names(full_coefs)[1] <- "alpha"

message("\nFull-sample factor exposures:")
for (fc in factor_cols) {
  message(sprintf("  %-12s: %+.4f", factor_labels[fc], full_coefs[fc]))
}
message(sprintf(
  "  %-12s: %+.5f (%.2f%% annualized)",
  "Alpha",
  full_coefs["alpha"],
  full_coefs["alpha"] * 12 * 100
))
message(sprintf("  %-12s: %.4f", "R-squared", full_fit$r.squared))
message(sprintf("  %-12s: %.3f", "Sum of betas", sum(full_coefs[factor_cols])))

# ============================================================================
# ANALYSIS 4: CUMULATIVE CONTRIBUTION TO RETURN
# ============================================================================

message("\n========================================")
message("ANALYSIS 4: CUMULATIVE CONTRIBUTION TO RETURN")
message("========================================")

target_returns_ctr <- monthly_returns %>%
  dplyr::filter(ticker == target_ticker) %>%
  dplyr::select(date, return)

# Fill NA factor returns with 0, matching the treatment in regression_data
# (line 143). Stale factors (e.g. FI/FX value when CPI lags by ~2 months)
# have trailing NAs. Without this fill, beta * NA = NA propagates through
# rowSums into total_explained and residual, and carino_link's cumsum turns
# all subsequent values NA — which are then silently replaced with 0 (line 63
# of carino_link.R). That causes accumulated cumulative components to jump
# to zero in a single period, producing the artificial ~30% idiosyncratic
# spike visible around the stale-factor cutoff date.
factor_data_filled <- factor_data %>%
  dplyr::mutate(dplyr::across(
    dplyr::all_of(factor_cols),
    ~ tidyr::replace_na(.x, 0)
  ))

ctr_data <- calculate_ctr(
  rolling_fit = target_fit,
  dates = regression_data$date,
  fund_returns = target_returns_ctr,
  factor_returns = factor_data_filled,
  factor_cols = factor_cols
)

ctr_col_names <- paste0(factor_cols, "_ctr")

cumulative_ctr <- calculate_cumulative_ctr(ctr_data, ctr_col_names)

message(paste0("CTR observations: ", nrow(ctr_data)))
message(paste0("Date range: ", min(ctr_data$date), " to ", max(ctr_data$date)))

p4 <- plot_cumulative_ctr(
  cumulative_ctr, factor_cols, target_ticker, roll_window_monthly
)

print(p4)

# ============================================================================
# ANALYSIS 5: FACTOR EXPOSURE DISTRIBUTION
# ============================================================================

message("\n========================================")
message("ANALYSIS 5: FACTOR EXPOSURE DISTRIBUTION")
message("========================================")

p5 <- plot_exposure_distribution(
  target_exposure, factor_cols, target_ticker, roll_window_monthly
)

print(p5)

# ============================================================================
# SUMMARY
# ============================================================================

message("\n========================================")
message("ANALYSIS COMPLETE!")
message("========================================\n")
