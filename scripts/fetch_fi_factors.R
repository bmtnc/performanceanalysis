# Fixed Income Factor Data: Fetch from FRED & Compute Factor Returns
#
# Fetches 10Y yields and 3M rates from FRED for 15 developed markets.
# Loads breakeven inflation panel (from fetch/build_breakeven_inflation.R)
# for the value signal. Computes bond returns, factor signals (carry, value,
# momentum, defensive), and long/short factor portfolio returns.
# Caches all outputs as .rds files.

# Config ----

library(dplyr)
library(fredr)
library(lubridate)
library(slider)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

# Country definitions: 2-letter code -> FRED series IDs
countries <- c(
  "US",
  "GB",
  "DE",
  "JP",
  "FR",
  "CA",
  "AU",
  "IT",
  "NL",
  "SE",
  "BE",
  "CH",
  "DK",
  "NO",
  "NZ"
)

yield_10y_series <- setNames(
  paste0("IRLTLT01", countries, "M156N"),
  countries
)

rate_3m_series <- setNames(
  paste0("IR3TIB01", countries, "M156N"),
  countries
)

output_dir <- "data/fred"
output_paths <- list(
  raw_yields = file.path(output_dir, "fi_raw_yields.rds"),
  bond_returns = file.path(output_dir, "fi_bond_returns.rds"),
  factor_signals = file.path(output_dir, "fi_factor_signals.rds"),
  factor_returns = file.path(output_dir, "fi_factor_returns.rds")
)

# Breakeven inflation panel (produced by fetch/build_breakeven_inflation.R)
breakeven_path <- file.path(output_dir, "breakeven_inflation.rds")

# Parameters for derived quantities
momentum_lookback <- 12L
momentum_skip <- 1L
beta_window <- 36L
beta_min_obs <- 24L
beta_shrink_ols <- 0.6 # Vasicek shrinkage weight on OLS beta (Fix 3)
beta_shrink_prior <- 1.0 # Vasicek prior beta (Fix 3)
min_countries <- 8L # Min countries with non-missing signal per month (Fix 4)
api_sleep <- 0.5

# Helper: fetch a single FRED series safely
fetch_one <- function(series_id) {
  tryCatch(
    fredr::fredr(series_id = series_id),
    error = function(e) {
      message("  Failed to fetch ", series_id, ": ", conditionMessage(e))
      NULL
    }
  )
}

# 1. Fetch yield and rate data ----

message("Fetching 10Y yields...")
yield_raw <- list()
for (cc in countries) {
  message("  ", cc, " -> ", yield_10y_series[[cc]])
  res <- fetch_one(yield_10y_series[[cc]])
  if (!is.null(res)) {
    yield_raw[[cc]] <- res |>
      dplyr::transmute(date, country = cc, yield_10y = value)
  }
  Sys.sleep(api_sleep)
}

message("Fetching 3M rates...")
rate_raw <- list()
for (cc in countries) {
  message("  ", cc, " -> ", rate_3m_series[[cc]])
  res <- fetch_one(rate_3m_series[[cc]])
  if (!is.null(res)) {
    rate_raw[[cc]] <- res |>
      dplyr::transmute(date, country = cc, rate_3m = value)
  }
  Sys.sleep(api_sleep)
}

yields_panel <- dplyr::bind_rows(yield_raw) |>
  dplyr::full_join(dplyr::bind_rows(rate_raw), by = c("date", "country"))

# Convert from percent to decimal; normalize dates to month-end
yields_panel <- yields_panel |>
  dplyr::mutate(
    date = lubridate::ceiling_date(date, "month") - 1L,
    yield_10y = yield_10y / 100,
    rate_3m = rate_3m / 100
  )

# 2. Load breakeven inflation panel ----

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if (!file.exists(breakeven_path)) {
  stop(
    "Breakeven inflation panel not found: ", breakeven_path,
    "\nRun fetch_breakeven_inflation.R then build_breakeven_inflation.R first."
  )
}
breakeven_panel <- readRDS(breakeven_path)
message(sprintf(
  "Loaded breakeven panel: %d countries, %s to %s",
  dplyr::n_distinct(breakeven_panel$country),
  format(min(breakeven_panel$date)),
  format(max(breakeven_panel$date))
))

# 3. Save raw data ----

saveRDS(yields_panel, output_paths$raw_yields)
message("Saved raw yields.")

# 4. Compute bond returns ----

bond_returns <- yields_panel |>
  dplyr::filter(!is.na(yield_10y)) |>
  dplyr::arrange(country, date) |>
  dplyr::group_by(country) |>
  dplyr::mutate(
    yield_lag = dplyr::lag(yield_10y),
    dy = yield_10y - yield_lag
  ) |>
  dplyr::filter(!is.na(yield_lag)) |>
  dplyr::mutate(
    # Exact constant-maturity par bond repricing (Approach B / Swinkels 2019)
    # At month start: par bond with coupon = y_{t-1}, price = 1, maturity = 10Y
    # At month end: reprice at y_t with remaining maturity = 10 - 1/12 years
    # Guard against zero/negative yields: floor at 0.1% for repricing
    y_start = pmax(yield_lag, 0.001),
    y_end = pmax(yield_10y, 0.001),
    remaining = 2 * (10 - 1 / 12), # semi-annual periods remaining
    bond_return = (y_start / 12) +
      (y_start / y_end) * (1 - 1 / (1 + y_end / 2)^remaining) +
      1 / (1 + y_end / 2)^remaining -
      1,
    # Modified duration (for reference / diagnostics, not used in return calc)
    d_mac = (1 + y_start / 2) / y_start * (1 - 1 / (1 + y_start / 2)^20),
    d_mod = d_mac / (1 + y_start / 2)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(date, country, yield_10y, rate_3m, bond_return, d_mod)

saveRDS(bond_returns, output_paths$bond_returns)
message("Saved bond returns.")

# 5. Compute factor signals ----

# 5a. Carry: term spread
carry_signals <- bond_returns |>
  dplyr::filter(!is.na(rate_3m)) |>
  dplyr::transmute(date, country, carry = yield_10y - rate_3m)

# 5b. Value: real yield = nominal yield - breakeven inflation
# Breakeven panel provides market-implied (US, GB, AU) or regression-estimated
# inflation expectations for all 15 countries.
value_signals <- bond_returns |>
  dplyr::inner_join(breakeven_panel, by = c("date", "country")) |>
  dplyr::mutate(value = yield_10y - breakeven) |>
  dplyr::transmute(date, country, value)

be_countries <- dplyr::n_distinct(value_signals$country)
be_obs <- nrow(value_signals)
message(sprintf("  Value signals: %d countries, %d obs (breakeven-based)", be_countries, be_obs))

# 5c. Momentum: 12-1 month cumulative bond total return
mom_signals <- bond_returns |>
  dplyr::arrange(country, date) |>
  dplyr::group_by(country) |>
  dplyr::mutate(
    # Compound returns over months t-12 to t-2 (skip most recent month)
    # slider::slide_prod computes the product over a window
    cum_ret_12_1 = slider::slide_dbl(
      1 + bond_return,
      .f = prod,
      .before = momentum_lookback,
      .after = -momentum_skip,
      .complete = TRUE
    ) -
      1
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(cum_ret_12_1)) |>
  dplyr::transmute(date, country, momentum = cum_ret_12_1)

# 5d. Defensive: negated 36-month rolling beta vs equal-weighted market

# Equal-weighted market return
mkt_return <- bond_returns |>
  dplyr::group_by(date) |>
  dplyr::summarise(r_mkt = mean(bond_return, na.rm = TRUE), .groups = "drop")

beta_data <- bond_returns |>
  dplyr::left_join(mkt_return, by = "date") |>
  dplyr::arrange(country, date) |>
  dplyr::group_by(country) |>
  dplyr::mutate(
    beta = slider::slide2_dbl(
      .x = bond_return,
      .y = r_mkt,
      .f = function(r, m) {
        if (sum(!is.na(r) & !is.na(m)) < beta_min_obs) {
          return(NA_real_)
        }
        fit <- lm(r ~ m)
        unname(coef(fit)[2])
      },
      .before = beta_window - 1L,
      .after = 0L,
      .complete = TRUE
    )
  ) |>
  dplyr::ungroup()

# Fix 3: Apply Vasicek shrinkage toward 1.0 before using beta for sort & leverage
defensive_signals <- beta_data |>
  dplyr::filter(!is.na(beta)) |>
  dplyr::mutate(
    beta_shrunk = beta_shrink_ols *
      beta +
      (1 - beta_shrink_ols) * beta_shrink_prior
  ) |>
  dplyr::transmute(
    date,
    country,
    defensive = -beta_shrunk,
    beta_raw = beta_shrunk
  )

# Combine all signals
all_signals <- carry_signals |>
  dplyr::full_join(value_signals, by = c("date", "country")) |>
  dplyr::full_join(mom_signals, by = c("date", "country")) |>
  dplyr::full_join(defensive_signals, by = c("date", "country"))

saveRDS(all_signals, output_paths$factor_signals)
message("Saved factor signals.")

# 6. Compute factor portfolio returns ----

# Fix 1: Demeaned rank weights (AQR methodology) instead of tercile sort.
# w_i = c * (rank(signal_i) - mean_rank), scaled so sum(w+) = 1, sum(w-) = -1.
# Fix 4: Require at least min_countries countries with non-missing signal per month.
compute_ls_factor <- function(signals_df, signal_col, returns_df) {
  sig_col <- rlang::ensym(signal_col)

  # Merge signals (lagged by one month) with next-month returns
  sig_dated <- signals_df |>
    dplyr::select(date, country, signal = !!sig_col) |>
    dplyr::filter(!is.na(signal))

  # Get unique dates from returns and map signal date -> return date
  ret_dates <- sort(unique(returns_df$date))
  date_map <- tibble::tibble(
    sig_date = ret_dates[-length(ret_dates)],
    ret_date = ret_dates[-1]
  )

  sig_dated <- sig_dated |>
    dplyr::inner_join(date_map, by = c("date" = "sig_date")) |>
    dplyr::inner_join(
      returns_df |> dplyr::select(date, country, bond_return),
      by = c("ret_date" = "date", "country" = "country")
    )

  sig_dated |>
    dplyr::group_by(ret_date) |>
    dplyr::filter(dplyr::n() >= min_countries) |>
    dplyr::mutate(
      n = dplyr::n(),
      rk = rank(signal, ties.method = "average"),
      w_raw = rk - (n + 1) / 2,
      # Scale so sum of positive weights = 1 and sum of negative weights = -1
      w = dplyr::if_else(
        w_raw > 0,
        w_raw / sum(w_raw[w_raw > 0]),
        w_raw / sum(abs(w_raw[w_raw < 0]))
      )
    ) |>
    dplyr::summarise(
      factor_return = sum(w * bond_return),
      n_long = sum(w > 0),
      n_short = sum(w < 0),
      .groups = "drop"
    ) |>
    dplyr::rename(date = ret_date) |>
    dplyr::filter(n_long > 0, n_short > 0)
}

carry_returns <- compute_ls_factor(all_signals, carry, bond_returns) |>
  dplyr::transmute(date, carry = factor_return)

value_returns <- compute_ls_factor(all_signals, value, bond_returns) |>
  dplyr::transmute(date, value = factor_return)

mom_returns <- compute_ls_factor(all_signals, momentum, bond_returns) |>
  dplyr::transmute(date, momentum = factor_return)

# Defensive/BAB: beta-neutral construction with rank weights within each leg
# Long low-beta (below median), short high-beta (above median)
# Scale each leg by 1/portfolio_beta
# Fix 1: Use rank weights within each leg instead of equal weights
# Fix 2: Subtract risk-free rate (US 3M T-bill) per Frazzini-Pedersen BAB formula
# Fix 4: Require min_countries countries per month

# Extract US 3M rate as risk-free rate (already in yields_panel, in decimal)
rf_monthly <- yields_panel |>
  dplyr::filter(country == "US", !is.na(rate_3m)) |>
  # Fix 2: Convert annualized decimal rate to monthly decimal
  dplyr::transmute(date, rf = rate_3m / 12)

compute_bab_factor <- function(def_signals, returns_df, rf_df) {
  # Use beta_raw (shrunk beta) for portfolio construction and leverage
  sig_dated <- def_signals |>
    dplyr::select(date, country, beta_raw) |>
    dplyr::filter(!is.na(beta_raw))

  ret_dates <- sort(unique(returns_df$date))
  date_map <- tibble::tibble(
    sig_date = ret_dates[-length(ret_dates)],
    ret_date = ret_dates[-1]
  )

  sig_dated <- sig_dated |>
    dplyr::inner_join(date_map, by = c("date" = "sig_date")) |>
    dplyr::inner_join(
      returns_df |> dplyr::select(date, country, bond_return),
      by = c("ret_date" = "date", "country" = "country")
    ) |>
    dplyr::left_join(rf_df, by = c("ret_date" = "date"))

  sig_dated |>
    dplyr::group_by(ret_date) |>
    dplyr::filter(dplyr::n() >= min_countries) |>
    dplyr::mutate(
      med_beta = median(beta_raw),
      side = dplyr::if_else(beta_raw <= med_beta, "low", "high")
    ) |>
    # Fix 1: Rank weights within each leg, normalized to sum to 1
    # Low-beta leg: rank by -beta (lowest beta -> highest weight)
    # High-beta leg: rank by beta (highest beta -> highest weight)
    dplyr::group_by(ret_date, side) |>
    dplyr::mutate(
      rk_within = dplyr::if_else(
        side == "low",
        rank(-beta_raw, ties.method = "average"),
        rank(beta_raw, ties.method = "average")
      ),
      w_within = rk_within / sum(rk_within)
    ) |>
    dplyr::group_by(ret_date) |>
    dplyr::mutate(
      rf = dplyr::if_else(is.na(rf), 0, rf)
    ) |>
    dplyr::summarise(
      r_low = sum(w_within[side == "low"] * bond_return[side == "low"]),
      r_high = sum(w_within[side == "high"] * bond_return[side == "high"]),
      beta_low = sum(w_within[side == "low"] * beta_raw[side == "low"]),
      beta_high = sum(w_within[side == "high"] * beta_raw[side == "high"]),
      rf = dplyr::first(rf),
      n_low = sum(side == "low"),
      n_high = sum(side == "high"),
      .groups = "drop"
    ) |>
    dplyr::filter(n_low > 0, n_high > 0, beta_low > 0, beta_high > 0) |>
    dplyr::mutate(
      # Fix 2: BAB with excess returns: r_BAB = (1/beta_L)*(r_L - rf) - (1/beta_H)*(r_H - rf)
      factor_return = (1 / beta_low) *
        (r_low - rf) -
        (1 / beta_high) * (r_high - rf)
    ) |>
    dplyr::rename(date = ret_date)
}

bab_returns <- compute_bab_factor(
  defensive_signals,
  bond_returns,
  rf_monthly
) |>
  dplyr::transmute(date, defensive = factor_return)

# Combine all factor returns
factor_returns <- carry_returns |>
  dplyr::full_join(value_returns, by = "date") |>
  dplyr::full_join(mom_returns, by = "date") |>
  dplyr::full_join(bab_returns, by = "date") |>
  dplyr::arrange(date)

saveRDS(factor_returns, output_paths$factor_returns)
message("Saved factor returns.")

# 7. Summary ----

message("\n=== Fixed Income Factor Data Summary ===")
message("Countries: ", paste(countries, collapse = ", "))
message(
  "Yield panel: ",
  format(min(yields_panel$date)),
  " to ",
  format(max(yields_panel$date)),
  " (",
  dplyr::n_distinct(yields_panel$country),
  " countries)"
)
message(
  "Breakeven panel: ",
  format(min(breakeven_panel$date)),
  " to ",
  format(max(breakeven_panel$date)),
  " (",
  dplyr::n_distinct(breakeven_panel$country),
  " countries)"
)
message(
  "Bond returns: ",
  format(min(bond_returns$date)),
  " to ",
  format(max(bond_returns$date)),
  " (",
  nrow(bond_returns),
  " obs)"
)

message("\nFactor return date ranges and annualized means:")
for (fac in c("carry", "value", "momentum", "defensive")) {
  col <- factor_returns[[fac]]
  valid <- !is.na(col)
  if (any(valid)) {
    dates <- factor_returns$date[valid]
    ann_mean <- mean(col[valid]) * 12
    ann_sd <- sd(col[valid]) * sqrt(12)
    sr <- ann_mean / ann_sd
    message(sprintf(
      "  %-10s: %s to %s | ann. mean: %+.2f%% | ann. SD: %.2f%% | SR: %.2f",
      fac,
      format(min(dates)),
      format(max(dates)),
      ann_mean * 100,
      ann_sd * 100,
      sr
    ))
  }
}

message("\nOutputs saved to: ", output_dir)
message("Done.")
