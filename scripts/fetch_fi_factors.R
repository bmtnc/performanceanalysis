# Fixed Income Factor Data: Fetch from FRED & Compute Factor Returns
#
# Fetches 10Y yields, 3M rates, and CPI from FRED for 15 developed markets.
# Computes bond returns, factor signals (carry, value, momentum, defensive),
# and long/short factor portfolio returns. Caches all outputs as .rds files.

# Config ----

library(dplyr)
library(fredr)
library(slider)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

# Country definitions: 2-letter code -> FRED series IDs
countries <- c("US", "GB", "DE", "JP", "FR", "CA", "AU", "IT", "NL", "SE",
               "BE", "CH", "DK", "NO", "NZ")

yield_10y_series <- setNames(
  paste0("IRLTLT01", countries, "M156N"),
  countries
)

rate_3m_series <- setNames(
  paste0("IR3TIB01", countries, "M156N"),
  countries
)

# CPI uses 3-letter ISO codes in FRED; Australia is quarterly
iso3_map <- c(
  US = "USA", GB = "GBR", DE = "DEU", JP = "JPN", FR = "FRA",
  CA = "CAN", AU = "AUS", IT = "ITA", NL = "NLD", SE = "SWE",
  BE = "BEL", CH = "CHE", DK = "DNK", NO = "NOR", NZ = "NZL"
)

cpi_series <- setNames(
  paste0(iso3_map, "CPIALLMINMEI"),
  countries
)
# Australia CPI is quarterly on FRED
cpi_series["AU"] <- "AUSCPIALLQINMEI"

output_dir <- "data/fred"
output_paths <- list(
  raw_yields  = file.path(output_dir, "fi_raw_yields.rds"),
  raw_cpi     = file.path(output_dir, "fi_raw_cpi.rds"),
  bond_returns = file.path(output_dir, "fi_bond_returns.rds"),
  factor_signals = file.path(output_dir, "fi_factor_signals.rds"),
  factor_returns = file.path(output_dir, "fi_factor_returns.rds")
)

# Parameters for derived quantities
trail_cpi_months <- 36L
momentum_lookback <- 12L
momentum_skip <- 1L
beta_window <- 36L
beta_min_obs <- 24L
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

# Convert from percent to decimal
yields_panel <- yields_panel |>
  dplyr::mutate(
    yield_10y = yield_10y / 100,
    rate_3m   = rate_3m / 100
  )

# 2. Fetch CPI data ----

message("Fetching CPI...")
cpi_raw <- list()
for (cc in countries) {
  message("  ", cc, " -> ", cpi_series[[cc]])
  res <- fetch_one(cpi_series[[cc]])
  if (!is.null(res)) {
    cpi_raw[[cc]] <- res |>
      dplyr::transmute(date, country = cc, cpi = value)
  }
  Sys.sleep(api_sleep)
}

cpi_panel <- dplyr::bind_rows(cpi_raw)

# Australia CPI is quarterly -- step-interpolate to monthly
au_cpi <- cpi_panel |> dplyr::filter(country == "AU")
if (nrow(au_cpi) > 0) {
  au_months <- tibble::tibble(
    date = seq.Date(min(au_cpi$date), max(au_cpi$date), by = "month")
  )
  au_cpi_monthly <- au_months |>
    dplyr::left_join(au_cpi, by = "date") |>
    dplyr::mutate(
      country = "AU",
      # Forward-fill quarterly CPI to monthly (base R, no tidyr dependency)
      cpi = {
        idx <- which(!is.na(cpi))
        if (length(idx) > 0) cpi[idx[findInterval(seq_along(cpi), idx, left.open = FALSE)]]
        else cpi
      }
    )

  cpi_panel <- cpi_panel |>
    dplyr::filter(country != "AU") |>
    dplyr::bind_rows(au_cpi_monthly)
}

# 3. Save raw data ----

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
saveRDS(yields_panel, output_paths$raw_yields)
saveRDS(cpi_panel, output_paths$raw_cpi)
message("Saved raw yields and CPI.")

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
    # Duration of 10Y par bond (semi-annual coupons) using lagged yield
    # Guard against zero/negative yields: use small floor for duration calc
    y_dur = pmax(yield_lag, 0.001),
    d_mac = (1 + y_dur / 2) / y_dur * (1 - 1 / (1 + y_dur / 2)^20),
    d_mod = d_mac / (1 + y_dur / 2),
    # Monthly total return approximation
    bond_return = yield_lag / 12 - d_mod * dy
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

# 5b. Value: real yield = nominal yield - annualized 3yr trailing CPI change
value_signals <- bond_returns |>
  dplyr::left_join(cpi_panel, by = c("date", "country")) |>
  dplyr::arrange(country, date) |>
  dplyr::group_by(country) |>
  dplyr::mutate(
    cpi_lag36 = dplyr::lag(cpi, trail_cpi_months),
    inflation_3y = cpi / cpi_lag36 - 1,
    inflation_ann = (1 + inflation_3y)^(1 / 3) - 1,
    real_yield = yield_10y - inflation_ann
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(real_yield)) |>
  dplyr::transmute(date, country, value = real_yield)

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
    ) - 1
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
        if (sum(!is.na(r) & !is.na(m)) < beta_min_obs) return(NA_real_)
        fit <- lm(r ~ m)
        unname(coef(fit)[2])
      },
      .before = beta_window - 1L,
      .after = 0L,
      .complete = TRUE
    )
  ) |>
  dplyr::ungroup()

defensive_signals <- beta_data |>
  dplyr::filter(!is.na(beta)) |>
  dplyr::transmute(date, country, defensive = -beta, beta_raw = beta)

# Combine all signals
all_signals <- carry_signals |>
  dplyr::full_join(value_signals, by = c("date", "country")) |>
  dplyr::full_join(mom_signals, by = c("date", "country")) |>
  dplyr::full_join(defensive_signals, by = c("date", "country"))

saveRDS(all_signals, output_paths$factor_signals)
message("Saved factor signals.")

# 6. Compute factor portfolio returns ----

# Helper: tercile long/short factor return
# At each date, rank by signal, go long top third, short bottom third
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
    dplyr::mutate(
      n = dplyr::n(),
      rank = rank(signal, ties.method = "average"),
      tercile = dplyr::case_when(
        rank <= n / 3 ~ "short",
        rank > 2 * n / 3 ~ "long",
        TRUE ~ "mid"
      )
    ) |>
    dplyr::filter(tercile %in% c("long", "short")) |>
    dplyr::summarise(
      r_long  = mean(bond_return[tercile == "long"], na.rm = TRUE),
      r_short = mean(bond_return[tercile == "short"], na.rm = TRUE),
      factor_return = r_long - r_short,
      n_long  = sum(tercile == "long"),
      n_short = sum(tercile == "short"),
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

# Defensive/BAB: beta-neutral construction
# Long low-beta (below median), short high-beta (above median)
# Scale each leg by 1/portfolio_beta

compute_bab_factor <- function(def_signals, returns_df) {
  # Use beta_raw (positive beta) for portfolio construction
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
    )

  sig_dated |>
    dplyr::group_by(ret_date) |>
    dplyr::mutate(
      med_beta = median(beta_raw),
      side = dplyr::if_else(beta_raw <= med_beta, "low", "high")
    ) |>
    dplyr::summarise(
      r_low  = mean(bond_return[side == "low"], na.rm = TRUE),
      r_high = mean(bond_return[side == "high"], na.rm = TRUE),
      beta_low  = mean(beta_raw[side == "low"], na.rm = TRUE),
      beta_high = mean(beta_raw[side == "high"], na.rm = TRUE),
      n_low  = sum(side == "low"),
      n_high = sum(side == "high"),
      .groups = "drop"
    ) |>
    dplyr::filter(n_low > 0, n_high > 0, beta_low > 0, beta_high > 0) |>
    dplyr::mutate(
      factor_return = (1 / beta_low) * r_low - (1 / beta_high) * r_high
    ) |>
    dplyr::rename(date = ret_date)
}

bab_returns <- compute_bab_factor(defensive_signals, bond_returns) |>
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
message("Yield panel: ", format(min(yields_panel$date)), " to ",
        format(max(yields_panel$date)),
        " (", dplyr::n_distinct(yields_panel$country), " countries)")
message("CPI panel: ", format(min(cpi_panel$date)), " to ",
        format(max(cpi_panel$date)),
        " (", dplyr::n_distinct(cpi_panel$country), " countries)")
message("Bond returns: ", format(min(bond_returns$date)), " to ",
        format(max(bond_returns$date)),
        " (", nrow(bond_returns), " obs)")

message("\nFactor return date ranges and annualized means:")
for (fac in c("carry", "value", "momentum", "defensive")) {
  col <- factor_returns[[fac]]
  valid <- !is.na(col)
  if (any(valid)) {
    dates <- factor_returns$date[valid]
    ann_mean <- mean(col[valid]) * 12
    ann_sd <- sd(col[valid]) * sqrt(12)
    sr <- ann_mean / ann_sd
    message(sprintf("  %-10s: %s to %s | ann. mean: %+.2f%% | ann. SD: %.2f%% | SR: %.2f",
                    fac,
                    format(min(dates)), format(max(dates)),
                    ann_mean * 100, ann_sd * 100, sr))
  }
}

message("\nOutputs saved to: ", output_dir)
message("Done.")
