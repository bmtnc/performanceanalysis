# Fetch & Cache FX/Currency Factor Data from FRED
# Pulls exchange rates, interest rates, and CPI data, then constructs
# carry, momentum, and value factor return series for G10 currencies.

# Config ----

data_dir <- "data/fred"
api_delay <- 0.5  # seconds between FRED API calls (rate limit)

# Date range for all FRED pulls
obs_start <- as.Date("1990-01-01")
obs_end   <- Sys.Date()

# G10 currencies (all vs USD)
currencies <- c("AUD", "CAD", "CHF", "EUR", "GBP", "JPY", "NOK", "NZD", "SEK")

# FRED monthly exchange rate series (H.10 release, monthly averages)
# Convention: some are "USD per foreign" (direct), some are "foreign per USD" (indirect)
fx_series <- c(
  AUD = "EXUSAL",   # USD per AUD  (direct)
  CAD = "EXCAUS",   # CAD per USD  (indirect)
  CHF = "EXSZUS",   # CHF per USD  (indirect)
  EUR = "EXUSEU",   # USD per EUR  (direct)
  GBP = "EXUSUK",   # USD per GBP  (direct)
  JPY = "EXJPUS",   # JPY per USD  (indirect)
  NOK = "EXNOUS",   # NOK per USD  (indirect)
  NZD = "EXUSNZ",   # USD per NZD  (direct)
  SEK = "EXSDUS"    # SEK per USD  (indirect)
)

# Currencies quoted as "USD per foreign" -- these need inverting to get
# everything into "foreign per USD" (our target convention)
direct_quote_currencies <- c("AUD", "EUR", "GBP", "NZD")

# 3-month interbank rates (OECD via FRED, percent per annum)
# Pattern: IR3TIB01{CC}M156N (CC = country code, EZ = Euro Area)
rate_series <- c(
  USD = "IR3TIB01USM156N",
  AUD = "IR3TIB01AUM156N",
  CAD = "IR3TIB01CAM156N",
  CHF = "IR3TIB01CHM156N",
  EUR = "IR3TIB01EZM156N",
  GBP = "IR3TIB01GBM156N",
  JPY = "IR3TIB01JPM156N",
  NOK = "IR3TIB01NOM156N",
  NZD = "IR3TIB01NZM156N",
  SEK = "IR3TIB01SEM156N"
)

# Japan splice series: CD rate has longer history (1979-2022)
jpy_cd_series <- "IR3TCD01JPM156N"

# CPI indices (OECD via FRED, index levels, not seasonally adjusted)
# AU, NZ are quarterly-only on FRED; EUR uses Eurostat series
cpi_series <- c(
  USD = "CPALTT01USM657N",
  CAD = "CPALTT01CAM657N",
  CHF = "CPALTT01CHM657N",
  EUR = "CP0000EZ19M086NEST",
  GBP = "CPALTT01GBM657N",
  JPY = "CPALTT01JPM657N",
  NOK = "CPALTT01NOM657N",
  SEK = "CPALTT01SEM657N"
)

# Quarterly CPI series (AU and NZ only have quarterly on FRED)
# Step-interpolated to monthly (carry forward)
cpi_quarterly_series <- c(
  AUD = "AUSCPIALLQINMEI",
  NZD = "NZLCPIALLQINMEI"
)

# Portfolio construction
n_long  <- 3  # top tercile
n_short <- 3  # bottom tercile

# Output file paths
out_raw_fx     <- file.path(data_dir, "fx_raw_rates.rds")
out_raw_rates  <- file.path(data_dir, "fx_raw_interest_rates.rds")
out_raw_cpi    <- file.path(data_dir, "fx_raw_cpi.rds")
out_returns    <- file.path(data_dir, "fx_returns.rds")
out_signals    <- file.path(data_dir, "fx_factor_signals.rds")
out_factors    <- file.path(data_dir, "fx_factor_returns.rds")

# Setup ----

library(fredr)
library(dplyr, warn.conflicts = FALSE)
library(tibble)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

# Fetch helper: pull one FRED series with rate-limit delay
fetch_fred <- function(series_id, start = obs_start, end = obs_end) {
  Sys.sleep(api_delay)
  cat(sprintf("  Fetching %s ...\n", series_id))
  fredr(
    series_id         = series_id,
    observation_start = start,
    observation_end   = end
  ) |>
    select(date, series_id, value) |>
    as_tibble()
}


# ============================================================
# 1. Fetch Exchange Rate Data
# ============================================================

cat("\n=== Fetching exchange rates ===\n")

fx_raw_list <- lapply(names(fx_series), function(ccy) {
  fetch_fred(fx_series[[ccy]]) |>
    mutate(currency = ccy)
})
fx_raw <- bind_rows(fx_raw_list)

cat(sprintf("  Pulled %d exchange rate observations\n", nrow(fx_raw)))
saveRDS(fx_raw, out_raw_fx)


# ============================================================
# 2. Fetch Interest Rate Data
# ============================================================

cat("\n=== Fetching interest rates ===\n")

ir_raw_list <- lapply(names(rate_series), function(ccy) {
  fetch_fred(rate_series[[ccy]]) |>
    mutate(currency = ccy)
})

# Japan splice: fill gaps in JPY interbank with CD rate
cat("  Fetching Japan CD rate for splice ...\n")
jpy_cd <- fetch_fred(jpy_cd_series) |>
  mutate(currency = "JPY_CD")

ir_raw <- bind_rows(ir_raw_list)

# Splice JPY: use interbank where available, fill earlier dates with CD rate
jpy_interbank <- ir_raw |> filter(currency == "JPY")
jpy_cd_only <- jpy_cd |>
  filter(!date %in% jpy_interbank$date) |>
  mutate(currency = "JPY", series_id = rate_series[["JPY"]])

ir_raw <- ir_raw |>
  filter(currency != "JPY") |>
  bind_rows(jpy_interbank) |>
  bind_rows(jpy_cd_only) |>
  arrange(currency, date)

cat(sprintf("  Pulled %d interest rate observations\n", nrow(ir_raw)))
saveRDS(ir_raw, out_raw_rates)


# ============================================================
# 3. Fetch CPI Data
# ============================================================

cat("\n=== Fetching CPI data ===\n")

# Monthly CPI series
cpi_raw_list <- lapply(names(cpi_series), function(ccy) {
  fetch_fred(cpi_series[[ccy]]) |>
    mutate(currency = ccy)
})

# Quarterly CPI series (AU, NZ) — step-interpolate to monthly
cpi_q_list <- lapply(names(cpi_quarterly_series), function(ccy) {
  cat(sprintf("  Fetching quarterly CPI for %s, will interpolate to monthly\n", ccy))
  q_data <- fetch_fred(cpi_quarterly_series[[ccy]])
  # Expand quarterly to monthly: carry forward each quarter's value
  monthly_dates <- seq.Date(min(q_data$date), max(q_data$date) + 60, by = "month")
  monthly_grid <- tibble(date = monthly_dates)
  # Left join and fill forward
  merged <- monthly_grid |>
    left_join(q_data, by = "date") |>
    mutate(value = Reduce(function(a, b) if (is.na(b)) a else b, value, accumulate = TRUE)) |>
    filter(!is.na(value)) |>
    mutate(currency = ccy)
  merged
})

cpi_raw <- bind_rows(c(cpi_raw_list, cpi_q_list))

cat(sprintf("  Pulled %d CPI observations\n", nrow(cpi_raw)))
saveRDS(cpi_raw, out_raw_cpi)


# ============================================================
# 4. Normalize Exchange Rates & Compute Spot Returns
# ============================================================
#
# TARGET CONVENTION: "foreign currency per 1 USD" (indirect quote)
#   - An INCREASE in the rate means the USD strengthened / foreign weakened
#   - Already indirect: CAD, CHF, JPY, NOK, SEK (use as-is)
#   - Direct quotes (USD per foreign): AUD, EUR, GBP, NZD -- INVERT these
#
# SPOT RETURN convention (return on a LONG foreign currency position):
#   With rates as foreign-per-USD:
#     spot_return_t = S_{t-1} / S_t - 1
#   A decrease in S (fewer foreign units per USD = foreign appreciated)
#   gives a positive return.

cat("\n=== Computing FX spot returns ===\n")

fx_normalized <- fx_raw |>
  mutate(
    # Invert direct quotes to get foreign-per-USD
    rate = if_else(currency %in% direct_quote_currencies, 1 / value, value)
  ) |>
  select(date, currency, rate) |>
  arrange(currency, date) |>
  group_by(currency) |>
  mutate(
    # Spot return: long foreign currency, funded in USD
    # With foreign/USD convention: spot_return = S_{t-1}/S_t - 1
    spot_return = lag(rate) / rate - 1
  ) |>
  ungroup()


# ============================================================
# 5. Compute Carry (Interest Rate Differential)
# ============================================================
#
# carry_signal = i_foreign - i_US  (in pct per annum)
# monthly carry component = (i_foreign - i_US) / 100 / 12
#   (FRED rates are in percent, e.g. 5.0 = 5%)

cat("=== Computing carry signals ===\n")

us_rate <- ir_raw |>
  filter(currency == "USD") |>
  select(date, rate_us = value)

carry_data <- ir_raw |>
  filter(currency != "USD") |>
  select(date, currency, rate_foreign = value) |>
  inner_join(us_rate, by = "date") |>
  mutate(
    carry_signal  = rate_foreign - rate_us,          # pct p.a.
    carry_monthly = carry_signal / 100 / 12          # decimal, monthly
  )


# ============================================================
# 6. Compute FX Excess Returns
# ============================================================
#
# excess_return_t = spot_return_t + carry_{t-1} / 12
# carry from previous month-end (signal known before the return period)

cat("=== Computing FX excess returns ===\n")

# Lag carry by one month so carry is known at the start of the return period
carry_lagged <- carry_data |>
  arrange(currency, date) |>
  group_by(currency) |>
  mutate(carry_monthly_lag = lag(carry_monthly)) |>
  ungroup() |>
  select(date, currency, carry_signal, carry_monthly_lag)

fx_returns <- fx_normalized |>
  inner_join(carry_lagged, by = c("date", "currency")) |>
  mutate(
    excess_return = spot_return + carry_monthly_lag
  ) |>
  filter(!is.na(excess_return)) |>
  select(date, currency, rate, spot_return, carry_signal, excess_return)

cat(sprintf("  Computed %d excess return observations\n", nrow(fx_returns)))
saveRDS(fx_returns, out_returns)


# ============================================================
# 7. Compute Factor Signals
# ============================================================

cat("\n=== Computing factor signals ===\n")

# --- 7a. Carry signal: interest rate differential (already computed) ---
# carry_signal = rate_foreign - rate_us (pct p.a.)

# --- 7b. Momentum signal: 12-1 month cumulative excess return ---
# Sum of log excess returns from t-12 to t-2 (skip most recent month)

fx_signals <- fx_returns |>
  arrange(currency, date) |>
  group_by(currency) |>
  mutate(
    log_xr = log(1 + excess_return),
    # 12-1 momentum: cumulative log return from t-12 to t-2
    # slider::slide_dbl with .before=12, .after=-2 gives window [t-12, t-2]
    # That is 11 months of returns, skipping the most recent
    momentum_signal = slider::slide_dbl(
      log_xr,
      sum,
      .before = 12,
      .after = -2,
      .complete = TRUE
    )
  ) |>
  ungroup() |>
  select(-log_xr)

# --- 7c. Value signal: log real exchange rate ---
# RER = S * (CPI_US / CPI_foreign)  [all in foreign-per-USD convention]
# log_rer = log(S) + log(CPI_US) - log(CPI_foreign)
# HIGH log_rer = foreign currency is CHEAP (many foreign units per USD,
#   but adjusted for price levels). CHEAP = value BUY (go long foreign).
# LOW log_rer = foreign currency is EXPENSIVE = value SELL (go short).

cpi_us <- cpi_raw |>
  filter(currency == "USD") |>
  select(date, cpi_us = value)

cpi_foreign <- cpi_raw |>
  filter(currency != "USD") |>
  select(date, currency, cpi_foreign = value)

value_data <- fx_normalized |>
  inner_join(cpi_us, by = "date") |>
  inner_join(cpi_foreign, by = c("date", "currency")) |>
  mutate(
    log_rer = log(rate) + log(cpi_us) - log(cpi_foreign)
  ) |>
  select(date, currency, log_rer)

# Merge all signals
fx_signals <- fx_signals |>
  left_join(value_data, by = c("date", "currency"))

cat(sprintf("  Signal observations: %d\n", nrow(fx_signals)))
saveRDS(fx_signals, out_signals)


# ============================================================
# 8. Compute Factor Portfolio Returns
# ============================================================
#
# For each factor, each month:
#   - Cross-sectionally rank the 9 currencies by the signal
#   - Long top 3 (highest signal), short bottom 3, equal-weighted
#   - Factor return = mean(long returns) - mean(short returns)
#
# Sign conventions for sorting:
#   Carry:    high carry_signal = high yield = LONG  (ascending rank, long top)
#   Momentum: high momentum_signal = winners = LONG  (ascending rank, long top)
#   Value:    high log_rer = cheap foreign ccy = LONG (ascending rank, long top)
#             [foreign ccy is cheap when log_rer is high in our convention,
#              because it takes many foreign units per USD even after CPI adjustment]

cat("\n=== Computing factor portfolio returns ===\n")

# Helper: compute long-short factor return from a signal column
# signal_col: name of the signal column
# long_high: if TRUE, long the highest-signal currencies (carry, momentum, value)
compute_factor_return <- function(df, signal_col, long_high = TRUE) {
  df |>
    filter(!is.na(.data[[signal_col]])) |>
    group_by(date) |>
    filter(n() >= (n_long + n_short)) |>
    mutate(
      signal_rank = rank(.data[[signal_col]], ties.method = "first")
    ) |>
    mutate(
      n_ccy = n(),
      leg = case_when(
        long_high & signal_rank > (n_ccy - n_long) ~ "long",
        long_high & signal_rank <= n_short          ~ "short",
        !long_high & signal_rank <= n_long          ~ "long",
        !long_high & signal_rank > (n_ccy - n_short) ~ "short",
        TRUE ~ "neutral"
      )
    ) |>
    filter(leg %in% c("long", "short")) |>
    # Use NEXT month's excess return for the portfolio (signal at t, return at t+1)
    # But since our signals and returns are already aligned (carry_signal is lagged),
    # we use the current month's excess_return with the signal from the same row.
    # The signal uses data through t-1 or earlier; the return is realized at t.
    summarize(
      long_ret  = mean(excess_return[leg == "long"]),
      short_ret = mean(excess_return[leg == "short"]),
      .groups = "drop"
    ) |>
    mutate(factor_return = long_ret - short_ret) |>
    select(date, long_ret, short_ret, factor_return)
}

carry_factor <- compute_factor_return(fx_signals, "carry_signal", long_high = TRUE) |>
  rename(carry_long = long_ret, carry_short = short_ret, carry_return = factor_return)

momentum_factor <- compute_factor_return(fx_signals, "momentum_signal", long_high = TRUE) |>
  rename(mom_long = long_ret, mom_short = short_ret, mom_return = factor_return)

value_factor <- compute_factor_return(fx_signals, "log_rer", long_high = TRUE) |>
  rename(val_long = long_ret, val_short = short_ret, val_return = factor_return)

# Combine into a single factor return tibble
fx_factor_returns <- carry_factor |>
  full_join(momentum_factor, by = "date") |>
  full_join(value_factor, by = "date") |>
  arrange(date)

cat(sprintf("  Factor return observations: %d\n", nrow(fx_factor_returns)))
saveRDS(fx_factor_returns, out_factors)


# ============================================================
# 9. Summary
# ============================================================

cat("\n========================================\n")
cat("           FX Factor Data Summary\n")
cat("========================================\n\n")

cat("--- Raw Exchange Rates ---\n")
cat(sprintf("  Currencies: %s\n", paste(currencies, collapse = ", ")))
cat(sprintf("  Date range: %s to %s\n",
            min(fx_raw$date), max(fx_raw$date)))
cat(sprintf("  Observations: %d\n\n", nrow(fx_raw)))

cat("--- Raw Interest Rates ---\n")
cat(sprintf("  Date range: %s to %s\n",
            min(ir_raw$date), max(ir_raw$date)))
cat(sprintf("  Observations: %d (includes JPY splice)\n\n", nrow(ir_raw)))

cat("--- Raw CPI ---\n")
cat(sprintf("  Date range: %s to %s\n",
            min(cpi_raw$date), max(cpi_raw$date)))
cat(sprintf("  Observations: %d\n\n", nrow(cpi_raw)))

cat("--- FX Excess Returns ---\n")
cat(sprintf("  Date range: %s to %s\n",
            min(fx_returns$date), max(fx_returns$date)))
cat(sprintf("  Observations: %d\n\n", nrow(fx_returns)))

cat("--- Factor Returns ---\n")
cat(sprintf("  Date range: %s to %s\n",
            min(fx_factor_returns$date, na.rm = TRUE),
            max(fx_factor_returns$date, na.rm = TRUE)))
cat(sprintf("  Observations: %d\n", nrow(fx_factor_returns)))

# Annualized stats (mean * 12, sd * sqrt(12), Sharpe = mean/sd * sqrt(12))
cat("\n  Annualized factor statistics:\n")
for (fac in c("carry_return", "mom_return", "val_return")) {
  x <- fx_factor_returns[[fac]]
  x <- x[!is.na(x)]
  if (length(x) > 12) {
    ann_mean  <- mean(x) * 12
    ann_sd    <- sd(x) * sqrt(12)
    sharpe    <- ann_mean / ann_sd
    cat(sprintf("    %-14s  mean=%.2f%%  vol=%.2f%%  SR=%.2f  (n=%d months)\n",
                fac, ann_mean * 100, ann_sd * 100, sharpe, length(x)))
  }
}

cat("\n  Sample factor returns (last 5 months):\n")
print(tail(fx_factor_returns, 5))

cat(sprintf("\nSaved to:\n  %s\n  %s\n  %s\n  %s\n  %s\n  %s\n",
            out_raw_fx, out_raw_rates, out_raw_cpi,
            out_returns, out_signals, out_factors))
