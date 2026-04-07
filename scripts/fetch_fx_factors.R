# Fetch & Cache FX/Currency Factor Data from FRED
# Pulls exchange rates, interest rates, and CPI data, then constructs
# carry, momentum, and value factor return series for G10 currencies.

# Config ----

data_dir <- "data/fred"
api_delay <- 0.5 # seconds between FRED API calls (rate limit)

# Date range for all FRED pulls
obs_start <- as.Date("1990-01-01")
obs_end <- Sys.Date()

# G10 currencies (all vs USD)
currencies <- c("AUD", "CAD", "CHF", "EUR", "GBP", "JPY", "NOK", "NZD", "SEK")

# FIX 1: Daily FRED exchange rate series (H.10 release, DEX prefix)
# Using daily series and taking last business day of each month eliminates
# artificial smoothing and half-month timing lag from monthly averages.
# Convention: some are "USD per foreign" (direct), some are "foreign per USD" (indirect)
fx_series <- c(
  AUD = "DEXUSAL", # USD per AUD  (direct)
  CAD = "DEXCAUS", # CAD per USD  (indirect)
  CHF = "DEXSZUS", # CHF per USD  (indirect)
  EUR = "DEXUSEU", # USD per EUR  (direct)
  GBP = "DEXUSUK", # USD per GBP  (direct)
  JPY = "DEXJPUS", # JPY per USD  (indirect)
  NOK = "DEXNOUS", # NOK per USD  (indirect)
  NZD = "DEXUSNZ", # USD per NZD  (direct)
  SEK = "DEXSDUS" # SEK per USD  (indirect)
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

# US T-bill splice: The OECD 3M interbank series (IR3TIB01USM156N) has sporadic
# gaps — notably April 2020 due to a publication delay during COVID. A single
# missing USD rate NAs out carry signals for ALL 9 currencies that month, and
# the lagged carry propagation also wipes out May 2020 excess returns entirely.
# TB3MS (3M T-bill secondary market rate) is a close substitute at the same
# tenor and fills these gaps.
us_tbill_series <- "TB3MS"

# FIX 2: CPI index-level series (OECD via FRED, 2015=100 base)
# Must be INDEX LEVELS (values ~50-150), NOT month-over-month percentage changes.
# Previous CPALTT01xxM657N series were percentage changes -- taking log() of
# percentage changes produced nonsensical values.
# Using xxCPIALLMINMEI pattern (verified index-level, monthly, not seasonally adj).
# AU, NZ are quarterly-only on FRED; EUR uses Eurostat series.
cpi_series <- c(
  USD = "USACPIALLMINMEI", # CPI: All Items for US, Index 2015=100
  CAD = "CANCPIALLMINMEI", # CPI: All Items for Canada, Index 2015=100
  CHF = "CHECPIALLMINMEI", # CPI: All Items for Switzerland, Index 2015=100
  EUR = "CP0000EZ19M086NEST", # Eurostat HICP for Euro Area, Index 2015=100
  GBP = "GBRCPIALLMINMEI", # CPI: All Items for UK, Index 2015=100
  JPY = "JPNCPIALLMINMEI", # CPI: All Items for Japan, Index 2015=100
  NOK = "NORCPIALLMINMEI", # CPI: All Items for Norway, Index 2015=100
  SEK = "SWECPIALLMINMEI" # CPI: All Items for Sweden, Index 2015=100
)

# Quarterly CPI series (AU and NZ only have quarterly on FRED)
# Step-interpolated to monthly (carry forward)
cpi_quarterly_series <- c(
  AUD = "AUSCPIALLQINMEI",
  NZD = "NZLCPIALLQINMEI"
)

# OECD PPP exchange rate API (free, no key needed)
# Annual PPP rates for GDP, used for FX value signal
oecd_ppp_url <- paste0(
  "https://sdmx.oecd.org/public/rest/data/",
  "OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,/",
  "A.AUS+CAN+CHE+EA20+GBR+JPN+NOR+NZL+SWE",
  ".S1.S1.PPP_B1GQ+EXC_E.F21._Z._Z.XDC_USD._Z.N.T001",
  "?startPeriod=1960&endPeriod=2025"
)

# OECD country code -> currency mapping
oecd_to_currency <- c(
  AUS = "AUD",
  CAN = "CAD",
  CHE = "CHF",
  EA20 = "EUR",
  GBR = "GBP",
  JPN = "JPY",
  NOR = "NOK",
  NZL = "NZD",
  SWE = "SEK"
)

# Output file paths
out_raw_fx <- file.path(data_dir, "fx_raw_rates.rds")
out_raw_rates <- file.path(data_dir, "fx_raw_interest_rates.rds")
out_raw_cpi <- file.path(data_dir, "fx_raw_cpi.rds")
out_returns <- file.path(data_dir, "fx_returns.rds")
out_signals <- file.path(data_dir, "fx_factor_signals.rds")
out_factors <- file.path(data_dir, "fx_factor_returns.rds")

# Setup ----

library(fredr)
library(dplyr, warn.conflicts = FALSE)
library(tibble)
library(lubridate, warn.conflicts = FALSE)
library(tidyr)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

# Fetch helper: pull one FRED series with rate-limit delay and retry on transient errors
fetch_fred <- function(
  series_id,
  start = obs_start,
  end = obs_end,
  max_retries = 3
) {
  for (attempt in seq_len(max_retries)) {
    Sys.sleep(api_delay)
    if (attempt > 1) {
      cat(sprintf("  Retry %d for %s ...\n", attempt, series_id))
    } else {
      cat(sprintf("  Fetching %s ...\n", series_id))
    }
    result <- tryCatch(
      {
        fredr(
          series_id = series_id,
          observation_start = start,
          observation_end = end
        ) |>
          select(date, series_id, value) |>
          as_tibble()
      },
      error = function(e) {
        if (attempt == max_retries) {
          stop(e)
        }
        Sys.sleep(2 * attempt) # back off before retry
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }
}


# ============================================================
# 1. Fetch Exchange Rate Data (daily, then take end-of-month)
# ============================================================

cat("\n=== Fetching daily exchange rates ===\n")

fx_daily_list <- lapply(names(fx_series), function(ccy) {
  fetch_fred(fx_series[[ccy]]) |>
    mutate(currency = ccy)
})
fx_daily <- bind_rows(fx_daily_list)
cat(sprintf("  Pulled %d daily exchange rate observations\n", nrow(fx_daily)))

# FIX 1 (cont): Take last non-NA observation per month for each currency.
# This gives end-of-month (last business day) values, eliminating the
# smoothing and timing lag inherent in monthly averages.
fx_raw <- fx_daily |>
  filter(!is.na(value)) |>
  mutate(ym = floor_date(date, "month")) |>
  arrange(currency, date) |>
  group_by(currency, ym) |>
  slice_tail(n = 1) |>
  ungroup() |>
  mutate(date = lubridate::ceiling_date(ym, "month") - 1L) |>
  select(date, series_id, value, currency)

cat(sprintf("  End-of-month observations: %d\n", nrow(fx_raw)))
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

# Splice USD: fill gaps in OECD interbank with 3M T-bill secondary market rate
cat("  Fetching US 3M T-bill rate for splice ...\n")
us_tbill <- fetch_fred(us_tbill_series) |>
  mutate(currency = "USD_TBILL")

# Fill dates where the OECD series is missing OR has NA value
usd_interbank <- ir_raw |> filter(currency == "USD")
usd_missing_dates <- usd_interbank |>
  filter(is.na(value)) |>
  pull(date)
usd_tbill_fill <- us_tbill |>
  filter(date %in% usd_missing_dates | !date %in% usd_interbank$date) |>
  mutate(currency = "USD", series_id = rate_series[["USD"]])

if (nrow(usd_tbill_fill) > 0) {
  cat(sprintf("  Splicing %d USD observations from TB3MS\n", nrow(usd_tbill_fill)))
  # Remove NA rows being replaced, then add T-bill values
  ir_raw <- ir_raw |>
    filter(!(currency == "USD" & date %in% usd_tbill_fill$date)) |>
    bind_rows(usd_tbill_fill) |>
    arrange(currency, date)
}

ir_raw <- ir_raw %>%
  mutate(date = lubridate::ceiling_date(date, "month") - 1L)
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
  cat(sprintf(
    "  Fetching quarterly CPI for %s, will interpolate to monthly\n",
    ccy
  ))
  q_data <- fetch_fred(cpi_quarterly_series[[ccy]])
  # Expand quarterly to monthly: carry forward each quarter's value
  monthly_dates <- seq.Date(
    min(q_data$date),
    max(q_data$date) + 60,
    by = "month"
  )
  monthly_grid <- tibble(date = monthly_dates)
  # Left join and fill forward
  merged <- monthly_grid |>
    left_join(q_data, by = "date") |>
    mutate(
      value = Reduce(
        function(a, b) if (is.na(b)) a else b,
        value,
        accumulate = TRUE
      )
    ) |>
    filter(!is.na(value)) |>
    mutate(currency = ccy)
  merged
})

cpi_raw <- bind_rows(c(cpi_raw_list, cpi_q_list))

cat(sprintf("  Pulled %d CPI observations\n", nrow(cpi_raw)))

# FIX 2 (cont): Verify CPI values are index levels (~50-150 for 2015=100 base),
# NOT percentage changes (~0-5). Log of percentage changes is nonsensical.
cat("\n  CPI index-level diagnostic (median values by currency):\n")
cpi_diag <- cpi_raw |>
  group_by(currency) |>
  summarize(
    median_val = median(value, na.rm = TRUE),
    min_val = min(value, na.rm = TRUE),
    max_val = max(value, na.rm = TRUE),
    .groups = "drop"
  )
for (i in seq_len(nrow(cpi_diag))) {
  row <- cpi_diag[i, ]
  status <- if (row$median_val > 20) {
    "OK (index)"
  } else {
    "WARNING: looks like pct changes!"
  }
  cat(sprintf(
    "    %s: median=%.1f  range=[%.1f, %.1f]  %s\n",
    row$currency,
    row$median_val,
    row$min_val,
    row$max_val,
    status
  ))
}

# If any series looks like percentage changes, attempt to find correct index series
bad_cpi <- cpi_diag |> filter(median_val <= 20)
if (nrow(bad_cpi) > 0) {
  cat(
    "\n  WARNING: Some CPI series appear to be percentage changes, not index levels.\n"
  )
  cat("  Attempting to find correct index-level series via FRED search...\n")
  for (ccy in bad_cpi$currency) {
    cat(sprintf("  Searching for CPI index for %s...\n", ccy))
    search_results <- tryCatch(
      {
        Sys.sleep(api_delay)
        fredr_series_search_text(
          search_text = paste("CPI All Items", ccy, "Index 2015"),
          limit = 5
        )
      },
      error = function(e) NULL
    )
    if (!is.null(search_results) && nrow(search_results) > 0) {
      cat(sprintf(
        "    Candidates: %s\n",
        paste(search_results$id, collapse = ", ")
      ))
    }
  }
  stop("Fix the CPI series IDs above before proceeding.")
}

cpi_raw <- cpi_raw %>%
  mutate(date = lubridate::ceiling_date(date, "month") - 1L)
saveRDS(cpi_raw, out_raw_cpi)


# ============================================================
# 3b. Fetch OECD PPP Exchange Rates (annual, free API)
# ============================================================

cat("\n=== Fetching OECD PPP exchange rates ===\n")

ppp_tmp <- tempfile(fileext = ".csv")
tryCatch(
  {
    download.file(
      oecd_ppp_url,
      ppp_tmp,
      quiet = TRUE,
      headers = c(Accept = "application/vnd.sdmx.data+csv;file=true")
    )
    ppp_raw <- read.csv(ppp_tmp, stringsAsFactors = FALSE)

    # Parse: columns REF_AREA (country), TRANSACTION (PPP_B1GQ or EXC_E),
    # TIME_PERIOD (year), OBS_VALUE
    ppp_data <- ppp_raw |>
      as_tibble() |>
      filter(TRANSACTION %in% c("PPP_B1GQ", "EXC_E")) |>
      select(
        country = REF_AREA,
        measure = TRANSACTION,
        year = TIME_PERIOD,
        value = OBS_VALUE
      ) |>
      mutate(
        currency = oecd_to_currency[country],
        year = as.integer(year),
        value = as.numeric(value)
      ) |>
      filter(!is.na(currency), !is.na(value)) |>
      pivot_wider(
        id_cols = c(currency, year),
        names_from = measure,
        values_from = value
      ) |>
      rename(ppp_rate = PPP_B1GQ, mkt_rate_eop = EXC_E)

    cat(sprintf(
      "  Fetched PPP data: %d currency-years, %d-%d\n",
      nrow(ppp_data),
      min(ppp_data$year),
      max(ppp_data$year)
    ))

    # Interpolate annual PPP to monthly using relative CPI
    # PPP_{i,m} = PPP_{i,Y} * (CPI_i,m / CPI_i,Y) / (CPI_US,m / CPI_US,Y)
    # where Y is the year of the annual PPP observation
    cpi_us <- cpi_raw |>
      filter(currency == "USD") |>
      select(date, cpi_us = value)

    ppp_monthly <- ppp_data |>
      # Create a Jan 1 date for each year to join with CPI
      mutate(year_date = as.Date(paste0(year, "-01-01"))) |>
      # Get CPI at year start for each currency
      inner_join(
        cpi_raw |>
          mutate(
            year = as.integer(format(date, "%Y")),
            year_date = as.Date(paste0(year, "-01-01"))
          ) |>
          filter(format(date, "%m") == "01") |>
          select(currency, year, cpi_base_foreign = value),
        by = c("currency", "year")
      ) |>
      inner_join(
        cpi_us |>
          mutate(year = as.integer(format(date, "%Y"))) |>
          filter(format(date, "%m") == "01") |>
          select(year, cpi_base_us = cpi_us),
        by = "year"
      )

    # Expand to monthly
    ppp_monthly_expanded <- ppp_monthly |>
      crossing(month = 1:12) |>
      mutate(
        date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))
      ) |>
      filter(date >= obs_start, date <= obs_end) |>
      inner_join(
        cpi_raw |>
          filter(currency != "USD") |>
          select(date, currency, cpi_foreign = value),
        by = c("date", "currency")
      ) |>
      inner_join(cpi_us, by = "date") |>
      mutate(
        # Interpolated monthly PPP = annual PPP adjusted by relative CPI drift
        ppp_monthly = ppp_rate *
          (cpi_foreign / cpi_base_foreign) /
          (cpi_us / cpi_base_us)
      ) |>
      select(date, currency, ppp_monthly) %>%
      mutate(date = lubridate::ceiling_date(date, "month") - 1L)

    cat(sprintf(
      "  Interpolated to %d monthly PPP observations\n",
      nrow(ppp_monthly_expanded)
    ))
    saveRDS(ppp_monthly_expanded, file.path(data_dir, "fx_ppp_rates.rds"))
    ppp_available <- TRUE
  },
  error = function(e) {
    cat(sprintf("  WARNING: Failed to fetch OECD PPP data: %s\n", e$message))
    cat("  Falling back to rolling-mean RER deviation for value signal.\n")
    ppp_available <<- FALSE
  }
)


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
    carry_signal = rate_foreign - rate_us, # pct p.a.
    carry_monthly = carry_signal / 100 / 12 # decimal, monthly
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

# --- 7c. Value signal: PPP deviation ---
#
# Signal = log(market_rate / ppp_rate) where both are in foreign-per-USD.
#   POSITIVE = foreign ccy is CHEAP vs PPP (market rate > PPP rate, meaning
#     you get more foreign currency per USD than PPP says you should).
#     This is a value BUY -> go LONG foreign.
#   NEGATIVE = foreign ccy is EXPENSIVE vs PPP -> go SHORT.
#
# PPP rates from OECD (annual), interpolated to monthly using relative CPI.
# Falls back to rolling-mean RER deviation if PPP data unavailable.
# TODO: PPP interpolation truncates when CPI runs out (~2 month lag), which
#       makes FX value the other bottleneck alongside FI value. Options:
#       1. Forward-fill last PPP estimate (PPP moves slowly, defensible for a few months)
#       2. Use TIPS breakevens to extend CPI proxy for the US leg of the interpolation

if (exists("ppp_available") && ppp_available) {
  cat("  Using OECD PPP exchange rates for value signal\n")
  value_data <- fx_normalized |>
    inner_join(ppp_monthly_expanded, by = c("date", "currency")) |>
    mutate(
      # Both rate and ppp_monthly are in foreign-per-USD units
      value_signal = log(rate / ppp_monthly)
    ) |>
    arrange(currency, date) |>
    group_by(currency) |>
    # Lag by one month: signal uses S_t, which determines the return
    # from t-1 to t. Must lag to avoid look-ahead bias.
    mutate(value_signal = lag(value_signal)) |>
    ungroup() |>
    select(date, currency, value_signal)
} else {
  cat("  Falling back to rolling-mean RER deviation for value signal\n")
  cpi_us <- cpi_raw |> filter(currency == "USD") |> select(date, cpi_us = value)
  cpi_foreign <- cpi_raw |>
    filter(currency != "USD") |>
    select(date, currency, cpi_foreign = value)
  value_data <- fx_normalized |>
    inner_join(cpi_us, by = "date") |>
    inner_join(cpi_foreign, by = c("date", "currency")) |>
    mutate(log_rer = log(rate) + log(cpi_us) - log(cpi_foreign)) |>
    arrange(currency, date) |>
    group_by(currency) |>
    mutate(
      log_rer_mean = slider::slide_dbl(
        log_rer,
        mean,
        .before = 59,
        .complete = TRUE
      ),
      value_signal = log_rer - log_rer_mean,
      value_signal = lag(value_signal)
    ) |>
    ungroup() |>
    select(date, currency, value_signal)
}

# Merge all signals
fx_signals <- fx_signals |>
  left_join(value_data, by = c("date", "currency"))

cat(sprintf("  Signal observations: %d\n", nrow(fx_signals)))
saveRDS(fx_signals, out_signals)


# ============================================================
# 8. Compute Factor Portfolio Returns
# ============================================================
#
# FIX 4: Demeaned rank weights (AQR-style) replace tercile sorting.
#
# For each factor, each month:
#   1. Cross-sectionally rank the N currencies by the signal
#   2. Compute raw weight: w_i = rank_i - mean(rank)
#      (positive for above-median signal, negative for below)
#   3. Scale so sum of positive weights = 1, sum of negative weights = -1
#   4. Factor return = sum(w_i * excess_return_i)
#
# This uses ALL currencies, not just the extremes, and weights more
# aggressively toward the tails. Dollar-neutral by construction.
#
# Sign conventions for sorting (high signal = LONG):
#   Carry:    high carry_signal = high yield = LONG
#   Momentum: high momentum_signal = winners = LONG
#   Value:    high value_signal = cheap (PPP deviation) = LONG

cat("\n=== Computing factor portfolio returns ===\n")

# Helper: compute long-short factor return using demeaned rank weights
# signal_col: name of the signal column
# long_high: if TRUE, high signal gets positive (long) weight
compute_factor_return <- function(df, signal_col, long_high = TRUE) {
  df |>
    filter(!is.na(.data[[signal_col]])) |>
    group_by(date) |>
    filter(n() >= 3) |>
    mutate(
      # Rank from 1 (lowest signal) to N (highest signal)
      signal_rank = rank(.data[[signal_col]], ties.method = "average"),
      # Demean: positive weight for above-median, negative for below
      raw_weight = signal_rank - mean(signal_rank),
      # Flip sign if low signal should be long
      raw_weight = raw_weight * ifelse(long_high, 1, -1),
      # Scale: sum of positive weights = 1, sum of negative weights = -1
      pos_sum = sum(raw_weight[raw_weight > 0]),
      neg_sum = sum(abs(raw_weight[raw_weight < 0])),
      weight = case_when(
        raw_weight > 0 ~ raw_weight / pos_sum,
        raw_weight < 0 ~ raw_weight / neg_sum,
        TRUE ~ 0
      )
    ) |>
    summarize(
      long_ret = sum(weight[weight > 0] * excess_return[weight > 0]),
      short_ret = sum(weight[weight < 0] * excess_return[weight < 0]),
      factor_return = sum(weight * excess_return),
      .groups = "drop"
    ) |>
    select(date, long_ret, short_ret, factor_return)
}

carry_factor <- compute_factor_return(
  fx_signals,
  "carry_signal",
  long_high = TRUE
) |>
  rename(
    carry_long = long_ret,
    carry_short = short_ret,
    carry_return = factor_return
  )

momentum_factor <- compute_factor_return(
  fx_signals,
  "momentum_signal",
  long_high = TRUE
) |>
  rename(mom_long = long_ret, mom_short = short_ret, mom_return = factor_return)

# Value: high value_signal = foreign ccy cheap vs PPP proxy = LONG
value_factor <- compute_factor_return(
  fx_signals,
  "value_signal",
  long_high = TRUE
) |>
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
cat(sprintf("  Date range: %s to %s\n", min(fx_raw$date), max(fx_raw$date)))
cat(sprintf("  Observations: %d\n\n", nrow(fx_raw)))

cat("--- Raw Interest Rates ---\n")
cat(sprintf("  Date range: %s to %s\n", min(ir_raw$date), max(ir_raw$date)))
cat(sprintf("  Observations: %d (includes JPY splice)\n\n", nrow(ir_raw)))

cat("--- Raw CPI ---\n")
cat(sprintf("  Date range: %s to %s\n", min(cpi_raw$date), max(cpi_raw$date)))
cat(sprintf("  Observations: %d\n\n", nrow(cpi_raw)))

cat("--- FX Excess Returns ---\n")
cat(sprintf(
  "  Date range: %s to %s\n",
  min(fx_returns$date),
  max(fx_returns$date)
))
cat(sprintf("  Observations: %d\n\n", nrow(fx_returns)))

cat("--- Factor Returns ---\n")
cat(sprintf(
  "  Date range: %s to %s\n",
  min(fx_factor_returns$date, na.rm = TRUE),
  max(fx_factor_returns$date, na.rm = TRUE)
))
cat(sprintf("  Observations: %d\n", nrow(fx_factor_returns)))

# Annualized stats (mean * 12, sd * sqrt(12), Sharpe = mean/sd * sqrt(12))
cat("\n  Annualized factor statistics:\n")
for (fac in c("carry_return", "mom_return", "val_return")) {
  x <- fx_factor_returns[[fac]]
  x <- x[!is.na(x)]
  if (length(x) > 12) {
    ann_mean <- mean(x) * 12
    ann_sd <- sd(x) * sqrt(12)
    sharpe <- ann_mean / ann_sd
    cat(sprintf(
      "    %-14s  mean=%.2f%%  vol=%.2f%%  SR=%.2f  (n=%d months)\n",
      fac,
      ann_mean * 100,
      ann_sd * 100,
      sharpe,
      length(x)
    ))
  }
}

cat("\n  Sample factor returns (last 5 months):\n")
print(tail(fx_factor_returns, 5))

cat(sprintf(
  "\nSaved to:\n  %s\n  %s\n  %s\n  %s\n  %s\n  %s\n",
  out_raw_fx,
  out_raw_rates,
  out_raw_cpi,
  out_returns,
  out_signals,
  out_factors
))
