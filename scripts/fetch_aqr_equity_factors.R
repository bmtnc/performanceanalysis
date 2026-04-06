# Fetch & Cache AQR Equity Factor Data
# Downloads the 4 AQR Excel files (if not already cached) and parses them
# into tidy RDS caches.

# Script Params ----

data_dir <- "data/aqr"

# AQR download URLs -> local file names
aqr_downloads <- c(
  hml_factors_monthly.xlsx = "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/The-Devil-in-HMLs-Details-Factors-Monthly.xlsx",
  bab_factors_monthly.xlsx = "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Betting-Against-Beta-Equity-Factors-Monthly.xlsx",
  qmj_factors_monthly.xlsx = "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Quality-Minus-Junk-Factors-Monthly.xlsx",
  momentum_factors_monthly.xlsx = "https://www.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/Momentum-Indices-Monthly.xlsx"
)

# Input files
hml_file <- file.path(data_dir, "hml_factors_monthly.xlsx")
bab_file <- file.path(data_dir, "bab_factors_monthly.xlsx")
qmj_file <- file.path(data_dir, "qmj_factors_monthly.xlsx")
mom_file <- file.path(data_dir, "momentum_factors_monthly.xlsx")

# Output files
equity_factors_out <- file.path(data_dir, "aqr_equity_factors.rds")
momentum_out <- file.path(data_dir, "aqr_momentum_factors.rds")
rf_out <- file.path(data_dir, "aqr_risk_free_rate.rds")

# Geographies to keep from the 30-column factor sheets
keep_geos <- c("USA", "Global", "Global Ex USA", "Europe", "Pacific")

# Download ----

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

for (fname in names(aqr_downloads)) {
  dest <- file.path(data_dir, fname)
  if (!file.exists(dest)) {
    cat(sprintf("Downloading %s ...\n", fname))
    download.file(aqr_downloads[[fname]], dest, mode = "wb", quiet = TRUE)
  } else {
    cat(sprintf("Already cached: %s\n", fname))
  }
}

# Helpers ----

# Pivot a wide AQR factor sheet (DATE + geography columns) to long format.
# Returns tibble with columns: date, geography, <value_col>.
pivot_factor_long <- function(wide_df, value_col, geos = keep_geos) {
  # Parse dates from character MM/DD/YYYY
  wide_df$DATE <- as.Date(wide_df$DATE, format = "%m/%d/%Y")

  # Keep only date + requested geographies
  cols_to_keep <- intersect(names(wide_df), geos)
  wide_df <- wide_df[, c("DATE", cols_to_keep), drop = FALSE]

  # Stack geography columns into long format
  rows <- lapply(cols_to_keep, function(geo) {
    tibble::tibble(
      date = wide_df$DATE,
      geography = geo,
      value = wide_df[[geo]]
    )
  })
  long_df <- do.call(rbind, rows)
  names(long_df)[names(long_df) == "value"] <- value_col

  # Drop rows where date or return is NA
  long_df <- long_df[!is.na(long_df$date) & !is.na(long_df[[value_col]]), ]
  tibble::as_tibble(long_df)
}

# Parse HML (Value) ----

cat("Parsing HML Devil (value) factor...\n")
hml_raw <- readxl::read_excel(hml_file, sheet = 1, skip = 18)
hml <- pivot_factor_long(hml_raw, "hml")

# Parse BAB (Defensive) ----

cat("Parsing BAB (betting against beta) factor...\n")
bab_raw <- readxl::read_excel(bab_file, sheet = 1, skip = 18)
bab <- pivot_factor_long(bab_raw, "bab")

# Parse QMJ (Quality) ----

cat("Parsing QMJ (quality minus junk) factor...\n")
qmj_raw <- readxl::read_excel(qmj_file, sheet = 1, skip = 18)
qmj <- pivot_factor_long(qmj_raw, "qmj")

# Parse MKT, SMB, RF from the HML file ----

cat("Parsing supplementary factors (MKT, SMB, RF)...\n")

mkt_raw <- readxl::read_excel(hml_file, sheet = "MKT", skip = 18)
mkt <- pivot_factor_long(mkt_raw, "mkt")

smb_raw <- readxl::read_excel(hml_file, sheet = "SMB", skip = 18)
smb <- pivot_factor_long(smb_raw, "smb")

# RF is just 2 columns: DATE, Risk Free Rate
rf_raw <- readxl::read_excel(hml_file, sheet = "RF", skip = 18)
rf <- tibble::tibble(
  date = as.Date(rf_raw$DATE, format = "%m/%d/%Y"),
  rf = rf_raw[["Risk Free Rate"]]
)
rf <- rf[!is.na(rf$date) & !is.na(rf$rf), ]

# Parse Momentum ----

cat("Parsing momentum factors...\n")
mom_raw <- readxl::read_excel(mom_file, sheet = "Returns", skip = 1)

# Only take the first 4 columns (monthly data); columns 5+ are annual
mom <- mom_raw[, 1:4]
names(mom) <- c("date", "us_large_cap", "us_small_cap", "international")

# readxl parses momentum dates as POSIXct; convert to Date
mom$date <- as.Date(mom$date)

# Drop trailing NA rows
mom <- mom[!is.na(mom$date), ]
mom <- tibble::as_tibble(mom)

# Combine Multi-Factor Panel ----

cat("Joining factors into multi-factor panel...\n")

# Merge HML, BAB, QMJ, MKT, SMB by date + geography
equity_factors <- hml |>
  dplyr::full_join(bab, by = c("date", "geography")) |>
  dplyr::full_join(qmj, by = c("date", "geography")) |>
  dplyr::full_join(mkt, by = c("date", "geography")) |>
  dplyr::full_join(smb, by = c("date", "geography")) |>
  dplyr::arrange(geography, date)

# Save ----

cat("Saving RDS files...\n")
saveRDS(equity_factors, equity_factors_out)
saveRDS(mom, momentum_out)
saveRDS(rf, rf_out)

# Summary ----

cat("\n--- Equity Factors Panel ---\n")
cat(sprintf("  Rows: %d\n", nrow(equity_factors)))
cat(sprintf(
  "  Date range: %s to %s\n",
  min(equity_factors$date),
  max(equity_factors$date)
))
cat(sprintf(
  "  Geographies: %s\n",
  paste(unique(equity_factors$geography), collapse = ", ")
))
cat("  Columns:", paste(names(equity_factors), collapse = ", "), "\n")
cat("  Sample:\n")
print(utils::head(equity_factors, 5))

cat("\n--- Momentum Factors ---\n")
cat(sprintf("  Rows: %d\n", nrow(mom)))
cat(sprintf("  Date range: %s to %s\n", min(mom$date), max(mom$date)))
cat("  Sample:\n")
print(utils::head(mom, 5))

cat("\n--- Risk-Free Rate ---\n")
cat(sprintf("  Rows: %d\n", nrow(rf)))
cat(sprintf("  Date range: %s to %s\n", min(rf$date), max(rf$date)))
cat("  Sample:\n")
print(utils::head(rf, 5))

cat(sprintf(
  "\nSaved to:\n  %s\n  %s\n  %s\n",
  equity_factors_out,
  momentum_out,
  rf_out
))
