# Fetch Breakeven Inflation Raw Data
#
# Fetches and caches raw breakeven inflation data from multiple central bank
# sources, plus CPI data for regression-based estimation.
#
# Direct sources (all 10Y, from inflation-linked government bonds):
#   US:         FRED T10YIE (10Y TIPS breakeven)
#   Euro area:  ECB SDMX API (nominal minus real 10Y benchmark yields)
#   UK:         Bank of England IADB (IUDMIZC, 10Y implied inflation)
#   Australia:  RBA Table F2 (10Y nominal minus 10Y indexed)
#
# CPI data (for regression estimation in build_breakeven_inflation.R):
#   SE, CH, DK, NO, NZ, CA, EA, US from FRED
#
# Caching: Raw data is cached as .rds. Delete cache files to force re-fetch.
#
# Outputs in data/fred/:
#   be_raw_us.rds   — US monthly breakeven (date, country, breakeven)
#   be_raw_ecb.rds  — EA monthly breakeven (date, country, breakeven)
#   be_raw_boe.rds  — GB monthly breakeven (date, country, breakeven)
#   be_raw_rba.rds  — AU monthly breakeven (date, country, breakeven)
#   be_raw_cpi.rds  — CPI panel for regression countries (date, country, cpi)
#
# Requires: FRED_API_KEY env var
# TODO: Japan (ECB backfill + BB.JBTS scrape)

# Config ----

library(dplyr)
library(fredr)
library(httr)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

output_dir <- "data/fred"
api_sleep <- 0.5

# CPI FRED series for regression countries + references
cpi_regression_series <- c(
  SE = "SWECPIALLMINMEI",
  CH = "CHECPIALLMINMEI",
  DK = "DNKCPIALLMINMEI",
  NO = "NORCPIALLMINMEI",
  CA = "CANCPIALLMINMEI",
  EA = "CP0000EZ19M086NEST",
  US = "USACPIALLMINMEI"
)
# AU CPI is quarterly on FRED, handled alongside NZ below
au_cpi_series <- "AUSCPIALLQINMEI"

# Cache paths
cache_paths <- list(
  us_breakeven = file.path(output_dir, "be_raw_us.rds"),
  ea_breakeven = file.path(output_dir, "be_raw_ecb.rds"),
  uk_breakeven = file.path(output_dir, "be_raw_boe.rds"),
  au_breakeven = file.path(output_dir, "be_raw_rba.rds"),
  cpi_panel    = file.path(output_dir, "be_raw_cpi.rds")
)

# Helpers ----

fetch_fred <- function(series_id) {
  tryCatch(
    fredr::fredr(series_id = series_id),
    error = function(e) {
      message("  Failed to fetch ", series_id, ": ", conditionMessage(e))
      NULL
    }
  )
}

daily_to_monthly <- function(df, value_col) {
  df %>%
    dplyr::filter(!is.na(.data[[value_col]])) %>%
    dplyr::mutate(ym = lubridate::floor_date(date, "month")) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(ym) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = ym) %>%
    dplyr::select(-ym)
}

fetch_ecb <- function(series_key) {
  url <- paste0("https://data-api.ecb.europa.eu/service/data/FM/", series_key)
  resp <- httr::GET(url, httr::add_headers(Accept = "text/csv"))
  if (httr::status_code(resp) != 200) {
    message("  ECB request failed (HTTP ", httr::status_code(resp), "): ", series_key)
    return(NULL)
  }
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  read.csv(text = txt, stringsAsFactors = FALSE)
}

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 1. US breakeven: FRED T10YIE ----

if (file.exists(cache_paths$us_breakeven)) {
  message("1. US breakeven: cached")
} else {
  message("1. Fetching US 10Y TIPS breakeven (T10YIE)...")
  us_raw <- fetch_fred("T10YIE")

  if (!is.null(us_raw)) {
    us_breakeven <- us_raw %>%
      dplyr::transmute(date, breakeven = value / 100) %>%
      daily_to_monthly("breakeven") %>%
      dplyr::transmute(date, country = "US", breakeven)
    saveRDS(us_breakeven, cache_paths$us_breakeven)
    message(sprintf("  US: %s to %s (%d months)",
      format(min(us_breakeven$date)), format(max(us_breakeven$date)),
      nrow(us_breakeven)))
  } else {
    warning("Failed to fetch US breakeven")
  }
}

# 2. Euro area breakeven: ECB SDMX API ----

if (file.exists(cache_paths$ea_breakeven)) {
  message("2. EA breakeven: cached")
} else {
  message("2. Fetching euro area breakeven from ECB...")
  ecb_nominal <- fetch_ecb("M.U2.EUR.4F.BB.U2_10Y.YLD")
  Sys.sleep(1)
  ecb_real <- fetch_ecb("M.U2.EUR.4F.BB.R_U2_10Y.YLDA")

  if (!is.null(ecb_nominal) && !is.null(ecb_real)) {
    ecb_nom <- ecb_nominal %>%
      dplyr::transmute(
        date = as.Date(paste0(TIME_PERIOD, "-01")),
        nominal = as.numeric(OBS_VALUE)
      ) %>%
      dplyr::filter(!is.na(nominal))

    ecb_re <- ecb_real %>%
      dplyr::transmute(
        date = as.Date(paste0(TIME_PERIOD, "-01")),
        real_yield = as.numeric(OBS_VALUE)
      ) %>%
      dplyr::filter(!is.na(real_yield))

    ea_breakeven <- ecb_nom %>%
      dplyr::inner_join(ecb_re, by = "date") %>%
      dplyr::transmute(date, country = "EA", breakeven = (nominal - real_yield) / 100)
    saveRDS(ea_breakeven, cache_paths$ea_breakeven)
    message(sprintf("  EA: %s to %s (%d months)",
      format(min(ea_breakeven$date)), format(max(ea_breakeven$date)),
      nrow(ea_breakeven)))
  } else {
    warning("Failed to fetch ECB breakeven")
  }
}

# 3. UK breakeven: Bank of England IADB ----

if (file.exists(cache_paths$uk_breakeven)) {
  message("3. GB breakeven: cached")
} else {
  message("3. Fetching UK 10Y breakeven from Bank of England (IUDMIZC)...")
  boe_url <- paste0(
    "https://www.bankofengland.co.uk/boeapps/database/",
    "_iadb-fromshowcolumns.asp?csv.x=yes",
    "&Datefrom=01/Jan/1985",
    "&Dateto=31/Dec/2026",
    "&SeriesCodes=IUDMIZC",
    "&CSVF=TN",
    "&UsingCodes=Y",
    "&VPD=Y",
    "&VFD=N"
  )

  boe_resp <- tryCatch(
    httr::GET(boe_url),
    error = function(e) {
      message("  BoE request failed: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(boe_resp) && httr::status_code(boe_resp) == 200) {
    boe_txt <- httr::content(boe_resp, as = "text", encoding = "UTF-8")
    boe_raw <- read.csv(text = boe_txt, stringsAsFactors = FALSE)

    uk_breakeven <- boe_raw %>%
      dplyr::transmute(
        date = as.Date(DATE, format = "%d %b %Y"),
        breakeven = as.numeric(IUDMIZC) / 100
      ) %>%
      dplyr::filter(!is.na(date), !is.na(breakeven)) %>%
      daily_to_monthly("breakeven") %>%
      dplyr::transmute(date, country = "GB", breakeven)
    saveRDS(uk_breakeven, cache_paths$uk_breakeven)
    message(sprintf("  GB: %s to %s (%d months)",
      format(min(uk_breakeven$date)), format(max(uk_breakeven$date)),
      nrow(uk_breakeven)))
  } else {
    warning("Failed to fetch BoE breakeven")
  }
}

# 4. Australia breakeven: RBA Table F2 ----

if (file.exists(cache_paths$au_breakeven)) {
  message("4. AU breakeven: cached")
} else {
  message("4. Fetching AU 10Y breakeven from RBA (Table F2)...")
  rba_url <- "https://www.rba.gov.au/statistics/tables/csv/f2-data.csv"

  rba_resp <- tryCatch(
    httr::GET(rba_url),
    error = function(e) {
      message("  RBA request failed: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(rba_resp) && httr::status_code(rba_resp) == 200) {
    rba_txt <- httr::content(rba_resp, as = "text", encoding = "UTF-8")
    rba_raw <- read.csv(text = rba_txt, skip = 10, header = TRUE,
      stringsAsFactors = FALSE, na.strings = c("", "NA"),
      check.names = FALSE)

    au_breakeven <- data.frame(
      date = as.Date(rba_raw[[1]], format = "%d-%b-%Y"),
      nominal_10y = as.numeric(rba_raw[[5]]),
      indexed_10y = as.numeric(rba_raw[[6]]),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::filter(!is.na(date), !is.na(nominal_10y), !is.na(indexed_10y)) %>%
      dplyr::mutate(breakeven = (nominal_10y - indexed_10y) / 100) %>%
      daily_to_monthly("breakeven") %>%
      dplyr::transmute(date, country = "AU", breakeven)
    saveRDS(au_breakeven, cache_paths$au_breakeven)
    message(sprintf("  AU: %s to %s (%d months)",
      format(min(au_breakeven$date)), format(max(au_breakeven$date)),
      nrow(au_breakeven)))
  } else {
    warning("Failed to fetch RBA breakeven")
  }
}

# 5. CPI data for regression countries ----

if (file.exists(cache_paths$cpi_panel)) {
  message("5. CPI panel: cached")
} else {
  message("5. Fetching CPI for regression countries...")
  cpi_list <- list()
  for (cc in names(cpi_regression_series)) {
    sid <- cpi_regression_series[[cc]]
    message("  ", cc, " -> ", sid)
    res <- fetch_fred(sid)
    if (!is.null(res)) {
      cpi_list[[cc]] <- res %>%
        dplyr::transmute(date, country = cc, cpi = value)
    }
    Sys.sleep(api_sleep)
  }

  # NZ: try monthly first, fall back to quarterly with step-interpolation
  message("  NZ -> NZLCPIALLMINMEI")
  nz_res <- fetch_fred("NZLCPIALLMINMEI")
  if (is.null(nz_res) || nrow(nz_res) == 0) {
    message("  NZ monthly unavailable, trying quarterly (NZLCPIALLQINMEI)...")
    nz_res <- fetch_fred("NZLCPIALLQINMEI")
    Sys.sleep(api_sleep)
  }
  if (!is.null(nz_res) && nrow(nz_res) > 0) {
    nz_cpi <- nz_res %>% dplyr::transmute(date, country = "NZ", cpi = value)
    nz_gaps <- as.numeric(diff(sort(nz_cpi$date)))
    if (length(nz_gaps) > 0 && median(nz_gaps) > 35) {
      message("  NZ CPI is quarterly -- step-interpolating to monthly...")
      nz_months <- tibble::tibble(
        date = seq.Date(min(nz_cpi$date), max(nz_cpi$date), by = "month")
      )
      nz_cpi <- nz_months %>%
        dplyr::left_join(nz_cpi, by = "date") %>%
        dplyr::mutate(
          country = "NZ",
          cpi = {
            idx <- which(!is.na(cpi))
            if (length(idx) > 0) cpi[idx[findInterval(seq_along(cpi), idx)]]
            else cpi
          }
        )
    }
    cpi_list[["NZ"]] <- nz_cpi
  } else {
    message("  NZ CPI unavailable")
  }

  # AU CPI: quarterly on FRED, step-interpolate to monthly (same as NZ)
  message("  AU -> ", au_cpi_series)
  au_cpi_res <- fetch_fred(au_cpi_series)
  Sys.sleep(api_sleep)
  if (!is.null(au_cpi_res) && nrow(au_cpi_res) > 0) {
    au_cpi <- au_cpi_res %>% dplyr::transmute(date, country = "AU", cpi = value)
    au_gaps <- as.numeric(diff(sort(au_cpi$date)))
    if (length(au_gaps) > 0 && median(au_gaps) > 35) {
      message("  AU CPI is quarterly -- step-interpolating to monthly...")
      au_months <- tibble::tibble(
        date = seq.Date(min(au_cpi$date), max(au_cpi$date), by = "month")
      )
      au_cpi <- au_months %>%
        dplyr::left_join(au_cpi, by = "date") %>%
        dplyr::mutate(
          country = "AU",
          cpi = {
            idx <- which(!is.na(cpi))
            if (length(idx) > 0) cpi[idx[findInterval(seq_along(cpi), idx)]]
            else cpi
          }
        )
    }
    cpi_list[["AU"]] <- au_cpi
  } else {
    message("  AU CPI unavailable")
  }

  cpi_panel <- dplyr::bind_rows(cpi_list)
  saveRDS(cpi_panel, cache_paths$cpi_panel)
  message(sprintf("  CPI panel: %d countries, %s to %s",
    dplyr::n_distinct(cpi_panel$country),
    format(min(cpi_panel$date)), format(max(cpi_panel$date))))
}

# Summary ----

message("\n=== Cached Breakeven Raw Data ===")
for (nm in names(cache_paths)) {
  path <- cache_paths[[nm]]
  if (file.exists(path)) {
    sz <- file.size(path)
    message(sprintf("  %-15s: %s (%.0f KB)", nm, path, sz / 1024))
  } else {
    message(sprintf("  %-15s: MISSING", nm))
  }
}
message("\nRun build_breakeven_inflation.R to compute regression estimates and final panel.")
message("Done.")
