# Build Breakeven Inflation Panel
#
# Reads cached raw breakeven data (from fetch_breakeven_inflation.R) and
# produces a unified monthly panel for all 14 FI countries.
#
# Direct breakevens (US, EA, GB, AU) are used as-is.
# Eurozone countries (DE, FR, IT, NL, BE) get the EA aggregate.
# Regression countries (SE, CH, DK, NO, NZ, CA) are estimated via rolling
# 20-quarter OLS on annualized quarterly CPI changes — independent,
# non-overlapping observations in the same units as the breakeven.
#
# Requires: cached .rds files from fetch_breakeven_inflation.R
# Output: data/fred/breakeven_inflation.rds

# Config ----

library(dplyr)
library(slider)

data_dir <- "data/fred"

# Regression parameters
regression_window <- 20L    # Quarters (= 5 years of independent obs)

# Country groupings for regression
# US-referenced: eurozone (no free linker data), nordics, Japan, Canada
us_regression_countries <- c("DE", "FR", "IT", "NL", "BE", "SE", "CH", "DK", "NO", "JP", "CA")
# AU-referenced: NZ (tightly linked economy)
au_regression_countries <- c("NZ")
all_regression_countries <- c(us_regression_countries, au_regression_countries)

# Cache paths (must match fetch_breakeven_inflation.R)
cache_paths <- list(
  us_breakeven = file.path(data_dir, "be_raw_us.rds"),
  uk_breakeven = file.path(data_dir, "be_raw_boe.rds"),
  au_breakeven = file.path(data_dir, "be_raw_rba.rds"),
  cpi_panel    = file.path(data_dir, "be_raw_cpi.rds")
)

output_path <- file.path(data_dir, "breakeven_inflation.rds")

# 1. Load cached raw data ----

message("1. Loading cached breakeven data...")

missing <- names(cache_paths)[!vapply(cache_paths, file.exists, logical(1))]
if (length(missing) > 0) {
  stop("Missing cached data: ", paste(missing, collapse = ", "),
    "\nRun fetch_breakeven_inflation.R first.")
}

us_breakeven <- readRDS(cache_paths$us_breakeven)
uk_breakeven <- readRDS(cache_paths$uk_breakeven)
au_breakeven <- readRDS(cache_paths$au_breakeven)
cpi_panel    <- readRDS(cache_paths$cpi_panel)

message(sprintf("  US: %d months | GB: %d months | AU: %d months",
  nrow(us_breakeven), nrow(uk_breakeven), nrow(au_breakeven)))
message(sprintf("  CPI: %d countries, %s to %s",
  dplyr::n_distinct(cpi_panel$country),
  format(min(cpi_panel$date)), format(max(cpi_panel$date))))

# 2. Compute annualized quarterly CPI changes ----
# Non-overlapping (quarter-end only) for independent observations.
# Annualized by compounding: (1 + q_change)^4 - 1.
# Both Y and X in same units as breakevens (annualized %).

message("2. Computing annualized quarterly CPI changes...")

quarterly_inflation <- cpi_panel %>%
  dplyr::arrange(country, date) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    cpi_lag3 = dplyr::lag(cpi, 3),
    q_change = cpi / cpi_lag3 - 1,
    q_infl_ann = (1 + q_change)^4 - 1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(q_infl_ann)) %>%
  dplyr::filter(as.integer(format(date, "%m")) %in% c(3L, 6L, 9L, 12L)) %>%
  dplyr::select(date, country, q_infl_ann)

# 3. Rolling regressions ----
# country_ann_q_infl ~ ref_ann_q_infl
# Prediction: country_breakeven = alpha + beta * reference_breakeven
# (no rescaling — same units)

message("3. Running rolling regressions...")
message("  Window: ", regression_window, " quarters (", regression_window / 4, " years)")

run_regression_group <- function(country_codes, ref_code, ref_label) {
  message(sprintf("  %s-referenced: %s", ref_label, paste(country_codes, collapse = ", ")))

  ref_data <- quarterly_inflation %>%
    dplyr::filter(country == ref_code) %>%
    dplyr::select(date, ref_infl = q_infl_ann)

  country_data <- quarterly_inflation %>%
    dplyr::filter(country %in% country_codes) %>%
    dplyr::inner_join(ref_data, by = "date") %>%
    dplyr::arrange(country, date)

  if (nrow(country_data) == 0) {
    message("    No overlapping data -- skipping")
    return(tibble::tibble(
      date = as.Date(character()), country = character(),
      alpha = numeric(), beta = numeric()
    ))
  }

  country_data %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(
      coefs = slider::slide2(
        .x = q_infl_ann,
        .y = ref_infl,
        .f = function(y_vec, x_vec) {
          if (length(y_vec) < regression_window) {
            return(list(alpha = NA_real_, beta = NA_real_))
          }
          fit <- lm(y_vec ~ x_vec)
          list(alpha = unname(coef(fit)[1]), beta = unname(coef(fit)[2]))
        },
        .before = regression_window - 1L,
        .after = 0L,
        .complete = TRUE
      ),
      alpha = vapply(coefs, function(c) {
        if (is.null(c)) NA_real_ else c$alpha
      }, numeric(1)),
      beta = vapply(coefs, function(c) {
        if (is.null(c)) NA_real_ else c$beta
      }, numeric(1))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(alpha)) %>%
    dplyr::select(date, country, alpha, beta)
}

us_coefs <- run_regression_group(us_regression_countries, "US", "US")
au_coefs <- run_regression_group(au_regression_countries, "AU", "AU")
all_coefs <- dplyr::bind_rows(us_coefs, au_coefs)

# 4. Predict country breakevens ----
# Quarterly coefficients are forward-filled to monthly, then multiplied by the
# monthly reference breakeven.

message("4. Predicting country breakevens...")

expand_quarterly_to_monthly <- function(coefs_df, ref_be_df) {
  countries <- unique(coefs_df$country)
  results <- list()

  for (cc in countries) {
    cc_coefs <- coefs_df %>% dplyr::filter(country == cc)

    expanded <- ref_be_df %>%
      dplyr::left_join(cc_coefs %>% dplyr::select(date, alpha, beta), by = "date") %>%
      dplyr::arrange(date)

    alpha_idx <- which(!is.na(expanded$alpha))
    if (length(alpha_idx) > 0) {
      fill_pos <- findInterval(seq_len(nrow(expanded)), alpha_idx)
      fill_pos[fill_pos == 0] <- NA_integer_
      expanded$alpha <- expanded$alpha[alpha_idx[fill_pos]]
      expanded$beta <- expanded$beta[alpha_idx[fill_pos]]
    }

    results[[cc]] <- expanded %>%
      dplyr::filter(!is.na(alpha)) %>%
      dplyr::transmute(
        date, country = cc,
        breakeven = alpha + beta * ref_breakeven
      )
  }

  dplyr::bind_rows(results)
}

us_be_for_pred <- us_breakeven %>%
  dplyr::select(date, ref_breakeven = breakeven)

au_be_for_pred <- au_breakeven %>%
  dplyr::select(date, ref_breakeven = breakeven)

us_predicted <- expand_quarterly_to_monthly(us_coefs, us_be_for_pred)
au_predicted <- expand_quarterly_to_monthly(au_coefs, au_be_for_pred)

regression_breakeven <- dplyr::bind_rows(us_predicted, au_predicted)

for (cc in all_regression_countries) {
  sub <- regression_breakeven %>% dplyr::filter(country == cc)
  if (nrow(sub) > 0) {
    latest_beta <- all_coefs %>%
      dplyr::filter(country == cc) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::pull(beta)
    ref <- if (cc %in% au_regression_countries) "AU" else "US"
    message(sprintf("  %s: %s to %s (%d months, ref=%s, latest beta=%.2f)",
      cc, format(min(sub$date)), format(max(sub$date)),
      nrow(sub), ref, latest_beta))
  } else {
    message(sprintf("  %s: no estimates produced", cc))
  }
}

# 5. Combine all sources ----

message("5. Combining...")

all_breakeven <- dplyr::bind_rows(
  us_breakeven,
  uk_breakeven,
  au_breakeven,
  regression_breakeven
) %>%
  dplyr::arrange(country, date)

saveRDS(all_breakeven, output_path)

# 6. Summary ----

message("\n=== Breakeven Inflation Summary ===")

source_label <- function(cc) {
  if (cc == "US") return("FRED T10YIE")
  if (cc == "GB") return("BoE IUDMIZC")
  if (cc == "AU") return("RBA F2")
  if (cc %in% au_regression_countries) return("Regr (AU ref)")
  return("Regr (US ref)")
}

summary_df <- all_breakeven %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    start = min(date),
    end = max(date),
    n = dplyr::n(),
    mean_be = mean(breakeven, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(source = vapply(country, source_label, character(1))) %>%
  dplyr::arrange(country)

for (i in seq_len(nrow(summary_df))) {
  r <- summary_df[i, ]
  message(sprintf("  %-3s [%-15s]: %s to %s | %3d months | mean %.2f%%",
    r$country, r$source,
    format(r$start), format(r$end),
    r$n, r$mean_be * 100))
}

message(sprintf("\nTotal: %d country-months across %d countries",
  nrow(all_breakeven), dplyr::n_distinct(all_breakeven$country)))
message("Output: ", output_path)

# 7. Visualization ----

library(ggplot2)
library(scales)

message("\n7. Plotting breakeven inflation time series...")

# Tag each country with its source type
plot_data <- all_breakeven %>%
  dplyr::mutate(
    source = dplyr::case_when(
      country == "US" ~ "Direct (FRED)",
      country == "GB" ~ "Direct (BoE)",
      country == "AU" ~ "Direct (RBA)",
      country %in% au_regression_countries ~ "Regression (AU ref)",
      TRUE ~ "Regression (US ref)"
    ),
    source_type = dplyr::if_else(
      grepl("^Direct", source), "Direct", "Regression-estimated"
    )
  )

# Panel plot: one facet per country, colored by source type
p <- plot_data %>%
  ggplot(aes(x = date, y = breakeven, color = source_type)) +
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3, linetype = "dashed") +
  facet_wrap(~ country, scales = "free_y", ncol = 3) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(
    values = c("Direct" = "#08519c", "Regression-estimated" = "#d95f02")
  ) +
  labs(
    title = "10Y Breakeven Inflation by Country",
    subtitle = paste0(
      "Direct: US (TIPS), GB (BoE), AU (RBA) | ",
      "Regression: DE/FR/IT/NL/BE/SE/CH/DK/NO/JP/CA (US ref), NZ (AU ref)"
    ),
    x = "",
    y = "Breakeven Inflation (annualized)",
    color = "Source",
    caption = "Data: FRED, ECB, BoE, RBA | Regression: 20-quarter rolling OLS on annualized quarterly CPI"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 9, color = "grey30"),
    plot.caption = element_text(size = 7, color = "grey40"),
    legend.position = "bottom",
    axis.text.x = element_text(size = 8)
  )

print(p)

message("Done.")
