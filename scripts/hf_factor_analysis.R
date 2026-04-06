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
start_date <- NULL
end_date <- NULL

# Equity factor geography: "Global", "USA", "Europe", "Pacific", "Global Ex USA"
equity_geography <- "USA"

# Toggle asset class factor groups
use_fi_factors <- TRUE
use_fx_factors <- TRUE
use_eq_factors <- TRUE

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
      "No equity data for geography '", equity_geography, "'. ",
      "Valid values: ", paste(unique(eq_raw$geography), collapse = ", ")
    ))
  }
  # Momentum is in a separate file with different structure
  # Map geography to momentum column
  mom_col <- switch(equity_geography,
    "USA" = "us_large_cap",
    "Global" = , "Global Ex USA" = , "Europe" = , "Pacific" = "international",
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

if (length(factor_dfs) == 0) stop("No factor groups selected")

factor_data <- Reduce(
  function(x, y) dplyr::full_join(x, y, by = "date"),
  factor_dfs
)

factor_cols <- setdiff(names(factor_data), "date")
message(paste0(
  "Using ", length(factor_cols), " cross-asset factors: ",
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
  dplyr::mutate(dplyr::across(dplyr::all_of(factor_cols), ~ tidyr::replace_na(.x, 0))) %>%
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

# Visualization setup
library(ggplot2)
library(ggrepel)
library(scales)

# Factor display names and color palette
factor_labels <- c(
  fi_carry = "FI Carry",
  fi_value = "FI Value",
  fi_mom = "FI Momentum",
  fi_def = "FI Defensive",
  fx_carry = "FX Carry",
  fx_mom = "FX Momentum",
  fx_value = "FX Value",
  eq_hml = "EQ Value",
  eq_bab = "EQ Low Beta",
  eq_qmj = "EQ Quality",
  eq_mkt = "EQ Market",
  eq_smb = "EQ Size",
  eq_mom = "EQ Momentum"
)

factor_palette <- c(
  "FI Carry" = "#08519c",
  "FI Value" = "#2171b5",
  "FI Momentum" = "#4292c6",
  "FI Defensive" = "#6baed6",
  "FX Carry" = "#006d2c",
  "FX Momentum" = "#238b45",
  "FX Value" = "#41ab5d",
  "EQ Value" = "#a50f15",
  "EQ Low Beta" = "#cb181d",
  "EQ Quality" = "#ef3b2c",
  "EQ Market" = "#d4a017",
  "EQ Size" = "#fb6a4a",
  "EQ Momentum" = "#fc9272"
)

factor_order <- factor_labels[factor_cols]

# ============================================================================
# ANALYSIS 1: ROLLING FACTOR EXPOSURES (UNCONSTRAINED)
# ============================================================================

message("\n========================================")
message("ANALYSIS 1: ROLLING FACTOR EXPOSURES")
message("========================================")

x_mat <- as.matrix(regression_data[, factor_cols])

message(paste0(
  "Running unconstrained rolling regression (",
  roll_window_monthly, "-month window)..."
))

target_fit <- roll_constrained_lm(
  x = x_mat,
  y = regression_data$target_return,
  width = roll_window_monthly,
  non_negative = FALSE,
  sum_to_one = FALSE,
  intercept = TRUE
)

target_coefs <- as.data.frame(target_fit$coefficients)
names(target_coefs)[1] <- "alpha"

target_exposure <- tibble::as_tibble(target_coefs) %>%
  dplyr::mutate(date = regression_data$date) %>%
  dplyr::filter(!is.na(alpha))

# Summary stats
latest <- target_exposure %>% dplyr::slice_tail(n = 1)
message(paste0(
  "\nLatest exposures (", format(latest$date), "):"
))
for (fc in factor_cols) {
  message(sprintf("  %-12s: %+.3f", factor_labels[fc], latest[[fc]]))
}
message(sprintf("  %-12s: %+.4f (%.1f%% annualized)",
  "Alpha", latest$alpha, latest$alpha * 12 * 100
))
message(sprintf("  %-12s: %.3f",
  "Sum of betas", sum(latest[factor_cols])
))

# Visualization 1: Stacked bar of rolling factor exposures
viz_exposure <- target_exposure %>%
  dplyr::select(date, dplyr::all_of(factor_cols)) %>%
  tidyr::pivot_longer(-date, names_to = "factor", values_to = "exposure") %>%
  dplyr::mutate(
    factor_label = factor(factor_labels[factor], levels = rev(factor_order))
  )

p1 <- viz_exposure %>%
  ggplot(aes(x = date, y = exposure, fill = factor_label)) +
  geom_col(width = 25, alpha = 0.85, linewidth = 0) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = c(0.02, 0)
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1)
  ) +
  scale_fill_manual(
    values = factor_palette,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = paste0(
      "Rolling Factor Exposures: ", target_ticker
    ),
    subtitle = paste0(
      "Unconstrained regression -- ",
      length(factor_cols), "-factor model, ",
      roll_window_monthly, "-month window"
    ),
    x = "",
    y = "Factor Exposure (Beta)",
    caption = "Data: FRED, AQR, Alpha Vantage"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8, color = "grey40"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

print(p1)

# ============================================================================
# ANALYSIS 2: ROLLING ALPHA
# ============================================================================

message("\n========================================")
message("ANALYSIS 2: ROLLING ALPHA")
message("========================================")

alpha_data <- target_exposure %>%
  dplyr::select(date, alpha) %>%
  dplyr::mutate(alpha_ann = alpha * 12)

p2 <- alpha_data %>%
  ggplot(aes(x = date, y = alpha_ann)) +
  geom_col(
    aes(fill = alpha_ann > 0),
    width = 25,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  geom_point(
    data = . %>% dplyr::slice_tail(n = 1),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = . %>% dplyr::slice_tail(n = 1),
    aes(label = scales::percent(alpha_ann, accuracy = 0.1)),
    nudge_x = 30,
    direction = "y",
    segment.color = NA,
    size = 3.5
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("TRUE" = "#2171b5", "FALSE" = "#cb181d")) +
  labs(
    title = paste0("Rolling Alpha: ", target_ticker),
    subtitle = paste0(
      "Annualized intercept from ",
      roll_window_monthly, "-month unconstrained regression"
    ),
    x = "",
    y = "Alpha (annualized)",
    caption = "Data: FRED, AQR, Alpha Vantage"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8, color = "grey40")
  )

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
message(sprintf("  %-12s: %+.5f (%.2f%% annualized)",
  "Alpha", full_coefs["alpha"], full_coefs["alpha"] * 12 * 100
))
message(sprintf("  %-12s: %.4f", "R-squared", full_fit$r.squared))
message(sprintf("  %-12s: %.3f", "Sum of betas", sum(full_coefs[factor_cols])))

# Visualization 3: Full-sample coefficient bar chart
coef_df <- tibble::tibble(
  factor = factor_cols,
  factor_label = factor(factor_labels[factor_cols], levels = factor_order),
  exposure = full_coefs[factor_cols]
)

p3 <- coef_df %>%
  ggplot(aes(x = factor_label, y = exposure, fill = factor_label)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  geom_text(
    aes(
      label = sprintf("%+.3f", exposure),
      vjust = ifelse(exposure >= 0, -0.5, 1.5)
    ),
    size = 3
  ) +
  scale_fill_manual(values = factor_palette) +
  labs(
    title = paste0("Full-Sample Factor Exposures: ", target_ticker),
    subtitle = paste0(
      "Unconstrained regression | ",
      nrow(regression_data), " months | ",
      "R² = ", sprintf("%.1f%%", full_fit$r.squared * 100),
      " | Alpha = ", sprintf("%+.2f%%", full_coefs["alpha"] * 12 * 100), " ann."
    ),
    x = "",
    y = "Factor Exposure (Beta)",
    caption = "Data: FRED, AQR, Alpha Vantage"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )

print(p3)

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
  dplyr::mutate(dplyr::across(dplyr::all_of(factor_cols), ~ tidyr::replace_na(.x, 0)))

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

# Combine alpha + residual into "Idiosyncratic" for plotting
ctr_plot_labels <- c(
  idiosyncratic = "Idiosyncratic",
  setNames(paste0(factor_labels[factor_cols]), ctr_col_names)
)

ctr_palette <- c(
  "Idiosyncratic" = "Black",
  factor_palette
)

ctr_plot_order <- unname(ctr_plot_labels)

viz_ctr <- cumulative_ctr %>%
  dplyr::mutate(
    cumulative_idiosyncratic = cumulative_alpha_ctr + cumulative_residual
  ) %>%
  dplyr::select(
    date, cumulative_idiosyncratic,
    dplyr::all_of(paste0("cumulative_", ctr_col_names))
  ) %>%
  tidyr::pivot_longer(-date, names_to = "component", values_to = "value") %>%
  dplyr::mutate(
    component_name = gsub("^cumulative_", "", component),
    label = factor(ctr_plot_labels[component_name], levels = rev(ctr_plot_order))
  )

p4 <- viz_ctr %>%
  ggplot(aes(x = date, y = value, fill = label)) +
  geom_col(aes(color = label), width = 30, alpha = 0.8, linewidth = 0.25) +
  scale_color_manual(values = ctr_palette, guide = "none") +
  geom_line(
    data = cumulative_ctr,
    aes(x = date, y = cumulative_fund_return, fill = NULL),
    color = "black",
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  geom_point(
    data = cumulative_ctr %>% dplyr::slice_tail(n = 1),
    aes(x = date, y = cumulative_fund_return, fill = NULL),
    color = "black",
    size = 2.5,
    show.legend = FALSE
  ) +
  geom_text(
    data = cumulative_ctr %>% dplyr::slice_tail(n = 1),
    aes(
      x = date, y = cumulative_fund_return, fill = NULL,
      label = scales::percent(cumulative_fund_return, accuracy = 0.1)
    ),
    color = "black",
    size = 3.5,
    hjust = -0.15,
    show.legend = FALSE
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.07))
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = ctr_palette,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = paste0("Cumulative Contribution to Return: ", target_ticker),
    subtitle = paste0(
      "Out-of-sample decomposition (lagged betas) -- ",
      length(factor_cols), "-factor model, ",
      roll_window_monthly, "-month window"
    ),
    x = "",
    y = "Cumulative Return",
    caption = "Black line = Total cumulative return | Data: FRED, AQR, Alpha Vantage"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8, color = "grey40"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

print(p4)

# ============================================================================
# ANALYSIS 5: FACTOR EXPOSURE DISTRIBUTION
# ============================================================================

message("\n========================================")
message("ANALYSIS 5: FACTOR EXPOSURE DISTRIBUTION")
message("========================================")

viz_box <- target_exposure %>%
  dplyr::select(date, dplyr::all_of(factor_cols)) %>%
  tidyr::pivot_longer(-date, names_to = "factor", values_to = "beta") %>%
  dplyr::mutate(
    factor_label = factor(factor_labels[factor], levels = factor_order)
  )

latest_betas <- viz_box %>%
  dplyr::group_by(factor_label) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::ungroup()

p5 <- viz_box %>%
  ggplot(aes(x = factor_label, y = beta)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  geom_boxplot(
    aes(fill = factor_label),
    alpha = 0.6,
    outlier.size = 0.8,
    outlier.alpha = 0.4,
    show.legend = FALSE
  ) +
  geom_point(
    data = latest_betas,
    color = "#d4a017",
    fill = "#d4a017",
    shape = 23,
    size = 3.5,
    stroke = 0.8
  ) +
  scale_fill_manual(values = factor_palette) +
  labs(
    title = paste0("Factor Exposure Distribution: ", target_ticker),
    subtitle = paste0(
      "Rolling ", roll_window_monthly, "-month betas | ",
      "Gold diamond = latest (", format(max(target_exposure$date), "%b %Y"), ")"
    ),
    x = "",
    y = "Factor Exposure (Beta)",
    caption = "Data: FRED, AQR, Alpha Vantage"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )

print(p5)

# ============================================================================
# SUMMARY
# ============================================================================

message("\n========================================")
message("ANALYSIS COMPLETE!")
message("========================================\n")
