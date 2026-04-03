# Full Analysis Pipeline V2 — Cross-Asset Factor Premia
# Analyzes fund returns against curated FI, FX, and Equity factor premia
#
# Key differences from v1:
# - Monthly frequency (factor premia are monthly)
# - Constrained regression (non-negative, sum-to-one) on cross-asset premia
# - 12 cross-asset factors: FI (4), FX (3), Equity (5)
# - Factors from cached AQR/FRED data, not live ETF prices

# Script Params ----

target_ticker <- "PSLDX"
benchmark_ticker <- "SPY"

roll_window_daily <- 252L   # rolling beta (daily)
roll_window_monthly <- 60L  # factor regressions (monthly, ~5 years)
save_images <- TRUE

# Date range filtering (NULL = use all available data)
start_date <- NULL
end_date <- NULL

# Equity factor geography
equity_geography <- "USA"

# Toggle asset class factor groups
use_fi_factors <- TRUE
use_fx_factors <- TRUE
use_eq_factors <- TRUE

target_lower <- tolower(target_ticker)
benchmark_lower <- tolower(benchmark_ticker)

# Load package functions
devtools::load_all()

# Data Pulls ----

message("========================================")
message("FETCHING DATA")
message("========================================")

tickers <- c(target_ticker, benchmark_ticker)
all_data <- fetch_adjusted_prices(tickers)

fi_raw <- readRDS("data/fred/fi_factor_returns.rds")
fx_raw <- readRDS("data/fred/fx_factor_returns.rds")
eq_raw <- readRDS("data/aqr/aqr_equity_factors.rds")

# Data Pre-processing ----

message("\nPre-processing data...")

all_data <- all_data %>%
  dplyr::arrange(ticker, date) %>%
  dplyr::group_by(ticker) %>%
  dplyr::add_count() %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > roll_window_daily) %>%
  dplyr::select(-n)

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
  factor_dfs[["eq"]] <- eq_raw %>%
    dplyr::filter(geography == equity_geography) %>%
    dplyr::mutate(date = lubridate::floor_date(date, "month")) %>%
    dplyr::select(
      date,
      eq_hml = hml,
      eq_bab = bab,
      eq_qmj = qmj,
      eq_mkt = mkt,
      eq_smb = smb
    )
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

# Build regression dataset (inner join keeps only complete months)
target_monthly <- monthly_returns %>%
  dplyr::filter(ticker == target_ticker) %>%
  dplyr::select(date, target_return = return)

benchmark_monthly <- monthly_returns %>%
  dplyr::filter(ticker == benchmark_ticker) %>%
  dplyr::select(date, benchmark_return = return)

regression_data <- target_monthly %>%
  dplyr::inner_join(benchmark_monthly, by = "date") %>%
  dplyr::inner_join(factor_data, by = "date") %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::arrange(date)

# Apply date filtering
if (!is.null(start_date)) {
  start_dt <- lubridate::ymd(start_date)
  regression_data <- regression_data %>% dplyr::filter(date >= start_dt)
  daily_returns <- daily_returns %>% dplyr::filter(date >= start_dt)
  message(paste0("Filtering: start_date >= ", start_date))
}
if (!is.null(end_date)) {
  end_dt <- lubridate::ymd(end_date)
  regression_data <- regression_data %>% dplyr::filter(date <= end_dt)
  daily_returns <- daily_returns %>% dplyr::filter(date <= end_dt)
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

if (save_images && !dir.exists("images")) {
  dir.create("images")
}

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
  eq_smb = "EQ Size"
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
  "EQ Market" = "#525252",
  "EQ Size" = "#fb6a4a"
)

factor_order <- factor_labels[factor_cols]

# ============================================================================
# ANALYSIS 1: ROLLING BETA
# ============================================================================

message("\n========================================")
message("ANALYSIS 1: ROLLING BETA")
message("========================================")

market_returns <- daily_returns %>%
  dplyr::filter(ticker == benchmark_ticker) %>%
  dplyr::select(date, market_return = return)

target_daily <- daily_returns %>%
  dplyr::filter(ticker == target_ticker) %>%
  dplyr::left_join(market_returns, by = "date") %>%
  dplyr::filter(!is.na(market_return)) %>%
  dplyr::arrange(date)

daily_roll <- roll::roll_lm(
  x = target_daily$market_return,
  y = target_daily$return,
  width = roll_window_daily
)

beta_data <- target_daily %>%
  dplyr::mutate(
    alpha = daily_roll$coefficients[, 1],
    beta = daily_roll$coefficients[, 2]
  ) %>%
  dplyr::filter(!is.na(beta))

p1 <- beta_data %>%
  ggplot(aes(x = date, y = beta)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(
    data = . %>% tail(1),
    color = "steelblue",
    size = 2
  ) +
  geom_text_repel(
    data = . %>% tail(1),
    aes(label = round(beta, 2)),
    nudge_x = 30,
    direction = "y",
    vjust = 2,
    segment.color = NA
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = paste0("Rolling Beta for $", target_ticker),
    subtitle = paste0(
      "vs. ", benchmark_ticker, " (", roll_window_daily, "-day window)"
    ),
    x = "",
    y = "Beta",
    caption = "Data: Alpha Vantage • Chart: brrymtnc"
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

beta_file <- paste0("images/", target_lower, "_v2_beta.svg")
if (save_images) {
  ggsave(beta_file, plot = p1, width = 8, height = 5, dpi = 320)
  message(paste0("Saved: ", beta_file))
}
print(p1)

# ============================================================================
# ANALYSIS 2: CROSS-ASSET FACTOR DECOMPOSITION
# ============================================================================

message("\n========================================")
message("ANALYSIS 2: CROSS-ASSET FACTOR DECOMPOSITION")
message("========================================")

x_mat <- as.matrix(regression_data[, factor_cols])

# Target: constrained rolling regression (with intercept for alpha)
message(paste0("Running ", target_ticker, " constrained regression (with intercept)..."))

target_fit <- roll_constrained_lm(
  x = x_mat,
  y = regression_data$target_return,
  width = roll_window_monthly,
  non_negative = TRUE,
  sum_to_one = TRUE,
  intercept = TRUE
)

target_coefs <- as.data.frame(target_fit$coefficients)
names(target_coefs)[1] <- "alpha"

target_exposure <- tibble::as_tibble(target_coefs) %>%
  dplyr::mutate(date = regression_data$date) %>%
  dplyr::filter(!is.na(alpha))

# Visualization 2: Stacked area of factor weights
viz_decomp <- target_exposure %>%
  dplyr::select(date, dplyr::all_of(factor_cols)) %>%
  tidyr::pivot_longer(-date, names_to = "factor", values_to = "weight") %>%
  dplyr::mutate(
    factor_label = factor(factor_labels[factor], levels = rev(factor_order))
  )

p2 <- viz_decomp %>%
  ggplot(aes(x = date, y = weight, fill = factor_label)) +
  geom_area(
    position = position_stack(reverse = FALSE),
    alpha = 0.85,
    linewidth = 0
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    labels = scales::percent_format(),
    expand = c(0, 0),
    oob = scales::squish
  ) +
  scale_fill_manual(
    values = factor_palette,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = paste0(
      "Rolling Cross-Asset Factor Decomposition of ", target_ticker
    ),
    subtitle = paste0(
      "Constrained weights (non-negative, sum to 1) — ",
      length(factor_cols), "-factor model, ",
      roll_window_monthly, "-month window"
    ),
    x = "",
    y = "Weight",
    caption = "Data: FRED, AQR, Alpha Vantage • Chart: brrymtnc"
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

decomp_file <- paste0("images/", target_lower, "_v2_factor_decomposition.svg")
if (save_images) {
  ggsave(decomp_file, plot = p2, width = 12, height = 6, dpi = 320)
  message(paste0("Saved: ", decomp_file))
}
print(p2)

# ============================================================================
# ANALYSIS 3: BENCHMARK FACTOR COMPARISON
# ============================================================================

message("\n========================================")
message("ANALYSIS 3: BENCHMARK FACTOR COMPARISON")
message("========================================")

# Benchmark: constrained rolling regression (with intercept)
message(paste0("Running ", benchmark_ticker, " constrained regression (with intercept)..."))

benchmark_fit <- roll_constrained_lm(
  x = x_mat,
  y = regression_data$benchmark_return,
  width = roll_window_monthly,
  non_negative = TRUE,
  sum_to_one = TRUE,
  intercept = TRUE
)

benchmark_coefs <- as.data.frame(benchmark_fit$coefficients)
names(benchmark_coefs)[1] <- "alpha"

benchmark_exposure <- tibble::as_tibble(benchmark_coefs) %>%
  dplyr::mutate(date = regression_data$date) %>%
  dplyr::filter(!is.na(alpha))

# Weight differences (target - benchmark)
common_dates <- dplyr::inner_join(
  target_exposure %>% dplyr::select(date, dplyr::all_of(factor_cols)),
  benchmark_exposure %>% dplyr::select(date, dplyr::all_of(factor_cols)),
  by = "date",
  suffix = c("_target", "_benchmark")
)

diff_data <- tibble::tibble(date = common_dates$date)
diff_data[factor_cols] <- common_dates[paste0(factor_cols, "_target")] -
  common_dates[paste0(factor_cols, "_benchmark")]

viz_diffs <- diff_data %>%
  tidyr::pivot_longer(-date, names_to = "factor", values_to = "difference") %>%
  dplyr::mutate(
    factor_label = factor(factor_labels[factor], levels = factor_order)
  )

p3 <- viz_diffs %>%
  ggplot(aes(
    x = date,
    y = difference,
    fill = factor_label,
    color = factor_label
  )) +
  geom_col(width = 25, alpha = 0.85, linewidth = 0.2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  facet_wrap(~ factor_label, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = factor_palette, guide = "none") +
  scale_color_manual(values = factor_palette, guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = paste0(
      target_ticker, " vs ", benchmark_ticker,
      ": Cross-Asset Factor Exposure Differences"
    ),
    subtitle = paste0(
      "Rolling ", roll_window_monthly, "-month constrained weights (",
      target_ticker, " minus ", benchmark_ticker, ")"
    ),
    x = "",
    y = "Weight Difference",
    caption = "Data: FRED, AQR, Alpha Vantage • Chart: brrymtnc"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8, color = "grey40"),
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_blank()
  )

diff_file <- paste0(
  "images/", target_lower, "_", benchmark_lower,
  "_v2_factor_diffs.svg"
)
if (save_images) {
  ggsave(diff_file, plot = p3, width = 14, height = 8, dpi = 320)
  message(paste0("Saved: ", diff_file))
}
print(p3)

# ============================================================================
# ANALYSIS 4: FACTOR ATTRIBUTION
# ============================================================================

message("\n========================================")
message("ANALYSIS 4: FACTOR ATTRIBUTION")
message("========================================")

# Run constrained regressions WITHOUT intercept for attribution
# (unexplained portion becomes the selection effect)
message(paste0("Running ", target_ticker, " constrained regression (no intercept)..."))

target_attr_fit <- roll_constrained_lm(
  x = x_mat,
  y = regression_data$target_return,
  width = roll_window_monthly,
  non_negative = TRUE,
  sum_to_one = TRUE,
  intercept = FALSE
)

target_attr_coefs <- as.data.frame(target_attr_fit$coefficients)
target_weights_attr <- tibble::as_tibble(target_attr_coefs) %>%
  dplyr::mutate(date = regression_data$date) %>%
  dplyr::filter(!is.na(.data[[factor_cols[1]]]))

message(paste0("Running ", benchmark_ticker, " constrained regression (no intercept)..."))

benchmark_attr_fit <- roll_constrained_lm(
  x = x_mat,
  y = regression_data$benchmark_return,
  width = roll_window_monthly,
  non_negative = TRUE,
  sum_to_one = TRUE,
  intercept = FALSE
)

benchmark_attr_coefs <- as.data.frame(benchmark_attr_fit$coefficients)
benchmark_weights_attr <- tibble::as_tibble(benchmark_attr_coefs) %>%
  dplyr::mutate(date = regression_data$date) %>%
  dplyr::filter(!is.na(.data[[factor_cols[1]]]))

target_returns_attr <- monthly_returns %>%
  dplyr::filter(ticker == target_ticker) %>%
  dplyr::select(date, return)

benchmark_returns_attr <- monthly_returns %>%
  dplyr::filter(ticker == benchmark_ticker) %>%
  dplyr::select(date, return)

message("Calculating factor attribution...")

daily_attribution <- calculate_factor_attribution(
  target_weights = target_weights_attr,
  benchmark_weights = benchmark_weights_attr,
  factor_returns = factor_data,
  target_returns = target_returns_attr,
  benchmark_returns = benchmark_returns_attr,
  factor_cols = factor_cols
)

message("Calculating cumulative attribution (Carino linking)...")
cumulative_attribution <- calculate_cumulative_attribution(daily_attribution)

# Diagnostics
message(paste0(
  "\nAttribution observations: ", nrow(daily_attribution)
))
message(paste0(
  "Date range: ",
  min(daily_attribution$date), " to ", max(daily_attribution$date)
))

identity_check <- daily_attribution %>%
  dplyr::mutate(
    reconstructed = factor_contribution + selection_effect,
    difference = excess_return - reconstructed
  )

max_diff <- max(abs(identity_check$difference), na.rm = TRUE)
message(paste0(
  "Attribution identity check (max |diff|): ",
  format(max_diff, scientific = FALSE)
))

if (max_diff > 1e-10) {
  message(
    "WARNING: Attribution components do not sum to excess return within tolerance"
  )
} else {
  message("Attribution identity holds: factor + selection = excess")
}

final_values <- cumulative_attribution %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::select(date, cumulative_excess, cumulative_factor, cumulative_selection)
message("\nFinal cumulative attribution:")
print(final_values)

# Visualization 4: Cumulative attribution
viz_cumulative <- cumulative_attribution %>%
  tidyr::pivot_longer(
    cols = c(cumulative_factor, cumulative_selection),
    names_to = "component",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    component = dplyr::case_when(
      component == "cumulative_factor" ~ "Factor Tilt Effects",
      component == "cumulative_selection" ~ "Idiosyncratic Effects",
      TRUE ~ component
    )
  )

final_point <- cumulative_attribution %>%
  dplyr::slice_tail(n = 1)

p4 <- viz_cumulative %>%
  ggplot(aes(x = date, y = value, fill = component)) +
  geom_area(alpha = 0.7, position = "stack") +
  geom_line(
    data = cumulative_attribution,
    aes(x = date, y = cumulative_excess, fill = NULL),
    color = "black",
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  geom_point(
    data = final_point,
    aes(x = date, y = cumulative_excess, fill = NULL),
    color = "black",
    size = 2,
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = final_point$date + 30,
    y = final_point$cumulative_excess,
    label = scales::percent(final_point$cumulative_excess, accuracy = 0.1),
    color = "black",
    size = 3,
    hjust = 0
  ) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.3) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = c(0.05, 0.3)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = c(0.02, 0.02)
  ) +
  scale_fill_manual(values = c(
    "Factor Tilt Effects" = "#4575b4",
    "Idiosyncratic Effects" = "#d73027"
  )) +
  labs(
    title = paste0(
      target_ticker, " vs ", benchmark_ticker,
      ": Cumulative Value-Add Attribution"
    ),
    subtitle = "Cross-asset factor model: factor tilt effects vs idiosyncratic effects",
    x = "",
    y = "Cumulative Value-Add",
    fill = "",
    caption = paste0(
      "Black line = Total excess return",
      " • Data: FRED, AQR, Alpha Vantage • Chart: brrymtnc"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    plot.caption = element_text(
      size = 8,
      color = "grey40",
      margin = margin(t = 10)
    ),
    legend.position = "bottom"
  )

attr_file <- paste0(
  "images/", target_lower, "_", benchmark_lower,
  "_v2_attribution.svg"
)
if (save_images) {
  ggsave(attr_file, plot = p4, width = 12, height = 7, dpi = 320)
  message(paste0("Saved: ", attr_file))
}
print(p4)

# ============================================================================
# SUMMARY
# ============================================================================

message("\n========================================")
message("ANALYSIS COMPLETE!")
message("========================================")
if (save_images) {
  message("\nGenerated visualizations:")
  message(paste0("  1. ", beta_file))
  message(paste0("  2. ", decomp_file))
  message(paste0("  3. ", diff_file))
  message(paste0("  4. ", attr_file))
}
message("\n========================================\n")
