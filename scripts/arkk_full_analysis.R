# ARKK Full Analysis Pipeline
# Combines: Beta Analysis, Factor Decomposition, Benchmark Comparison, and Attribution Analysis

# Script Params ----

roll_window <- 252L

# Complete ticker list (union of all scripts)
tickers <- c(
  "ARKK", # ARK Innovation ETF (target)
  "IWR",  # iShares Russell Midcap ETF (benchmark)
  "IWD",  # R1000V
  "IWF",  # R1000G
  "IWM",  # R2000 (only used in starter)
  "IWN",  # R2000V
  "IWO",  # R2000G
  "MTUM", # MSCI Momentum
  "USMV", # MSCI USA Min Vol
  "QUAL"  # MSCI USA Quality
)

factor_cols <- c("IWD", "IWF", "IWN", "IWO", "MTUM", "USMV", "QUAL")

# Load package functions
devtools::load_all()

# Data Pulls ----

message("========================================")
message("FETCHING PRICE DATA")
message("========================================")

all_data <- fetch_adjusted_prices(tickers)

# Data Pre-processing ----

message("\nPre-processing data...")

all_data <- all_data %>% 
  dplyr::arrange(ticker, date) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::add_count() %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n > roll_window) %>% 
  dplyr::select(-n)

return_data <- calculate_log_returns(all_data)

# Load visualization libraries
library(ggplot2)
library(ggrepel)
library(scales)

# Create images directory if needed
if (!dir.exists("images")) dir.create("images")

# ============================================================================
# ANALYSIS 1: ROLLING BETA (ARKK vs IWR)
# ============================================================================

message("\n========================================")
message("ANALYSIS 1: ROLLING BETA")
message("========================================")

market_returns <- return_data %>%
  dplyr::filter(ticker == "IWR") %>%
  dplyr::select(date, market_return = return)

return_data_with_market <- return_data %>%
  dplyr::left_join(market_returns, by = "date")

simple_regression <- return_data_with_market %>%
  dplyr::arrange(ticker, date) %>%
  dplyr::group_by(ticker) %>%
  dplyr::mutate(
    roll_res = list(roll::roll_lm(
      x = market_return,
      y = return,
      width = roll_window
    )),
    alpha = roll_res[[1]]$coefficients[, "(Intercept)"],
    beta = roll_res[[1]]$coefficients[, "x1"]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    -roll_res,
    -adjusted_close,
    -return,
    -market_return
  ) %>%
  dplyr::filter(!is.na(beta))

# Visualization 1: Beta Chart
p1 <- simple_regression %>%
  dplyr::filter(ticker == "ARKK") %>%
  ggplot(aes(x = date, y = beta)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(data = . %>% tail(1), color = "steelblue", size = 2) +
  geom_text_repel(
    data = . %>% tail(1),
    aes(label = round(beta, 2)),
    nudge_x = 30,
    direction = "y",
    vjust = 2,
    segment.color = NA
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    limits = c(0.8, 2.25),
    breaks = seq(0.5, 2.25, by = 0.25)
  ) + 
  labs(
    title = "Rolling Beta for $ARKK",
    subtitle = "vs. Russell Midcap Index ($IWR)",
    x = "",
    y = "Beta",
    caption = "Data: alphavantage • Chart: brrymtnc"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    plot.caption = element_text(size = 8, color = "grey40")
  )

ggsave("images/arkk_beta.svg", plot = p1, width = 8, height = 5, dpi = 320)
print(p1)
message("✓ Beta chart saved to images/arkk_beta.svg")

# ============================================================================
# ANALYSIS 2: FACTOR DECOMPOSITION (ARKK only)
# ============================================================================

message("\n========================================")
message("ANALYSIS 2: FACTOR DECOMPOSITION")
message("========================================")

# Prepare data for ARKK-only analysis
factor_returns_arkk <- return_data %>%
  dplyr::filter(ticker != "ARKK", ticker != "IWM") %>%
  dplyr::select(date, ticker, return) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = return)

arkk_returns_only <- return_data %>%
  dplyr::filter(ticker == "ARKK") %>%
  dplyr::select(date, arkk_return = return)

regression_data_arkk <- arkk_returns_only %>%
  dplyr::left_join(factor_returns_arkk, by = "date") %>%
  dplyr::filter(complete.cases(.))

# Rolling Multi-Factor Constrained Regression
message("Running ARKK factor decomposition...")

factor_decomposition <- regression_data_arkk %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    roll_res = list(roll_constrained_lm(
      x = as.matrix(dplyr::select(., dplyr::all_of(factor_cols))),
      y = arkk_return,
      width = roll_window,
      non_negative = TRUE,
      sum_to_one = TRUE,
      intercept = TRUE
    )),
    alpha = roll_res[[1]]$coefficients[, "(Intercept)"],
    large_value = roll_res[[1]]$coefficients[, "IWD"],
    large_growth = roll_res[[1]]$coefficients[, "IWF"],
    small_value = roll_res[[1]]$coefficients[, "IWN"],
    small_growth = roll_res[[1]]$coefficients[, "IWO"],
    momentum = roll_res[[1]]$coefficients[, "MTUM"],
    min_vol = roll_res[[1]]$coefficients[, "USMV"],
    quality = roll_res[[1]]$coefficients[, "QUAL"]
  ) %>%
  dplyr::select(
    -roll_res,
    -arkk_return,
    -dplyr::all_of(factor_cols)
  ) %>%
  dplyr::filter(!is.na(alpha))

# Visualization 2: Factor Decomposition
viz_data_decomp <- factor_decomposition %>%
  tidyr::pivot_longer(
    cols = c(large_value, large_growth, small_value, small_growth, 
            momentum, min_vol, quality),
    names_to = "factor",
    values_to = "weight"
  ) %>%
  dplyr::mutate(
    factor = dplyr::case_when(
      factor == "large_value"   ~ "Large Value",
      factor == "large_growth"  ~ "Large Growth",
      factor == "small_value"   ~ "Small Value",
      factor == "small_growth"  ~ "Small Growth",
      factor == "momentum"      ~ "Momentum",
      factor == "min_vol"       ~ "Min Vol",
      factor == "quality"       ~ "Quality",
      TRUE                      ~ factor
    ),
    factor = factor(factor, levels = c(
      "Large Value", "Large Growth", "Small Value", "Small Growth",
      "Momentum", "Min Vol", "Quality"
    ))
  ) %>%
  dplyr::arrange(date, factor)

p2 <- viz_data_decomp %>%
  ggplot(aes(x = date, y = weight, fill = factor)) +
  geom_area(position = position_stack(reverse = FALSE), 
            alpha = 0.85, 
            size = 0,
            na.rm = TRUE) +
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
  scale_fill_brewer(
    type = "qual", 
    palette = "Set3",
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "Rolling 1-Year Multi-Factor Decomposition of ARKK",
    subtitle = "Constrained weights (non-negative, sum to 1) - 7 Factor Model",
    x = "", y = "Weight",
    caption = "Data: alphavantage • Chart: brrymtnc"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    plot.caption = element_text(size = 8, color = "grey40"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0)
  )

ggsave("images/arkk_factor_decomposition.svg", plot = p2, width = 12, height = 6, dpi = 320)
print(p2)
message("✓ Factor decomposition chart saved to images/arkk_factor_decomposition.svg")

# ============================================================================
# ANALYSIS 3: BENCHMARK COMPARISON (ARKK vs IWR Factor Differences)
# ============================================================================

message("\n========================================")
message("ANALYSIS 3: BENCHMARK COMPARISON")
message("========================================")

# Prepare factor returns (exclude both ARKK and IWR)
factor_returns <- return_data %>%
  dplyr::filter(!ticker %in% c("ARKK", "IWR", "IWM")) %>%
  dplyr::select(date, ticker, return) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = return)

# Prepare ARKK and IWR returns
arkk_returns <- return_data %>%
  dplyr::filter(ticker == "ARKK") %>%
  dplyr::select(date, arkk_return = return)

iwr_returns <- return_data %>%
  dplyr::filter(ticker == "IWR") %>%
  dplyr::select(date, iwr_return = return)

# Create regression datasets
arkk_regression_data <- arkk_returns %>%
  dplyr::left_join(factor_returns, by = "date") %>%
  dplyr::filter(complete.cases(.))

iwr_regression_data <- iwr_returns %>%
  dplyr::left_join(factor_returns, by = "date") %>%
  dplyr::filter(complete.cases(.))

# Rolling Multi-Factor Constrained Regression - ARKK
message("Running ARKK constrained regression (with intercept)...")

arkk_decomposition <- arkk_regression_data %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    roll_res = list(roll_constrained_lm(
      x = as.matrix(dplyr::select(., dplyr::all_of(factor_cols))),
      y = arkk_return,
      width = roll_window,
      non_negative = TRUE,
      sum_to_one = TRUE,
      intercept = TRUE
    )),
    alpha = roll_res[[1]]$coefficients[, "(Intercept)"],
    large_value = roll_res[[1]]$coefficients[, "IWD"],
    large_growth = roll_res[[1]]$coefficients[, "IWF"],
    small_value = roll_res[[1]]$coefficients[, "IWN"],
    small_growth = roll_res[[1]]$coefficients[, "IWO"],
    momentum = roll_res[[1]]$coefficients[, "MTUM"],
    min_vol = roll_res[[1]]$coefficients[, "USMV"],
    quality = roll_res[[1]]$coefficients[, "QUAL"]
  ) %>%
  dplyr::select(
    date, alpha, large_value, large_growth, small_value, small_growth,
    momentum, min_vol, quality
  ) %>%
  dplyr::filter(!is.na(alpha))

# Rolling Multi-Factor Constrained Regression - IWR
message("Running IWR constrained regression (with intercept)...")

iwr_decomposition <- iwr_regression_data %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    roll_res = list(roll_constrained_lm(
      x = as.matrix(dplyr::select(., dplyr::all_of(factor_cols))),
      y = iwr_return,
      width = roll_window,
      non_negative = TRUE,
      sum_to_one = TRUE,
      intercept = TRUE
    )),
    alpha = roll_res[[1]]$coefficients[, "(Intercept)"],
    large_value = roll_res[[1]]$coefficients[, "IWD"],
    large_growth = roll_res[[1]]$coefficients[, "IWF"],
    small_value = roll_res[[1]]$coefficients[, "IWN"],
    small_growth = roll_res[[1]]$coefficients[, "IWO"],
    momentum = roll_res[[1]]$coefficients[, "MTUM"],
    min_vol = roll_res[[1]]$coefficients[, "USMV"],
    quality = roll_res[[1]]$coefficients[, "QUAL"]
  ) %>%
  dplyr::select(
    date, alpha, large_value, large_growth, small_value, small_growth,
    momentum, min_vol, quality
  ) %>%
  dplyr::filter(!is.na(alpha))

# Calculate Differences
factor_differences <- arkk_decomposition %>%
  dplyr::inner_join(
    iwr_decomposition,
    by = "date",
    suffix = c("_arkk", "_iwr")
  ) %>%
  dplyr::mutate(
    large_value_diff = large_value_arkk - large_value_iwr,
    large_growth_diff = large_growth_arkk - large_growth_iwr,
    small_value_diff = small_value_arkk - small_value_iwr,
    small_growth_diff = small_growth_arkk - small_growth_iwr,
    momentum_diff = momentum_arkk - momentum_iwr,
    min_vol_diff = min_vol_arkk - min_vol_iwr,
    quality_diff = quality_arkk - quality_iwr,
    alpha_diff = alpha_arkk - alpha_iwr
  ) %>%
  dplyr::select(
    date, large_value_diff, large_growth_diff, small_value_diff,
    small_growth_diff, momentum_diff, min_vol_diff, quality_diff, alpha_diff
  )

# Materiality Filter
materiality_threshold <- 0.05

max_differences <- factor_differences %>%
  dplyr::summarise(
    large_value_max = max(abs(large_value_diff), na.rm = TRUE),
    large_growth_max = max(abs(large_growth_diff), na.rm = TRUE),
    small_value_max = max(abs(small_value_diff), na.rm = TRUE),
    small_growth_max = max(abs(small_growth_diff), na.rm = TRUE),
    momentum_max = max(abs(momentum_diff), na.rm = TRUE),
    min_vol_max = max(abs(min_vol_diff), na.rm = TRUE),
    quality_max = max(abs(quality_diff), na.rm = TRUE)
  )

material_factors <- c()
if (max_differences$large_value_max >= materiality_threshold) material_factors <- c(material_factors, "large_value_diff")
if (max_differences$large_growth_max >= materiality_threshold) material_factors <- c(material_factors, "large_growth_diff")
if (max_differences$small_value_max >= materiality_threshold) material_factors <- c(material_factors, "small_value_diff")
if (max_differences$small_growth_max >= materiality_threshold) material_factors <- c(material_factors, "small_growth_diff")
if (max_differences$momentum_max >= materiality_threshold) material_factors <- c(material_factors, "momentum_diff")
if (max_differences$min_vol_max >= materiality_threshold) material_factors <- c(material_factors, "min_vol_diff")
if (max_differences$quality_max >= materiality_threshold) material_factors <- c(material_factors, "quality_diff")

message(paste0("Factors exceeding ", materiality_threshold * 100, "% materiality threshold: ", 
               paste(material_factors, collapse = ", ")))

# Visualization 3: Factor Differences
viz_data_diff <- factor_differences %>%
  tidyr::pivot_longer(
    cols = c(large_value_diff, large_growth_diff, small_value_diff,
            small_growth_diff, momentum_diff, min_vol_diff, quality_diff),
    names_to = "factor",
    values_to = "difference"
  ) %>%
  dplyr::filter(factor %in% material_factors) %>%
  dplyr::mutate(
    factor = dplyr::case_when(
      factor == "large_value_diff" ~ "LCV",
      factor == "large_growth_diff" ~ "LCG",
      factor == "small_value_diff" ~ "SCV",
      factor == "small_growth_diff" ~ "SCG",
      factor == "momentum_diff" ~ "Momo",
      factor == "min_vol_diff" ~ "Min Vol",
      factor == "quality_diff" ~ "Quality",
      TRUE ~ factor
    )
  )

factor_order <- viz_data_diff %>%
  dplyr::group_by(factor) %>%
  dplyr::summarise(mean_diff = mean(difference, na.rm = TRUE)) %>%
  dplyr::arrange(desc(mean_diff)) %>%
  dplyr::pull(factor)

viz_data_diff <- viz_data_diff %>%
  dplyr::mutate(factor = factor(factor, levels = factor_order))

factor_colors <- c(
  "LCV" = "#fc8d62",
  "LCG" = "#8da0cb",
  "SCV" = "#66c2a5",
  "SCG" = "#e78ac3",
  "Momo" = "#ffd92f",
  "Min Vol" = "#e5c494",
  "Quality" = "#b3b3b3"
)

p3 <- viz_data_diff %>%
  ggplot(aes(x = date, y = difference, fill = factor, color = factor)) +
  geom_col(width = 1, alpha = 0.85, linewidth = 0.2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  facet_grid(
    rows = vars(factor),
    scales = "fixed"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.5),
    labels = scales::percent_format(),
    expand = c(0, 0),
    oob = scales::squish
  ) +
  scale_fill_manual(
    values = factor_colors,
    guide = "none"
  ) +
  scale_color_manual(
    values = factor_colors,
    guide = "none"
  ) +
  labs(
    title = "ARKK vs Russell Midcap Index: Factor Loading Differences Over Time",
    subtitle = "Rolling 1-year constrained regression (ARKK loading - RMid loading)",
    x = "",
    y = "Loading Difference",
    caption = "Data: alphavantage • Chart: brrymtnc"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = "grey40", margin = margin(t = 10)),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 10, hjust = 0.5),
    strip.background = element_blank(),
    panel.spacing.y = unit(1.5, "lines"),
    axis.text.x = element_text(angle = 0)
  )

ggsave("images/arkk_iwr_factor_differences.svg", plot = p3, width = 12, height = 10, dpi = 320)
print(p3)
message("✓ Factor differences chart saved to images/arkk_iwr_factor_differences.svg")

# ============================================================================
# ANALYSIS 4: FACTOR ATTRIBUTION
# ============================================================================

message("\n========================================")
message("ANALYSIS 4: FACTOR ATTRIBUTION")
message("========================================")

# Run constrained regressions WITHOUT intercept for attribution
message("Running ARKK constrained regression (no intercept)...")

arkk_weights <- arkk_regression_data %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    roll_res = list(roll_constrained_lm(
      x = as.matrix(dplyr::select(., dplyr::all_of(factor_cols))),
      y = arkk_return,
      width = roll_window,
      non_negative = TRUE,
      sum_to_one = TRUE,
      intercept = FALSE
    )),
    IWD = roll_res[[1]]$coefficients[, "IWD"],
    IWF = roll_res[[1]]$coefficients[, "IWF"],
    IWN = roll_res[[1]]$coefficients[, "IWN"],
    IWO = roll_res[[1]]$coefficients[, "IWO"],
    MTUM = roll_res[[1]]$coefficients[, "MTUM"],
    USMV = roll_res[[1]]$coefficients[, "USMV"],
    QUAL = roll_res[[1]]$coefficients[, "QUAL"]
  ) %>%
  dplyr::select(date, IWD, IWF, IWN, IWO, MTUM, USMV, QUAL) %>%
  dplyr::filter(!is.na(IWD))

message("Running IWR constrained regression (no intercept)...")

iwr_weights <- iwr_regression_data %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    roll_res = list(roll_constrained_lm(
      x = as.matrix(dplyr::select(., dplyr::all_of(factor_cols))),
      y = iwr_return,
      width = roll_window,
      non_negative = TRUE,
      sum_to_one = TRUE,
      intercept = FALSE
    )),
    IWD = roll_res[[1]]$coefficients[, "IWD"],
    IWF = roll_res[[1]]$coefficients[, "IWF"],
    IWN = roll_res[[1]]$coefficients[, "IWN"],
    IWO = roll_res[[1]]$coefficients[, "IWO"],
    MTUM = roll_res[[1]]$coefficients[, "MTUM"],
    USMV = roll_res[[1]]$coefficients[, "USMV"],
    QUAL = roll_res[[1]]$coefficients[, "QUAL"]
  ) %>%
  dplyr::select(date, IWD, IWF, IWN, IWO, MTUM, USMV, QUAL) %>%
  dplyr::filter(!is.na(IWD))

# Prepare returns for attribution (need to rename columns)
arkk_returns_attr <- return_data %>%
  dplyr::filter(ticker == "ARKK") %>%
  dplyr::select(date, return)

iwr_returns_attr <- return_data %>%
  dplyr::filter(ticker == "IWR") %>%
  dplyr::select(date, return)

# Calculate Daily Attribution
message("Calculating factor attribution...")

daily_attribution <- calculate_factor_attribution(
  target_weights = arkk_weights,
  benchmark_weights = iwr_weights,
  factor_returns = factor_returns,
  target_returns = arkk_returns_attr,
  benchmark_returns = iwr_returns_attr,
  factor_cols = factor_cols
)

# Calculate Cumulative Attribution
message("Calculating cumulative attribution...")

cumulative_attribution <- calculate_cumulative_attribution(daily_attribution)

# Diagnostics
message(paste0("\nTotal observations in attribution: ", nrow(daily_attribution)))
message(paste0("Date range: ", min(daily_attribution$date), " to ", max(daily_attribution$date)))

# Check attribution identity
identity_check <- daily_attribution %>%
  dplyr::mutate(
    reconstructed = factor_contribution + selection_effect,
    difference = excess_return - reconstructed
  )

max_diff <- max(abs(identity_check$difference), na.rm = TRUE)
message(paste0("Attribution identity check (max absolute difference): ", format(max_diff, scientific = FALSE)))

if (max_diff > 1e-10) {
  message("WARNING: Attribution components do not sum to excess return within tolerance")
} else {
  message("✓ Attribution identity holds: factor + selection = excess")
}

# Final cumulative values
final_values <- cumulative_attribution %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::select(date, cumulative_excess, cumulative_factor, cumulative_selection)

message("\nFinal cumulative attribution:")
print(final_values)

# Visualization 4: Cumulative Attribution
viz_cumulative <- cumulative_attribution %>%
  tidyr::pivot_longer(
    cols = c(cumulative_factor, cumulative_selection),
    names_to = "component",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    component = dplyr::case_when(
      component == "cumulative_factor" ~ "Factor Contribution",
      component == "cumulative_selection" ~ "Selection Effect",
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
  scale_fill_manual(
    values = c(
      "Factor Contribution" = "#4575b4",
      "Selection Effect" = "#d73027"
    )
  ) +
  labs(
    title = "ARKK vs Russell Midcap Index: Cumulative Value-Add Attribution",
    subtitle = "Decomposing excess returns into factor tilts vs stock selection (rolling 1-year)",
    x = "",
    y = "Cumulative Value-Add",
    fill = "",
    caption = "Black line = Total excess return • Data: alphavantage • Chart: brrymtnc"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = "grey40", margin = margin(t = 10)),
    legend.position = "bottom"
  )

ggsave("images/arkk_iwr_cumulative_attribution.svg", plot = p4, width = 12, height = 7, dpi = 320)
print(p4)
message("✓ Cumulative attribution chart saved to images/arkk_iwr_cumulative_attribution.svg")

# ============================================================================
# SUMMARY
# ============================================================================

message("\n========================================")
message("ANALYSIS COMPLETE!")
message("========================================")
message("\nGenerated 4 visualizations:")
message("  1. images/arkk_beta.svg")
message("  2. images/arkk_factor_decomposition.svg")
message("  3. images/arkk_iwr_factor_differences.svg")
message("  4. images/arkk_iwr_cumulative_attribution.svg")
message("\n========================================\n")
