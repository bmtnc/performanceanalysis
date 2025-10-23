# Script Params ----

roll_window <- 756L # 3Y

tickers <- c(
  "ARKK", # ARK Innovation ETF (target)
  "IWV",  # iShares Russell 3000 ETF (benchmark)
  "IWD",  # R1000V
  "IWF",  # R1000G
  "IWN",  # R2000V
  "IWO",  # R2000G
  "MTUM", # MSCI Momentum
  "USMV", # MSCI USA Min Vol
  "QUAL", # MSCI USA Quality
  "IJR"   # Small Cap Quality
)

# Data Pulls ----

devtools::load_all()

all_data <- fetch_adjusted_prices(tickers)

# Data Pre-processing ----

all_data <- all_data %>%
  dplyr::arrange(ticker, date) %>%
  dplyr::group_by(ticker) %>%
  dplyr::add_count() %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > roll_window) %>%
  dplyr::select(-n)

return_data <- calculate_log_returns(all_data)

# Prepare factor returns (exclude both ARKK and IWV from factors)
factor_returns <- return_data %>%
  dplyr::filter(!ticker %in% c("ARKK", "IWV")) %>%
  dplyr::select(date, ticker, return) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = return)

# Prepare ARKK returns
arkk_returns <- return_data %>%
  dplyr::filter(ticker == "ARKK") %>%
  dplyr::select(date, return)

# Prepare IWV returns
iwv_returns <- return_data %>%
  dplyr::filter(ticker == "IWV") %>%
  dplyr::select(date, return)

# Create regression datasets
arkk_regression_data <- arkk_returns %>%
  dplyr::left_join(factor_returns, by = "date") %>%
  dplyr::filter(complete.cases(.))

iwv_regression_data <- iwv_returns %>%
  dplyr::left_join(factor_returns, by = "date") %>%
  dplyr::filter(complete.cases(.))

# Rolling Multi-Factor Constrained Regression - ARKK ----

factor_cols <- c("IWD", "IWF", "IWN", "IWO", "MTUM", "USMV", "QUAL", "IJR")

message("Running ARKK constrained regression (no intercept)...")

arkk_weights <- arkk_regression_data %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    roll_res = list(roll_constrained_lm(
      x = as.matrix(dplyr::select(., dplyr::all_of(factor_cols))),
      y = return,
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
    QUAL = roll_res[[1]]$coefficients[, "QUAL"],
    IJR = roll_res[[1]]$coefficients[, "IJR"]
  ) %>%
  dplyr::select(
    date,
    IWD,
    IWF,
    IWN,
    IWO,
    MTUM,
    USMV,
    QUAL,
    IJR
  ) %>%
  dplyr::filter(!is.na(IWD))

# Rolling Multi-Factor Constrained Regression - IWV ----

message("Running IWV constrained regression (no intercept)...")

iwv_weights <- iwv_regression_data %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    roll_res = list(roll_constrained_lm(
      x = as.matrix(dplyr::select(., dplyr::all_of(factor_cols))),
      y = return,
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
    QUAL = roll_res[[1]]$coefficients[, "QUAL"],
    IJR = roll_res[[1]]$coefficients[, "IJR"]
  ) %>%
  dplyr::select(
    date,
    IWD,
    IWF,
    IWN,
    IWO,
    MTUM,
    USMV,
    QUAL,
    IJR
  ) %>%
  dplyr::filter(!is.na(IWD))

# Calculate Daily Attribution ----

message("Calculating factor attribution...")

daily_attribution <- calculate_factor_attribution(
  target_weights = arkk_weights,
  benchmark_weights = iwv_weights,
  factor_returns = factor_returns,
  target_returns = arkk_returns,
  benchmark_returns = iwv_returns,
  factor_cols = factor_cols
)

# Calculate Cumulative Attribution ----

message("Calculating cumulative attribution...")

cumulative_attribution <- calculate_cumulative_attribution(daily_attribution)

# Diagnostics ----

message(paste0("\nTotal observations in attribution: ", nrow(daily_attribution)))
message(paste0("Date range: ", min(daily_attribution$date), " to ", max(daily_attribution$date)))

# Check attribution identity: excess_return ≈ factor_contribution + selection_effect
identity_check <- daily_attribution %>%
  dplyr::mutate(
    reconstructed = factor_contribution + selection_effect,
    difference = excess_return - reconstructed
  )

max_diff <- max(abs(identity_check$difference), na.rm = TRUE)
message(paste0("\nAttribution identity check (max absolute difference): ", format(max_diff, scientific = FALSE)))

if (max_diff > 1e-10) {
  message("WARNING: Attribution components do not sum to excess return within tolerance")
} else {
  message("✓ Attribution identity holds: factor + selection = excess")
}

# Summary statistics
summary_stats <- daily_attribution %>%
  dplyr::summarise(
    mean_excess = mean(excess_return, na.rm = TRUE),
    mean_factor = mean(factor_contribution, na.rm = TRUE),
    mean_selection = mean(selection_effect, na.rm = TRUE),
    sd_excess = sd(excess_return, na.rm = TRUE),
    sd_factor = sd(factor_contribution, na.rm = TRUE),
    sd_selection = sd(selection_effect, na.rm = TRUE)
  )

message("\nDaily attribution summary statistics:")
print(summary_stats)

# Final cumulative values
final_values <- cumulative_attribution %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::select(date, cumulative_excess, cumulative_factor, cumulative_selection)

message("\nFinal cumulative attribution:")
print(final_values)

# Dataviz ----

library(ggplot2)
library(scales)

# Chart 1: Cumulative Attribution Over Time
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

p1 <- viz_cumulative %>%
  ggplot(aes(x = date, y = value, fill = component)) +
  geom_area(alpha = 0.7, position = "stack") +
  geom_line(
    data = cumulative_attribution,
    aes(x = date, y = cumulative_excess, fill = NULL),
    color = "black",
    linewidth = 0.8
  ) +
  geom_point(
    data = final_point,
    aes(x = date, y = cumulative_excess, fill = NULL),
    color = "black",
    size = 2.5
  ) +
  annotate(
    "text",
    x = final_point$date + 150,
    y = final_point$cumulative_excess,
    label = scales::percent(final_point$cumulative_excess, accuracy = 0.1),
    color = "black",
    size = 3.5,
    hjust = 0
  ) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.3) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = c(0.01, 0.05)
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
    title = "ARKK vs IWV: Cumulative Value-Add Attribution",
    subtitle = "Decomposing excess returns into factor tilts vs stock selection (rolling 3-year)",
    x = "",
    y = "Cumulative Value-Add",
    fill = "Component",
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

print(p1)

# Save chart 1
if (!dir.exists("images")) dir.create("images")
ggsave(
  "images/arkk_iwv_cumulative_attribution.svg",
  plot = p1,
  width = 12,
  height = 7,
  dpi = 320
)

message("\n✓ Analysis complete! Charts saved to images/ directory")
