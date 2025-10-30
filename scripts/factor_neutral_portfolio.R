# Factor-Neutral Portfolio Analysis
# Creates a portfolio that is 100% long target but neutralizes factor tilts
# relative to benchmark by taking offsetting positions in factor ETFs

# Script Params ----

# CONFIGURE TICKERS HERE
target_ticker <- "ARKK"
benchmark_ticker <- "IWR"

roll_window <- 252L
save_images <- FALSE
max_gross_exposure <- 2.0  # 200% cap

# Date range filtering (set to NULL to use all available data)
start_date <- "2016-01-01"  # e.g., "2020-01-01"
end_date <- NULL            # e.g., "2024-12-31"

# Complete ticker list (7-factor model)
tickers <- c(
  target_ticker,    # Target fund
  benchmark_ticker, # Benchmark
  "IWD",            # R1000V
  "IWF",            # R1000G
  "IWN",            # R2000V
  "IWO",            # R2000G
  "MTUM",           # Momo
  "USMV",           # MinVol
  "QUAL"            # Quality
)

factor_cols <- c("IWD", "IWF", "IWN", "IWO", "MTUM", "USMV", "QUAL")

# Generate lowercase versions for file naming
target_lower <- tolower(target_ticker)
benchmark_lower <- tolower(benchmark_ticker)

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

# Apply date filtering if specified
if (!is.null(start_date) || !is.null(end_date)) {
  if (!is.null(start_date)) {
    start_date_parsed <- lubridate::ymd(start_date)
    return_data <- return_data %>% dplyr::filter(date >= start_date_parsed)
    message(paste0("Filtering data: start_date >= ", start_date))
  }
  if (!is.null(end_date)) {
    end_date_parsed <- lubridate::ymd(end_date)
    return_data <- return_data %>% dplyr::filter(date <= end_date_parsed)
    message(paste0("Filtering data: end_date <= ", end_date))
  }
}

# Prepare factor returns (exclude both target and benchmark from factors)
factor_returns <- return_data %>%
  dplyr::filter(!ticker %in% c(target_ticker, benchmark_ticker)) %>%
  dplyr::select(date, ticker, return) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = return)

# Prepare target returns
target_returns <- return_data %>%
  dplyr::filter(ticker == target_ticker) %>%
  dplyr::select(date, return)

# Prepare benchmark returns
benchmark_returns <- return_data %>%
  dplyr::filter(ticker == benchmark_ticker) %>%
  dplyr::select(date, return)

# Create regression datasets
target_regression_data <- target_returns %>%
  dplyr::left_join(factor_returns, by = "date") %>%
  dplyr::filter(complete.cases(.))

benchmark_regression_data <- benchmark_returns %>%
  dplyr::left_join(factor_returns, by = "date") %>%
  dplyr::filter(complete.cases(.))

# Rolling Multi-Factor Constrained Regression - Target ----

message(paste0("Running ", target_ticker, " constrained regression (no intercept)..."))

target_weights <- target_regression_data %>%
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
    QUAL = roll_res[[1]]$coefficients[, "QUAL"]
  ) %>%
  dplyr::select(date, IWD, IWF, IWN, IWO, MTUM, USMV, QUAL) %>%
  dplyr::filter(!is.na(IWD))

# Rolling Multi-Factor Constrained Regression - Benchmark ----

message(paste0("Running ", benchmark_ticker, " constrained regression (no intercept)..."))

benchmark_weights <- benchmark_regression_data %>%
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
    QUAL = roll_res[[1]]$coefficients[, "QUAL"]
  ) %>%
  dplyr::select(date, IWD, IWF, IWN, IWO, MTUM, USMV, QUAL) %>%
  dplyr::filter(!is.na(IWD))

# Calculate Hedge Weights ----

message("Calculating factor hedge weights...")

hedge_weights <- target_weights %>%
  dplyr::inner_join(benchmark_weights, by = "date", suffix = c("_target", "_benchmark")) %>%
  dplyr::mutate(
    hedge_IWD = IWD_benchmark - IWD_target,
    hedge_IWF = IWF_benchmark - IWF_target,
    hedge_IWN = IWN_benchmark - IWN_target,
    hedge_IWO = IWO_benchmark - IWO_target,
    hedge_MTUM = MTUM_benchmark - MTUM_target,
    hedge_USMV = USMV_benchmark - USMV_target,
    hedge_QUAL = QUAL_benchmark - QUAL_target
  ) %>%
  dplyr::select(
    date,
    hedge_IWD, hedge_IWF, hedge_IWN, hedge_IWO,
    hedge_MTUM, hedge_USMV, hedge_QUAL
  )

# Calculate gross exposure and apply cap
hedge_weights <- hedge_weights %>%
  dplyr::mutate(
    gross_exposure_uncapped = 1.0 + abs(hedge_IWD) + abs(hedge_IWF) + abs(hedge_IWN) +
      abs(hedge_IWO) + abs(hedge_MTUM) + abs(hedge_USMV) + abs(hedge_QUAL),
    scale_factor = dplyr::if_else(
      gross_exposure_uncapped > max_gross_exposure,
      (max_gross_exposure - 1.0) / (gross_exposure_uncapped - 1.0),
      1.0
    ),
    hedge_IWD = hedge_IWD * scale_factor,
    hedge_IWF = hedge_IWF * scale_factor,
    hedge_IWN = hedge_IWN * scale_factor,
    hedge_IWO = hedge_IWO * scale_factor,
    hedge_MTUM = hedge_MTUM * scale_factor,
    hedge_USMV = hedge_USMV * scale_factor,
    hedge_QUAL = hedge_QUAL * scale_factor,
    gross_exposure = 1.0 + abs(hedge_IWD) + abs(hedge_IWF) + abs(hedge_IWN) +
      abs(hedge_IWO) + abs(hedge_MTUM) + abs(hedge_USMV) + abs(hedge_QUAL),
    net_exposure = 1.0 + hedge_IWD + hedge_IWF + hedge_IWN + hedge_IWO +
      hedge_MTUM + hedge_USMV + hedge_QUAL
  ) %>%
  dplyr::select(-gross_exposure_uncapped, -scale_factor)

# Calculate Neutral Portfolio Returns ----

message("Calculating factor-neutral portfolio returns...")

neutral_returns <- hedge_weights %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(future_date = dplyr::lead(date, 1)) %>%
  dplyr::filter(!is.na(future_date)) %>%
  dplyr::inner_join(
    factor_returns %>% dplyr::rename(future_date = date),
    by = "future_date"
  ) %>%
  dplyr::inner_join(
    target_returns %>% dplyr::rename(future_date = date, target_return = return),
    by = "future_date"
  ) %>%
  dplyr::inner_join(
    benchmark_returns %>% dplyr::rename(future_date = date, benchmark_return = return),
    by = "future_date"
  ) %>%
  dplyr::mutate(
    hedge_return = hedge_IWD * IWD + hedge_IWF * IWF + hedge_IWN * IWN +
      hedge_IWO * IWO + hedge_MTUM * MTUM + hedge_USMV * USMV +
      hedge_QUAL * QUAL,
    neutral_return = target_return + hedge_return
  ) %>%
  dplyr::select(
    date = future_date,
    target_return,
    benchmark_return,
    neutral_return,
    hedge_return,
    gross_exposure,
    net_exposure
  )

# Calculate Cumulative Returns ----

message("Calculating cumulative returns...")

cumulative_returns <- neutral_returns %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    excess_target_vs_benchmark = target_return - benchmark_return,
    excess_neutral_vs_benchmark = neutral_return - benchmark_return,
    excess_neutral_vs_target = neutral_return - target_return,
    cum_target = cumprod(1 + target_return) - 1,
    cum_benchmark = cumprod(1 + benchmark_return) - 1,
    cum_neutral = cumprod(1 + neutral_return) - 1,
    cum_excess_target_vs_benchmark = cumprod(1 + excess_target_vs_benchmark) - 1,
    cum_excess_neutral_vs_benchmark = cumprod(1 + excess_neutral_vs_benchmark) - 1,
    cum_excess_neutral_vs_target = cumprod(1 + excess_neutral_vs_target) - 1
  )

# Diagnostics ----

message(paste0("\nTotal observations: ", nrow(neutral_returns)))
message(paste0("Date range: ", min(neutral_returns$date), " to ", max(neutral_returns$date)))

exposure_stats <- neutral_returns %>%
  dplyr::summarise(
    mean_gross = mean(gross_exposure, na.rm = TRUE),
    max_gross = max(gross_exposure, na.rm = TRUE),
    mean_net = mean(net_exposure, na.rm = TRUE),
    sd_net = sd(net_exposure, na.rm = TRUE)
  )

message("\nExposure statistics:")
message(paste0("  Mean gross exposure: ", round(exposure_stats$mean_gross, 3)))
message(paste0("  Max gross exposure: ", round(exposure_stats$max_gross, 3)))
message(paste0("  Mean net exposure: ", round(exposure_stats$mean_net, 3)))
message(paste0("  SD net exposure: ", round(exposure_stats$sd_net, 3)))

return_stats <- neutral_returns %>%
  dplyr::summarise(
    mean_target = mean(target_return, na.rm = TRUE),
    mean_benchmark = mean(benchmark_return, na.rm = TRUE),
    mean_neutral = mean(neutral_return, na.rm = TRUE),
    sd_target = sd(target_return, na.rm = TRUE),
    sd_benchmark = sd(benchmark_return, na.rm = TRUE),
    sd_neutral = sd(neutral_return, na.rm = TRUE)
  )

message("\nDaily return statistics:")
message(paste0("  ", target_ticker, " mean: ", round(return_stats$mean_target * 252 * 100, 2), "% ann."))
message(paste0("  ", benchmark_ticker, " mean: ", round(return_stats$mean_benchmark * 252 * 100, 2), "% ann."))
message(paste0("  Neutral mean: ", round(return_stats$mean_neutral * 252 * 100, 2), "% ann."))
message(paste0("  ", target_ticker, " vol: ", round(return_stats$sd_target * sqrt(252) * 100, 2), "% ann."))
message(paste0("  ", benchmark_ticker, " vol: ", round(return_stats$sd_benchmark * sqrt(252) * 100, 2), "% ann."))
message(paste0("  Neutral vol: ", round(return_stats$sd_neutral * sqrt(252) * 100, 2), "% ann."))

final_values <- cumulative_returns %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::select(date, cum_target, cum_benchmark, cum_neutral, cum_excess_target_vs_benchmark, cum_excess_neutral_vs_benchmark)

message("\nFinal cumulative returns:")
message(paste0("  ", target_ticker, ": ", round(final_values$cum_target * 100, 2), "%"))
message(paste0("  ", benchmark_ticker, ": ", round(final_values$cum_benchmark * 100, 2), "%"))
message(paste0("  Factor-Neutral: ", round(final_values$cum_neutral * 100, 2), "%"))

message("\nFinal cumulative excess returns:")
message(paste0("  ", target_ticker, " vs ", benchmark_ticker, ": ", round(final_values$cum_excess_target_vs_benchmark * 100, 2), "%"))
message(paste0("  Neutral vs ", benchmark_ticker, ": ", round(final_values$cum_excess_neutral_vs_benchmark * 100, 2), "%"))

# Dataviz ----

library(ggplot2)
library(scales)

# Chart 1: Cumulative Returns Comparison
p1 <- cumulative_returns %>%
  dplyr::select(date, cum_target, cum_benchmark, cum_neutral) %>%
  tidyr::pivot_longer(
    cols = c(cum_target, cum_benchmark, cum_neutral),
    names_to = "strategy",
    values_to = "cumulative_return"
  ) %>%
  dplyr::mutate(
    strategy = dplyr::case_when(
      strategy == "cum_target" ~ target_ticker,
      strategy == "cum_benchmark" ~ benchmark_ticker,
      strategy == "cum_neutral" ~ paste0("Factor-Neutral ", target_ticker),
      TRUE ~ strategy
    )
  ) %>%
  ggplot(aes(x = date, y = cumulative_return, color = strategy)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.3) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  scale_color_manual(
    values = c(
      setNames("#d73027", target_ticker),
      setNames("#4575b4", benchmark_ticker),
      setNames("#1a9850", paste0("Factor-Neutral ", target_ticker))
    )
  ) +
  labs(
    title = paste0("Factor-Neutral ", target_ticker, ": Isolating Idiosyncratic Effects"),
    subtitle = paste0("100% ", target_ticker, " + factor hedges to neutralize tilts vs ", benchmark_ticker, " (rolling 1-year)"),
    x = "",
    y = "Cumulative Return",
    color = "",
    caption = "Data: alphavantage • Chart: brrymtnc"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = "grey40", margin = margin(t = 10)),
    legend.position = "bottom"
  )

print(p1)

# Chart 2: Gross and Net Exposure Over Time
p2 <- neutral_returns %>%
  dplyr::select(date, gross_exposure, net_exposure) %>%
  tidyr::pivot_longer(
    cols = c(gross_exposure, net_exposure),
    names_to = "exposure_type",
    values_to = "exposure"
  ) %>%
  dplyr::mutate(
    exposure_type = dplyr::case_when(
      exposure_type == "gross_exposure" ~ "Gross Exposure",
      exposure_type == "net_exposure" ~ "Net Exposure",
      TRUE ~ exposure_type
    )
  ) %>%
  ggplot(aes(x = date, y = exposure, color = exposure_type)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 1.0, color = "grey30", linewidth = 0.3, linetype = "dashed") +
  geom_hline(yintercept = max_gross_exposure, color = "grey30", linewidth = 0.3, linetype = "dotted") +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  scale_color_manual(
    values = c(
      "Gross Exposure" = "#d73027",
      "Net Exposure" = "#4575b4"
    )
  ) +
  labs(
    title = paste0("Factor-Neutral ", target_ticker, ": Leverage and Exposure"),
    subtitle = paste0("Gross exposure capped at ", scales::percent(max_gross_exposure), " • Net exposure stays near 100%"),
    x = "",
    y = "Exposure",
    color = "",
    caption = "Dashed line = 100% • Dotted line = Cap"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = "grey40", margin = margin(t = 10)),
    legend.position = "bottom"
  )

print(p2)

# Chart 3: Excess Returns vs Benchmarks
p3 <- cumulative_returns %>%
  dplyr::select(date, cum_excess_neutral_vs_benchmark, cum_excess_neutral_vs_target, cum_excess_target_vs_benchmark) %>%
  dplyr::rename(
    neutral_vs_benchmark = cum_excess_neutral_vs_benchmark,
    neutral_vs_target = cum_excess_neutral_vs_target,
    target_vs_benchmark = cum_excess_target_vs_benchmark
  ) %>%
  tidyr::pivot_longer(
    cols = c(neutral_vs_benchmark, neutral_vs_target, target_vs_benchmark),
    names_to = "comparison",
    values_to = "excess_return"
  ) %>%
  dplyr::mutate(
    comparison = dplyr::case_when(
      comparison == "neutral_vs_benchmark" ~ paste0("Neutral vs ", benchmark_ticker),
      comparison == "neutral_vs_target" ~ paste0("Neutral vs ", target_ticker),
      comparison == "target_vs_benchmark" ~ paste0(target_ticker, " vs ", benchmark_ticker),
      TRUE ~ comparison
    )
  ) %>%
  ggplot(aes(x = date, y = excess_return, color = comparison)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.3) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  scale_color_manual(
    values = setNames(
      c("#1a9850", "#d73027", "#4575b4"),
      c(
        paste0("Neutral vs ", benchmark_ticker),
        paste0("Neutral vs ", target_ticker),
        paste0(target_ticker, " vs ", benchmark_ticker)
      )
    )
  ) +
  labs(
    title = "Excess Returns: Factor-Neutral vs Benchmarks",
    subtitle = paste0("Neutral portfolio tracks close to ", benchmark_ticker, " (neutralized factors), differs from ", target_ticker, " (no factor tilts)"),
    x = "",
    y = "Cumulative Excess Return",
    color = "",
    caption = "Data: alphavantage • Chart: brrymtnc"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = "grey40", margin = margin(t = 10)),
    legend.position = "bottom"
  )

print(p3)

# Save charts
if (save_images && !dir.exists("images")) dir.create("images")

cumulative_file <- paste0("images/factor_neutral_", target_lower, "_cumulative_returns.svg")
exposure_file <- paste0("images/factor_neutral_", target_lower, "_exposure.svg")
excess_file <- paste0("images/factor_neutral_", target_lower, "_excess_returns.svg")

if (save_images) {
  ggsave(
    cumulative_file,
    plot = p1,
    width = 12,
    height = 7,
    dpi = 320
  )
  message(paste0("✓ Cumulative returns chart saved to ", cumulative_file))

  ggsave(
    exposure_file,
    plot = p2,
    width = 12,
    height = 7,
    dpi = 320
  )
  message(paste0("✓ Exposure chart saved to ", exposure_file))

  ggsave(
    excess_file,
    plot = p3,
    width = 12,
    height = 7,
    dpi = 320
  )
  message(paste0("✓ Excess returns chart saved to ", excess_file))
}

message("\n✓ Analysis complete!")
if (save_images) {
  message("✓ Charts saved to images/ directory")
}
