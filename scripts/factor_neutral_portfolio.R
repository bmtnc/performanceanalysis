# Factor-Neutral Portfolio Analysis
# Creates a portfolio that is 100% long ARKK but neutralizes factor tilts
# relative to Russell 3000 by taking offsetting positions in factor ETFs

# Script Params ----

roll_window <- 252L

tickers <- c(
  "ARKK", # ARK Innovation ETF (target)
  "IWR",  # iShares Russell Midcap ETF (benchmark)
  "IWD",  # R1000V
  "IWF",  # R1000G
  "IWN",  # R2000V
  "IWO",  # R2000G
  "MTUM", # MSCI Momentum
  "USMV", # MSCI USA Min Vol
  "QUAL", # MSCI USA Quality
  "IJR"   # Small Cap Quality
)

max_gross_exposure <- 2.0  # 200% cap

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

# Prepare factor returns (exclude both ARKK and IWR from factors)
factor_returns <- return_data %>%
  dplyr::filter(!ticker %in% c("ARKK", "IWR")) %>%
  dplyr::select(date, ticker, return) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = return)

# Prepare ARKK returns
arkk_returns <- return_data %>%
  dplyr::filter(ticker == "ARKK") %>%
  dplyr::select(date, return)

# Prepare IWR returns
iwr_returns <- return_data %>%
  dplyr::filter(ticker == "IWR") %>%
  dplyr::select(date, return)

# Create regression datasets
arkk_regression_data <- arkk_returns %>%
  dplyr::left_join(factor_returns, by = "date") %>%
  dplyr::filter(complete.cases(.))

iwr_regression_data <- iwr_returns %>%
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
  dplyr::select(date, IWD, IWF, IWN, IWO, MTUM, USMV, QUAL, IJR) %>%
  dplyr::filter(!is.na(IWD))

# Rolling Multi-Factor Constrained Regression - IWR ----

message("Running IWR constrained regression (no intercept)...")

iwr_weights <- iwr_regression_data %>%
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
  dplyr::select(date, IWD, IWF, IWN, IWO, MTUM, USMV, QUAL, IJR) %>%
  dplyr::filter(!is.na(IWD))

# Calculate Hedge Weights ----

message("Calculating factor hedge weights...")

hedge_weights <- arkk_weights %>%
  dplyr::inner_join(iwr_weights, by = "date", suffix = c("_arkk", "_iwr")) %>%
  dplyr::mutate(
    hedge_IWD = IWD_iwr - IWD_arkk,
    hedge_IWF = IWF_iwr - IWF_arkk,
    hedge_IWN = IWN_iwr - IWN_arkk,
    hedge_IWO = IWO_iwr - IWO_arkk,
    hedge_MTUM = MTUM_iwr - MTUM_arkk,
    hedge_USMV = USMV_iwr - USMV_arkk,
    hedge_QUAL = QUAL_iwr - QUAL_arkk,
    hedge_IJR = IJR_iwr - IJR_arkk
  ) %>%
  dplyr::select(
    date,
    hedge_IWD, hedge_IWF, hedge_IWN, hedge_IWO,
    hedge_MTUM, hedge_USMV, hedge_QUAL, hedge_IJR
  )

# Calculate gross exposure and apply cap
hedge_weights <- hedge_weights %>%
  dplyr::mutate(
    gross_exposure_uncapped = 1.0 + abs(hedge_IWD) + abs(hedge_IWF) + abs(hedge_IWN) +
      abs(hedge_IWO) + abs(hedge_MTUM) + abs(hedge_USMV) + abs(hedge_QUAL) + abs(hedge_IJR),
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
    hedge_IJR = hedge_IJR * scale_factor,
    gross_exposure = 1.0 + abs(hedge_IWD) + abs(hedge_IWF) + abs(hedge_IWN) +
      abs(hedge_IWO) + abs(hedge_MTUM) + abs(hedge_USMV) + abs(hedge_QUAL) + abs(hedge_IJR),
    net_exposure = 1.0 + hedge_IWD + hedge_IWF + hedge_IWN + hedge_IWO +
      hedge_MTUM + hedge_USMV + hedge_QUAL + hedge_IJR
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
    arkk_returns %>% dplyr::rename(future_date = date, arkk_return = return),
    by = "future_date"
  ) %>%
  dplyr::inner_join(
    iwr_returns %>% dplyr::rename(future_date = date, iwr_return = return),
    by = "future_date"
  ) %>%
  dplyr::mutate(
    hedge_return = hedge_IWD * IWD + hedge_IWF * IWF + hedge_IWN * IWN +
      hedge_IWO * IWO + hedge_MTUM * MTUM + hedge_USMV * USMV +
      hedge_QUAL * QUAL + hedge_IJR * IJR,
    neutral_return = arkk_return + hedge_return
  ) %>%
  dplyr::select(
    date = future_date,
    arkk_return,
    iwr_return,
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
    excess_arkk_vs_iwr = arkk_return - iwr_return,
    excess_neutral_vs_iwr = neutral_return - iwr_return,
    excess_neutral_vs_arkk = neutral_return - arkk_return,
    cum_arkk = cumprod(1 + arkk_return) - 1,
    cum_iwr = cumprod(1 + iwr_return) - 1,
    cum_neutral = cumprod(1 + neutral_return) - 1,
    cum_excess_arkk_vs_iwr = cumprod(1 + excess_arkk_vs_iwr) - 1,
    cum_excess_neutral_vs_iwr = cumprod(1 + excess_neutral_vs_iwr) - 1,
    cum_excess_neutral_vs_arkk = cumprod(1 + excess_neutral_vs_arkk) - 1
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
    mean_arkk = mean(arkk_return, na.rm = TRUE),
    mean_iwr = mean(iwr_return, na.rm = TRUE),
    mean_neutral = mean(neutral_return, na.rm = TRUE),
    sd_arkk = sd(arkk_return, na.rm = TRUE),
    sd_iwr = sd(iwr_return, na.rm = TRUE),
    sd_neutral = sd(neutral_return, na.rm = TRUE)
  )

message("\nDaily return statistics:")
message(paste0("  ARKK mean: ", round(return_stats$mean_arkk * 252 * 100, 2), "% ann."))
message(paste0("  Russell Midcap mean: ", round(return_stats$mean_iwr * 252 * 100, 2), "% ann."))
message(paste0("  Neutral mean: ", round(return_stats$mean_neutral * 252 * 100, 2), "% ann."))
message(paste0("  ARKK vol: ", round(return_stats$sd_arkk * sqrt(252) * 100, 2), "% ann."))
message(paste0("  Russell Midcap vol: ", round(return_stats$sd_iwr * sqrt(252) * 100, 2), "% ann."))
message(paste0("  Neutral vol: ", round(return_stats$sd_neutral * sqrt(252) * 100, 2), "% ann."))

final_values <- cumulative_returns %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::select(date, cum_arkk, cum_iwr, cum_neutral, cum_excess_arkk_vs_iwr, cum_excess_neutral_vs_iwr)

message("\nFinal cumulative returns:")
message(paste0("  ARKK: ", round(final_values$cum_arkk * 100, 2), "%"))
message(paste0("  Russell Midcap: ", round(final_values$cum_iwr * 100, 2), "%"))
message(paste0("  Factor-Neutral: ", round(final_values$cum_neutral * 100, 2), "%"))

message("\nFinal cumulative excess returns:")
message(paste0("  ARKK vs Russell Midcap: ", round(final_values$cum_excess_arkk_vs_iwr * 100, 2), "%"))
message(paste0("  Neutral vs Russell Midcap: ", round(final_values$cum_excess_neutral_vs_iwr * 100, 2), "%"))

# Dataviz ----

library(ggplot2)
library(scales)

# Chart 1: Cumulative Returns Comparison
p1 <- cumulative_returns %>%
  dplyr::select(date, cum_arkk, cum_iwr, cum_neutral) %>%
  tidyr::pivot_longer(
    cols = c(cum_arkk, cum_iwr, cum_neutral),
    names_to = "strategy",
    values_to = "cumulative_return"
  ) %>%
  dplyr::mutate(
    strategy = dplyr::case_when(
      strategy == "cum_arkk" ~ "ARKK",
      strategy == "cum_iwr" ~ "Russell Midcap",
      strategy == "cum_neutral" ~ "Factor-Neutral ARKK",
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
      "ARKK" = "#d73027",
      "Russell Midcap" = "#4575b4",
      "Factor-Neutral ARKK" = "#1a9850"
    )
  ) +
  labs(
    title = "Factor-Neutral ARKK: Isolating Selection Effect",
    subtitle = "100% ARKK + factor hedges to neutralize tilts vs Russell Midcap (rolling 1-year)",
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
    title = "Factor-Neutral Portfolio: Leverage and Exposure",
    subtitle = paste0("Gross exposure capped at ", scales::percent(max_gross_exposure), " • Net exposure should stay near 100%"),
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
  dplyr::select(date, cum_excess_neutral_vs_iwr, cum_excess_neutral_vs_arkk, cum_excess_arkk_vs_iwr) %>%
  dplyr::rename(
    neutral_vs_russell = cum_excess_neutral_vs_iwr,
    neutral_vs_arkk = cum_excess_neutral_vs_arkk,
    arkk_vs_russell = cum_excess_arkk_vs_iwr
  ) %>%
  tidyr::pivot_longer(
    cols = c(neutral_vs_russell, neutral_vs_arkk, arkk_vs_russell),
    names_to = "comparison",
    values_to = "excess_return"
  ) %>%
  dplyr::mutate(
    comparison = dplyr::case_when(
      comparison == "neutral_vs_russell" ~ "Neutral vs Russell 3000",
      comparison == "neutral_vs_arkk" ~ "Neutral vs ARKK",
      comparison == "arkk_vs_russell" ~ "ARKK vs Russell 3000",
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
    values = c(
      "Neutral vs Russell Midcap" = "#1a9850",
      "Neutral vs ARKK" = "#d73027",
      "ARKK vs Russell Midcap" = "#4575b4"
    )
  ) +
  labs(
    title = "Excess Returns: Factor-Neutral vs Benchmarks",
    subtitle = "Neutral portfolio should track close to Russell Midcap (neutralized factors), differ from ARKK (no factor tilts)",
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
if (!dir.exists("images")) dir.create("images")

ggsave(
  "images/factor_neutral_cumulative_returns.svg",
  plot = p1,
  width = 12,
  height = 7,
  dpi = 320
)

ggsave(
  "images/factor_neutral_exposure.svg",
  plot = p2,
  width = 12,
  height = 7,
  dpi = 320
)

ggsave(
  "images/factor_neutral_excess_returns.svg",
  plot = p3,
  width = 12,
  height = 7,
  dpi = 320
)

message("\n✓ Analysis complete! Charts saved to images/ directory")
