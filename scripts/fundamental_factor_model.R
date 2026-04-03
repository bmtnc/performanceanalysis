# Fundamental Factor Model
# 
# This script implements a basic fundamental factor model using daily cross-sectional
# regressions on fundamental factor exposures. Similar to Barra-style factor models.
#
# TODO: Add sector/industry dummy variables once classification data is curated

# Load TTM per-share artifact from S3
message("Loading TTM artifact from S3...")
ttm_per_share_data <- arrow::read_parquet(
  "s3://avpipeline-artifacts-prod/ttm-artifacts/2025-10-29/ttm_per_share_financial_artifact.parquet"
)

message(paste0("Loaded ", nrow(ttm_per_share_data), " rows across ", 
               dplyr::n_distinct(ttm_per_share_data$ticker), " tickers"))

# Filter to latest 3 years and apply data quality filters
max_date <- max(ttm_per_share_data$date)
min_date <- max_date - lubridate::years(10)

message(paste0("Filtering to date range: ", min_date, " to ", max_date))

filtered_data <- ttm_per_share_data %>%
  dplyr::filter(
    date >= min_date,
    date <= max_date,
    has_complete_financial_data == TRUE
  )

# Calculate minimum data availability per ticker (at least 250 trading days)
ticker_data_counts <- filtered_data %>%
  dplyr::group_by(ticker) %>%
  dplyr::summarize(n_obs = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n_obs >= 250)

message(paste0("Applying minimum data availability filter (>= 250 days)..."))
message(paste0("Retained ", nrow(ticker_data_counts), " tickers"))

filtered_data <- filtered_data %>%
  dplyr::inner_join(ticker_data_counts %>% dplyr::select(ticker), by = "ticker")

# Calculate daily log returns
message("Calculating daily log returns...")
returns_data <- filtered_data %>%
  dplyr::arrange(ticker, date) %>%
  dplyr::group_by(ticker) %>%
  dplyr::mutate(
    log_return = log(adjusted_close / dplyr::lag(adjusted_close))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(log_return))

# Calculate quarterly revenue growth for Growth factor
message("Calculating quarterly revenue growth factor...")
quarterly_growth <- filtered_data %>%
  dplyr::distinct(ticker, fiscalDateEnding, .keep_all = TRUE) %>%
  dplyr::arrange(ticker, fiscalDateEnding) %>%
  dplyr::group_by(ticker) %>%
  dplyr::mutate(
    revenue_growth_qoq = (totalRevenue_ttm_per_share / dplyr::lag(totalRevenue_ttm_per_share)) - 1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(ticker, fiscalDateEnding, revenue_growth_qoq)

# Forward-fill growth factor to daily frequency using inequality join
growth_daily <- returns_data %>%
  dplyr::select(ticker, date) %>%
  dplyr::left_join(
    quarterly_growth,
    by = "ticker",
    relationship = "many-to-many"
  ) %>%
  dplyr::filter(date >= fiscalDateEnding) %>%
  dplyr::group_by(ticker, date) %>%
  dplyr::slice_max(fiscalDateEnding, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(ticker, date, revenue_growth_qoq)

# Join growth factor back to main data
returns_data <- returns_data %>%
  dplyr::left_join(growth_daily, by = c("ticker", "date"))

# Calculate 5 fundamental factor exposures
message("Calculating fundamental factor exposures...")
factor_data <- returns_data %>%
  dplyr::mutate(
    fcf_yield = fcf_ttm_per_share / adjusted_close,
    log_market_cap = log(market_cap),
    roic_proxy = nopat_ttm_per_share / totalShareholderEquity_per_share,
    revenue_growth = revenue_growth_qoq,
    asset_turnover = totalRevenue_ttm_per_share / totalAssets_per_share
  ) %>%
  dplyr::select(
    ticker, date, log_return, market_cap,
    fcf_yield, log_market_cap, roic_proxy, revenue_growth, asset_turnover
  ) %>%
  dplyr::filter(
    !is.na(fcf_yield),
    !is.na(log_market_cap),
    !is.na(roic_proxy),
    !is.na(revenue_growth),
    !is.na(asset_turnover),
    is.finite(fcf_yield),
    is.finite(log_market_cap),
    is.finite(roic_proxy),
    is.finite(revenue_growth),
    is.finite(asset_turnover)
  )

message(paste0("Factor data prepared: ", nrow(factor_data), " stock-day observations"))

# Winsorize and standardize exposures cross-sectionally
message("Winsorizing and standardizing factor exposures...")

winsorize_vector <- function(x, lower_pct = 0.01, upper_pct = 0.99) {
  lower_bound <- stats::quantile(x, lower_pct, na.rm = TRUE)
  upper_bound <- stats::quantile(x, upper_pct, na.rm = TRUE)
  pmin(pmax(x, lower_bound), upper_bound)
}

standardize_weighted <- function(x, weights) {
  weighted_mean <- stats::weighted.mean(x, weights, na.rm = TRUE)
  weighted_var <- stats::weighted.mean((x - weighted_mean)^2, weights, na.rm = TRUE)
  weighted_sd <- sqrt(weighted_var)
  (x - weighted_mean) / weighted_sd
}

standardized_data <- factor_data %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    fcf_yield_win = winsorize_vector(fcf_yield),
    log_market_cap_win = winsorize_vector(log_market_cap),
    roic_proxy_win = winsorize_vector(roic_proxy),
    revenue_growth_win = winsorize_vector(revenue_growth),
    asset_turnover_win = winsorize_vector(asset_turnover),
    fcf_yield_std = standardize_weighted(fcf_yield_win, market_cap),
    log_market_cap_std = standardize_weighted(log_market_cap_win, market_cap),
    roic_proxy_std = standardize_weighted(roic_proxy_win, market_cap),
    revenue_growth_std = standardize_weighted(revenue_growth_win, market_cap),
    asset_turnover_std = standardize_weighted(asset_turnover_win, market_cap)
  ) %>%
  dplyr::ungroup()

# Run daily cross-sectional weighted least squares regressions
message("Running daily cross-sectional WLS regressions...")

regression_results <- standardized_data %>%
  split(.$date) %>%
  lapply(function(day_data) {
    tryCatch({
      model <- stats::lm(
        log_return ~ fcf_yield_std + log_market_cap_std + roic_proxy_std + 
          revenue_growth_std + asset_turnover_std,
        data = day_data,
        weights = day_data$market_cap
      )
      
      coefficients <- stats::coef(model)
      residuals <- stats::residuals(model)
      r_squared <- summary(model)$r.squared
      
      tibble::tibble(
        date = unique(day_data$date),
        fcf_yield_return = coefficients["fcf_yield_std"],
        log_market_cap_return = coefficients["log_market_cap_std"],
        roic_proxy_return = coefficients["roic_proxy_std"],
        revenue_growth_return = coefficients["revenue_growth_std"],
        asset_turnover_return = coefficients["asset_turnover_std"],
        r_squared = r_squared,
        n_stocks = nrow(day_data)
      )
    }, error = function(e) {
      tibble::tibble(
        date = unique(day_data$date),
        fcf_yield_return = NA_real_,
        log_market_cap_return = NA_real_,
        roic_proxy_return = NA_real_,
        revenue_growth_return = NA_real_,
        asset_turnover_return = NA_real_,
        r_squared = NA_real_,
        n_stocks = nrow(day_data)
      )
    })
  }) %>%
  dplyr::bind_rows()

message(paste0("Completed ", nrow(regression_results), " daily regressions"))

# Calculate factor return statistics
message("\n=== Factor Return Statistics ===\n")

factor_stats <- tibble::tibble(
  factor = c("FCF Yield", "Log Market Cap", "ROIC Proxy", "Revenue Growth", "Asset Turnover"),
  mean_return = c(
    mean(regression_results$fcf_yield_return, na.rm = TRUE),
    mean(regression_results$log_market_cap_return, na.rm = TRUE),
    mean(regression_results$roic_proxy_return, na.rm = TRUE),
    mean(regression_results$revenue_growth_return, na.rm = TRUE),
    mean(regression_results$asset_turnover_return, na.rm = TRUE)
  ),
  volatility = c(
    stats::sd(regression_results$fcf_yield_return, na.rm = TRUE),
    stats::sd(regression_results$log_market_cap_return, na.rm = TRUE),
    stats::sd(regression_results$roic_proxy_return, na.rm = TRUE),
    stats::sd(regression_results$revenue_growth_return, na.rm = TRUE),
    stats::sd(regression_results$asset_turnover_return, na.rm = TRUE)
  )
) %>%
  dplyr::mutate(
    annualized_return = mean_return * 252,
    annualized_vol = volatility * sqrt(252),
    sharpe_ratio = annualized_return / annualized_vol,
    t_stat = mean_return / (volatility / sqrt(nrow(regression_results)))
  )

print(factor_stats)

# Calculate factor correlation matrix
message("\n=== Factor Correlation Matrix ===\n")

factor_returns_matrix <- regression_results %>%
  dplyr::select(
    fcf_yield_return, log_market_cap_return, roic_proxy_return,
    revenue_growth_return, asset_turnover_return
  ) %>%
  as.matrix()

colnames(factor_returns_matrix) <- c("FCF Yield", "Log Market Cap", "ROIC Proxy", 
                                      "Revenue Growth", "Asset Turnover")

factor_correlation <- stats::cor(factor_returns_matrix, use = "complete.obs")
print(round(factor_correlation, 3))

# Calculate factor covariance matrix (annualized)
message("\n=== Factor Covariance Matrix (Annualized) ===\n")

factor_covariance <- stats::cov(factor_returns_matrix, use = "complete.obs") * 252
print(round(factor_covariance, 6))

# Summary of regression fit quality
message("\n=== Regression Diagnostics ===\n")

diagnostics <- tibble::tibble(
  metric = c("Mean R-squared", "Median R-squared", "Mean # Stocks"),
  value = c(
    mean(regression_results$r_squared, na.rm = TRUE),
    stats::median(regression_results$r_squared, na.rm = TRUE),
    mean(regression_results$n_stocks, na.rm = TRUE)
  )
)

print(diagnostics)

message("\n=== Analysis Complete ===")
message("Objects created:")
message("  - regression_results: Daily factor returns time series")
message("  - factor_stats: Factor performance statistics")
message("  - factor_correlation: Factor correlation matrix")
message("  - factor_covariance: Factor covariance matrix (annualized)")
message("  - standardized_data: Full dataset with standardized exposures")

# Visualize cumulative factor returns
message("\n=== Creating Factor Performance Visualization ===\n")

cumulative_factor_returns <- regression_results %>%
  dplyr::select(date, fcf_yield_return, log_market_cap_return, roic_proxy_return,
                revenue_growth_return, asset_turnover_return) %>%
  tidyr::pivot_longer(
    cols = -date,
    names_to = "factor",
    values_to = "daily_return"
  ) %>%
  dplyr::mutate(
    factor = dplyr::case_when(
      factor == "fcf_yield_return" ~ "FCF Yield",
      factor == "log_market_cap_return" ~ "Log Market Cap",
      factor == "roic_proxy_return" ~ "ROIC Proxy",
      factor == "revenue_growth_return" ~ "Revenue Growth",
      factor == "asset_turnover_return" ~ "Asset Turnover",
      TRUE ~ factor
    )
  ) %>%
  dplyr::arrange(factor, date) %>%
  dplyr::group_by(factor) %>%
  dplyr::mutate(
    cumulative_return = cumprod(1 + tidyr::replace_na(daily_return, 0))
  ) %>%
  dplyr::ungroup()

# Get final values for each factor
final_values <- cumulative_factor_returns %>%
  dplyr::group_by(factor) %>%
  dplyr::slice_max(date, n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    label_text = paste0(factor, ": ", sprintf("%.2f", cumulative_return))
  )

# Create color palette
factor_colors <- c(
  "FCF Yield" = "#E41A1C",
  "Log Market Cap" = "#377EB8",
  "ROIC Proxy" = "#4DAF4A",
  "Revenue Growth" = "#984EA3",
  "Asset Turnover" = "#FF7F00"
)

# Create the plot
p <- cumulative_factor_returns %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = cumulative_return, color = factor)) +
  ggplot2::geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  ggplot2::geom_line(linewidth = 1.2, alpha = 0.9) +
  ggplot2::scale_color_manual(values = factor_colors) +
  ggplot2::labs(
    title = "Fundamental Factor Model: Cumulative Factor Returns",
    subtitle = paste0(
      "Daily cross-sectional WLS regression factor returns (indexed to 1.0)\n",
      "Universe: ", round(mean(regression_results$n_stocks, na.rm = TRUE)), " stocks | ",
      "Period: ", format(min(regression_results$date), "%b %Y"), " - ",
      format(max(regression_results$date), "%b %Y")
    ),
    x = "Date",
    y = "Cumulative Return (Indexed to 1.0)",
    color = "Factor",
    caption = paste0(
      "Interpretation: A value of 1.20 indicates a +1 std dev exposure to that factor ",
      "would have generated 20% cumulative return.\n",
      "Mean R-squared: ", sprintf("%.1f%%", mean(regression_results$r_squared, na.rm = TRUE) * 100),
      " | Factors standardized cross-sectionally with market-cap weighting."
    )
  ) +
  ggplot2::scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(face = "bold"),
    plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = ggplot2::element_text(size = 11, lineheight = 1.2, hjust = 0),
    plot.caption = ggplot2::element_text(hjust = 0, lineheight = 1.2),
    panel.grid.minor = ggplot2::element_blank()
  )

print(p)

message("\nVisualization complete.")
