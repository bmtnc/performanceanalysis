# Performance Cone -- Expected Performance Paths
#
# Plots a cone of performance expectations around actual fund returns,
# given exogenous Sharpe ratio and volatility assumptions. The center
# line is the geometric growth rate (arithmetic return minus variance
# drain); bands show 1/2/3 std. dev. dispersion from the lognormal
# distribution of cumulative wealth.

# Script Params ----

target_ticker <- "BIMBX"

sharpe_ratio <- 0.5    # annualized, excess return / vol
volatility <- 0.10     # annualized vol (e.g. 0.10 = 10%)

# Date range filtering (NULL = use all available data)
start_date <- as.Date("2018-01-31")
end_date <- NULL

# Load package functions
devtools::load_all()

# Data Pull ----

message("========================================")
message("FETCHING DATA")
message("========================================")

all_data <- fetch_adjusted_prices(target_ticker)

# Compute Monthly Returns ----

message("\nComputing returns...")

daily_returns <- calculate_log_returns(all_data)

monthly_returns <- daily_returns %>%
  dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
  dplyr::group_by(ticker, month) %>%
  dplyr::summarise(return = compound_returns(return), .groups = "drop") %>%
  dplyr::rename(date = month) %>%
  dplyr::filter(ticker == target_ticker) %>%
  dplyr::select(date, return) %>%
  dplyr::arrange(date)

# Apply date filtering
if (!is.null(start_date)) {
  monthly_returns <- monthly_returns %>%
    dplyr::filter(date >= lubridate::ymd(start_date))
  message(paste0("Filtering: start_date >= ", start_date))
}
if (!is.null(end_date)) {
  monthly_returns <- monthly_returns %>%
    dplyr::filter(date <= lubridate::ymd(end_date))
  message(paste0("Filtering: end_date <= ", end_date))
}

message(paste0("Observations: ", nrow(monthly_returns), " months"))
message(paste0(
  "Date range: ",
  min(monthly_returns$date),
  " to ",
  max(monthly_returns$date)
))

# Build Cone ----

message("\n========================================")
message("BUILDING PERFORMANCE CONE")
message("========================================")

cone_data <- calculate_performance_cone(
  monthly_returns,
  sharpe_ratio = sharpe_ratio,
  volatility = volatility
)

message("\nAssumptions:")
message(sprintf("  Sharpe ratio:       %.2f", attr(cone_data, "sharpe_ratio")))
message(sprintf("  Volatility:         %.1f%%", attr(cone_data, "volatility") * 100))
message(sprintf("  Arithmetic return:  %.2f%%", attr(cone_data, "mu_arithmetic") * 100))
message(sprintf("  Geometric growth:   %.2f%%", attr(cone_data, "mu_geometric") * 100))
message(sprintf("  Variance drain:     %.2f%%", attr(cone_data, "variance_drain") * 100))

# Plot ----

message("\n========================================")
message("PLOTTING")
message("========================================")

p <- plot_performance_cone(cone_data, target_ticker = target_ticker)

print(p)

message("\n========================================")
message("COMPLETE!")
message("========================================\n")
