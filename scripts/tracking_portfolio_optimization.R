library(ggplot2)
library(scales)

# --- PARAMETERS ----------------------------------------------------------
benchmark_tkr      <- "SPY"
candidate_tickers  <- c(
    "AMZN", 
    "META", 
    "ENTG", 
    "UNH", 
    "ELV", 
    "TMO", 
    "ASHTY", 
    "ASML",
    "KKR",
    "APO",
    "BN"
)   

# -------------------------------------------------------------------------

all_tickers <- c(benchmark_tkr, candidate_tickers)
prices <- fetch_adjusted_prices(all_tickers)

roll_window <- 252L    # n-day window


# --- DAILY LOG RETURNS ---------------------------------------------------
returns <- calculate_log_returns(prices) %>%
    dplyr::select(date, ticker, return) %>% 
    dplyr::group_by(ticker) %>% 
    dplyr::filter(!is.na(return)) %>%  # Remove NA returns (first obs per ticker)
    dplyr::ungroup()
    
# Wide matrix for regression
ret_mat <- returns %>%
    tidyr::pivot_wider(names_from = ticker, values_from = return) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(complete.cases(.))

cat("Rows after complete.cases():", nrow(ret_mat), "\n")

y <- ret_mat[[benchmark_tkr]]
x <- ret_mat %>% dplyr::select(-date, -dplyr::all_of(benchmark_tkr)) %>% as.matrix()

# --- ROLLING OPTIMISATION ------------------------------------------------
roll_fit <- roll_constrained_lm(
    x          = x,
    y          = y,
    width      = roll_window,
    non_negative = TRUE,
    sum_to_one   = TRUE,
    intercept    = FALSE       # replication portfolio must add to 100 %
)

weights <- cbind(date = ret_mat$date,
                 as.data.frame(roll_fit$coefficients))

# --- DIAGNOSTICS ---------------------------------------------------------
# Extract weight matrix and LAG by 1 day to avoid look-ahead bias
weight_matrix <- as.matrix(weights[, candidate_tickers])
lagged_weights <- rbind(
    rep(NA, ncol(weight_matrix)),  # First row is NA (no previous weights)
    weight_matrix[-nrow(weight_matrix), ]  # Shift everything down by 1 day
)

# Calculate portfolio returns using PREVIOUS day's optimal weights
portfolio_returns <- rowSums(x * lagged_weights, na.rm = TRUE)

# ... existing code ...

# --- PORTFOLIO LOG-RETURN ------------------------------------------------
# Convert constituent log returns to simple, aggregate, then back to log
simple_ret_mat     <- exp(x) - 1
simple_port_return <- rowSums(simple_ret_mat * lagged_weights, na.rm = TRUE)
portfolio_returns  <- log1p(simple_port_return)

# Active return -----------------------------------------------------------
active_returns <- portfolio_returns - y
ann_sqrt  <- sqrt(252)
ann_days  <- 252

# --- ROLLING METRICS -----------------------------------------------------
rolling_mean_ar <- slider::slide_dbl(
  active_returns, mean,
  .before   = roll_window - 1,
  .complete = TRUE,
  na.rm     = TRUE
)

tracking_error <- ann_sqrt * slider::slide_dbl(
  active_returns, sd,
  .before   = roll_window - 1,
  .complete = TRUE,
  na.rm     = TRUE
)

info_ratio <- (ann_days * rolling_mean_ar) / tracking_error

# ... rest of code ...# ... rest of code ...# Remove the first day (since we don't have lagged weights)
# ... existing code ...

# --- DIAGNOSTICS ---------------------------------------------------------
# Identify the first day where a tracking-error value exists
first_valid <- which(!is.na(tracking_error))[1]

valid_indices <- first_valid:length(y)      # start from that day
diagnostics <- data.frame(
    date             = ret_mat$date[valid_indices],
    portfolio_return = portfolio_returns[valid_indices],
    benchmark_return = y[valid_indices],
    active_return    = active_returns[valid_indices],
    tracking_error   = tracking_error[valid_indices],
    info_ratio       = info_ratio[valid_indices]
)

# --- QUICK PLOT ----------------------------------------------------------
ggplot(diagnostics, aes(date, tracking_error)) +
    geom_line(color = "steelblue") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Rolling Tracking Error of Optimised Stock Basket vs SPY",
         y = "Tracking Error (annualised approx.)",
         x = "")

# ... rest of code ...
# Save weights if you want to trade them later:
# readr::write_csv(weights, "data/rolling_index_tracking_weights.csv")

# --- WEIGHT VISUALIZATION ------------------------------------------------

# Convert weights to long format for plotting
weights_long <- weights %>%
    tidyr::pivot_longer(
        cols = -date,
        names_to = "ticker",
        values_to = "weight"
    ) %>%
    dplyr::filter(!is.na(weight)) %>%  # Remove NA weights from early periods
    dplyr::mutate(
        ticker = factor(ticker, levels = candidate_tickers)  # Preserve order
    )

# Stacked area chart
p1 <- weights_long %>%
    ggplot(aes(x = date, y = weight, fill = ticker)) +
    geom_area(position = position_stack(reverse = FALSE),
              alpha = 0.8,
              size = 0,
              na.rm = TRUE) +
    scale_x_date(
        date_breaks = "6 months",
        date_labels = "%Y-%m",
        expand = c(0, 0)
    ) +
    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        labels = scales::percent_format(),
        expand = c(0, 0)
    ) +
    scale_fill_brewer(
        type = "qual", 
        palette = "Set3",
        guide = guide_legend(reverse = TRUE)
    ) +
    labs(
        title = "Rolling Optimal Weights for SPY Tracking Portfolio",
        subtitle = paste("Constrained optimization (non-negative, sum to 1) -", 
                        roll_window, "day window"),
        x = "", 
        y = "Portfolio Weight",
        fill = "Stock"
    ) +
    theme_minimal(base_size = 11) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )


# Display plots
print(p1)

# Save plots
if (!dir.exists("images")) dir.create("images")
ggsave("images/spy_tracking_weights_stacked.png", plot = p1, 
       width = 12, height = 8, dpi = 300)

# --- WEIGHT STATISTICS ---------------------------------------------------
# Summary statistics for final weights
final_weights <- weights[nrow(weights), candidate_tickers]
cat("\nFinal portfolio weights:\n")
print(round(sort(unlist(final_weights), decreasing = TRUE), 4))

# Sum of final weights
final_weight_sum <- weights %>% 
    dplyr::slice_tail() %>% 
    dplyr::select(all_of(candidate_tickers)) %>% 
    sum(na.rm = TRUE)

cat("\nSum of final weights:", round(final_weight_sum, 4), "\n")

# --- CUMULATIVE VALUE-ADD ANALYSIS ---------------------------------------
cumulative_active <- diagnostics %>%
    dplyr::mutate(
        cumulative_value_add = exp(cumsum(active_return)) - 1
    ) %>%
    dplyr::select(date, active_return, cumulative_value_add)

# ... rest of code ...
# Plot cumulative value-add
p2 <- ggplot(cumulative_active, aes(x = date, y = cumulative_value_add)) +
    geom_line(color = "darkgreen", size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_x_date(
        date_breaks = "6 months",
        date_labels = "%Y-%m"
    ) +
    scale_y_continuous(
        labels = scales::percent_format(),
        breaks = scales::pretty_breaks(n = 8)
    ) +
    labs(
        title = "Cumulative Value-Add: Tracking Portfolio vs SPY",
        subtitle = paste("Cumulative active returns using", roll_window, "day rolling optimization"),
        x = "",
        y = "Cumulative Value-Add vs Benchmark"
    ) +
    theme_minimal(base_size = 11) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey80"),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

# Display plot
print(p2)

# Save plot
ggsave("images/spy_tracking_cumulative_value_add.png", plot = p2, 
       width = 12, height = 8, dpi = 300)

# Print final cumulative value-add
final_value_add <- tail(cumulative_active$cumulative_value_add, 1)
cat("\nFinal cumulative value-add vs benchmark:", scales::percent(final_value_add, accuracy = 0.01), "\n")