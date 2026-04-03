# Plot Cumulative Factor Returns by Asset Class
# One plot per asset class: FI, FX, Equity (Global)

library(dplyr)
library(tidyr)
library(ggplot2)

# Config ----
equity_geography <- "Global" # which geography to plot for equity factors
start_date <- as.Date("1990-01-01") # common start for comparability

# Load cached data ----
fi <- readRDS("data/fred/fi_factor_returns.rds")
fx <- readRDS("data/fred/fx_factor_returns.rds")
eq <- readRDS("data/aqr/aqr_equity_factors.rds")

# Color palette ----
factor_colors <- c(
  "Carry" = "#2166AC",
  "Value" = "#B2182B",
  "Momentum" = "#1B7837",
  "Defensive" = "#762A83",
  "HML" = "#B2182B",
  "BAB" = "#762A83",
  "QMJ" = "#E08214",
  "MKT" = "#525252",
  "SMB" = "#2166AC"
)

# Helper: compute cumulative log return from simple returns
cum_log_return <- function(r) exp(cumsum(log(1 + r))) - 1

# 1. Fixed Income ----
fi_long <- fi |>
  filter(date >= start_date) |>
  select(
    date,
    Carry = carry,
    Value = value,
    Momentum = momentum,
    Defensive = defensive
  ) |>
  pivot_longer(-date, names_to = "factor", values_to = "return") |>
  filter(!is.na(return)) |>
  group_by(factor) |>
  arrange(date) |>
  mutate(cumulative = cum_log_return(return)) |>
  ungroup()

p_fi <- ggplot(fi_long, aes(x = date, y = cumulative, color = factor)) +
  geom_line(linewidth = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.3
  ) +
  scale_color_manual(values = factor_colors) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Fixed Income Factor Returns",
    subtitle = "Cumulative returns, cross-country long/short portfolios (FRED data)",
    x = NULL,
    y = "Cumulative Return",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# 2. FX / Currencies ----
fx_long <- fx |>
  filter(date >= start_date) |>
  select(
    date,
    Carry = carry_return,
    Momentum = mom_return,
    Value = val_return
  ) |>
  pivot_longer(-date, names_to = "factor", values_to = "return") |>
  filter(!is.na(return)) |>
  group_by(factor) |>
  arrange(date) |>
  mutate(cumulative = cum_log_return(return)) |>
  ungroup()

p_fx <- ggplot(fx_long, aes(x = date, y = cumulative, color = factor)) +
  geom_line(linewidth = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.3
  ) +
  scale_color_manual(values = factor_colors) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Currency (FX) Factor Returns",
    subtitle = "Cumulative returns, G10 long/short portfolios (FRED data)",
    x = NULL,
    y = "Cumulative Return",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# 3. Equity (AQR) ----
eq_long <- eq |>
  filter(geography == equity_geography, date >= start_date) |>
  select(date, HML = hml, BAB = bab, QMJ = qmj, MKT = mkt, SMB = smb) |>
  pivot_longer(-date, names_to = "factor", values_to = "return") |>
  filter(!is.na(return)) |>
  group_by(factor) |>
  arrange(date) |>
  mutate(cumulative = cum_log_return(return)) |>
  ungroup()

p_eq <- ggplot(eq_long, aes(x = date, y = cumulative, color = factor)) +
  geom_line(linewidth = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.3
  ) +
  scale_color_manual(values = factor_colors) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = paste0("Equity Factor Returns (", equity_geography, ")"),
    subtitle = "Cumulative returns, long/short factors (AQR data)",
    x = NULL,
    y = "Cumulative Return",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Print ----
print(p_fi)
print(p_fx)
print(p_eq)
