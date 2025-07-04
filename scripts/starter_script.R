# Script Params ----

roll_window <- 756L #3Y

tickers <- c(
  "IWB", #R1000
  "IWD", #R1000V
  "IWF", #R1000G
  "IWM", #R2000
  "IWN", #R2000V
  "IWO", #R2000G
  "MTUM", #MSCI Momentum
  "USMV", #MSCI USA Min Vol
  "QUAL" #MSCI USA Quality
)

# Data Pulls ----

all_data <- fetch_adjusted_prices(tickers)

# Data Pre-processing ----
# note - this is a long-form dataframe indexed by `date` and `ticker` and the values are `adjusted_close`
# here we filter for sufficient price history

all_data <- all_data %>% 
  dplyr::arrange(ticker, date) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::add_count() %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n > roll_window) %>% 
  dplyr::select(-n)

return_data <- calculate_log_returns(all_data)

market_returns <- return_data %>%
  dplyr::filter(ticker == "IWB") %>%
  dplyr::select(date, market_return = return)

return_data <- return_data %>%
  dplyr::left_join(market_returns, by = "date")

# Rolling Linear Regression ----

simple_regression <- return_data %>%
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
  dplyr::filter(
    !is.na(beta)
  )

# Dataviz ----

library(ggplot2)
library(ggrepel)
library(scales)

p <- simple_regression %>%
  dplyr::filter(ticker == "USMV") %>%
  ggplot(aes(x = date, y = beta)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(data = . %>% tail(1), color = "steelblue", size = 2) +
  geom_text_repel(
    data = . %>% tail(1),
    aes(label = round(beta, 2)),
    nudge_x    = 30,
    direction  = "y",
    vjust      = 2,
    # hjust      = 1,
    segment.color = NA
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
    scale_y_continuous(
    limits = c(0.5, 1),
    breaks = seq(0.5, 1, by = 0.1)
  ) +
  labs(
    title    = "Rolling 3-Year Beta for $USMV",
    subtitle = "vs. Russell 1000 Index ($IWB)",
    x        = "",
    y        = "Beta",
    caption  = "Data: alphavantage • Chart: brrymtnc"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x    = element_blank(),   # drop vertical grid
    panel.grid.minor.x    = element_blank(),
    panel.grid.major.y    = element_line(color = "grey80"),
    panel.grid.minor.y    = element_blank(),
    plot.title            = element_text(face = "bold", size = 14),
    plot.subtitle         = element_text(size = 12),
    axis.title            = element_text(size = 11),
    plot.caption          = element_text(size = 8, color = "grey40")
  )

if (!dir.exists("images")) dir.create("images")
ggsave("images/usmv_beta.svg", plot = p, width = 8, height = 5, dpi = 320)