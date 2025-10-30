# Script Params ----

roll_window <- 252L

tickers <- c(
    "IWV",  # Russell 3000 (target variable)
    "IWD",  # Russell 1000 Value
    "IWF",  # Russell 1000 Growth
    "IWN",  # Russell 2000 Value
    "IWO"   # Russell 2000 Growth
)

# Data Pulls ----

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

factor_returns <- return_data %>%
    dplyr::filter(ticker %in% c("IWD", "IWF", "IWN", "IWO")) %>%
    dplyr::select(date, ticker, return) %>%
    tidyr::pivot_wider(names_from = ticker, values_from = return)

iwv_returns <- return_data %>%
    dplyr::filter(ticker == "IWV") %>%
    dplyr::select(date, iwv_return = return)

regression_data <- iwv_returns %>%
    dplyr::left_join(factor_returns, by = "date") %>%
    dplyr::filter(complete.cases(.))

# Rolling 4-Factor Constrained Regression ----

factor_cols <- c("IWD", "IWF", "IWN", "IWO")

factor_decomposition <- regression_data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
        roll_res = list(roll_constrained_lm(
            x = as.matrix(dplyr::select(., dplyr::all_of(factor_cols))),
            y = iwv_return,
            width = roll_window,
            non_negative = TRUE,
            sum_to_one = TRUE
        )),
        alpha = roll_res[[1]]$coefficients[, "(Intercept)"],
        large_value = roll_res[[1]]$coefficients[, "IWD"],
        large_growth = roll_res[[1]]$coefficients[, "IWF"],
        small_value = roll_res[[1]]$coefficients[, "IWN"],
        small_growth = roll_res[[1]]$coefficients[, "IWO"]
    ) %>%
    dplyr::select(
        -roll_res,
        -iwv_return,
        -dplyr::all_of(factor_cols)
    ) %>%
    dplyr::filter(
        !is.na(alpha)
    )

# Dataviz ----

library(ggplot2)
library(scales)

# Convert to long format for stacked area chart
viz_data <- factor_decomposition %>%
    tidyr::pivot_longer(
        cols = c(large_value, large_growth, small_value, small_growth),
        names_to = "factor",
        values_to = "weight"
    ) %>%
    dplyr::mutate(
        factor = dplyr::case_when(
            factor == "large_value"  ~ "Large Value",
            factor == "large_growth" ~ "Large Growth",
            factor == "small_value"  ~ "Small Value",
            factor == "small_growth" ~ "Small Growth",
            TRUE ~ factor
        ),
        factor = factor(factor, levels = c("Large Value", "Large Growth", "Small Value", "Small Growth"))
    ) %>%
    dplyr::arrange(date, factor)

# Stacked area chart
p_area <- viz_data %>%
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
    scale_fill_manual(
        values = c(
            "Large Value" = "#1f77b4",
            "Large Growth" = "#ff7f0e", 
            "Small Value" = "#2ca02c",
            "Small Growth" = "#d62728"
        ),
        guide = guide_legend(reverse = TRUE)
    ) +
    labs(
        title = "Rolling 1-Year Style & Size Decomposition of Russell 3000",
        subtitle = "Constrained weights (non-negative, sum to 1) - 4 Factor Model",
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

print(p_area)

# Save the plot
if (!dir.exists("images")) dir.create("images")
ggsave("images/russell_3000_factor_decomposition.svg", plot = p_area, width = 12, height = 6, dpi = 320)
