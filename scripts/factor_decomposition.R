# Script Params ----

roll_window <- 252L

tickers <- c(
    "ARKK", # ARK Innovation ETF (response variable)
    "IWD",  # R1000V
    "IWF",  # R1000G
    "IWN",  # R2000V
    "IWO",  # R2000G
    "MTUM", # MSCI Momentum
    "USMV", # MSCI USA Min Vol
    "QUAL" # MSCI USA Quality (FIXED: missing comma)
    # "IJR"   # Small Cap Quality
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
    dplyr::filter(ticker != "ARKK") %>%
    dplyr::select(date, ticker, return) %>%
    tidyr::pivot_wider(names_from = ticker, values_from = return)

arkk_returns <- return_data %>%
    dplyr::filter(ticker == "ARKK") %>%
    dplyr::select(date, arkk_return = return)

regression_data <- arkk_returns %>%
    dplyr::left_join(factor_returns, by = "date") %>%
    dplyr::filter(complete.cases(.))

# Rolling Multi-Factor Constrained Regression - UPDATED ----

# Updated factor columns to include new tickers
factor_cols <- c("IWD", "IWF", "IWN", "IWO", "MTUM", "USMV", "QUAL")

factor_decomposition <- regression_data %>%
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
        small_value = roll_res[[1]]$coefficients[, "IWN"],     # NEW
        small_growth = roll_res[[1]]$coefficients[, "IWO"],    # NEW
        momentum = roll_res[[1]]$coefficients[, "MTUM"],
        min_vol = roll_res[[1]]$coefficients[, "USMV"],
        quality = roll_res[[1]]$coefficients[, "QUAL"]
        # small_quality = roll_res[[1]]$coefficients[, "IJR"]    # NEW
    ) %>%
    dplyr::select(
        -roll_res,
        -arkk_return,
        -dplyr::all_of(factor_cols)
    ) %>%
    dplyr::filter(
        !is.na(alpha)
    )

# Dataviz - UPDATED ----

library(ggplot2)
library(scales)

# Convert to long format - UPDATED with new factors
viz_data <- factor_decomposition %>%
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
            factor == "small_value"   ~ "Small Value",      # NEW
            factor == "small_growth"  ~ "Small Growth",     # NEW
            factor == "momentum"      ~ "Momentum",
            factor == "min_vol"       ~ "Min Vol",
            factor == "quality"       ~ "Quality",
            # factor == "small_quality" ~ "Small Quality",    # NEW
            TRUE                      ~ factor
        ),
        # Updated factor order for consistent stacking
        factor = factor(factor, levels = c(
            "Large Value", "Large Growth", "Small Value", "Small Growth",
            "Momentum", "Min Vol", "Quality"
        ))
    ) %>%
    dplyr::arrange(date, factor)

p <- viz_data %>%
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
        palette = "Set3",  # Changed to Set3 for more colors
        guide = guide_legend(reverse = TRUE)
    ) +
    labs(
        title = "Rolling 1-Year Multi-Factor Decomposition of ARKK",
        subtitle = "Constrained weights (non-negative, sum to 1) - 8 Factor Model",
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

print(p)

# Save the plot
if (!dir.exists("images")) dir.create("images")
ggsave("images/arkk_factor_decomposition.svg", plot = p, width = 12, height = 6, dpi = 320)
