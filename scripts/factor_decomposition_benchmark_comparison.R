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
    "QUAL" # MSCI USA Quality
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

# Prepare factor returns (exclude both ARKK and IWR from factors)
factor_returns <- return_data %>%
    dplyr::filter(!ticker %in% c("ARKK", "IWR")) %>%
    dplyr::select(date, ticker, return) %>%
    tidyr::pivot_wider(names_from = ticker, values_from = return)

# Prepare ARKK returns
arkk_returns <- return_data %>%
    dplyr::filter(ticker == "ARKK") %>%
    dplyr::select(date, arkk_return = return)

# Prepare IWR returns
iwr_returns <- return_data %>%
    dplyr::filter(ticker == "IWR") %>%
    dplyr::select(date, iwr_return = return)

# Create regression datasets
arkk_regression_data <- arkk_returns %>%
    dplyr::left_join(factor_returns, by = "date") %>%
    dplyr::filter(complete.cases(.))

iwr_regression_data <- iwr_returns %>%
    dplyr::left_join(factor_returns, by = "date") %>%
    dplyr::filter(complete.cases(.))

# Rolling Multi-Factor Constrained Regression - ARKK ----

factor_cols <- c("IWD", "IWF", "IWN", "IWO", "MTUM", "USMV", "QUAL")

arkk_decomposition <- arkk_regression_data %>%
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
        small_value = roll_res[[1]]$coefficients[, "IWN"],
        small_growth = roll_res[[1]]$coefficients[, "IWO"],
        momentum = roll_res[[1]]$coefficients[, "MTUM"],
        min_vol = roll_res[[1]]$coefficients[, "USMV"],
        quality = roll_res[[1]]$coefficients[, "QUAL"]
        # small_quality = roll_res[[1]]$coefficients[, "IJR"]
    ) %>%
    dplyr::select(
        date,
        alpha,
        large_value,
        large_growth,
        small_value,
        small_growth,
        momentum,
        min_vol,
        quality
        # small_quality
    ) %>%
    dplyr::filter(!is.na(alpha))

# Rolling Multi-Factor Constrained Regression - IWR ----

iwr_decomposition <- iwr_regression_data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
        roll_res = list(roll_constrained_lm(
            x = as.matrix(dplyr::select(., dplyr::all_of(factor_cols))),
            y = iwr_return,
            width = roll_window,
            non_negative = TRUE,
            sum_to_one = TRUE,
            intercept = TRUE
        )),
        alpha = roll_res[[1]]$coefficients[, "(Intercept)"],
        large_value = roll_res[[1]]$coefficients[, "IWD"],
        large_growth = roll_res[[1]]$coefficients[, "IWF"],
        small_value = roll_res[[1]]$coefficients[, "IWN"],
        small_growth = roll_res[[1]]$coefficients[, "IWO"],
        momentum = roll_res[[1]]$coefficients[, "MTUM"],
        min_vol = roll_res[[1]]$coefficients[, "USMV"],
        quality = roll_res[[1]]$coefficients[, "QUAL"]
        # small_quality = roll_res[[1]]$coefficients[, "IJR"]
    ) %>%
    dplyr::select(
        date,
        alpha,
        large_value,
        large_growth,
        small_value,
        small_growth,
        momentum,
        min_vol,
        quality
        # small_quality
    ) %>%
    dplyr::filter(!is.na(alpha))

# Calculate Differences ----

factor_differences <- arkk_decomposition %>%
    dplyr::inner_join(
        iwr_decomposition,
        by = "date",
        suffix = c("_arkk", "_iwr")
    ) %>%
    dplyr::mutate(
        large_value_diff = large_value_arkk - large_value_iwr,
        large_growth_diff = large_growth_arkk - large_growth_iwr,
        small_value_diff = small_value_arkk - small_value_iwr,
        small_growth_diff = small_growth_arkk - small_growth_iwr,
        momentum_diff = momentum_arkk - momentum_iwr,
        min_vol_diff = min_vol_arkk - min_vol_iwr,
        quality_diff = quality_arkk - quality_iwr,
        # small_quality_diff = small_quality_arkk - small_quality_iwr,
        alpha_diff = alpha_arkk - alpha_iwr
    ) %>%
    dplyr::select(
        date,
        large_value_diff,
        large_growth_diff,
        small_value_diff,
        small_growth_diff,
        momentum_diff,
        min_vol_diff,
        quality_diff,
        # small_quality_diff,
        alpha_diff
    )

# Diagnostics ----

message(paste0("Total observations in factor_differences: ", nrow(factor_differences)))
message(paste0("Date range: ", min(factor_differences$date), " to ", max(factor_differences$date)))

# Check for NA values in differences
na_counts <- factor_differences %>%
    dplyr::summarise(
        large_value = sum(is.na(large_value_diff)),
        large_growth = sum(is.na(large_growth_diff)),
        small_value = sum(is.na(small_value_diff)),
        small_growth = sum(is.na(small_growth_diff)),
        momentum = sum(is.na(momentum_diff)),
        min_vol = sum(is.na(min_vol_diff)),
        quality = sum(is.na(quality_diff))
        # small_quality = sum(is.na(small_quality_diff))
    )

message("NA counts by factor:")
print(na_counts)

# Check for values outside [-1, 1]
range_violations <- factor_differences %>%
    dplyr::filter(
        abs(large_value_diff) > 1 |
        abs(large_growth_diff) > 1 |
        abs(small_value_diff) > 1 |
        abs(small_growth_diff) > 1 |
        abs(momentum_diff) > 1 |
        abs(min_vol_diff) > 1 |
        abs(quality_diff) > 1 
        # abs(small_quality_diff) > 1
    )

if (nrow(range_violations) > 0) {
    message(paste0("\nWARNING: ", nrow(range_violations), " dates have factor differences outside [-1, 1]"))
    message("Sample of problematic dates:")
    print(head(range_violations))
    
    # Show which factors violate the range
    range_violations_long <- range_violations %>%
        tidyr::pivot_longer(
            cols = ends_with("_diff"),
            names_to = "factor",
            values_to = "difference"
        ) %>%
        dplyr::filter(abs(difference) > 1)
    
    message("\nFactors violating [-1, 1] range:")
    print(table(range_violations_long$factor))
}

# Materiality Filter ----

materiality_threshold <- 0.05

max_differences <- factor_differences %>%
    dplyr::summarise(
        large_value_max = max(abs(large_value_diff), na.rm = TRUE),
        large_growth_max = max(abs(large_growth_diff), na.rm = TRUE),
        small_value_max = max(abs(small_value_diff), na.rm = TRUE),
        small_growth_max = max(abs(small_growth_diff), na.rm = TRUE),
        momentum_max = max(abs(momentum_diff), na.rm = TRUE),
        min_vol_max = max(abs(min_vol_diff), na.rm = TRUE),
        quality_max = max(abs(quality_diff), na.rm = TRUE)
        # small_quality_max = max(abs(small_quality_diff), na.rm = TRUE)
    )

message("\nMaximum absolute differences by factor:")
print(max_differences)

material_factors <- c()
if (max_differences$large_value_max >= materiality_threshold) material_factors <- c(material_factors, "large_value_diff")
if (max_differences$large_growth_max >= materiality_threshold) material_factors <- c(material_factors, "large_growth_diff")
if (max_differences$small_value_max >= materiality_threshold) material_factors <- c(material_factors, "small_value_diff")
if (max_differences$small_growth_max >= materiality_threshold) material_factors <- c(material_factors, "small_growth_diff")
if (max_differences$momentum_max >= materiality_threshold) material_factors <- c(material_factors, "momentum_diff")
if (max_differences$min_vol_max >= materiality_threshold) material_factors <- c(material_factors, "min_vol_diff")
if (max_differences$quality_max >= materiality_threshold) material_factors <- c(material_factors, "quality_diff")
# if (max_differences$small_quality_max >= materiality_threshold) material_factors <- c(material_factors, "small_quality_diff")

message(paste0("\nFactors exceeding ", materiality_threshold * 100, "% materiality threshold:"))
message(paste(material_factors, collapse = ", "))

# Dataviz ----

library(ggplot2)
library(scales)

viz_data <- factor_differences %>%
    tidyr::pivot_longer(
        cols = c(
            large_value_diff,
            large_growth_diff,
            small_value_diff,
            small_growth_diff,
            momentum_diff,
            min_vol_diff,
            quality_diff
            # small_quality_diff
        ),
        names_to = "factor",
        values_to = "difference"
    ) %>%
    dplyr::filter(factor %in% material_factors) %>%
    dplyr::mutate(
        factor = dplyr::case_when(
            factor == "large_value_diff" ~ "LCV",
            factor == "large_growth_diff" ~ "LCG",
            factor == "small_value_diff" ~ "SCV",
            factor == "small_growth_diff" ~ "SCG",
            factor == "momentum_diff" ~ "Momo",
            factor == "min_vol_diff" ~ "Min Vol",
            factor == "quality_diff" ~ "Quality",
            # factor == "small_quality_diff" ~ "Small Quality",
            TRUE ~ factor
        )
    )

factor_order <- viz_data %>%
    dplyr::group_by(factor) %>%
    dplyr::summarise(mean_diff = mean(difference, na.rm = TRUE)) %>%
    dplyr::arrange(desc(mean_diff)) %>%
    dplyr::pull(factor)

message("\nFactor ordering (by mean difference, highest to lowest):")
print(factor_order)

viz_data <- viz_data %>%
    dplyr::mutate(factor = factor(factor, levels = factor_order))

factor_colors <- c(
    "Large Value" = "#fc8d62",
    "LCG" = "#8da0cb",
    "SCV" = "#66c2a5",
    "SCG" = "#e78ac3",
    "Momo" = "#ffd92f",
    "Min Vol" = "#e5c494",
    "Quality" = "#b3b3b3"
    # "Small Quality" = "#a6d854"
)

p <- viz_data %>%
    ggplot(aes(x = date, y = difference, fill = factor, color = factor)) +
    geom_col(width = 1, alpha = 0.85, linewidth = 0.2) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    facet_grid(
        rows = vars(factor),
        scales = "fixed"
    ) +
    scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y",
        expand = c(0, 0)
    ) +
    scale_y_continuous(
        limits = c(-1, 1),
        breaks = seq(-1, 1, by = 0.5),
        labels = scales::percent_format(),
        expand = c(0, 0),
        oob = scales::squish
    ) +
    scale_fill_manual(
        values = factor_colors,
        guide = "none"
    ) +
    scale_color_manual(
        values = factor_colors,
        guide = "none"
    ) +
    labs(
        title = "ARKK vs Russell Midcap Index: Factor Loading Differences Over Time",
        subtitle = "Rolling 1-year constrained regression (ARKK loading - RMid loading)",
        x = "",
        y = "Loading Difference",
        caption = "Data: alphavantage • Chart: brrymtnc"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "grey40", margin = margin(t = 10)),
      axis.title.y = element_text(size = 10),
      strip.text = element_text(face = "bold", size = 10, hjust = 0.5),
      strip.background = element_blank(),
      panel.spacing.y = unit(1.5, "lines"),
      axis.text.x = element_text(angle = 0)
    )

print(p)

# Save the plot
if (!dir.exists("images")) dir.create("images")
ggsave(
    "images/arkk_iwr_factor_differences.svg",
    plot = p,
    width = 12,
    height = 10,
    dpi = 320
)
