# Script Parameters ----

roll_window <- 756L # 3Y

# Define tickers
spy_ticker <- "SPY"

sector_etfs <- c(
    "XLK",  # Technology
    "XLF",  # Financial
    "XLV",  # Health Care
    "XLI",  # Industrial
    "XLY",  # Consumer Discretionary
    "XLP",  # Consumer Staples
    "XLE",  # Energy
    "XLB",  # Materials
    "XLU",  # Utilities
    "XLRE", # Real Estate
    "XLC"   # Communication Services
)

style_factors <- c(
    "IWD",  # Large Value
    "IWF",  # Large Growth
    "IWN",  # Small Value
    "IWO",  # Small Growth
    "MTUM", # Momentum
    "USMV", # Min Vol
    "QUAL"  # Quality
)

all_tickers <- c(spy_ticker, sector_etfs, style_factors)

# Data Pulls ----

all_data <- fetch_adjusted_prices(all_tickers)

# Data Pre-processing ----

all_data <- all_data %>% 
    dplyr::arrange(ticker, date) %>% 
    dplyr::group_by(ticker) %>% 
    dplyr::add_count() %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(n > roll_window) %>% 
    dplyr::select(-n)

return_data <- calculate_log_returns(all_data)

# Prepare data for regressions ----

# SPY returns
spy_returns <- return_data %>%
    dplyr::filter(ticker == spy_ticker) %>%
    dplyr::select(date, spy_return = return)

# Sector ETF returns (wide format)
sector_returns <- return_data %>%
    dplyr::filter(ticker %in% sector_etfs) %>%
    dplyr::select(date, ticker, return) %>%
    tidyr::pivot_wider(names_from = ticker, values_from = return)

# Style factor returns (wide format)
style_returns <- return_data %>%
    dplyr::filter(ticker %in% style_factors) %>%
    dplyr::select(date, ticker, return) %>%
    tidyr::pivot_wider(names_from = ticker, values_from = return)

# Combine all data
regression_data <- spy_returns %>%
    dplyr::left_join(sector_returns, by = "date") %>%
    dplyr::left_join(style_returns, by = "date") %>%
    dplyr::filter(complete.cases(.))

# Step 1: SPY decomposition into sector ETFs ----

spy_sector_weights <- regression_data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
        roll_res = list(roll_constrained_lm(
            x = as.matrix(dplyr::select(., dplyr::all_of(sector_etfs))),
            y = spy_return,
            width = roll_window,
            non_negative = TRUE,
            sum_to_one = TRUE
        ))
    ) %>%
    dplyr::mutate(
        alpha = roll_res[[1]]$coefficients[, "(Intercept)"],
        XLK = roll_res[[1]]$coefficients[, "XLK"],
        XLF = roll_res[[1]]$coefficients[, "XLF"],
        XLV = roll_res[[1]]$coefficients[, "XLV"],
        XLI = roll_res[[1]]$coefficients[, "XLI"],
        XLY = roll_res[[1]]$coefficients[, "XLY"],
        XLP = roll_res[[1]]$coefficients[, "XLP"],
        XLE = roll_res[[1]]$coefficients[, "XLE"],
        XLB = roll_res[[1]]$coefficients[, "XLB"],
        XLU = roll_res[[1]]$coefficients[, "XLU"],
        XLRE = roll_res[[1]]$coefficients[, "XLRE"],
        XLC = roll_res[[1]]$coefficients[, "XLC"]
    ) %>%
    dplyr::select(date, alpha, dplyr::all_of(sector_etfs)) %>%
    dplyr::filter(!is.na(alpha))

# Step 2: Style factor loadings for each sector ETF ----

# Function to calculate style loadings for a single sector
calculate_sector_style_loadings <- function(sector_name, data) {
    sector_data <- data %>%
        dplyr::select(date, sector_return = !!rlang::sym(sector_name), dplyr::all_of(style_factors))
    
    result <- sector_data %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
            roll_res = list(roll_constrained_lm(
                x = as.matrix(dplyr::select(., dplyr::all_of(style_factors))),
                y = sector_return,
                width = roll_window,
                non_negative = TRUE,
                sum_to_one = TRUE
            ))
        ) %>%
        dplyr::mutate(
            sector = sector_name,
            alpha = roll_res[[1]]$coefficients[, "(Intercept)"],
            IWD = roll_res[[1]]$coefficients[, "IWD"],
            IWF = roll_res[[1]]$coefficients[, "IWF"],
            IWN = roll_res[[1]]$coefficients[, "IWN"],
            IWO = roll_res[[1]]$coefficients[, "IWO"],
            MTUM = roll_res[[1]]$coefficients[, "MTUM"],
            USMV = roll_res[[1]]$coefficients[, "USMV"],
            QUAL = roll_res[[1]]$coefficients[, "QUAL"]
        ) %>%
        dplyr::select(date, sector, alpha, dplyr::all_of(style_factors)) %>%
        dplyr::filter(!is.na(alpha))
    
    return(result)
}

# Calculate style loadings for all sectors
sector_style_loadings <- lapply(sector_etfs, calculate_sector_style_loadings, regression_data)
sector_style_loadings <- dplyr::bind_rows(sector_style_loadings)

# Step 3: SPY's direct style factor loadings ----

spy_style_loadings <- regression_data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
        roll_res = list(roll_constrained_lm(
            x = as.matrix(dplyr::select(., dplyr::all_of(style_factors))),
            y = spy_return,
            width = roll_window,
            non_negative = TRUE,
            sum_to_one = TRUE
        ))
    ) %>%
    dplyr::mutate(
        alpha = roll_res[[1]]$coefficients[, "(Intercept)"],
        IWD = roll_res[[1]]$coefficients[, "IWD"],
        IWF = roll_res[[1]]$coefficients[, "IWF"],
        IWN = roll_res[[1]]$coefficients[, "IWN"],
        IWO = roll_res[[1]]$coefficients[, "IWO"],
        MTUM = roll_res[[1]]$coefficients[, "MTUM"],
        USMV = roll_res[[1]]$coefficients[, "USMV"],
        QUAL = roll_res[[1]]$coefficients[, "QUAL"]
    ) %>%
    dplyr::select(date, alpha, dplyr::all_of(style_factors)) %>%
    dplyr::filter(!is.na(alpha))

# Step 4: Calculate sector contributions to SPY's factor loadings ----

# For each style factor, calculate each sector's contribution
calculate_factor_contributions <- function(factor_name, weights_data, loadings_data) {
    
    # Get sector weights for this date range
    weights_subset <- weights_data %>%
        dplyr::select(date, dplyr::all_of(sector_etfs))
    
    # Get factor loadings for this factor
    loadings_subset <- loadings_data %>%
        dplyr::filter(sector %in% sector_etfs) %>%
        dplyr::select(date, sector, factor_loading = !!rlang::sym(factor_name)) %>%
        tidyr::pivot_wider(names_from = sector, values_from = factor_loading, names_prefix = "loading_")
    
    # Calculate contributions: weight × loading for each sector
    contributions <- weights_subset %>%
        dplyr::left_join(loadings_subset, by = "date") %>%
        dplyr::filter(complete.cases(.)) %>%
        dplyr::mutate(
            factor = factor_name,
            XLK_contrib = XLK * loading_XLK,
            XLF_contrib = XLF * loading_XLF,
            XLV_contrib = XLV * loading_XLV,
            XLI_contrib = XLI * loading_XLI,
            XLY_contrib = XLY * loading_XLY,
            XLP_contrib = XLP * loading_XLP,
            XLE_contrib = XLE * loading_XLE,
            XLB_contrib = XLB * loading_XLB,
            XLU_contrib = XLU * loading_XLU,
            XLRE_contrib = XLRE * loading_XLRE,
            XLC_contrib = XLC * loading_XLC
        ) %>%
        dplyr::select(date, factor, dplyr::ends_with("_contrib"))
    
    return(contributions)
}

# Calculate contributions for all style factors
factor_contributions <- lapply(style_factors, calculate_factor_contributions, 
                              spy_sector_weights, sector_style_loadings)
factor_contributions <- dplyr::bind_rows(factor_contributions)

# Step 5: Visualization ----

# Focus on Large Growth (IWF) factor as example
growth_contributions <- factor_contributions %>%
    dplyr::filter(factor == "IWF") %>%
    dplyr::select(-factor) %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("_contrib"), 
                       names_to = "sector", 
                       values_to = "contribution") %>%
    dplyr::mutate(
        sector = stringr::str_remove(sector, "_contrib"),
        sector = dplyr::case_when(
            sector == "XLK" ~ "Technology",
            sector == "XLF" ~ "Financial",
            sector == "XLV" ~ "Health Care",
            sector == "XLI" ~ "Industrial",
            sector == "XLY" ~ "Consumer Discr",
            sector == "XLP" ~ "Consumer Staples",
            sector == "XLE" ~ "Energy",
            sector == "XLB" ~ "Materials",
            sector == "XLU" ~ "Utilities",
            sector == "XLRE" ~ "Real Estate",
            sector == "XLC" ~ "Communication",
            TRUE ~ sector
        )
    )

# Create visualization
p <- growth_contributions %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = contribution, fill = sector)) +
    ggplot2::geom_area(position = ggplot2::position_stack(), alpha = 0.85) +
    ggplot2::scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y",
        expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0)
    ) +
    ggplot2::scale_fill_brewer(type = "qual", palette = "Set3") +
    ggplot2::labs(
        title = "Sector Contributions to SPY's Large Growth Factor Loading",
        subtitle = "Rolling 3-Year Analysis: How much of SPY's growth tilt comes from each sector",
        x = "", y = "Contribution to Growth Loading",
        caption = "Data: alphavantage • Analysis: Constrained factor decomposition"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        legend.position = "bottom",
        legend.title = ggplot2::element_blank()
    )

print(p)

# Save the plot
if (!dir.exists("images")) dir.create("images")
ggplot2::ggsave("images/spy_growth_sector_contributions.svg", plot = p, width = 12, height = 8, dpi = 320)

# Summary statistics for latest period
latest_date <- max(growth_contributions$date)
latest_contributions <- growth_contributions %>%
    dplyr::filter(date == latest_date) %>%
    dplyr::arrange(dplyr::desc(contribution))

print("Latest Growth Factor Contributions by Sector:")
print(latest_contributions)


# Focus on Large Value (IWD) factor instead of Growth
value_contributions <- factor_contributions %>%
    dplyr::filter(factor == "IWD") %>%
    dplyr::select(-factor) %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("_contrib"), 
                       names_to = "sector", 
                       values_to = "contribution") %>%
    dplyr::mutate(
        sector = stringr::str_remove(sector, "_contrib"),
        sector = dplyr::case_when(
            sector == "XLK" ~ "Technology",
            sector == "XLF" ~ "Financial",
            sector == "XLV" ~ "Health Care",
            sector == "XLI" ~ "Industrial",
            sector == "XLY" ~ "Consumer Discr",
            sector == "XLP" ~ "Consumer Staples",
            sector == "XLE" ~ "Energy",
            sector == "XLB" ~ "Materials",
            sector == "XLU" ~ "Utilities",
            sector == "XLRE" ~ "Real Estate",
            sector == "XLC" ~ "Communication",
            TRUE ~ sector
        )
    )

# Create visualization for Value factor
p_value <- value_contributions %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = contribution, fill = sector)) +
    ggplot2::geom_area(position = ggplot2::position_stack(), alpha = 0.85) +
    ggplot2::scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y",
        expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0)
    ) +
    ggplot2::scale_fill_brewer(type = "qual", palette = "Set3") +
    ggplot2::labs(
        title = "Sector Contributions to SPY's Large Value Factor Loading",
        subtitle = "Rolling 3-Year Analysis: How much of SPY's value tilt comes from each sector",
        x = "", y = "Contribution to Value Loading",
        caption = "Data: alphavantage • Analysis: Constrained factor decomposition"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        legend.position = "bottom",
        legend.title = ggplot2::element_blank()
    )

print(p_value)

# Save the plot
ggplot2::ggsave("images/spy_value_sector_contributions.svg", plot = p_value, width = 12, height = 8, dpi = 320)

# Summary statistics for latest period - Value factor
latest_date <- max(value_contributions$date)
latest_value_contributions <- value_contributions %>%
    dplyr::filter(date == latest_date) %>%
    dplyr::arrange(dplyr::desc(contribution))

print("Latest Value Factor Contributions by Sector:")
print(latest_value_contributions)

# Validation for Value factor
spy_direct_value <- spy_style_loadings %>% 
    dplyr::filter(date == latest_date) %>% 
    dplyr::pull(IWD)

sector_value_contribution_sum <- sum(latest_value_contributions$contribution)

print(paste("SPY Direct Value Loading:", round(spy_direct_value, 3)))
print(paste("Sum of Sector Contributions:", round(sector_value_contribution_sum, 3)))
print(paste("Explanation Ratio:", round(sector_value_contribution_sum/spy_direct_value, 3)))

# ... existing code above ...

# Focus on Momentum (MTUM) factor
momentum_contributions <- factor_contributions %>%
    dplyr::filter(factor == "MTUM") %>%
    dplyr::select(-factor) %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("_contrib"), 
                       names_to = "sector", 
                       values_to = "contribution") %>%
    dplyr::mutate(
        sector = stringr::str_remove(sector, "_contrib"),
        sector = dplyr::case_when(
            sector == "XLK" ~ "Technology",
            sector == "XLF" ~ "Financial",
            sector == "XLV" ~ "Health Care",
            sector == "XLI" ~ "Industrial",
            sector == "XLY" ~ "Consumer Discr",
            sector == "XLP" ~ "Consumer Staples",
            sector == "XLE" ~ "Energy",
            sector == "XLB" ~ "Materials",
            sector == "XLU" ~ "Utilities",
            sector == "XLRE" ~ "Real Estate",
            sector == "XLC" ~ "Communication",
            TRUE ~ sector
        )
    )

# Create momentum visualization
p_momentum <- momentum_contributions %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = contribution, fill = sector)) +
    ggplot2::geom_area(position = ggplot2::position_stack(), alpha = 0.85) +
    ggplot2::scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y",
        expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0)
    ) +
    ggplot2::scale_fill_brewer(type = "qual", palette = "Set3") +
    ggplot2::labs(
        title = "Sector Contributions to SPY's Momentum Factor Loading",
        subtitle = "Rolling 3-Year Analysis: How much of SPY's momentum tilt comes from each sector",
        x = "", y = "Contribution to Momentum Loading",
        caption = "Data: alphavantage • Analysis: Constrained factor decomposition"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        legend.position = "bottom",
        legend.title = ggplot2::element_blank()
    )

print(p_momentum)

# Save the momentum plot
ggplot2::ggsave("images/spy_momentum_sector_contributions.svg", plot = p_momentum, width = 12, height = 8, dpi = 320)

# Summary statistics for latest period
latest_date_momentum <- max(momentum_contributions$date)
latest_momentum_contributions <- momentum_contributions %>%
    dplyr::filter(date == latest_date_momentum) %>%
    dplyr::arrange(dplyr::desc(contribution))

print("Latest Momentum Factor Contributions by Sector:")
print(latest_momentum_contributions)