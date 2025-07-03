# Performance Analysis of ETF Returns

This R project fetches adjusted prices for a set of ETFs, computes daily log-returns, and applies a rolling linear regression against a market benchmark (IWB) to estimate alpha and beta over time.

## Features

- Fetches adjusted closing prices via `fetch_adjusted_prices()`
- Cleans and filters tickers for sufficient history
- Calculates daily log-returns
- Isolates market returns (IWB) and merges with each ticker
- Performs rolling linear regressions (window default: 36 trading days) to derive:
  - **alpha**: rolling intercept
  - **beta**: rolling slope against market return

## Project Structure

.Rbuildignore      # Files to exclude from build  
.Rprofile          # R session options  
DESCRIPTION        # Package metadata  
NAMESPACE          # Exported functions  
performanceanalysis.Rproj  
renv/              # Reproducible environment  
man/               # Documentation .Rd files  
R/                 # Source code  
tests/             # Test suite  
scripts/           # Example/driver scripts  

## Installation

1. Clone repo  
2. Initialize using renv and install dependencies from the lockfile:
   ```sh
   Rscript -e "renv::init()"
   Rscript -e "renv::restore()"
   ```  


## Rolling Regression Details

The idea of estimating an asset’s systematic risk through a linear regression dates back to the early work on the Capital Asset Pricing Model (CAPM) in the late 1950s and 1960s by Sharpe, Lintner, and Mossin. In that framework, an individual stock’s excess return is regressed on the market’s excess return to obtain  
- **beta**: the slope, measuring sensitivity to market movements  
- **alpha**: the intercept, representing risk-adjusted outperformance  

We model returns rather than raw prices because prices are typically non-stationary and exhibit trends and heteroskedasticity. Returns (especially log-returns) are approximately homoscedastic and additive over time, satisfying the OLS assumptions more closely and making coefficients comparable across assets and periods.

## Our Implementation

Starting from `all_data`, a long-form df with columns:
- `date`  
- `ticker`  
- `adjusted_close`  

we first compute daily log-returns:
```r
return_data <- all_data %>%
  dplyr::arrange(ticker, date) %>%
  dplyr::group_by(ticker) %>%
  dplyr::mutate(return = log(adjusted_close / dplyr::lag(adjusted_close))) %>%
  dplyr::ungroup()
```
We isolate the market benchmark (the $IWB Russell 1000 ETF) returns and join back so each row has both an asset return and `market_return`. Then, for each `ticker` group, we apply a sliding‐window OLS:
```r
linreg <- roll::roll_lm(
  x     = market_return,
  y     = return,
  width = roll_window  # e.g. 252 days
)
alpha <-  linreg$coefficients[, "(Intercept)"]
beta  <-  linreg$coefficients[, "x1"]
```

This yields time-series of rolling alpha and beta estimates for each ETF.

## Rolling 3-Year Beta for USMV

![Rolling 3-Year Beta for USMV](images/usmv_beta.svg)
