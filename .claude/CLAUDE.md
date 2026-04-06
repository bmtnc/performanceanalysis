# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands

```bash
# Install dependencies (renv-managed)
Rscript -e 'renv::restore()'

# Run all tests
Rscript -e 'devtools::test()'

# Run a single test file
Rscript -e 'testthat::test_file("tests/testthat/test-constrained_linear_regression.R")'

# Rebuild documentation (after editing roxygen comments)
Rscript -e 'devtools::document()'

# Load package interactively
Rscript -e 'devtools::load_all()'

# Run a script
Rscript scripts/starter_script.R
```

## Architecture

This is an R package (`performanceanalysis`) for quantitative factor-based performance analysis. It has two layers:

1. **Package functions** (`R/`) — Reusable, tested, exported via NAMESPACE. Cover data fetching (Alpha Vantage API), log-return calculation, constrained regression (via `quadprog::solve.QP`), rolling regressions, factor attribution, and Carino cumulative linking.

2. **Analysis scripts** (`scripts/`) — Driver scripts that compose package functions for specific analyses (ETF decomposition, cross-asset factor construction, benchmark comparison). Scripts are standalone with configurable parameters at the top.

### Key abstractions

- **Constrained regression pipeline:** `construct_linear_regression_constraints()` → `constrained_linear_regression()` → `roll_constrained_lm()`. The rolling version returns a structure matching `roll::roll_lm()` output so downstream code works with either.

- **Attribution pipeline:** `calculate_factor_attribution()` (daily, out-of-sample: weights from t applied to returns at t+1) → `calculate_cumulative_attribution()` (Carino log-linking to preserve additive decomposition under geometric compounding).

- **Cross-asset factor framework:** Scripts in `scripts/fetch_*.R` build value/momentum/carry/defensive factors for equities (AQR data), fixed income (FRED yields + duration approximation), and FX (FRED spot rates + interbank rates). Implementation notes and alignment results vs AQR published data are in `.claude/factor_implementation_notes.md` and `.claude/plans/`.

### Data flow

- **Equity factors:** Cached AQR downloads in `data/aqr/*.rds` (HML Devil, momentum, century premia, risk-free rate). AQR Excel files have 18 header rows (`readxl::read_excel(..., skip = 18)`).
- **FI/FX factors:** Cached FRED data in `data/fred/*.rds`. Bond returns approximated from yield changes via 1st-order duration formula.
- **ETF prices:** Fetched live from Alpha Vantage API (key via `ALPHA_VANTAGE_API_KEY` env var). Rate-limited with 1s+ delays between requests.
- **FRED API:** Key stored in `.Renviron` as `FRED_API_KEY`.

### Rolling regression conventions

- Default window: 252 trading days (~1 year)
- First `width - 1` observations produce NA coefficients
- Multi-factor models use constrained regression: `non_negative = TRUE`, `sum_to_one = TRUE`

## Dependencies

Managed by `renv`. Key packages: `dplyr`, `quadprog` (QP solver), `roll` (rolling OLS), `slider` (general sliding windows), `httr`/`jsonlite` (API calls), `rlang` (NSE), `ggplot2`/`scales` (visualization in scripts), `fredr` (FRED API in scripts), `readxl` (AQR Excel ingestion in scripts).

## Validation patterns

Functions validate inputs at entry: `validate_tickers()` for ticker vectors, `validate_df_cols()` for data frames (type + required columns), `validate_regression_predictors()` for matrix diagnostics (p >= n, constant columns, multicollinearity). Error messages include expected vs actual values.

## R Coding Conventions

### Package code (`R/`)

- **One function per file**: `R/function_name.R`
- **Explicit namespacing**: Always `dplyr::select()`, `tibble::tibble()`, etc. Never use `library()` in package code. Do NOT namespace base R or default packages (`log()`, `mean()`, `stats::lm()` are fine without prefix).
- **Roxygen2 documentation**: All exported functions need `@param`, `@return`, `@export`
- **Vectorization over loops**: Prefer vectorized operations; only use loops for sequential dependencies or progress reporting
- **Declarative style**: Use `dplyr` pipelines over imperative loops; prefer `group_by() + mutate()` over split-apply-bind

### Scripts (`scripts/`)

- `library()` calls are acceptable in scripts (unlike package code)
- Configurable parameters at the top of the script
- Standalone — should run end-to-end via `Rscript`
- **Do not save plots to files** — scripts should only `print()` plots, not use `ggsave()`. The user will save manually if needed.
