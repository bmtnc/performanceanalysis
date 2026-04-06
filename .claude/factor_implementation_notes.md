# Factor Implementation Notes

## Overview

This project implements cross-asset factor premia (value, momentum, carry, defensive) across Fixed Income, Currencies, and Equities. The methodology follows Ilmanen, Israel, Lee, Moskowitz, and Thapar (2021), "How Do Factor Premia Vary Over Time? A Century of Evidence," *Journal of Investment Management*, Vol. 19, No. 4. That paper documents four factors across six asset classes with data as far back as 1877.

We build FI and FX factors from scratch using free public data (FRED, OECD). For equities, we use AQR's published pre-built factor returns. Factor returns from AQR's "Century of Factor Premia" dataset serve as the validation benchmark throughout.

---

## Validation Summary

Final correlations between our factor returns and AQR's published data, after all fixes:

| Factor | Correlation | Our Ann. Mean | Our Ann. Vol | Our SR | Status |
|---|---|---|---|---|---|
| **FX Carry** | **0.91** | +3.24% | 7.66% | 0.42 | Excellent replication |
| **FX Momentum** | **0.83** | +0.76% | 7.84% | 0.10 | Strong replication |
| **FX Value** | **0.76** | +2.94% | 7.12% | 0.41 | Good replication |
| **FI Value** | **0.56** | +2.41% | 3.35% | 0.72 | Moderate — data-constrained |
| **FI Momentum** | **0.44** | -0.49% | 5.33% | -0.09 | Moderate — data-constrained |
| **FI Carry** | **0.38** | -0.10% | 5.01% | -0.02 | Moderate — data-constrained |
| **FI Defensive** | **0.33** | -0.16% | 5.24% | -0.03 | Moderate — data-constrained |
| **Equity factors** | **1.00** | — | — | — | AQR pre-built (identical) |

FX factors achieve 0.76–0.91 correlation — strong replications given the data constraints. FI factors are structurally limited to ~0.35–0.56 by the underlying yield data source (see Fixed Income section below). Equity factors are AQR's own data and match by definition.

---

## Asset Classes and Data Sources

### Fixed Income

**AQR's approach:** 10-year government bond total returns for ~13 developed market countries, sourced from Global Financial Data (GFD). GFD provides actual bond total return indices — price appreciation plus coupon income — computed from traded sovereign bonds going back decades.

**Our approach:** 10-year government bond yields from FRED (OECD series `IRLTLT01xxM156N`) for 15 countries: US, UK, Germany, Japan, France, Canada, Australia, Italy, Netherlands, Sweden, Belgium, Switzerland, Denmark, Norway, New Zealand. Monthly total returns are derived from yield changes using the exact constant-maturity par bond repricing formula (Swinkels 2019):

```
r_t = (y_{t-1} / 12)
    + (y_{t-1} / y_t) * [1 - 1/(1 + y_t/2)^(2*(10 - 1/12))]
    + 1/(1 + y_t/2)^(2*(10 - 1/12))
    - 1
```

This captures the full nonlinear price-yield relationship, convexity effects, and partial-period roll-down. It assumes a par bond at month start repriced at month end with 9.917 years remaining maturity at the new yield.

**Why this is the binding constraint on FI factors:** The OECD `IRLTLT01` series are "harmonized" benchmark yields — they may not track the exact same instruments as GFD's total return indices. Different countries may report on-the-run vs off-the-run bonds, use different interpolation methods, or reference different auction cycles. The equal-weighted market return from our yields correlates only ~0.72 with AQR's FI market return; if the underlying data were identical, this would be >0.95. This data source gap limits all four FI factor correlations to a ceiling of approximately 0.35–0.56, regardless of methodology improvements.

**Why we can't fix this with FRED:** We conducted a comprehensive search of FRED for government bond total return indices. FRED hosts 193 ICE BofA (formerly BAML) bond index series, but every single one is corporate credit, high yield, or emerging markets. Zero sovereign/government bond total return indices exist on FRED for any country. Actual government bond TR indices (ICE BofA, Bloomberg Barclays, FTSE WGBI) require paid terminals (Bloomberg, Refinitiv, ICE Data Services).

**Additional FI data:** CPI index levels for 14 countries from FRED (`{ISO3}CPIALLMINMEI` series), used for the value signal's inflation proxy. Australia CPI is quarterly-only on FRED (`AUSCPIALLQINMEI`); we step-interpolate to monthly by carrying forward quarterly values. New Zealand CPI was unavailable from FRED, so NZ is excluded from the value signal.

### Currencies (FX)

**AQR's approach:** ~20 exchange rates including G10 currencies plus 10 legacy European currencies (DEM, FRF, ITL, NLG, BEF, ESP, FIM, ATS, IEP, PTE) pre-1999. Point-in-time (end-of-month) spot and forward exchange rates from Bloomberg and Citi. 3-month ICE LIBOR or local equivalent interbank rates. PPP exchange rates from Penn World Tables.

**Our approach:** 9 G10 currencies vs USD (AUD, CAD, CHF, EUR, GBP, JPY, NOK, NZD, SEK). Data sources:

- **Exchange rates:** FRED daily series (H.10 release, `DEX` prefix), taking the last business day of each month for end-of-month values. All rates normalized to "foreign currency per 1 USD" convention (AUD, EUR, GBP, NZD are inverted from their native USD-per-foreign quoting).
- **Interest rates:** 3-month interbank rates from FRED (OECD series `IR3TIB01xxM156N`). Japan's interbank series starts only in April 2002; we splice with the CD rate (`IR3TCD01JPM156N`, available from 1979) to extend history.
- **PPP exchange rates:** Annual PPP rates from the OECD SDMX API (SNA Table 4, `PPP_B1GQ` — PPP for GDP), free and without API key. Interpolated to monthly using relative CPI drift: `PPP_{monthly} = PPP_{annual} * (CPI_foreign_m / CPI_foreign_Jan) / (CPI_US_m / CPI_US_Jan)`.
- **CPI:** Monthly index levels from FRED (`{ISO3}CPIALLMINMEI`, Eurozone via `CP0000EZ19M086NEST`). Australia and New Zealand are quarterly, step-interpolated to monthly.

**Key differences from AQR:**

| Dimension | AQR | Ours | Impact |
|---|---|---|---|
| Currency universe | ~20 (G10 + legacy EUR) | 9 (G10 only) | ~10% of FX gap; noisier cross-sectional sorts |
| Exchange rate timing | End-of-month, point-in-time | End-of-month (daily series, last biz day) | Aligned after fix |
| Interest rates | 3M ICE LIBOR | OECD 3M interbank | Minor; CIP deviations post-2008 |
| PPP source | Penn World Tables | OECD SNA Table 4 | Same underlying data; OECD is more current |
| Forward rates | Bloomberg/Citi FX forwards | Not available; use rate differentials | Equivalent via CIP, small divergence post-2008 |

### Equities

**AQR's approach:** Individual stock-level long/short portfolios formed on book-to-market, 12-1 month momentum, beta, and profitability across 24 countries and 5 regional aggregates.

**Our approach:** We use AQR's published pre-built equity factor returns directly from the AQR Data Library (downloaded as `.xlsx`, parsed and cached as `.rds`):

| Dataset | Factors | Date Range | Geographies |
|---|---|---|---|
| The Devil in HML's Details | HML Devil (value) | 1926–2026 | 24 countries + 5 aggregates |
| Betting Against Beta | BAB (defensive) | 1930–2026 | 24 countries + 5 aggregates |
| Quality Minus Junk | QMJ (quality) | 1957–2026 | 24 countries + 5 aggregates |
| Momentum Indices | Momentum | 1980–2026 | US Large/Small Cap, International |

Supplementary factors extracted from the HML file: MKT (market excess return), SMB (small minus big), RF (risk-free rate).

**Rationale for using pre-built:** Constructing equity factors from scratch requires individual stock returns and fundamentals data across 24 countries. Alpha Vantage (our equity data source) covers US single stocks only. International stock-level data would require Worldscope or CRSP Global (paid academic databases). Since AQR publishes these factors monthly with full geographic coverage, using them directly is both more accurate and more practical than a partial replication.

---

## Factor Definitions: Ours vs AQR's

### Value

| Asset Class | Our Signal | AQR Signal | Difference |
|---|---|---|---|
| FI | Real yield = 10Y nominal yield − annualized 3yr trailing CPI change | Same | Inflation proxy may differ; AQR may use survey-based expectations |
| FX | `log(market_rate / PPP_rate)` using OECD annual PPP interpolated to monthly via CPI | `log(market_rate / PPP_rate)` using Penn World Tables | Same concept; OECD and PWT use the same underlying ICP data |
| Equity | HML Devil (AQR pre-built) | HML Devil | Identical |

**FI Value implementation detail:** We annualize the 3-year trailing inflation as `(1 + CPI_t/CPI_{t-36} − 1)^(1/3) − 1`. Testing confirmed that the non-annualized cumulative 3-year change produces significantly weaker correlation with AQR (0.14 vs 0.56), validating the annualization.

**FX Value implementation detail:** The PPP signal is `log(S / PPP)` where both `S` (market exchange rate) and `PPP` are in foreign-per-USD units. Positive = foreign currency is cheap (more foreign currency per USD than PPP implies) = long. The annual OECD PPP rate is interpolated to monthly by tracking CPI drift within each year: `PPP_monthly = PPP_annual × (CPI_foreign_m / CPI_foreign_Jan) / (CPI_US_m / CPI_US_Jan)`. The signal is lagged one month to avoid look-ahead bias (the exchange rate that determines the signal also determines the contemporaneous return).

### Momentum

| Asset Class | Our Signal | AQR Signal | Difference |
|---|---|---|---|
| FI | 12-1 month cumulative bond total return (yield-derived) | 12-1 month cumulative bond total return (actual TR indices) | Data source: yield approx vs GFD total returns |
| FX | 12-1 month cumulative FX excess return (spot + carry) | Same | Aligned |
| Equity | AQR pre-built | AQR pre-built | Identical |

Uniform across all asset classes: cumulative compounded return from t-12 to t-2, skipping the most recent month to avoid microstructure effects. Following Jegadeesh and Titman (1993).

**FI Momentum note:** This factor is doubly affected by the yield data issue. The momentum signal IS the return — so data quality impacts both the signal (which determines portfolio weights) and the payoff (which determines the factor return). This compounding effect is why FI momentum (0.44 correlation) is harder to replicate than FI carry or value, where the signal is computed from yields but the payoff comes from independent return data.

### Carry

| Asset Class | Our Signal | AQR Signal | Difference |
|---|---|---|---|
| FI | Term spread = 10Y yield − 3M rate | Same | Aligned |
| FX | 3M interest rate differential (foreign − US) | Forward discount or rate differential | Equivalent via CIP; may diverge post-2008 |
| Equity | N/A | N/A | — |

Carry is the expected return assuming unchanged market conditions, following Koijen, Moskowitz, Pedersen, and Vrugt (2018).

**FI Carry note:** Our 15-country panel has sparse coverage before 1985 (4–8 countries). We impose a minimum threshold of 8 countries with non-missing signals before computing factor returns to avoid noisy early-period sorts.

### Defensive (Betting Against Beta)

| Asset Class | Our Signal | AQR Signal | Difference |
|---|---|---|---|
| FI | Negated 36-month rolling beta vs EW bond market, with 0.6/0.4 Vasicek shrinkage | Same | Aligned |
| FX | Not constructed (no market index) | Not constructed | Aligned |
| Equity | AB pre-built | AQR pre-built | Identical |

Following Frazzini and Pedersen (2013). Key implementation details:

- **Beta shrinkage:** `beta_shrunk = 0.6 × beta_OLS + 0.4 × 1.0`. Without this, extreme OLS betas (observed range: −0.41 to 4.45 in our data) produce excessive leverage via the 1/beta scaling.
- **Beta-neutral construction:** `r_BAB = (1/beta_L) × (r_L − r_f) − (1/beta_H) × (r_H − r_f)`. The portfolio is constructed to have zero beta to the equal-weighted bond market, not zero dollars. The long (low-beta) side is leveraged up; the short (high-beta) side is de-leveraged.
- **Excess returns:** The BAB formula requires excess returns (r − r_f). We use the US 3-month rate as the common funding cost. This is critical: omitting the risk-free rate subtraction produces a spurious premium of ~4.3% annually (the risk-free rate earned through the leverage differential).

---

## Portfolio Construction

**AQR's approach:** At each month, for each factor × asset class, assign weights proportional to demeaned cross-sectional ranks:

```
w_i = c × (rank(signal_i) − mean(rank))
```

where `c` scales the portfolio to $1 long / $1 short. This is purely ordinal — the raw signal magnitude doesn't matter, only relative ranking. The portfolio is dollar-neutral (except BAB, which is beta-neutral).

**Our approach:** We use z-score-based weighting. For each factor at each month:

1. Compute signal for all securities with non-missing data
2. Cross-sectionally z-score the signals: `z_i = (signal_i − mean) / sd`
3. Positive z-scores → long weight; negative → short weight
4. Scale so sum of positive weights = 1, sum of negative weights = −1
5. Factor return = `Σ(w_i × r_i)`

**Why z-scores instead of ranks:** With small cross-sections (9 currencies, 10–15 bond markets), rank-based and z-score-based weights produce very similar results. Z-scores preserve signal magnitude information, which can be informative when signals are well-calibrated (e.g., PPP deviations have natural economic units). The correlation impact of this choice is small (+/−0.02 in our testing). We use z-scores for simplicity and consistency across factors.

**Defensive/BAB exception:** For the BAB factor, we split at the median shrunk beta (not z-score weighted), with rank-based weights within each leg. The long and short legs are then scaled by 1/beta to achieve beta neutrality.

**Minimum observation threshold:** FI factors require at least 8 countries with non-missing signals. This avoids the pre-1985 period when only 4–5 countries had data, making any cross-sectional sort unreliable.

**Multi-asset combination:** AQR combines factor portfolios across asset classes using inverse-volatility weighting (each asset class scaled by 1/σ estimated from 36-month rolling standard deviation). We do not currently perform cross-asset combination — each asset class factor is reported separately.

---

## Divergence Analysis

### Why FX factors replicate well (0.76–0.91)

Exchange rates and interest rates are directly observed, standardized, and available from FRED. The signals are unambiguous (rate differential IS carry, cumulative return IS momentum, PPP deviation IS value). The main remaining gap is universe size (9 vs ~20 currencies).

### Why FI factors are structurally limited (0.33–0.56)

The bottleneck is not methodology — it's the yield data. Three compounding issues:

1. **Different instruments:** OECD harmonized yields may reference different bonds than GFD's total return indices (on-the-run vs off-the-run, different benchmark selection criteria per country).
2. **Approximation vs observation:** Even a perfect repricing formula applied to the wrong yield produces the wrong return. The error is in the input, not the formula.
3. **Compounding through momentum:** For FI momentum, the signal IS the return. So yield data noise affects both the ranking (which countries are in the long vs short portfolio) and the payoff. For carry and value, the signal comes from yields but the payoff comes from (independently noisy) returns — a single layer of noise, not double.

This is confirmed empirically: our equal-weighted FI market return correlates only 0.72 with AQR's, whereas our FX market returns correlate >0.95 with observable benchmarks. No methodology fix can close this gap without replacing the underlying data source.

**What would fix it:** ICE BofA, Bloomberg Barclays, or FTSE government bond total return indices — none of which are available for free. FRED hosts ICE BofA indices only for corporate credit/high yield/EM, not sovereigns.

---

## Inflation Data: CPI Limitations and Breakeven Alternatives

### The problem

The FI value signal (real yield = nominal yield − inflation) and FX value signal (PPP deviation, interpolated with CPI) both depend on CPI data from FRED. This creates two issues:

1. **Publication lag:** OECD CPI series on FRED lag ~2 months behind the present, truncating the entire combined factor panel to whenever CPI runs out. Since the HF analysis script inner-joins all factors, two stale factors (FI value, FX value) were dragging the analysis back to April 2025 while all other factors extended to February 2026.

2. **Country coverage gaps:**
   - Japan: OECD stopped publishing CPI to FRED in April 2022. All monthly JP CPI series are stale.
   - Australia, New Zealand: Monthly CPI doesn't exist on FRED (quarterly only, step-interpolated).
   - Germany, France: Actually available on FRED (`DEUCPIALLMINMEI`, `FRACPIALLMINMEI`) but were not being fetched due to a config issue in the FI script.

### Current approach (as of April 2026)

**US FI value:** Uses 10Y TIPS breakeven inflation (`T10YIE` on FRED) instead of trailing CPI. This is an improvement over the CPI approach:
- Forward-looking (market-implied) vs backward-looking (trailing 3yr CPI)
- Updated daily on FRED vs CPI's ~2 month lag
- Tenor-matched to the 10Y nominal yield

**Non-US FI value:** Still uses trailing CPI where available. Countries with stale or missing CPI produce NA value signals for those months.

**FX value:** Still uses OECD PPP rates interpolated with CPI. Truncates when CPI runs out. The HF analysis script fills NAs with zero so stale factors don't drop entire months.

### Available free sources for breakeven inflation, by country

#### US — FRED (implemented)

| Series | Description | Frequency |
|---|---|---|
| `T10YIE` | 10Y TIPS breakeven inflation | Daily |
| `T5YIE` | 5Y TIPS breakeven inflation | Daily |
| `T10YIEM` / `T5YIEM` | Monthly versions of above | Monthly |
| `T5YIFR` | 5Y/5Y forward inflation expectation | Daily |
| `DFII10` | 10Y TIPS real yield (breakeven = nominal − this) | Daily |
| `EXPINF10YR` | Cleveland Fed 10Y expected inflation (model-based) | Monthly |

#### Euro area (DE, FR, IT, NL, BE as bloc) — ECB Statistical Data Warehouse

The ECB publishes both nominal and real (inflation-linked) euro area benchmark bond yields. Breakeven = nominal − real.

| Series | Description | Frequency | API |
|---|---|---|---|
| `FM.M.U2.EUR.4F.BB.U2_10Y.YLD` | Nominal 10Y euro area benchmark yield | Monthly | ECB SDMX REST |
| `FM.M.U2.EUR.4F.BB.R_U2_10Y.YLDA` | Real 10Y euro area benchmark yield | Monthly | ECB SDMX REST |

API base: `https://data-api.ecb.europa.eu/service/data/FM/`
No authentication required. Add `?lastNObservations=N` and header `Accept: text/csv` for CSV output. Current through March 2026.

Limitation: Euro area aggregate only — not available at individual country level (DE, FR, IT, etc.). For the cross-country value signal, all 5 eurozone countries in our panel would share the same breakeven.

#### UK (GB) — Bank of England

The BoE publishes nominal, real (from index-linked gilts), and implied inflation yield curves at maturities out to 25+ years.

| Curve | Description | Frequency |
|---|---|---|
| Nominal zero-coupon | Fitted from conventional gilts | Daily/Monthly |
| Real zero-coupon | Fitted from index-linked gilts | Daily/Monthly |
| Implied inflation | Nominal minus real | Daily/Monthly |

Access: File download only — **no API**. ZIP archives containing spreadsheets.
URL: `https://www.bankofengland.co.uk/statistics/yield-curves`
Actively updated, current data available.

#### Canada (CA) — Bank of Canada Valet API

| Series | Description | Frequency | API |
|---|---|---|---|
| Real Return Bond yields | Spread vs conventional bonds gives breakeven | Daily | REST JSON/CSV |
| `CES_C1A_*` series | Consumer inflation expectations (1Y, 2Y, 5Y) | Quarterly | REST JSON/CSV |

API base: `https://www.bankofcanada.ca/valet/observations/{series_id}/json`
No authentication required.

#### Australia (AU) — Reserve Bank of Australia

| Source | Description | Frequency |
|---|---|---|
| Statistical Table F16 | Individual Treasury Indexed Bond (inflation-linked) yields | Daily |
| Statistical Table F17 | Nominal zero-coupon yield curve | Daily |

Breakeven derived by interpolating F16 indexed bond yields to match nominal maturities from F17.
URLs: `https://www.rba.gov.au/statistics/tables/csv/f16-data.csv` and `https://www.rba.gov.au/statistics/tables/csv/f17-yields.csv`
No authentication. CSV download. F16 last updated March 2026.

#### Japan (JP) — Ministry of Finance

JGB inflation-indexed bond (JGBi) yields published as CSV downloads since 2004.
URL: `https://www.mof.go.jp/english/policy/jgbs/reference/interest_rate/index.htm`
File-based, not API-based. Would need to derive breakeven from nominal JGB yield minus JGBi yield.

Note: The Bank of Japan launched a time-series API in February 2026 (`https://www.stat-search.boj.or.jp/`) but it covers call rates, money market rates, FX, and prices — not JGB yields directly.

#### Countries with no free breakeven source

| Country | Status |
|---|---|
| Sweden (SE) | Issues inflation-linked bonds but Riksbank API only publishes nominal yields |
| Switzerland (CH) | No free source found |
| Denmark (DK) | No free source found |
| Norway (NO) | Norges Bank publishes quarterly survey expectations (PDF reports only, no API) |
| New Zealand (NZ) | No free source found |

These 5 countries would remain on trailing CPI for the value signal.

### Implementation priority

1. **Done:** US via `T10YIE` on FRED
2. **High value:** Euro area via ECB (covers 5 countries with one API call)
3. **High value:** UK via BoE file download (major bond market)
4. **Medium:** Canada via Bank of Canada Valet API
5. **Medium:** Australia via RBA CSV downloads
6. **Lower:** Japan via MOF CSV (requires scraping, and JP CPI on FRED is stale since 2022 anyway)
7. **Not feasible:** SE, CH, DK, NO, NZ — stay on trailing CPI

---

## Implementation History

The factors went through several iterations. Key fixes and their impact:

| Fix | Factor(s) | Before | After | What Changed |
|---|---|---|---|---|
| End-of-month exchange rates | FX Mom, Carry | 0.58, 0.62 | 0.83, 0.91 | Eliminated half-month timing lag and artificial autocorrelation |
| Correct CPI series | FX Value | -0.28 | 0.17 | Previous series gave MoM % changes, not index levels |
| OECD PPP exchange rates | FX Value | 0.17 | 0.76 | Proper PPP deviation signal vs rolling-mean RER proxy |
| Z-score weighting | All | +0.02–0.05 each | — | Full cross-section utilization vs noisy tercile sorts |
| BAB excess returns | FI Defensive | 0.23 (mean +3.47%) | 0.33 (mean -0.16%) | Removed spurious risk-free rate premium |
| Beta shrinkage | FI Defensive | — | +0.05 | Reduced noise from extreme leverage |
| Min country threshold | FI all | — | +0.03–0.06 | Dropped noisy pre-1985 period with <8 countries |
| Exact repricing formula | FI all | — | ~+0.01 | Captures convexity and roll-down; marginal improvement |
| US TIPS breakeven for value | FI Value (US) | ends Apr 2025 | extends to present | Forward-looking, real-time vs 2-month CPI lag |
| US 3M T-bill splice for FX | FX all | 2-month gap Apr-May 2020 | no gap | Single missing OECD USD rate cascaded to all currencies |

---

## Data Pipeline

All factor data is cached locally as `.rds` files. No API calls on subsequent script runs unless cache files are deleted.

```
FRED API (yields, rates, CPI, FX)    OECD API (PPP)    AQR website (xlsx)
    |                                     |                    |
    v                                     v                    v
Raw data (.rds cache in data/fred/)    PPP rates (.rds)    Excel files (data/aqr/)
    |                                     |                    |
    v                                     v                    v
Derived returns (.rds)               Monthly PPP (.rds)   Parsed factors (.rds)
    |                                     |
    v                                     v
Factor signals (.rds)  <---- value signal uses PPP ----
    |
    v
Factor returns (.rds)
```

### Scripts

| Script | Purpose | Data Source | Outputs |
|---|---|---|---|
| `fetch_fi_factors.R` | FI factor pipeline | FRED (yields, rates, CPI) | 5 `.rds` files in `data/fred/` |
| `fetch_fx_factors.R` | FX factor pipeline | FRED (FX, rates, CPI) + OECD (PPP) | 7 `.rds` files in `data/fred/` |
| `fetch_aqr_equity_factors.R` | Parse AQR equity factors | AQR `.xlsx` files | 3 `.rds` files in `data/aqr/` |
| `plot_factor_returns.R` | Cumulative return plots | Cached `.rds` files | 3 ggplot objects |

### Cached Data Files

**`data/fred/`** — FRED-sourced FI and FX data:
- `fi_raw_yields.rds` — 15-country yield + 3M rate panel
- `fi_raw_cpi.rds` — 14-country CPI index panel
- `fi_bond_returns.rds` — Monthly bond total returns (exact repricing)
- `fi_factor_signals.rds` — Carry, value, momentum, defensive signals
- `fi_factor_returns.rds` — Factor return time series
- `fx_raw_rates.rds` — 9 G10 end-of-month exchange rates
- `fx_raw_interest_rates.rds` — 10 interbank rate series
- `fx_raw_cpi.rds` — 10 CPI index series
- `fx_ppp_rates.rds` — Monthly interpolated OECD PPP rates
- `fx_returns.rds` — FX excess returns (spot + carry)
- `fx_factor_signals.rds` — Carry, momentum, value signals
- `fx_factor_returns.rds` — Factor return time series

**`data/aqr/`** — AQR-sourced equity factor data:
- `hml_factors_monthly.xlsx` — Raw HML Devil download
- `momentum_factors_monthly.xlsx` — Raw momentum download
- `bab_factors_monthly.xlsx` — Raw BAB download
- `qmj_factors_monthly.xlsx` — Raw QMJ download
- `aqr_century_factor_premia.rds` — Parsed AQR Century data (validation benchmark)
- `aqr_equity_factors.rds` — Multi-factor panel (date, geography, hml, bab, qmj, mkt, smb)
- `aqr_momentum_factors.rds` — Momentum (date, us_large_cap, us_small_cap, international)
- `aqr_risk_free_rate.rds` — Monthly risk-free rate

---

## References

- Asness, C. S., Moskowitz, T. J., & Pedersen, L. H. (2013). Value and Momentum Everywhere. *Journal of Finance*, 68(3), 929–985.
- Frazzini, A., & Pedersen, L. H. (2014). Betting Against Beta. *Journal of Financial Economics*, 111(1), 1–25.
- Ilmanen, A., Israel, R., Lee, R., Moskowitz, T. J., & Thapar, A. (2021). How Do Factor Premia Vary Over Time? A Century of Evidence. *Journal of Investment Management*, 19(4).
- Jegadeesh, N., & Titman, S. (1993). Returns to Buying Winners and Selling Losers. *Journal of Finance*, 48(1), 65–91.
- Koijen, R. S. J., Moskowitz, T. J., Pedersen, L. H., & Vrugt, E. B. (2018). Carry. *Journal of Financial Economics*, 127(2), 197–225.
- Swinkels, L. (2019). Treasury Bond Return Data Starting in 1962. *Data*, 4(3), 91.
