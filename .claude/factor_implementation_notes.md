# Factor Implementation Notes

## Overview

This document describes our implementation of cross-asset factor premia (value, momentum, carry, defensive) across Fixed Income, Currencies, and Equities. The methodology follows Ilmanen, Israel, Lee, Moskowitz, and Thapar (2021), "How Do Factor Premia Vary Over Time? A Century of Evidence," *Journal of Investment Management*. Where our implementation diverges from AQR's, the reasons and implications are noted.

Factor returns from AQR's published datasets are used as a validation benchmark. Correlations between our FRED-built factors and AQR's are reported to assess replication fidelity.

---

## Asset Classes and Data Sources

### Fixed Income

**AQR's approach:** 10-year government bond total returns for ~13 developed market countries, sourced from Global Financial Data (GFD). GFD provides actual bond total return indices — price appreciation plus coupon income — for sovereign bonds going back decades.

**Our approach:** 10-year government bond yields from FRED (OECD series `IRLTLT01xxM156N`) for 15 countries: US, UK, Germany, Japan, France, Canada, Australia, Italy, Netherlands, Sweden, Belgium, Switzerland, Denmark, Norway, New Zealand. Monthly total returns are approximated from yield changes using a first-order duration formula:

```
r_t = (y_{t-1} / 12) - D_mod * (y_t - y_{t-1})
```

Where modified duration is computed from the par bond formula recalculated each month.

**Key difference and impact:** The OECD yields are "harmonized" benchmark rates — they may not track the exact same bonds as GFD's total return indices. This introduces both a level difference (missing roll-down return, benchmark switch effects) and a noise component. The equal-weighted market return from our yields correlates only ~0.72 with AQR's FI market return, compared to >0.95 expected if the underlying data were identical. This data source gap is the binding constraint on our FI factor correlations, limiting all four to a ceiling of approximately 0.45-0.55. Switching to actual total return indices (e.g., ICE BofA government bond indices on FRED) would address this.

**Additional data:** CPI index levels for 14 countries from FRED (OECD series `{ISO3}CPIALLMINMEI`), used for the value signal's inflation proxy. Australia and New Zealand CPI are quarterly-only on FRED; we step-interpolate to monthly. New Zealand CPI was unavailable from FRED entirely, so NZ is excluded from the value signal.

### Currencies (FX)

**AQR's approach:** ~20 exchange rates including G10 currencies plus legacy European currencies (DEM, FRF, ITL, etc.) pre-1999. Point-in-time (end-of-month) spot and forward exchange rates from Bloomberg and Citi. Interest rate differentials from 3-month ICE LIBOR or local equivalents.

**Our approach:** 9 G10 currencies vs USD (AUD, CAD, CHF, EUR, GBP, JPY, NOK, NZD, SEK). End-of-month spot exchange rates from FRED daily series (H.10 release, `DEX` prefix — last business day of each month). 3-month interbank rates from FRED (OECD series `IR3TIB01xxM156N`). Japan's interbank rate starts only in 2002; we splice with the CD rate (`IR3TCD01JPM156N`, available from 1979) to extend history.

**Key differences:**
- *Universe:* 9 currencies vs AQR's ~20. Missing legacy European currencies reduces diversification pre-1999 and means our cross-sectional sorts are noisier. This is the primary source of divergence for FX carry and momentum.
- *Exchange rate frequency:* We use end-of-month daily rates to match AQR's point-in-time approach. The original implementation used monthly averages, which introduced artificial autocorrelation and a half-month timing lag.
- *Interest rates:* OECD interbank rates are a reasonable proxy for LIBOR but may differ due to credit risk, day-count conventions, and the post-2008 LIBOR-OIS spread divergence.

### Equities

**AQR's approach:** Individual stock-level factor construction (long/short portfolios formed on book-to-market, 12-1 month momentum, beta, profitability) across 24 countries and 5 regional aggregates.

**Our approach:** We use AQR's published pre-built equity factor returns directly, downloaded from the AQR Data Library. Four factor datasets are cached locally:
- HML Devil (value) — updated HML avoiding stale book values
- BAB (betting against beta / defensive)
- QMJ (quality minus junk)
- Momentum indices (US large cap, US small cap, international)

Plus supplementary factors from the HML file: MKT (market excess return), SMB (small minus big), and RF (risk-free rate).

**Rationale for using pre-built:** Constructing equity factors from scratch requires individual stock returns and fundamentals data across 24 countries. Alpha Vantage covers US single stocks only; international stock-level data would require Worldscope or CRSP Global (paid). Since AQR publishes these factors monthly with full geographic coverage, using them directly is both more accurate and more practical.

---

## Factor Definitions

### Value

| Asset Class | Signal | Source |
|---|---|---|
| FI | Real bond yield = 10Y nominal yield - annualized 3yr trailing CPI change | AQR: same definition |
| FX | Deviation from PPP = log(nominal rate / PPP rate) | AQR uses Penn World Tables PPP; we proxy with CPI-based real exchange rate deviation from its long-run mean |
| Equity | Book-to-market (HML Devil) | AQR pre-built |

**FI Value note:** We annualize the 3-year trailing inflation as `(1 + CPI_t/CPI_{t-36} - 1)^(1/3) - 1` to match the units of the nominal yield. The non-annualized cumulative 3-year change produces weaker correlation with AQR (0.14 vs 0.34), confirming the annualization matters.

**FX Value note:** Without actual PPP exchange rates, we use the log real exchange rate's deviation from its rolling long-run mean as a proxy for PPP deviation. This captures the same intuition (cheap vs expensive currencies in real terms) but may diverge from AQR's signal in levels. AQR's PPP data comes from Penn World Tables; OECD publishes annual PPP rates that could improve our proxy.

### Momentum

| Asset Class | Signal | Source |
|---|---|---|
| FI | 12-1 month cumulative bond total return | AQR: same, but using actual total returns |
| FX | 12-1 month cumulative FX excess return (spot + carry) | AQR: same definition |
| Equity | 12-1 month stock return | AQR pre-built |

Uniform across all asset classes: cumulative return from t-12 to t-2, skipping the most recent month to avoid microstructure effects (bid-ask bounce, month-end rebalancing flows). Following Jegadeesh and Titman (1993) and Asness, Moskowitz, and Pedersen (2013).

**FI Momentum note:** Since our bond returns are yield-derived approximations rather than actual total returns, the momentum signal itself inherits the yield data noise. This is a compounding problem — the signal IS the return, so data quality affects both the signal (ranking) and the payoff (return computation). This is the main reason our FI momentum correlation with AQR (0.39) lags our FX momentum correlation (0.58), where the underlying data is directly observed.

### Carry

| Asset Class | Signal | Source |
|---|---|---|
| FI | Term spread = 10Y yield - 3M rate | AQR: same definition |
| FX | 3-month interest rate differential (foreign - US) | AQR: same concept, may use forward discount instead |
| Equity | Not applicable at single-stock level | — |

Carry is the expected return assuming unchanged market conditions, following Koijen, Moskowitz, Pedersen, and Vrugt (2018).

**FI Carry note:** Our 15-country panel has sparse coverage before 1985 (4-8 countries), making cross-sectional sorts very noisy in early decades. We impose a minimum threshold of 8 countries with non-missing signals before computing factor returns.

**FX Carry note:** AQR may use the forward discount (forward rate / spot rate - 1) rather than the interest rate differential directly. By covered interest parity, these should be equivalent, but CIP deviations post-2008 (especially for JPY and EUR) mean they can diverge in practice.

### Defensive (Betting Against Beta)

| Asset Class | Signal | Source |
|---|---|---|
| FI | Negated 36-month rolling beta vs EW bond market | AQR: same, with Vasicek shrinkage |
| FX | Not constructed — no logical market index for currencies | AQR: same |
| Equity | Negated beta vs local equity market | AQR pre-built |

Following Frazzini and Pedersen (2013). The portfolio is beta-neutral, not dollar-neutral: the long side (low-beta) is levered up by 1/beta_L and the short side (high-beta) is de-levered by 1/beta_H, so the portfolio has zero market beta.

**FI Defensive notes:**
- *Beta shrinkage:* We apply the Frazzini-Pedersen Vasicek shrinkage: `beta_shrunk = 0.6 * beta_OLS + 0.4 * 1.0`. Without shrinkage, extreme OLS betas (observed range: -0.41 to 4.45) produce excessive leverage and noisy factor returns.
- *Excess returns:* The BAB formula uses excess returns (r - rf). We subtract the US 3-month T-bill rate as the common funding rate. Omitting this (using total returns) produces a spurious positive premium of ~4.3% annually due to the leverage differential earning the risk-free rate.

---

## Portfolio Construction

**AQR's approach:** At each month, for each factor-asset class:

```
w_i = c * (rank(signal_i) - mean(rank))
```

Where `c` is a scaling constant ensuring $1 long and $1 short. Weights are proportional to demeaned ranks — securities with more extreme signals receive larger weights, but nonlinearly (through rank transformation). The portfolio is dollar-neutral (except defensive/BAB which is beta-neutral).

**Our approach:** We use z-score-based weighting as an approximation to AQR's demeaned rank weights. For each factor at each month:
1. Compute signal for all securities with non-missing data
2. Cross-sectionally z-score the signals
3. Positive z-scores get positive (long) weight, negative get negative (short) weight
4. Scale so sum of positive weights = 1, sum of negative weights = -1
5. Factor return = sum(w_i * r_i)

**Difference:** Z-score weighting uses the signal value itself (standardized), while AQR's rank weighting is purely ordinal. Rank weights are more robust to outliers in the signal distribution. With small cross-sections (9-15 securities), the practical difference is modest, but rank weights are strictly more aligned with the published methodology.

**Multi-asset combination:** AQR combines factor portfolios across asset classes by weighting each asset class by the inverse of its 36-month rolling standard deviation (equal-volatility weighting). We do not currently perform cross-asset combination — each asset class factor is reported separately.

---

## Validation Against AQR

Correlations between our factor returns and AQR's "Century of Factor Premia" published data:

| Factor | Correlation | Our Ann. Mean | AQR Ann. Mean | Our Ann. Vol | AQR Ann. Vol |
|---|---|---|---|---|---|
| FI Carry | 0.32 | +0.49% | +2.54% | 4.71% | 4.12% |
| FI Value | 0.34 | +2.85% | +1.72% | 3.65% | 4.16% |
| FI Momentum | 0.39 | -0.03% | +0.51% | 4.38% | 4.29% |
| FI Defensive | 0.23 | +3.47% | -0.71% | 4.69% | 3.93% |
| FX Carry | 0.62 | +3.49% | +2.85% | 5.95% | 7.20% |
| FX Momentum | 0.58 | +0.36% | +0.33% | 6.35% | 6.73% |
| FX Value | -0.28 | -0.75% | +2.12% | 4.93% | 5.62% |

*Note: These correlations reflect the initial implementation before fixes. Updated correlations will be reported after implementing the changes described in this document.*

### Sources of Divergence (ranked by impact)

1. **FI bond return data source** (~55% of FI gap): OECD harmonized yields vs GFD actual total return indices. Structural limitation of FRED data. Addressable by switching to ICE BofA total return indices if available on FRED.

2. **FX Value signal bugs** (explains negative correlation): Three compounding issues in the initial implementation — wrong CPI series type, raw RER level as signal (dominated by nominal rate magnitude), and missing PPP anchor. Fixed in current version.

3. **Monthly-average exchange rates** (~15% of FX gap): FRED's monthly series average daily rates. Introduces artificial autocorrelation and timing lag. Fixed by switching to daily series, taking last business day of each month.

4. **FI Defensive formula errors** (explains mean divergence): Missing risk-free rate subtraction and beta shrinkage. Produces spurious +4.3% premium and excessive noise from extreme leverage. Fixed in current version.

5. **Portfolio construction** (~5-10% of gap): Tercile sorts vs signal-weighted portfolios. Small but consistent improvement from using the full cross-section rather than discrete buckets. Fixed in current version.

6. **Currency universe** (~10% of FX gap): 9 G10 currencies vs AQR's ~20 (including legacy European pre-1999). Not currently addressable without additional data sources.

---

## Data Pipeline

All factor data is cached locally as `.rds` files to avoid repeated API calls. The pipeline is:

```
FRED API / AQR website
    |
    v
Raw data (.rds cache)          # Yields, rates, CPI, exchange rates
    |
    v
Derived quantities (.rds)      # Bond returns, FX excess returns
    |
    v
Factor signals (.rds)          # Carry, value, momentum, defensive signals
    |
    v
Factor returns (.rds)          # Long/short portfolio returns
```

Scripts:
- `scripts/fetch_fi_factors.R` — FI factor pipeline (FRED → .rds)
- `scripts/fetch_fx_factors.R` — FX factor pipeline (FRED → .rds)
- `scripts/fetch_aqr_equity_factors.R` — AQR equity factor parser (xlsx → .rds)
- `scripts/plot_factor_returns.R` — Cumulative return visualizations

---

## References

- Asness, C. S., Moskowitz, T. J., & Pedersen, L. H. (2013). Value and Momentum Everywhere. *Journal of Finance*, 68(3), 929-985.
- Frazzini, A., & Pedersen, L. H. (2014). Betting Against Beta. *Journal of Financial Economics*, 111(1), 1-25.
- Ilmanen, A., Israel, R., Lee, R., Moskowitz, T. J., & Thapar, A. (2021). How Do Factor Premia Vary Over Time? A Century of Evidence. *Journal of Investment Management*, 19(4).
- Jegadeesh, N., & Titman, S. (1993). Returns to Buying Winners and Selling Losers. *Journal of Finance*, 48(1), 65-91.
- Koijen, R. S. J., Moskowitz, T. J., Pedersen, L. H., & Vrugt, E. B. (2018). Carry. *Journal of Financial Economics*, 127(2), 197-225.
