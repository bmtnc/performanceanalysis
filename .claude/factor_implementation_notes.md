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

**All FI value (15 countries):** Uses breakeven inflation panel (`breakeven_inflation.rds`) for all countries. `fetch_fi_factors.R` no longer fetches CPI or T10YIE directly — it loads the pre-built breakeven panel and computes `real_yield = nominal_yield - breakeven`. This means:
- US, GB, AU: market-implied breakevens (TIPS, BoE linkers, RBA indexed bonds)
- DE, FR, IT, NL, BE, SE, CH, DK, NO, JP, CA: regression-estimated from CPI co-movement with US breakeven
- NZ: regression-estimated from CPI co-movement with AU breakeven
- Value signal series start later than the old trailing-CPI approach (e.g., US from 2003 vs ~1960s) but are forward-looking and not subject to CPI publication lag

**FX value:** Still uses OECD PPP rates interpolated with CPI. Truncates when CPI runs out. The HF analysis script fills NAs with zero so stale factors don't drop entire months.

### Available free sources for breakeven inflation, by country

All sources below derive breakeven inflation from the spread between nominal and inflation-linked government bond yields (10Y tenor unless noted). These embed an inflation risk premium and liquidity premium on top of pure inflation expectations, but for a cross-sectional value signal the biases are roughly uniform across countries and cancel in the long/short construction.

#### US — FRED (implemented)

| Series | Description | Frequency |
|---|---|---|
| `T10YIE` | 10Y TIPS breakeven inflation | Daily |
| `T5YIE` | 5Y TIPS breakeven inflation | Daily |
| `T10YIEM` / `T5YIEM` | Monthly versions of above | Monthly |
| `T5YIFR` | 5Y/5Y forward inflation expectation | Daily |
| `DFII10` | 10Y TIPS real yield (breakeven = nominal − this) | Daily |
| `EXPINF10YR` | Cleveland Fed 10Y expected inflation (model-based) | Monthly |

#### Euro area (DE, FR, IT, NL, BE) — NO USABLE FREE SOURCE

**The ECB "real yield" series is NOT market-implied.** The series `FM.M.U2.EUR.4F.BB.R_U2_10Y.YLDA` is computed as `nominal yield − year-on-year HICP inflation`, NOT derived from inflation-linked bonds. This means the "breakeven" computed as `nominal − real` cancels out to just `HICP YoY` — i.e., backward-looking realized inflation. Verified by exact arithmetic matching across multiple months (e.g., Oct 2022: nominal 3.184, HICP YoY 10.6%, real = 3.184 − 10.6 = −7.416, which matches the published series exactly). The 9%+ "breakeven" spike in late 2022 was simply the HICP inflation peak, not market expectations.

Additionally, `YLD` vs `YLDA` is a timing difference (end-of-period vs period-average), introducing a minor inconsistency between the nominal and real series.

**No country-level inflation-linked bond yields available for free.** Exhaustive search findings:
- **ECB**: All 85+ SDMX dataflows searched. No inflation-linked bond yields or curves. The ECB yield curve model explicitly excludes linkers. Inflation-linked swap (ILS) rates used internally by the ECB come from Refinitiv and are not redistributed.
- **Germany (Bundesbank)**: The BBSSY dataflow provides market-observed real yields from individual Bundei bonds via a free API (`api.statistiken.bundesbank.de`). However, there is no 10Y bond — the closest is ~7Y (DE0001030583, maturing 2033). Germany stopped issuing new linkers in 2024. Liquidity halved in 2024.
- **France (Banque de France)**: Webstat API exists but requires free registration (API key). Unclear whether OATi/OATei yield series are in the catalog. France has the deepest eurozone linker market (18 bonds) but the data may not be publicly accessible even with a key.
- **Italy**: No confirmed free source. Banca d'Italia Rendistato covers nominals only. BTPei auction results available as individual PDFs only.
- **Eurostat, OECD, FRED**: None publish eurozone inflation-linked bond yields.

**Implementation decision:** All 5 eurozone countries use the **regression-based approach** with US TIPS breakeven as reference, alongside the nordics and Japan.

#### UK (GB) — Bank of England IADB (implemented)

The BoE publishes fitted yield curves including an implied inflation curve (nominal minus real) derived from conventional gilts and index-linked gilts (linked to RPI). The data is available via a **CSV API endpoint** on the Interactive Analytical Database (IADB) — no manual download, ZIP files, or JavaScript required.

| Series Code | Description | Maturity | Available From |
|---|---|---|---|
| `IUDMIZC` | **Implied inflation zero-coupon yield** | 10Y | Jan 1985 |
| `IUDMNZC` | Nominal zero-coupon yield | 10Y | Jan 1982 |
| `IUDMRZC` | Real zero-coupon yield | 10Y | Jan 1985 |
| `IUDSIZC` / `IUDLIZC` | Implied inflation zero-coupon | 5Y / 20Y | Jan 1985 |

Naming convention: `IUD` + maturity (`S`=5Y, `M`=10Y, `L`=20Y) + curve (`N`=nominal, `R`=real, `I`=inflation) + measure (`ZC`=zero-coupon, `PY`=par yield).

API endpoint:
```
https://www.bankofengland.co.uk/boeapps/database/_iadb-fromshowcolumns.asp?csv.x=yes
  &Datefrom=01/Jan/1985
  &Dateto=31/Dec/2026
  &SeriesCodes=IUDMIZC
  &CSVF=TN
  &UsingCodes=Y
  &VPD=Y
  &VFD=N
```

No authentication. Up to 300 series per request. Response is CSV with `DATE` (DD Mon YYYY format) and series code columns. Values in percent. Daily frequency, updated by noon the next business day. We only need `IUDMIZC` (the BoE pre-computes the breakeven for us).

#### Canada (CA) — Bank of Canada Valet API

**Maturity mismatch: ~30Y, not 10Y.** Canada's Real Return Bond (RRB) is a long-term (~30-year) instrument. There is no 10-year inflation-linked Canadian government bond, so the Canadian breakeven is structurally a long-term measure, not comparable to the 10Y breakevens from US/UK/ECB/AU.

| Series ID | Description | Frequency | From |
|---|---|---|---|
| `STATIC_ATABLE_V122544_V122553` | **Pre-computed monthly breakeven** (long nominal − RRB) | Monthly | 1996 |
| `BD.CDN.LONG.DQ.YLD` | Long-term (~30Y) nominal benchmark yield | Daily | 2001 |
| `BD.CDN.RRB.DQ.YLD` | Real Return Bond yield | Daily | 2001 |
| `BD.CDN.10YR.DQ.YLD` | 10Y nominal benchmark yield (no matching real) | Daily | 2001 |

API base: `https://www.bankofcanada.ca/valet/observations/{series_id}/json`
No authentication required. Query params: `start_date`, `end_date` (YYYY-MM-DD), `order_dir=asc`. JSON response: observations array with `"d"` (date) and `"series_id": {"v": "value"}` (values as strings, need `as.numeric()`).

Verified: daily long nominal minus RRB matches the pre-computed monthly breakeven exactly (2.03% for Feb 2026). The 10Y nominal minus RRB gives ~1.56% — incorrect, because the RRB is a ~30Y instrument.

**Implementation decision:** Rather than using the ~30Y breakeven (which introduces a term premium mismatch vs the 10Y breakevens from US/UK/AU), Canada uses the **regression-based approach** with US TIPS breakeven as the reference. See regression section below for methodology and results.

#### Australia (AU) — Reserve Bank of Australia (implemented)

The RBA publishes **Table F2 "Capital Market Yields — Government Bonds"** which contains pre-interpolated constant-maturity 10Y yields for both nominal and inflation-indexed bonds. No individual bond interpolation needed (that would be F16).

| Column | Series ID | Description | Available From |
|---|---|---|---|
| 5 | `FCMYGBAG10D` | 10Y nominal government bond yield | May 2013 |
| 6 | `FCMYGBAGID` | 10Y inflation-indexed government bond yield | Nov 2014 |

Breakeven = column 5 − column 6. Both available from Nov 2014.

URLs:
- Daily: `https://www.rba.gov.au/statistics/tables/csv/f2-data.csv`
- Monthly: `https://www.rba.gov.au/statistics/tables/csv/f2.1-data.csv`

No authentication. CSV download. Parsing notes:
- UTF-8 with BOM (`fileEncoding = "UTF-8-BOM"` in `read.csv()`)
- Skip 10 header rows (`skip = 10, header = TRUE`)
- Date format: `DD-Mon-YYYY` (parse with `as.Date(x, format = "%d-%b-%Y")`)
- Missing values are empty strings (handled automatically by `read.csv()`)
- ~2,841 daily observations (Nov 2014–Mar 2026), 13 minor gaps around holidays

Validation: RBA also publishes a quarterly pre-computed breakeven in Table G3 (`g3-data.csv`, series `GBONYLD`). Our computed breakeven matches (e.g., 2.30% for Q4 2025 vs 4.740 − 2.436 = 2.304% from F2).

#### Japan (JP) — No usable free source; regression-estimated

No free source provides a clean 10Y breakeven for Japan:
- **MOF** publishes nominal JGB yields as CSV (`jgbcme_all.csv`, daily, back to 1974) but does NOT publish JGBi (inflation-indexed) yields in any downloadable format.
- **BoJ API** (launched Feb 2026) covers call rates, money market rates, FX, and prices — not JGB yields.
- **ECB** publishes a Japan 10Y "real yield" (`FM.M.JP.JPY.4F.BB.R_JP10YT_RR.YLDA`) but this follows the same ex-post `nominal − HICP` construction as the euro area series — not market-implied. Also stale since February 2025.
- **BB.JBTS** (Japan Bond Trading Co.) publishes daily breakeven data from July 2023, but embedded in HTML/JavaScript requiring scraping.

**Implementation decision:** Japan uses the regression-based approach with US TIPS breakeven as reference. However, R-sq is near zero (0.02) with beta of -0.08 — Japanese inflation is essentially uncorrelated with US inflation. The regression output is effectively Japan's own historical average inflation (~0.17% annualized), which doesn't respond to changing US breakevens. This is a known limitation.

#### Regression-estimated countries (implemented)

Countries without usable free breakeven sources use regression-estimated breakevens. This includes the eurozone (no free linker data), nordics (no free source), Japan (no free source), and Canada (~30Y maturity mismatch).

| Country | Why no direct breakeven |
|---|---|
| DE, FR, IT, NL, BE | ECB "real yield" is ex-post (nominal − HICP), not market-implied. No free country-level linker yields. |
| SE | Issues inflation-linked bonds (SGB IL) but Riksbank API only publishes nominal yields |
| CH | No inflation-linked sovereign bonds |
| DK | No free source found |
| NO | Norges Bank publishes quarterly survey expectations (PDF reports only, no API) |
| JP | MOF publishes nominal JGB yields only; JGBi yields not in downloadable format |
| CA | Bank of Canada RRB is ~30Y maturity — no 10Y equivalent |
| NZ | No free source; monthly CPI unavailable on FRED (quarterly only, step-interpolated) |

**Regression methodology:**

1. Compute quarterly CPI percent change from index levels: `CPI_t / CPI_{t-3} - 1`
2. Annualize by compounding: `(1 + q_change)^4 - 1`
3. Keep only quarter-end months (Mar/Jun/Sep/Dec) for fully independent, non-overlapping observations
4. Fit a rolling 20-quarter (5-year) OLS: `country_ann_q_infl ~ reference_ann_q_infl`
5. Predict: `country_breakeven = alpha + beta * reference_breakeven` (no rescaling — both sides in annualized %)

Quarterly coefficients are forward-filled to monthly for the output panel.

**Why quarterly, not monthly or trailing?** We tested three specifications:
- *10-year trailing CPI* (original): DW stats of 0.02 — massive autocorrelation from 119/120 month overlap. R-sq of 0.97 was spurious (two persistent series appearing correlated just because they're both trending). Betas unreliable.
- *Monthly CPI log-changes*: DW ~2.0 (perfect independence) but R-sq near zero for most countries. Monthly inflation is too noisy — the structural co-movement is buried in idiosyncratic monthly variation. The regression collapses to a constant (intercept only), defeating the purpose of incorporating the reference breakeven.
- *Annualized quarterly CPI changes* (current): DW ~1.8–2.5 (independent, non-overlapping quarters). R-sq of 0.01–0.74 — real but modest explanatory power. Betas are economically sensible and stable. Both Y and X are in the same annualized units as the breakeven, so prediction requires no rescaling.

**Two regression groups:**
- **US-referenced** (DE, FR, IT, NL, BE, SE, CH, DK, NO, JP, CA): regressed on US CPI, predicted with US TIPS breakeven (`T10YIE`). US TIPS is the deepest, most liquid inflation-linked bond market and the cleanest available reference.
- **AU-referenced** (NZ): regressed on Australian CPI, predicted with RBA breakeven. NZ inflation co-moves with Australia far more than with the US or euro area (R-sq 0.51 vs 0.01–0.32).

Domain shift caveat: the regression is fitted on backward-looking realized CPI but applied to a forward-looking market-implied breakeven. For the cross-sectional value signal, this bias is roughly constant across countries and cancels in the long/short construction.

**Latest regression results (20-quarter window, as of April 2026):**

| Country | Ref | Beta | R-sq | DW | Quality |
|---|---|---|---|---|---|
| CA | US | 0.79 | 0.74 | 2.89 | Strong |
| CH | US | 0.49 | 0.71 | 1.85 | Strong |
| NZ | AU | 0.57 | 0.51 | 1.37 | Decent |
| DK | US | 0.78 | 0.50 | 1.64 | Decent |
| FR | US | 0.49 | 0.45 | 1.54 | Moderate |
| DE | US | 0.55 | 0.30 | 2.42 | Weak-moderate |
| BE | US | 0.41 | 0.16 | 1.16 | Weak |
| IT | US | 0.38 | 0.10 | 1.77 | Weak |
| NO | US | 0.14 | 0.03 | 2.64 | Near zero |
| NL | US | 0.37 | 0.04 | 2.74 | Near zero |
| JP | US | -0.08 | 0.02 | 2.59 | Near zero |
| SE | US | 0.10 | 0.01 | 0.83 | Near zero |

CA, CH, NZ, DK, FR respond meaningfully to their reference breakeven. DE is borderline. IT, NL, BE, SE, NO, JP are effectively running on intercept only — the reference breakeven doesn't predict their inflation. For these weak-fit countries, the output is the country's own historical average inflation rate, which is still a reasonable (if static) estimate for the value signal.

### Implementation status

| Source | Country/Countries | Tenor | Script | Status |
|---|---|---|---|---|
| FRED `T10YIE` | US | 10Y | `fetch_breakeven_inflation.R` | Done |
| BoE IADB `IUDMIZC` | GB | 10Y | `fetch_breakeven_inflation.R` | Done |
| RBA Table F2 | AU | 10Y | `fetch_breakeven_inflation.R` | Done |
| FRED CPI (14 countries) | regression inputs | — | `fetch_breakeven_inflation.R` | Done |
| Regression (CPI ~ US) | DE, FR, IT, NL, BE, SE, CH, DK, NO, JP, CA | 10Y (estimated) | `build_breakeven_inflation.R` | Done |
| Regression (CPI ~ AU) | NZ | 10Y (estimated) | `build_breakeven_inflation.R` | Done |

Pipeline: `fetch_breakeven_inflation.R` (fetches & caches raw data) → `build_breakeven_inflation.R` (regressions & final panel).
Output: `data/fred/breakeven_inflation.rds` — panel with columns `date`, `country`, `breakeven` (decimal).

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
| Breakeven panel for all FI value | FI Value (all) | US breakeven + 14 trailing CPI | all 15 via breakeven panel | Removed CPI dependency from FI script; non-US use regression-estimated breakevens |
| US 3M T-bill splice for FX | FX all | 2-month gap Apr-May 2020 | no gap | Single missing OECD USD rate cascaded to all currencies |

---

## Data Pipeline

All factor data is cached locally as `.rds` files. No API calls on subsequent script runs unless cache files are deleted.

```
Step 1 (parallel):
  FRED API (yields, rates)  FRED/BoE/RBA (breakevens, CPI)  FRED+OECD (FX)  AQR (xlsx)
       |                            |                            |               |
       v                            v                            v               v
  fi_raw_yields.rds          be_raw_*.rds                   fx_raw_*.rds    data/aqr/

Step 2:
  be_raw_*.rds → build_breakeven_inflation.R → breakeven_inflation.rds

Step 3:
  fi_raw_yields.rds + breakeven_inflation.rds → fetch_fi_factors.R → fi_factor_returns.rds
```

### Scripts

| Script | Purpose | Data Source | Outputs | Dependencies |
|---|---|---|---|---|
| `fetch_aqr_equity_factors.R` | Parse AQR equity factors | AQR `.xlsx` files | 3 `.rds` in `data/aqr/` | None |
| `fetch_breakeven_inflation.R` | Fetch & cache raw breakeven data | FRED, BoE, RBA, FRED CPI | 5 `be_raw_*.rds` in `data/fred/` | None |
| `fetch_fx_factors.R` | FX factor pipeline | FRED (FX, rates, CPI) + OECD (PPP) | 7 `.rds` in `data/fred/` | None |
| `build_breakeven_inflation.R` | Regression estimates + final panel | Cached `be_raw_*.rds` | `breakeven_inflation.rds` | `fetch_breakeven_inflation.R` |
| `fetch_fi_factors.R` | FI factor pipeline | FRED (yields, rates) + breakeven panel | 4 `.rds` in `data/fred/` | `build_breakeven_inflation.R` |
| `plot_factor_returns.R` | Cumulative return plots | Cached `.rds` files | 3 ggplot objects | All factor scripts |

### Cached Data Files

**`data/fred/`** — FRED-sourced FI and FX data:
- `fi_raw_yields.rds` — 15-country yield + 3M rate panel
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
- `be_raw_us.rds` — US monthly breakeven (FRED T10YIE)
- `be_raw_ecb.rds` — Euro area monthly breakeven (ECB nominal - real)
- `be_raw_boe.rds` — UK monthly breakeven (BoE IUDMIZC)
- `be_raw_rba.rds` — Australia monthly breakeven (RBA F2)
- `be_raw_cpi.rds` — CPI panel for regression countries (8 countries)
- `breakeven_inflation.rds` — Final combined panel (14 countries, monthly)

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
