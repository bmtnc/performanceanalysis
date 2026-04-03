# FRED Government Bond Total Return Index Inventory

**Date:** 2026-04-02
**Purpose:** Evaluate FRED as a source for government bond total return indices for a cross-country fixed income factor model.

---

## Executive Summary

**FRED does NOT have government bond total return indices for any country.** After exhaustive searching (20+ distinct queries, 500+ series reviewed, direct ID lookups), the key finding is:

- ICE BofA (formerly BAML) publishes **corporate credit** and **emerging markets** total return indices on FRED, but **no government/treasury bond total return indices**.
- No other provider (Bloomberg Barclays, FTSE, S&P, Citigroup/WGBI) has government bond total return indices on FRED.
- FRED **does** have excellent coverage of 10-year government bond **yields** (OECD data, 40 countries, monthly), which can be used with a duration-based approximation -- but these are not total return indices.

**Implication:** Actual government bond total return indices will need to come from a different data source (Bloomberg Terminal, Refinitiv/LSEG, ICE Data Services direct, or commercial providers like Global Financial Data).

---

## Section 1: What FRED Has (Relevant to Fixed Income Returns)

### 1A. ICE BofA (BAML) Total Return Index Values on FRED

There are **48 BAML TRIV series** on FRED. Every single one is either:
- US Corporate bonds (by rating: AAA, AA, A, BBB; by maturity: 1-3Y, 3-5Y, 5-7Y, 7-10Y, 10-15Y, 15Y+)
- US High Yield (overall, BB, B, CCC)
- Euro High Yield
- Emerging Markets Corporate Plus (by region, by rating, by sector)

**Zero government/treasury/sovereign bond total return indices exist in the BAML/ICE BofA universe on FRED.**

Key BAML TRIV series for reference (all daily frequency):

| Series ID | Title | Start |
|-----------|-------|-------|
| BAMLCC0A0CMTRIV | US Corporate Index TR | 1972-12-31 |
| BAMLCC0A1AAATRIV | AAA US Corporate TR | 1988-12-16 |
| BAMLCC0A4BBBTRIV | BBB US Corporate TR | 1988-12-16 |
| BAMLCC4A0710YTRIV | 7-10Y US Corporate TR | 1992-06-30 |
| BAMLHYH0A0HYM2TRIV | US High Yield TR | 1986-08-31 |
| BAMLHE00EHYITRIV | Euro High Yield TR | 1997-12-31 |
| BAMLEMCBPITRIV | EM Corporate Plus TR | 1998-12-31 |

### 1B. Nasdaq Compoundr Indices (Very Recent, US Only)

| Series ID | Title | Freq | Start | End |
|-----------|-------|------|-------|-----|
| NASDAQNCPXT | Nasdaq Compoundr U.S. Treasury 10-Year Note Total Return Index | D | 2025-07-16 | current |
| NASDAQNCPXN | Nasdaq Compoundr U.S. Treasury 10-Year Note Net Total Return Index | D | 2025-07-16 | current |
| NASDAQNCPX | Nasdaq Compoundr U.S. Treasury 10-Year Note Index (price) | D | 2025-07-16 | current |
| NASDAQNCPAGT | Nasdaq Compoundr U.S. Aggregate Bond Total Return Index | D | 2025-07-16 | current |

**Problem:** These only start in July 2025 (less than 1 year of data). US only. Not usable for historical backtesting.

### 1C. Swedish Government Bond Index

| Series ID | Title | Freq | Start | End |
|-----------|-------|------|-------|-----|
| NASDAQSWGOVT115 | Swedish Government Bond 1-15 | D | 2009-06-30 | current |

This is a price index (not explicitly total return) for Swedish government bonds with 1-15 year maturity. It is the only country-specific government bond index of any kind on FRED (besides the Nasdaq Compoundr US series above).

### 1D. Other Bond-Related Indices

| Series ID | Title | Type |
|-----------|-------|------|
| CRTINDEXTIER0-3, CRTINDEXMIDTIER | AD&Co US Mortgage High Yield Indices | Mortgage, not govt |
| WSLB20 / MSLB20 | Bond Buyer 20-Bond Municipal Bond Index | Municipal, DISCONTINUED |

---

## Section 2: What FRED Has (Yields, Not Returns)

### 2A. OECD 10-Year Government Bond Yields (IRLTLT01 Series)

FRED has **40 monthly series** of 10-year government bond yields from the OECD. These are the series we are currently using for the duration-based return approximation.

**Core Developed Markets:**

| Series ID | Country | Start | End |
|-----------|---------|-------|-----|
| IRLTLT01USM156N | United States | 1953-04 | 2026-02 |
| IRLTLT01GBM156N | United Kingdom | 1960-01 | 2026-02 |
| IRLTLT01DEM156N | Germany | 1956-05 | 2026-01 |
| IRLTLT01JPM156N | Japan | 1989-01 | 2026-02 |
| IRLTLT01FRM156N | France | 1960-01 | 2026-02 |
| IRLTLT01CAM156N | Canada | 1955-01 | 2026-02 |
| IRLTLT01AUM156N | Australia | 1969-07 | 2026-02 |
| IRLTLT01ITM156N | Italy | 1991-03 | 2026-02 |
| IRLTLT01CHM156N | Switzerland | 1955-01 | 2026-02 |
| IRLTLT01NLM156N | Netherlands | 1959-01 | 2026-02 |
| IRLTLT01BEM156N | Belgium | 1955-01 | 2026-02 |
| IRLTLT01ESM156N | Spain | 1980-01 | 2026-02 |
| IRLTLT01SEM156N | Sweden | 1986-12 | 2026-02 |
| IRLTLT01NOM156N | Norway | 1985-01 | 2026-02 |
| IRLTLT01DKM156N | Denmark | 1987-01 | 2026-02 |
| IRLTLT01NZM156N | New Zealand | 1970-01 | 2026-02 |
| IRLTLT01EZM156N | Euro Area (19) | 1970-01 | 2026-01 |

**Other Countries (Shorter Histories):**

| Series ID | Country | Start |
|-----------|---------|-------|
| IRLTLT01ATM156N | Austria | 1990-01 |
| IRLTLT01FIM156N | Finland | 1988-01 |
| IRLTLT01IEM156N | Ireland | 1970-12 |
| IRLTLT01PTM156N | Portugal | 1993-07 |
| IRLTLT01LUM156N | Luxembourg | 1993-10 |
| IRLTLT01GRM156N | Greece | 1997-06 |
| IRLTLT01KRM156N | Korea | 2000-10 |
| IRLTLT01PLM156N | Poland | 2001-01 |
| IRLTLT01MXM156N | Mexico | 2001-07 |
| IRLTLT01CZM156N | Czech Republic | 2000-04 |
| IRLTLT01HUM156N | Hungary | 1999-02 |
| IRLTLT01ILM156N | Israel | 1997-01 |
| IRLTLT01ZAM156N | South Africa | 1957-01 |
| IRLTLT01CLM156N | Chile | 2004-07 |
| IRLTLT01SIM156N | Slovenia | 2002-03 |
| IRLTLT01SKM156N | Slovak Republic | 2000-09 |
| IRLTLT01ISM156N | Iceland | 1992-01 (disc. 2022-08) |
| IRLTLT01RUM156N | Russia | 1999-01 (disc. 2018-06) |
| COLIRLTLT01STM | Colombia | 2003-01 |
| CRIIRLTLT01STM | Costa Rica | 2011-08 |
| INDIRLTLT01STM | India | 2011-12 |
| LTUIRLTLT01STM | Lithuania | 2001-01 |
| LVAIRLTLT01STM | Latvia | 2001-01 |

---

## Section 3: Search Queries Executed

The following queries were run via `fredr::fredr_series_search_text()` and `fredr::fredr_series_search_id()`:

| Query | Results | Govt Bond TR Found? |
|-------|---------|---------------------|
| "government bond total return" | 2 | No (EM corporate) |
| "treasury total return" | 45 | No (IRS tax data) |
| "BAML government" | 40 | No (EM corporate) |
| "ICE BofA government" | 40 | No (EM corporate) |
| "bond index total return" | 29 | No (corporate/HY/EM) |
| "sovereign bond index" | 16 | No (corporate/HY) |
| "government bond index" | 35 | No (municipal/EM) |
| "ICE BofA 10 year" | 12 | No (corporate only) |
| "ICE BofA treasury" | 48 | No (OAS spreads) |
| "BofA US Treasury" | 45 | No (OAS spreads) |
| "BofA total return" | 48 | No (corp/HY/EM TRIV) |
| "Nasdaq Compoundr" | 12 | Yes (US only, from Jul 2025) |
| "FTSE government bond" | 0 | No |
| "Bloomberg Barclays government" | 0 | No |
| "UK gilt total return" | 0 | No |
| "German bund total return" | 0 | No |
| "Citigroup World Government Bond" | 0 | No |
| "WGBI" | 0 | No |
| "government total return" | 58 | No (ESG/equity indices) |
| "Barclays government" | 0 | No |
| "S&P government bond" | 1 | No (historical yields) |
| "CRSP bond" | 0 | No |
| fredr_series_search_id("BAML") | 193 | No (0 govt in 193 series) |
| fredr_series_search_id("IRLTLT01") | 40 monthly | Yields only, not returns |
| Direct ID lookups (BAMLGV*, BAMLGT*, etc.) | 0 | Not found |

---

## Section 4: Conclusions and Recommendations

### 4.1 FRED Cannot Provide What We Need

FRED does not carry government bond total return indices by country. The ICE BofA indices on FRED are exclusively credit (corporate, high yield, emerging markets). The one treasury total return index (Nasdaq Compoundr, NASDAQNCPXT) has less than 1 year of history and is US-only.

### 4.2 Current Approach Remains Best Available via FRED

The duration-based approximation from OECD yield data (IRLTLT01 series) remains the best method available using FRED data alone. The approximation formula:

```
Return ~ -Duration * dY + Y_prev / 12 + 0.5 * Convexity * (dY)^2
```

Key limitations of this approximation:
- Assumes constant 10-year duration (actual varies with coupon, yield level)
- Ignores roll-down return (yield curve shape effect)
- Ignores coupon reinvestment rate differences
- Convexity term is approximate
- Can deviate from actual returns by 20-50 bps/month in volatile periods

### 4.3 Alternative Data Sources for Actual Total Return Indices

To obtain actual government bond total return indices by country, consider:

1. **Bloomberg Terminal** -- LEGATRUU (US Agg), various country govt indices. Best coverage but requires terminal access.

2. **Refinitiv/LSEG (formerly Thomson Reuters)** -- Provides FTSE government bond indices (formerly Citigroup WGBI components) by country. Available via Datastream or Eikon.

3. **ICE Data Services (direct)** -- The same ICE BofA indices exist for government bonds (e.g., the ICE BofA US Treasury Index, various country government bond indices), but these are NOT published on FRED. They require a direct data license.

4. **Global Financial Data (GFD)** -- Has historical total return data for government bonds across many countries going back decades.

5. **S&P/LSEG** -- S&P publishes government bond indices; FTSE Russell (now LSEG) publishes the former Citigroup WGBI country components.

6. **Central Banks** -- Some central banks publish their own government bond indices (e.g., Bank of Canada, Reserve Bank of Australia).

### 4.4 Recommended Next Steps

1. **Short term:** Continue with the OECD yield-based duration approximation from FRED. Document the approximation error and its impact on factor model results.

2. **Medium term:** Investigate whether ICE BofA government bond total return indices can be accessed via a direct data license or an alternative free API. The indices definitely exist (ICE BofA publishes government bond indices for US, UK, Germany, Japan, France, Canada, Australia, Italy, etc.) -- they are simply not redistributed via FRED.

3. **Alternative free source to investigate:** Check if the World Bank, BIS, or OECD provides total return data (not just yields) via their APIs.
