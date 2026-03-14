# Common_Risk_Factors_in_Cryptocurrency

A reproducible **R-based research repository** for analyzing cryptocurrency returns through a structured empirical pipeline. The project combines **data cleaning, descriptive statistics, cross-sectional predictors, crypto factor construction, grouped market comparison, attention models, macro-sentiment-hashrate models, and blockchain network factor models** into one transparent workflow.

This repository is designed not just as a code archive, but as a **research handoff system**: the outputs are organized, interpretable, reproducible, and ready for thesis writing, portfolio presentation, or methodological defense.

---

## Research Focus

This project studies how cryptocurrency returns behave across several complementary dimensions:

- **descriptive return dynamics**
- **cross-sectional return predictors**
- **factor-style portfolio construction**
- **meme vs conventional coin comparison**
- **Google Trends attention effects**
- **macro-financial, sentiment, and hash-rate drivers**
- **blockchain network activity factors**

The central goal is to move from a raw crypto dataset to a complete analytical framework with exportable tables, figures, and model outputs.

---

## What the Project Covers

### 1. Data ingestion and cleaning
The pipeline loads raw cryptocurrency data, validates variables, interpolates missing values where appropriate, merges a risk-free rate series, and constructs:

- daily panel
- weekly panel
- monthly panel
- market proxy series

### 2. Descriptive statistics
The project generates:

- summary statistics by frequency
- return distribution plots
- extreme return event tables
- benchmark comparisons against the crypto market and S&P 500
- panel-style market characteristic tables

### 3. Cross-sectional predictors
The repository evaluates characteristic-sorted portfolio effects using quintile-based analysis for:

- size-related characteristics
- momentum
- volume
- volatility

### 4. Crypto factor model
The project estimates crypto factor-style series such as:

- **CMKT** — crypto market factor
- **CSMB** — size-proxy factor
- **CMOM** — momentum-related factor

It also tests characteristic-sorted long-short portfolios against those factors.

### 5. Meme vs conventional comparison
The pipeline builds grouped return series for:

- **Conventional coins**: BTC, ETH, XRP
- **Meme coins**: DOGE, SHIB

and compares their performance, market-relative behavior, and drawdowns.

### 6. Attention models
The repository links cryptocurrency returns to **Google Trends attention series** across multiple keywords and target return groups.

### 7. Macro / sentiment / hash-rate models
The project incorporates:

- S&P 500
- Dow Jones
- 10Y Treasury yield
- Gold futures
- Fear & Greed Index
- Bitcoin hash rate

to study how external macro and sentiment conditions relate to crypto returns.

### 8. Network factor models
The repository builds Bitcoin blockchain network variables from raw JSON files, aggregates them to weekly frequency, constructs a PCA-based network factor, and tests whether on-chain activity helps explain crypto return dynamics.

---

## Project Structure

```text
Common_Risk_Factors_in_Cryptocurrency/
├── README.md
├── Common_Risk_Factors_in_Cryptocurrency.Rproj
├── addin.R
├── sync.R
│
├── assets/
├── code/
│   ├── 00_setup.R
│   ├── 01_load_and_clean_data.R
│   ├── 02_descriptive_statistics.R
│   ├── 03_cross_sectional_predictors.R
│   ├── 04_crypto_factor_model.R
│   ├── 05_memecoin_vs_conventional_analysis.R
│   ├── 06_google_trends_attention_models.R
│   ├── 07_macro_sentiment_hashrate_models.R
│   ├── 08_network_factors_models.R
│   ├── 09_export_tables_and_figures.R
│   └── 10_run_all.R
│
├── data/
│   ├── raw/
│   └── processed/
│
├── env/
├── memo/
├── paper/
│
└── outputs/
    ├── figures/
    │   ├── google_trends/
    │   ├── conventional_assets_vs_coin_market.png
    │   ├── crypto_factor_cumulative_returns.png
    │   ├── crypto_vs_coin_market.png
    │   ├── crypto_vs_coin_market_sp500.png
    │   ├── generic_long_short_returns.png
    │   ├── generic_quintile_returns.png
    │   ├── macro_sentiment_hashrate_correlation_heatmap.png
    │   ├── meme_vs_conventional_drawdowns.png
    │   ├── meme_vs_conventional_vs_market.png
    │   ├── network_factor_correlation_heatmap.png
    │   ├── network_pca_factor_vs_returns.png
    │   └── return_distributions.png
    │
    ├── logs/
    │   ├── pipeline_run_log.csv
    │   └── pipeline_run_summary.txt
    │
    ├── models/
    ├── tables/
    │   ├── crypto_factor_correlations.csv
    │   ├── crypto_factor_model_results.csv
    │   ├── crypto_factor_model_results.html
    │   ├── crypto_factor_returns.csv
    │   ├── extreme_return_events.csv
    │   ├── group_return_summary_statistics.csv
    │   ├── group_return_summary_statistics.html
    │   ├── macro_sentiment_hashrate_correlations.csv
    │   ├── macro_sentiment_hashrate_model_results.csv
    │   ├── macro_sentiment_hashrate_model_results.html
    │   ├── macro_sentiment_hashrate_summary.csv
    │   ├── macro_sentiment_hashrate_summary.html
    │   ├── memecoin_vs_conventional_summary_statistics.csv
    │   ├── memecoin_vs_conventional_summary_statistics.html
    │   ├── momentum_strategy_summary.csv
    │   ├── momentum_strategy_summary.html
    │   ├── network_factor_correlations.csv
    │   ├── network_factor_summary.csv
    │   ├── network_factor_summary.html
    │   ├── network_granger_results.csv
    │   ├── network_multivariate_model_results.csv
    │   ├── network_multivariate_model_results.html
    │   ├── network_pca_loadings.csv
    │   ├── network_univariate_model_results.csv
    │   ├── network_univariate_model_results.html
    │   ├── panel_a_market_characteristics.csv
    │   ├── panel_a_market_characteristics.html
    │   ├── panel_b_return_characteristics.csv
    │   ├── panel_b_return_characteristics.html
    │   ├── selected_assets_summary_statistics.csv
    │   ├── selected_assets_summary_statistics.html
    │   ├── size_strategy_quintiles.csv
    │   ├── size_strategy_quintiles.html
    │   ├── size_strategy_spreads.csv
    │   ├── size_strategy_spreads.html
    │   ├── strategy_test_portfolios.csv
    │   ├── summary_statistics_by_frequency.csv
    │   ├── summary_statistics_by_frequency.html
    │   ├── volatility_strategy_summary.csv
    │   ├── volatility_strategy_summary.html
    │   ├── volume_strategy_summary.csv
    │   └── volume_strategy_summary.html
    │
    ├── DELIVERABLES_INDEX.md
    ├── export_manifest.csv
    ├── export_manifest.html
    ├── portfolio_summary.md
    ├── project_inventory.csv
    └── reproducibility_summary.csv
```

## Core Pipeline Modules

### `00_setup.R`
Loads packages, creates project directories, sets global options, and defines lightweight helper utilities used throughout the project.

### `01_load_and_clean_data.R`
Builds the cleaned daily, weekly, and monthly cryptocurrency datasets and prepares the basic market proxy and risk-free rate series.

### `02_descriptive_statistics.R`
Generates descriptive tables and benchmark plots, including return distributions, market comparisons, and characteristic panels.

### `03_cross_sectional_predictors.R`
Runs characteristic-sorted quintile analysis for size, momentum, volume, and volatility strategies.

### `04_crypto_factor_model.R`
Constructs crypto factor returns, builds long-short test portfolios, and estimates factor regressions.

### `05_memecoin_vs_conventional_analysis.R`
Creates grouped return baskets for conventional and meme coins, then compares performance and drawdowns.

### `06_google_trends_attention_models.R`
Studies how Google search attention relates to cryptocurrency return movements using lag-augmented regression models.

### `07_macro_sentiment_hashrate_models.R`
Combines weekly crypto returns with macro-financial, sentiment, and hash-rate variables and estimates baseline explanatory models.

### `08_network_factors_models.R`
Builds and tests blockchain network factors from raw Bitcoin network JSON files, including a PCA-based composite factor.

### `09_export_tables_and_figures.R`
Audits and indexes all generated outputs, producing a handoff-ready manifest and reproducibility summary.

### `10_run_all.R`
Runs the full project pipeline sequentially and writes a complete execution log.

---

## Raw Data Requirements

The repository expects source files in `data/raw/`. Depending on the module, these may include:

- raw cryptocurrency market data
- Google Trends exports
- Bitcoin network JSON files such as:
  - `n-payments.json`
  - `n-transactions.json`
  - `n-unique-addresses.json`
  - `output-volume.json`
- additional manually exported or downloaded series used by specific modules

Processed outputs are saved automatically to `data/processed/`.

---

## Reproducibility

This project is designed as a reproducible analytical pipeline.

### Run the full project

From the project root:

```r
source("code/10_run_all.R")
```

### Or run modules individually

For example:

```r
source("code/01_load_and_clean_data.R")
source("code/02_descriptive_statistics.R")
source("code/03_cross_sectional_predictors.R")
source("code/04_crypto_factor_model.R")
source("code/05_memecoin_vs_conventional_analysis.R")
source("code/06_google_trends_attention_models.R")
source("code/07_macro_sentiment_hashrate_models.R")
source("code/08_network_factors_models.R")
source("code/09_export_tables_and_figures.R")
```

### Execution logs

After a full run, logs are stored in:

- `outputs/logs/pipeline_run_log.csv`

- `outputs/logs/pipeline_run_summary.txt`

---

## Main Outputs

The repository produces three main categories of deliverables.

### 1. Processed datasets

Examples:

- cleaned daily crypto panel

- weekly grouped return dataset

- macro/sentiment/hash-rate merged dataset

- network-crypto merged weekly dataset

### 2. Tables

Examples:

- descriptive summary statistics

- cross-sectional strategy tables

- factor model results

- grouped meme vs conventional summaries

- macro and network model outputs

- HTML-ready export tables for handoff

### 3. Figures

Examples:

- return distribution plots

- crypto vs market benchmark charts

- meme vs conventional performance comparison

- factor cumulative return plots

- correlation heatmaps

- PCA-based network factor comparison charts

---

## Interpretation Notes

A few variables in the project require careful interpretation.

### Market size proxy

In some modules, market size is approximated by:

`Close × Volume`

This is not true market capitalization. It should be interpreted as a size-liquidity proxy.

### Attention data
 
Google Trends data are normalized search-intensity indices, not absolute search counts. They are best interpreted as relative attention measures.

### Grouped portfolios

Value-weighted grouped series may be heavily influenced by the largest asset within a category. This matters especially for meme-coin baskets and should be acknowledged when interpreting results.

### Factor interpretation

Crypto factors in this project are empirical constructions tailored to the available dataset and should not be treated as exact analogs of traditional equity factors without qualification.

---

## Why This Repository Is Structured This Way

This project follows a simple principle:

A research project should be transferred as a transparent analytical system, not as an archive of disconnected files.

The final result is not only code, but a complete research handoff:

- structured processed data

- modular and readable scripts

- exportable tables and figures

- interpretable outputs for writing and defense

- logs and metadata for reproducibility

- portfolio-ready documentation

This makes the repository useful for:

- thesis and dissertation work

- empirical finance portfolio presentation

- research consulting showcase

- methodological handoff to supervisors or collaborators

- GitHub-based academic profile building

---

## Suggested Next Improvements

Natural future extensions include:

- HAC / Newey-West standard errors

- rolling-window estimation

- out-of-sample testing

- regime-dependent models

- stronger robustness checks

- equal-weight vs value-weight grouped portfolios

- further modularization into dedicated utils/ files

---

## Final Note

This repository is intended to demonstrate a professional standard of empirical research organization:

- structured

- transparent

- modular

- interpretable

- reproducible

- portfolio-ready





## Code Organization

The codebase is structured as a staged research pipeline:

- `00_setup.R` — package loading and project setup
- `01_load_and_clean_data.R` — raw data import, cleaning, interpolation, and return construction
- `02_descriptive_statistics.R` — summary statistics and descriptive plots
- `03_cross_sectional_predictors.R` — size, momentum, volume, and volatility-based portfolio tests
- `04_crypto_factor_model.R` — cryptocurrency factor construction and regression analysis
- `05_memecoin_vs_conventional_analysis.R` — comparison of established cryptocurrencies and memecoins
- `06_google_trends_attention_models.R` — investor attention analysis using Google Trends
- `07_macro_sentiment_hashrate_models.R` — macro-financial and sentiment regressions
- `08_network_factors_models.R` — blockchain network factor analysis
- `09_export_tables_and_figures.R` — export of final tables and figures
- `10_run_all.R` — full project pipeline