# ============================================================
# 01_load_and_clean_data.R
# Purpose:
#   - Load raw cryptocurrency data
#   - Clean and validate core variables
#   - Download and merge 1-month Treasury risk-free rate
#   - Construct daily, weekly, and monthly return datasets
#   - Save processed datasets for downstream analysis
# ============================================================

# ---------------------------
# 0. Setup
# ---------------------------
source("code/00_setup.R")

message("01_load_and_clean_data.R: started")

# ---------------------------
# 1. File paths
# ---------------------------
raw_crypto_file <- "data/raw/crypto_data_full.xlsx"

processed_daily_file   <- "data/processed/crypto_daily_clean.csv"
processed_weekly_file  <- "data/processed/crypto_weekly.csv"
processed_monthly_file <- "data/processed/crypto_monthly.csv"
processed_market_file  <- "data/processed/market_index_daily.csv"
processed_rf_file      <- "data/processed/risk_free_daily.csv"

# ---------------------------
# 2. Helper functions
# ---------------------------
safe_na_approx <- function(x) {
        # Interpolate internal gaps only; keep leading/trailing NA if present
        zoo::na.approx(x, na.rm = FALSE)
}

weighted_mean_safe <- function(x, w) {
        valid <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w)
        if (!any(valid)) return(NA_real_)
        sum(x[valid] * w[valid], na.rm = TRUE) / sum(w[valid], na.rm = TRUE)
}

assert_required_columns <- function(data, required_cols) {
        missing_cols <- setdiff(required_cols, names(data))
        if (length(missing_cols) > 0) {
                stop(
                        "Missing required columns in raw crypto dataset: ",
                        paste(missing_cols, collapse = ", ")
                )
        }
}

# ---------------------------
# 3. Load raw cryptocurrency data
# ---------------------------
if (!file.exists(raw_crypto_file)) {
        stop("Raw data file not found: ", raw_crypto_file)
}

crypto_data <- readxl::read_excel(raw_crypto_file)

required_cols <- c("Currency", "Date", "Close", "Volume", "Adjusted")
assert_required_columns(crypto_data, required_cols)

# ---------------------------
# 4. Initial cleaning
# ---------------------------
crypto_data <- crypto_data %>%
        dplyr::mutate(
                Date = as.Date(Date),
                Currency = as.character(Currency),
                Close = as.numeric(Close),
                Volume = as.numeric(Volume),
                Adjusted = as.numeric(Adjusted)
        ) %>%
        dplyr::arrange(Currency, Date)

# Remove rows with missing identifiers
crypto_data <- crypto_data %>%
        dplyr::filter(!is.na(Currency), !is.na(Date))

# Interpolate key numeric variables within each cryptocurrency series
crypto_data <- crypto_data %>%
        dplyr::group_by(Currency) %>%
        dplyr::arrange(Date, .by_group = TRUE) %>%
        dplyr::mutate(
                Close = safe_na_approx(Close),
                Volume = safe_na_approx(Volume),
                Adjusted = safe_na_approx(Adjusted)
        ) %>%
        dplyr::ungroup()

# Basic positivity filters before logs/returns
crypto_data <- crypto_data %>%
        dplyr::filter(
                !is.na(Close), !is.na(Volume), !is.na(Adjusted),
                is.finite(Close), is.finite(Volume), is.finite(Adjusted),
                Close > 0, Volume >= 0, Adjusted > 0
        )

# ---------------------------
# 5. Download risk-free rate from FRED
# ---------------------------
# DGS1MO = 1-Month Treasury Constant Maturity Rate
# Source: FRED via quantmod
suppressWarnings(
        quantmod::getSymbols("DGS1MO", src = "FRED", auto.assign = TRUE)
)

risk_free_data <- data.frame(
        Date = as.Date(zoo::index(DGS1MO)),
        Rate = as.numeric(zoo::coredata(DGS1MO))
) %>%
        dplyr::filter(Date >= as.Date("2018-01-01"),
                      Date <= as.Date("2024-12-31")) %>%
        dplyr::arrange(Date)

# Fill missing observations conservatively
# First: linear interpolation across internal gaps
# Second: LOCF / backward fill for edges if needed
risk_free_data <- risk_free_data %>%
        dplyr::mutate(
                Rate = zoo::na.approx(Rate, na.rm = FALSE),
                Rate = zoo::na.locf(Rate, na.rm = FALSE),
                Rate = zoo::na.locf(Rate, fromLast = TRUE, na.rm = FALSE),
                Rate = Rate / 100
        )

# Save risk-free series
write.csv(risk_free_data, processed_rf_file, row.names = FALSE)

# ---------------------------
# 6. Merge risk-free rate into crypto panel
# ---------------------------
crypto_data <- crypto_data %>%
        dplyr::left_join(risk_free_data, by = "Date") %>%
        dplyr::arrange(Currency, Date)

# Carry risk-free rate down/up after merge to avoid isolated holidays/weekends gaps
crypto_data <- crypto_data %>%
        tidyr::fill(Rate, .direction = "downup")

# ---------------------------
# 7. Construct daily arithmetic and log returns
# ---------------------------
crypto_data <- crypto_data %>%
        dplyr::group_by(Currency) %>%
        dplyr::arrange(Date, .by_group = TRUE) %>%
        dplyr::mutate(
                Return_simple = Close / dplyr::lag(Close) - 1,
                Return = log(Close / dplyr::lag(Close))
        ) %>%
        dplyr::ungroup()

# Daily excess return:
# user originally subtracted annualized FRED level directly from returns.
# To preserve your project logic while improving clarity, we convert the annual
# rate to an approximate daily rate (252 trading days).
crypto_data <- crypto_data %>%
        dplyr::mutate(
                RiskFree_daily = Rate / 252,
                ExcessReturn = Return - RiskFree_daily
        )

# Remove invalid return rows
crypto_daily <- crypto_data %>%
        dplyr::filter(
                !is.na(Return),
                is.finite(Return),
                !is.na(ExcessReturn),
                is.finite(ExcessReturn)
        )

# ---------------------------
# 8. Add market-cap proxy and daily market index
# ---------------------------
# IMPORTANT:
# Your original code used Close * Volume as a market-cap proxy.
# This is not true market capitalization, but we preserve it as a liquidity-size proxy
# to remain consistent with the existing empirical design.
crypto_daily <- crypto_daily %>%
        dplyr::mutate(
                MarketCap_proxy = Close * Volume
        )

market_index_daily <- crypto_daily %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(
                Total_MarketCap_proxy = sum(MarketCap_proxy, na.rm = TRUE),
                MarketReturn = weighted_mean_safe(Return, MarketCap_proxy),
                .groups = "drop"
        ) %>%
        dplyr::arrange(Date) %>%
        dplyr::filter(!is.na(MarketReturn), is.finite(MarketReturn)) %>%
        dplyr::mutate(
                InvestmentValue = exp(cumsum(MarketReturn))
        )

# ---------------------------
# 9. Aggregate to weekly frequency
# ---------------------------
crypto_weekly <- crypto_daily %>%
        dplyr::mutate(Week = lubridate::floor_date(Date, unit = "week")) %>%
        dplyr::group_by(Currency, Week) %>%
        dplyr::summarise(
                Close = dplyr::last(Close),
                Adjusted = dplyr::last(Adjusted),
                Volume = sum(Volume, na.rm = TRUE),
                Return = sum(Return, na.rm = TRUE),
                ExcessReturn = sum(ExcessReturn, na.rm = TRUE),
                Rate = sum(RiskFree_daily, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        dplyr::mutate(
                MarketCap_proxy = Close * Volume
        ) %>%
        dplyr::filter(
                !is.na(Return), is.finite(Return),
                !is.na(ExcessReturn), is.finite(ExcessReturn),
                !is.na(MarketCap_proxy), is.finite(MarketCap_proxy)
        )

# Optional liquidity-size threshold retained from original logic
crypto_weekly <- crypto_weekly %>%
        dplyr::filter(MarketCap_proxy >= 1e6)

market_index_weekly <- crypto_weekly %>%
        dplyr::group_by(Week) %>%
        dplyr::summarise(
                Total_MarketCap_proxy = sum(MarketCap_proxy, na.rm = TRUE),
                MarketReturn = weighted_mean_safe(Return, MarketCap_proxy),
                .groups = "drop"
        )

# ---------------------------
# 10. Aggregate to monthly frequency
# ---------------------------
crypto_monthly <- crypto_daily %>%
        dplyr::mutate(Month = lubridate::floor_date(Date, unit = "month")) %>%
        dplyr::group_by(Currency, Month) %>%
        dplyr::summarise(
                Close = dplyr::last(Close),
                Adjusted = dplyr::last(Adjusted),
                Volume = sum(Volume, na.rm = TRUE),
                Return = sum(Return, na.rm = TRUE),
                ExcessReturn = sum(ExcessReturn, na.rm = TRUE),
                Rate = sum(RiskFree_daily, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        dplyr::mutate(
                MarketCap_proxy = Close * Volume
        ) %>%
        dplyr::filter(
                !is.na(Return), is.finite(Return),
                !is.na(ExcessReturn), is.finite(ExcessReturn)
        )

# ---------------------------
# 11. Quality checks
# ---------------------------
message("Rows in cleaned daily panel: ", nrow(crypto_daily))
message("Rows in weekly panel: ", nrow(crypto_weekly))
message("Rows in monthly panel: ", nrow(crypto_monthly))
message("Number of currencies (daily): ", dplyr::n_distinct(crypto_daily$Currency))
message("Date range: ", min(crypto_daily$Date), " to ", max(crypto_daily$Date))

if (nrow(crypto_daily) == 0) stop("Cleaned daily dataset is empty.")
if (nrow(crypto_weekly) == 0) stop("Weekly dataset is empty.")
if (nrow(crypto_monthly) == 0) stop("Monthly dataset is empty.")

# ---------------------------
# 12. Save processed outputs
# ---------------------------
write.csv(crypto_daily, processed_daily_file, row.names = FALSE)
write.csv(crypto_weekly, processed_weekly_file, row.names = FALSE)
write.csv(crypto_monthly, processed_monthly_file, row.names = FALSE)
write.csv(market_index_daily, processed_market_file, row.names = FALSE)

# ---------------------------
# 13. Export objects to global environment (optional)
# ---------------------------
# This is useful when running scripts interactively in RStudio.
assign("crypto_daily", crypto_daily, envir = .GlobalEnv)
assign("crypto_weekly", crypto_weekly, envir = .GlobalEnv)
assign("crypto_monthly", crypto_monthly, envir = .GlobalEnv)
assign("market_index_daily", market_index_daily, envir = .GlobalEnv)
assign("risk_free_data", risk_free_data, envir = .GlobalEnv)

message("01_load_and_clean_data.R: completed successfully")
