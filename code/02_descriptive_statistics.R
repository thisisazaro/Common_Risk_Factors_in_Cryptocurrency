# ============================================================
# 02_descriptive_statistics.R
# Purpose:
#   - Load processed cryptocurrency datasets
#   - Produce descriptive statistics for daily, weekly, monthly returns
#   - Visualize return distributions
#   - Compare selected cryptocurrencies with the crypto market proxy
#   - Compare selected cryptocurrencies with the S&P 500
#   - Export tables and figures for thesis/reporting
# ============================================================

# ---------------------------
# 0. Setup
# ---------------------------
source("code/00_setup.R")

message("02_descriptive_statistics.R: started")

# ---------------------------
# 1. File paths
# ---------------------------
daily_file   <- "data/processed/crypto_daily_clean.csv"
weekly_file  <- "data/processed/crypto_weekly.csv"
monthly_file <- "data/processed/crypto_monthly.csv"
market_file  <- "data/processed/market_index_daily.csv"

summary_stats_file        <- "outputs/tables/summary_statistics_by_frequency.csv"
selected_stats_file       <- "outputs/tables/selected_assets_summary_statistics.csv"
extreme_events_file       <- "outputs/tables/extreme_return_events.csv"
panel_a_file              <- "outputs/tables/panel_a_market_characteristics.csv"
panel_b_file              <- "outputs/tables/panel_b_return_characteristics.csv"

dist_plot_file            <- "outputs/figures/return_distributions.png"
crypto_vs_market_file     <- "outputs/figures/crypto_vs_coin_market.png"
crypto_vs_market_sp_file  <- "outputs/figures/crypto_vs_coin_market_sp500.png"

# ---------------------------
# 2. Helper functions
# ---------------------------
calculate_t_stat <- function(x) {
        x <- x[is.finite(x) & !is.na(x)]
        n <- length(x)
        if (n < 2) return(NA_real_)
        mean(x) / (stats::sd(x) / sqrt(n))
}

safe_skewness <- function(x) {
        x <- x[is.finite(x) & !is.na(x)]
        if (length(x) < 3) return(NA_real_)
        moments::skewness(x)
}

safe_kurtosis <- function(x) {
        x <- x[is.finite(x) & !is.na(x)]
        if (length(x) < 4) return(NA_real_)
        moments::kurtosis(x)
}

plot_distribution <- function(data, title_text) {
        ggplot2::ggplot(data, ggplot2::aes(x = Return)) +
                ggplot2::geom_histogram(
                        ggplot2::aes(y = after_stat(density)),
                        bins = 50,
                        fill = "steelblue",
                        alpha = 0.55
                ) +
                ggplot2::geom_density(linewidth = 1) +
                ggplot2::labs(
                        title = title_text,
                        x = "Log Return",
                        y = "Density"
                ) +
                ggplot2::theme_minimal()
}

calculate_summary_stats <- function(data, freq_label) {
        data %>%
                dplyr::group_by(Currency) %>%
                dplyr::summarise(
                        Frequency = freq_label,
                        Mean = mean(Return, na.rm = TRUE) * 100,
                        SD = stats::sd(Return, na.rm = TRUE) * 100,
                        t_Stat = calculate_t_stat(Return),
                        Sharpe = mean(Return, na.rm = TRUE) / stats::sd(Return, na.rm = TRUE),
                        Skewness = safe_skewness(Return),
                        Kurtosis = safe_kurtosis(Return),
                        Positive = mean(Return > 0, na.rm = TRUE) * 100,
                        N = sum(is.finite(Return)),
                        .groups = "drop"
                )
}

save_kable_html <- function(df, caption_text, output_file) {
        html_table <- knitr::kable(df, format = "html", caption = caption_text) %>%
                kableExtra::kable_styling(
                        full_width = FALSE,
                        bootstrap_options = c("striped", "hover", "condensed")
                )
        save_kable(html_table, file = output_file)
}

# ---------------------------
# 3. Load processed datasets
# ---------------------------
if (!file.exists(daily_file)) stop("Missing file: ", daily_file)
if (!file.exists(weekly_file)) stop("Missing file: ", weekly_file)
if (!file.exists(monthly_file)) stop("Missing file: ", monthly_file)
if (!file.exists(market_file)) stop("Missing file: ", market_file)

crypto_daily <- read.csv(daily_file)
crypto_weekly <- read.csv(weekly_file)
crypto_monthly <- read.csv(monthly_file)
market_index_daily <- read.csv(market_file)

crypto_daily$Date <- as.Date(crypto_daily$Date)
crypto_weekly$Week <- as.Date(crypto_weekly$Week)
crypto_monthly$Month <- as.Date(crypto_monthly$Month)
market_index_daily$Date <- as.Date(market_index_daily$Date)

# ---------------------------
# 4. Basic validation
# ---------------------------
required_daily_cols <- c("Currency", "Date", "Close", "Volume", "Return", "ExcessReturn")
required_weekly_cols <- c("Currency", "Week", "Return", "ExcessReturn")
required_monthly_cols <- c("Currency", "Month", "Return", "ExcessReturn")

missing_daily <- setdiff(required_daily_cols, names(crypto_daily))
missing_weekly <- setdiff(required_weekly_cols, names(crypto_weekly))
missing_monthly <- setdiff(required_monthly_cols, names(crypto_monthly))

if (length(missing_daily) > 0) stop("Missing columns in crypto_daily: ", paste(missing_daily, collapse = ", "))
if (length(missing_weekly) > 0) stop("Missing columns in crypto_weekly: ", paste(missing_weekly, collapse = ", "))
if (length(missing_monthly) > 0) stop("Missing columns in crypto_monthly: ", paste(missing_monthly, collapse = ", "))

# Keep only valid rows for descriptive analysis
crypto_daily <- crypto_daily %>%
        dplyr::filter(!is.na(Return), is.finite(Return))

crypto_weekly <- crypto_weekly %>%
        dplyr::filter(!is.na(Return), is.finite(Return))

crypto_monthly <- crypto_monthly %>%
        dplyr::filter(!is.na(Return), is.finite(Return))

# ---------------------------
# 5. Return distributions
# ---------------------------
p_daily <- plot_distribution(crypto_daily, "Daily Cryptocurrency Returns")
p_weekly <- plot_distribution(crypto_weekly, "Weekly Cryptocurrency Returns")
p_monthly <- plot_distribution(crypto_monthly, "Monthly Cryptocurrency Returns")

png(dist_plot_file, width = 900, height = 1500, res = 140)
gridExtra::grid.arrange(p_daily, p_weekly, p_monthly, ncol = 1)
dev.off()

# ---------------------------
# 6. Crypto market proxy vs selected assets
# ---------------------------
selected_currencies <- c("BTC", "ETH", "XRP")

# Create asset-level normalized investment series
crypto_investment <- crypto_daily %>%
        dplyr::filter(Currency %in% selected_currencies) %>%
        dplyr::group_by(Currency) %>%
        dplyr::arrange(Date, .by_group = TRUE) %>%
        dplyr::mutate(
                InvestmentValue = exp(cumsum(Return))
        ) %>%
        dplyr::ungroup()

# Merge with market index proxy
plot_data_market <- crypto_investment %>%
        dplyr::left_join(
                market_index_daily %>%
                        dplyr::select(Date, InvestmentValue),
                by = "Date",
                suffix = c("", "_Market")
        ) %>%
        dplyr::rename(InvestmentValue_Market = InvestmentValue_Market)

p_btc_market <- ggplot2::ggplot(
        plot_data_market %>% dplyr::filter(Currency == "BTC"),
        ggplot2::aes(x = Date)
) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue_Market, color = "Coin Market")) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue, color = "Bitcoin")) +
        ggplot2::labs(
                title = "Bitcoin vs Coin Market",
                y = "Normalized Investment Value",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

p_eth_market <- ggplot2::ggplot(
        plot_data_market %>% dplyr::filter(Currency == "ETH"),
        ggplot2::aes(x = Date)
) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue_Market, color = "Coin Market")) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue, color = "Ethereum")) +
        ggplot2::labs(
                title = "Ethereum vs Coin Market",
                y = "Normalized Investment Value",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

p_xrp_market <- ggplot2::ggplot(
        plot_data_market %>% dplyr::filter(Currency == "XRP"),
        ggplot2::aes(x = Date)
) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue_Market, color = "Coin Market")) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue, color = "Ripple")) +
        ggplot2::labs(
                title = "Ripple vs Coin Market",
                y = "Normalized Investment Value",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

png(crypto_vs_market_file, width = 1000, height = 1500, res = 140)
gridExtra::grid.arrange(p_btc_market, p_eth_market, p_xrp_market, ncol = 1)
dev.off()

# ---------------------------
# 7. Add S&P 500 benchmark comparison
# ---------------------------
suppressWarnings(
        quantmod::getSymbols("^GSPC", src = "yahoo", from = "2018-01-01", auto.assign = TRUE)
)

sp500_data <- data.frame(
        Date = as.Date(zoo::index(GSPC)),
        SP500_Close = as.numeric(quantmod::Cl(GSPC))
) %>%
        dplyr::mutate(
                InvestmentValue_SP500 = SP500_Close / dplyr::first(SP500_Close)
        )

plot_data_sp <- plot_data_market %>%
        dplyr::left_join(
                sp500_data %>% dplyr::select(Date, InvestmentValue_SP500),
                by = "Date"
        ) %>%
        dplyr::filter(
                !is.na(InvestmentValue_Market),
                !is.na(InvestmentValue),
                !is.na(InvestmentValue_SP500)
        )

p_btc_sp <- ggplot2::ggplot(
        plot_data_sp %>% dplyr::filter(Currency == "BTC"),
        ggplot2::aes(x = Date)
) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue_Market, color = "Coin Market")) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue, color = "Bitcoin")) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue_SP500, color = "S&P 500")) +
        ggplot2::labs(
                title = "Bitcoin vs Coin Market and S&P 500",
                y = "Normalized Investment Value",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

p_eth_sp <- ggplot2::ggplot(
        plot_data_sp %>% dplyr::filter(Currency == "ETH"),
        ggplot2::aes(x = Date)
) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue_Market, color = "Coin Market")) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue, color = "Ethereum")) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue_SP500, color = "S&P 500")) +
        ggplot2::labs(
                title = "Ethereum vs Coin Market and S&P 500",
                y = "Normalized Investment Value",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

p_xrp_sp <- ggplot2::ggplot(
        plot_data_sp %>% dplyr::filter(Currency == "XRP"),
        ggplot2::aes(x = Date)
) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue_Market, color = "Coin Market")) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue, color = "Ripple")) +
        ggplot2::geom_line(ggplot2::aes(y = InvestmentValue_SP500, color = "S&P 500")) +
        ggplot2::labs(
                title = "Ripple vs Coin Market and S&P 500",
                y = "Normalized Investment Value",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

png(crypto_vs_market_sp_file, width = 1000, height = 1500, res = 140)
gridExtra::grid.arrange(p_btc_sp, p_eth_sp, p_xrp_sp, ncol = 1)
dev.off()

# ---------------------------
# 8. Summary statistics by frequency
# ---------------------------
daily_stats <- calculate_summary_stats(crypto_daily, "Daily")
weekly_stats <- calculate_summary_stats(crypto_weekly, "Weekly")
monthly_stats <- calculate_summary_stats(crypto_monthly, "Monthly")

summary_stats <- dplyr::bind_rows(daily_stats, weekly_stats, monthly_stats) %>%
        dplyr::arrange(Currency, Frequency)

write.csv(summary_stats, summary_stats_file, row.names = FALSE)

# Optional HTML export
save_kable_html(
        summary_stats,
        "Summary Statistics of Cryptocurrency Returns by Frequency",
        sub("\\.csv$", ".html", summary_stats_file)
)

# ---------------------------
# 9. Extreme return events (daily panel)
# ---------------------------
extreme_events <- crypto_daily %>%
        dplyr::summarise(
                LessThanMinus5  = mean(Return < -0.05, na.rm = TRUE) * 100,
                LessThanMinus10 = mean(Return < -0.10, na.rm = TRUE) * 100,
                LessThanMinus20 = mean(Return < -0.20, na.rm = TRUE) * 100,
                LessThanMinus30 = mean(Return < -0.30, na.rm = TRUE) * 100,
                MoreThan5       = mean(Return > 0.05, na.rm = TRUE) * 100,
                MoreThan10      = mean(Return > 0.10, na.rm = TRUE) * 100,
                MoreThan20      = mean(Return > 0.20, na.rm = TRUE) * 100,
                MoreThan30      = mean(Return > 0.30, na.rm = TRUE) * 100
        )

write.csv(extreme_events, extreme_events_file, row.names = FALSE)

# ---------------------------
# 10. Selected assets and market summary statistics
# ---------------------------
selected_stats <- crypto_daily %>%
        dplyr::filter(Currency %in% selected_currencies) %>%
        dplyr::group_by(Currency) %>%
        dplyr::summarise(
                Mean = mean(ExcessReturn, na.rm = TRUE),
                Median = stats::median(ExcessReturn, na.rm = TRUE),
                SD = stats::sd(ExcessReturn, na.rm = TRUE),
                Skewness = safe_skewness(ExcessReturn),
                Kurtosis = safe_kurtosis(ExcessReturn),
                .groups = "drop"
        )

market_stats <- market_index_daily %>%
        dplyr::summarise(
                Mean = mean(MarketReturn, na.rm = TRUE),
                Median = stats::median(MarketReturn, na.rm = TRUE),
                SD = stats::sd(MarketReturn, na.rm = TRUE),
                Skewness = safe_skewness(MarketReturn),
                Kurtosis = safe_kurtosis(MarketReturn)
        ) %>%
        dplyr::mutate(Currency = "Coin Market Return") %>%
        dplyr::select(Currency, dplyr::everything())

selected_stats_full <- dplyr::bind_rows(selected_stats, market_stats)

write.csv(selected_stats_full, selected_stats_file, row.names = FALSE)

save_kable_html(
        selected_stats_full,
        "Selected Asset and Market Return Statistics",
        sub("\\.csv$", ".html", selected_stats_file)
)

# ---------------------------
# 11. Panel A: market characteristics by year
# ---------------------------
panel_a <- crypto_daily %>%
        dplyr::mutate(Year = as.character(lubridate::year(Date))) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(
                Number_of_Currencies = dplyr::n_distinct(Currency),
                Mean_Close = mean(Close, na.rm = TRUE),
                Median_Close = stats::median(Close, na.rm = TRUE),
                Mean_Volume = mean(Volume, na.rm = TRUE),
                Median_Volume = stats::median(Volume, na.rm = TRUE),
                .groups = "drop"
        )

panel_a_full <- dplyr::bind_rows(
        panel_a,
        crypto_daily %>%
                dplyr::summarise(
                        Year = "Full",
                        Number_of_Currencies = dplyr::n_distinct(Currency),
                        Mean_Close = mean(Close, na.rm = TRUE),
                        Median_Close = stats::median(Close, na.rm = TRUE),
                        Mean_Volume = mean(Volume, na.rm = TRUE),
                        Median_Volume = stats::median(Volume, na.rm = TRUE)
                )
)

write.csv(panel_a_full, panel_a_file, row.names = FALSE)

save_kable_html(
        panel_a_full,
        "Panel A. Market Characteristics by Year",
        sub("\\.csv$", ".html", panel_a_file)
)

# ---------------------------
# 12. Panel B: return characteristics by cryptocurrency
# ---------------------------
panel_b <- crypto_daily %>%
        dplyr::group_by(Currency) %>%
        dplyr::summarise(
                Mean = mean(Return, na.rm = TRUE),
                Median = stats::median(Return, na.rm = TRUE),
                SD = stats::sd(Return, na.rm = TRUE),
                Skewness = safe_skewness(Return),
                Kurtosis = safe_kurtosis(Return),
                .groups = "drop"
        ) %>%
        dplyr::rename(Coin = Currency)

write.csv(panel_b, panel_b_file, row.names = FALSE)

save_kable_html(
        panel_b,
        "Panel B. Return Characteristics by Cryptocurrency",
        sub("\\.csv$", ".html", panel_b_file)
)

# ---------------------------
# 13. Export objects to global environment (optional)
# ---------------------------
assign("summary_stats", summary_stats, envir = .GlobalEnv)
assign("selected_stats_full", selected_stats_full, envir = .GlobalEnv)
assign("extreme_events", extreme_events, envir = .GlobalEnv)
assign("panel_a_full", panel_a_full, envir = .GlobalEnv)
assign("panel_b", panel_b, envir = .GlobalEnv)

message("02_descriptive_statistics.R: completed successfully")
