# ============================================================
# 05_memecoin_vs_conventional_analysis.R
# Purpose:
#   - Compare conventional cryptocurrencies and meme coins
#   - Build grouped return indices and drawdowns
#   - Produce descriptive statistics and comparative figures
#   - Save processed datasets for downstream modules
# ============================================================

# ---------------------------
# 0. Setup
# ---------------------------
source("code/00_setup.R")

message("05_memecoin_vs_conventional_analysis.R: started")

# ---------------------------
# 1. File paths
# ---------------------------
processed_group_daily_file   <- "data/processed/crypto_group_comparison_daily.csv"
processed_group_weekly_file  <- "data/processed/crypto_group_comparison_weekly.csv"
processed_group_monthly_file <- "data/processed/crypto_group_comparison_monthly.csv"
processed_weekly_caps_file   <- "data/processed/weekly_market_caps.csv"

summary_stats_file           <- "outputs/tables/memecoin_vs_conventional_summary_statistics.csv"
group_summary_file           <- "outputs/tables/group_return_summary_statistics.csv"

plot_assets_vs_market_file   <- "outputs/figures/conventional_assets_vs_coin_market.png"
plot_group_comparison_file   <- "outputs/figures/meme_vs_conventional_vs_market.png"
plot_group_drawdown_file     <- "outputs/figures/meme_vs_conventional_drawdowns.png"

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

compute_drawdown <- function(index_series) {
        running_max <- cummax(index_series)
        (index_series / running_max) - 1
}

to_index_100 <- function(ret) {
        100 * exp(cumsum(dplyr::coalesce(dplyr::lag(ret), 0)))
}

save_kable_html <- function(df, caption_text, output_file) {
        html_table <- knitr::kable(df, format = "html", caption = caption_text, escape = FALSE) %>%
                kableExtra::kable_styling(
                        full_width = FALSE,
                        bootstrap_options = c("striped", "hover", "condensed")
                )
        save_kable(html_table, file = output_file)
}

calculate_summary_table <- function(data, value_cols, frequency_label) {
        result <- lapply(value_cols, function(col_name) {
                x <- data[[col_name]]
                tibble::tibble(
                        Series = col_name,
                        Frequency = frequency_label,
                        Mean = mean(x, na.rm = TRUE) * 100,
                        Median = stats::median(x, na.rm = TRUE) * 100,
                        SD = stats::sd(x, na.rm = TRUE) * 100,
                        t_Stat = calculate_t_stat(x),
                        Sharpe = mean(x, na.rm = TRUE) / stats::sd(x, na.rm = TRUE),
                        Skewness = safe_skewness(x),
                        Kurtosis = safe_kurtosis(x),
                        Positive = mean(x > 0, na.rm = TRUE) * 100,
                        N = sum(is.finite(x))
                )
        })
        
        dplyr::bind_rows(result)
}

# ---------------------------
# 3. Download and prepare asset data
# ---------------------------
tickers <- c("BTC-USD", "ETH-USD", "XRP-USD", "DOGE-USD", "SHIB-USD")

suppressWarnings(
        quantmod::getSymbols(
                Symbols = tickers,
                src = "yahoo",
                from = "2021-01-01",
                auto.assign = TRUE
        )
)

crypto_group_daily <- data.frame(
        date = as.Date(zoo::index(`BTC-USD`)),
        BTC_Close = as.numeric(quantmod::Cl(`BTC-USD`)),
        BTC_Volume = as.numeric(quantmod::Vo(`BTC-USD`)),
        
        ETH_Close = as.numeric(quantmod::Cl(`ETH-USD`)),
        ETH_Volume = as.numeric(quantmod::Vo(`ETH-USD`)),
        
        XRP_Close = as.numeric(quantmod::Cl(`XRP-USD`)),
        XRP_Volume = as.numeric(quantmod::Vo(`XRP-USD`)),
        
        DOGE_Close = as.numeric(quantmod::Cl(`DOGE-USD`)),
        DOGE_Volume = as.numeric(quantmod::Vo(`DOGE-USD`)),
        
        SHIB_Close = as.numeric(quantmod::Cl(`SHIB-USD`)),
        SHIB_Volume = as.numeric(quantmod::Vo(`SHIB-USD`))
) %>%
        tidyr::drop_na()

# ---------------------------
# 4. Construct returns and market-cap proxies
# ---------------------------
crypto_group_daily <- crypto_group_daily %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
                market_cap_BTC  = BTC_Close * BTC_Volume,
                market_cap_ETH  = ETH_Close * ETH_Volume,
                market_cap_XRP  = XRP_Close * XRP_Volume,
                market_cap_DOGE = DOGE_Close * DOGE_Volume,
                market_cap_SHIB = SHIB_Close * SHIB_Volume,
                
                ret_BTC  = log(BTC_Close / dplyr::lag(BTC_Close)),
                ret_ETH  = log(ETH_Close / dplyr::lag(ETH_Close)),
                ret_XRP  = log(XRP_Close / dplyr::lag(XRP_Close)),
                ret_DOGE = log(DOGE_Close / dplyr::lag(DOGE_Close)),
                ret_SHIB = log(SHIB_Close / dplyr::lag(SHIB_Close))
        ) %>%
        dplyr::mutate(
                total_market_cap =
                        market_cap_BTC + market_cap_ETH + market_cap_XRP + market_cap_DOGE + market_cap_SHIB
        ) %>%
        dplyr::mutate(
                market_return = (
                        ret_BTC  * market_cap_BTC +
                                ret_ETH  * market_cap_ETH +
                                ret_XRP  * market_cap_XRP +
                                ret_DOGE * market_cap_DOGE +
                                ret_SHIB * market_cap_SHIB
                ) / total_market_cap
        ) %>%
        dplyr::filter(
                dplyr::if_all(
                        dplyr::everything(),
                        ~ !is.infinite(.x)
                )
        ) %>%
        tidyr::drop_na(
                ret_BTC, ret_ETH, ret_XRP, ret_DOGE, ret_SHIB, market_return
        )

# ---------------------------
# 5. Aggregate to weekly and monthly frequency
# ---------------------------
crypto_group_weekly <- crypto_group_daily %>%
        dplyr::mutate(week = lubridate::floor_date(date, "week")) %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(
                weekly_return_BTC = mean(ret_BTC, na.rm = TRUE),
                weekly_return_ETH = mean(ret_ETH, na.rm = TRUE),
                weekly_return_XRP = mean(ret_XRP, na.rm = TRUE),
                weekly_return_DOGE = mean(ret_DOGE, na.rm = TRUE),
                weekly_return_SHIB = mean(ret_SHIB, na.rm = TRUE),
                weekly_market_return = mean(market_return, na.rm = TRUE),
                .groups = "drop"
        )

crypto_group_monthly <- crypto_group_daily %>%
        dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(
                monthly_return_BTC = mean(ret_BTC, na.rm = TRUE),
                monthly_return_ETH = mean(ret_ETH, na.rm = TRUE),
                monthly_return_XRP = mean(ret_XRP, na.rm = TRUE),
                monthly_return_DOGE = mean(ret_DOGE, na.rm = TRUE),
                monthly_return_SHIB = mean(ret_SHIB, na.rm = TRUE),
                monthly_market_return = mean(market_return, na.rm = TRUE),
                .groups = "drop"
        )

# ---------------------------
# 6. Weekly market caps for grouped weighting
# ---------------------------
weekly_market_caps <- crypto_group_daily %>%
        dplyr::mutate(week = lubridate::floor_date(date, "week")) %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(
                market_cap_BTC = dplyr::last(market_cap_BTC[!is.na(market_cap_BTC)]),
                market_cap_ETH = dplyr::last(market_cap_ETH[!is.na(market_cap_ETH)]),
                market_cap_XRP = dplyr::last(market_cap_XRP[!is.na(market_cap_XRP)]),
                market_cap_DOGE = dplyr::last(market_cap_DOGE[!is.na(market_cap_DOGE)]),
                market_cap_SHIB = dplyr::last(market_cap_SHIB[!is.na(market_cap_SHIB)]),
                .groups = "drop"
        )

# ---------------------------
# 7. Construct grouped weekly returns
# ---------------------------
crypto_group_weekly <- crypto_group_weekly %>%
        dplyr::left_join(weekly_market_caps, by = "week") %>%
        dplyr::mutate(
                total_meme_cap = market_cap_DOGE + market_cap_SHIB,
                total_conventional_cap = market_cap_BTC + market_cap_ETH + market_cap_XRP,
                
                weight_DOGE = market_cap_DOGE / total_meme_cap,
                weight_SHIB = market_cap_SHIB / total_meme_cap,
                
                weight_BTC = market_cap_BTC / total_conventional_cap,
                weight_ETH = market_cap_ETH / total_conventional_cap,
                weight_XRP = market_cap_XRP / total_conventional_cap
        ) %>%
        dplyr::mutate(
                weekly_return_meme =
                        weekly_return_DOGE * weight_DOGE +
                        weekly_return_SHIB * weight_SHIB,
                
                weekly_return_conventional =
                        weekly_return_BTC * weight_BTC +
                        weekly_return_ETH * weight_ETH +
                        weekly_return_XRP * weight_XRP
        ) %>%
        dplyr::filter(
                !is.na(weekly_return_meme),
                !is.na(weekly_return_conventional),
                !is.na(weekly_market_return)
        )

# ---------------------------
# 8. Save processed datasets
# ---------------------------
write.csv(crypto_group_daily, processed_group_daily_file, row.names = FALSE)
write.csv(crypto_group_weekly, processed_group_weekly_file, row.names = FALSE)
write.csv(crypto_group_monthly, processed_group_monthly_file, row.names = FALSE)
write.csv(weekly_market_caps, processed_weekly_caps_file, row.names = FALSE)

# ---------------------------
# 9. Plot conventional assets vs coin market
# ---------------------------
normalized_assets <- crypto_group_daily %>%
        dplyr::mutate(
                BTC_Index = to_index_100(ret_BTC),
                ETH_Index = to_index_100(ret_ETH),
                XRP_Index = to_index_100(ret_XRP),
                CoinMarket_Index = to_index_100(market_return)
        ) %>%
        dplyr::select(date, BTC_Index, ETH_Index, XRP_Index, CoinMarket_Index)

btc_plot_data <- normalized_assets %>%
        dplyr::select(date, BTC_Index, CoinMarket_Index) %>%
        tidyr::pivot_longer(-date, names_to = "Series", values_to = "Index") %>%
        dplyr::mutate(
                Series = dplyr::recode(
                        Series,
                        "BTC_Index" = "Bitcoin",
                        "CoinMarket_Index" = "Coin Market"
                )
        )

eth_plot_data <- normalized_assets %>%
        dplyr::select(date, ETH_Index, CoinMarket_Index) %>%
        tidyr::pivot_longer(-date, names_to = "Series", values_to = "Index") %>%
        dplyr::mutate(
                Series = dplyr::recode(
                        Series,
                        "ETH_Index" = "Ethereum",
                        "CoinMarket_Index" = "Coin Market"
                )
        )

xrp_plot_data <- normalized_assets %>%
        dplyr::select(date, XRP_Index, CoinMarket_Index) %>%
        tidyr::pivot_longer(-date, names_to = "Series", values_to = "Index") %>%
        dplyr::mutate(
                Series = dplyr::recode(
                        Series,
                        "XRP_Index" = "Ripple",
                        "CoinMarket_Index" = "Coin Market"
                )
        )

p_btc <- ggplot2::ggplot(btc_plot_data, ggplot2::aes(x = date, y = Index, color = Series)) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::labs(
                title = "Bitcoin vs Coin Market",
                x = "Date",
                y = "Growth of $100 Invested",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

p_eth <- ggplot2::ggplot(eth_plot_data, ggplot2::aes(x = date, y = Index, color = Series)) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::labs(
                title = "Ethereum vs Coin Market",
                x = "Date",
                y = "Growth of $100 Invested",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

p_xrp <- ggplot2::ggplot(xrp_plot_data, ggplot2::aes(x = date, y = Index, color = Series)) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::labs(
                title = "Ripple vs Coin Market",
                x = "Date",
                y = "Growth of $100 Invested",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

png(plot_assets_vs_market_file, width = 1000, height = 1500, res = 140)
gridExtra::grid.arrange(p_btc, p_eth, p_xrp, ncol = 1)
dev.off()

# ---------------------------
# 10. Plot meme vs conventional vs market
# ---------------------------
group_plot_data <- crypto_group_weekly %>%
        dplyr::arrange(week) %>%
        dplyr::mutate(
                Meme_Index = to_index_100(weekly_return_meme),
                Conventional_Index = to_index_100(weekly_return_conventional),
                Market_Index = to_index_100(weekly_market_return)
        ) %>%
        dplyr::select(week, Meme_Index, Conventional_Index, Market_Index) %>%
        tidyr::pivot_longer(-week, names_to = "Series", values_to = "Index") %>%
        dplyr::mutate(
                Series = dplyr::recode(
                        Series,
                        "Meme_Index" = "Meme Coins",
                        "Conventional_Index" = "Conventional Coins",
                        "Market_Index" = "Coin Market"
                )
        )

p_group <- ggplot2::ggplot(group_plot_data, ggplot2::aes(x = week, y = Index, color = Series)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::labs(
                title = "Meme Coins vs Conventional Coins vs Coin Market",
                x = "Week",
                y = "Growth of $100 Invested",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

ggplot2::ggsave(
        plot_group_comparison_file,
        plot = p_group,
        width = 10,
        height = 5
)

# ---------------------------
# 11. Plot group drawdowns
# ---------------------------
drawdown_plot_data <- crypto_group_weekly %>%
        dplyr::arrange(week) %>%
        dplyr::mutate(
                Meme_Index = to_index_100(weekly_return_meme),
                Conventional_Index = to_index_100(weekly_return_conventional),
                Market_Index = to_index_100(weekly_market_return),
                
                Meme_Drawdown = compute_drawdown(Meme_Index),
                Conventional_Drawdown = compute_drawdown(Conventional_Index),
                Market_Drawdown = compute_drawdown(Market_Index)
        ) %>%
        dplyr::select(week, Meme_Drawdown, Conventional_Drawdown, Market_Drawdown) %>%
        tidyr::pivot_longer(-week, names_to = "Series", values_to = "Drawdown") %>%
        dplyr::mutate(
                Series = dplyr::recode(
                        Series,
                        "Meme_Drawdown" = "Meme Coins",
                        "Conventional_Drawdown" = "Conventional Coins",
                        "Market_Drawdown" = "Coin Market"
                )
        )

p_drawdown <- ggplot2::ggplot(drawdown_plot_data, ggplot2::aes(x = week, y = Drawdown, color = Series)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::labs(
                title = "Drawdowns: Meme vs Conventional vs Coin Market",
                x = "Week",
                y = "Drawdown",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

ggplot2::ggsave(
        plot_group_drawdown_file,
        plot = p_drawdown,
        width = 10,
        height = 5
)

# ---------------------------
# 12. Descriptive statistics
# ---------------------------
daily_summary_stats <- calculate_summary_table(
        crypto_group_daily,
        value_cols = c("ret_BTC", "ret_ETH", "ret_XRP", "ret_DOGE", "ret_SHIB", "market_return"),
        frequency_label = "Daily"
)

weekly_summary_stats <- calculate_summary_table(
        crypto_group_weekly,
        value_cols = c(
                "weekly_return_BTC",
                "weekly_return_ETH",
                "weekly_return_XRP",
                "weekly_return_DOGE",
                "weekly_return_SHIB",
                "weekly_market_return",
                "weekly_return_conventional",
                "weekly_return_meme"
        ),
        frequency_label = "Weekly"
)

summary_stats <- dplyr::bind_rows(daily_summary_stats, weekly_summary_stats) %>%
        dplyr::mutate(
                Series = dplyr::recode(
                        Series,
                        "ret_BTC" = "Bitcoin",
                        "ret_ETH" = "Ethereum",
                        "ret_XRP" = "Ripple",
                        "ret_DOGE" = "Dogecoin",
                        "ret_SHIB" = "Shiba Inu",
                        "market_return" = "Coin Market",
                        "weekly_return_BTC" = "Bitcoin",
                        "weekly_return_ETH" = "Ethereum",
                        "weekly_return_XRP" = "Ripple",
                        "weekly_return_DOGE" = "Dogecoin",
                        "weekly_return_SHIB" = "Shiba Inu",
                        "weekly_market_return" = "Coin Market",
                        "weekly_return_conventional" = "Conventional Coins",
                        "weekly_return_meme" = "Meme Coins"
                )
        )

write.csv(summary_stats, summary_stats_file, row.names = FALSE)

save_kable_html(
        summary_stats,
        "Summary Statistics: Individual Assets and Grouped Crypto Returns",
        sub("\\.csv$", ".html", summary_stats_file)
)

# ---------------------------
# 13. Group-level summary table
# ---------------------------
group_summary <- tibble::tibble(
        Series = c("Meme Coins", "Conventional Coins", "Coin Market"),
        Mean = c(
                mean(crypto_group_weekly$weekly_return_meme, na.rm = TRUE) * 100,
                mean(crypto_group_weekly$weekly_return_conventional, na.rm = TRUE) * 100,
                mean(crypto_group_weekly$weekly_market_return, na.rm = TRUE) * 100
        ),
        SD = c(
                stats::sd(crypto_group_weekly$weekly_return_meme, na.rm = TRUE) * 100,
                stats::sd(crypto_group_weekly$weekly_return_conventional, na.rm = TRUE) * 100,
                stats::sd(crypto_group_weekly$weekly_market_return, na.rm = TRUE) * 100
        ),
        t_Stat = c(
                calculate_t_stat(crypto_group_weekly$weekly_return_meme),
                calculate_t_stat(crypto_group_weekly$weekly_return_conventional),
                calculate_t_stat(crypto_group_weekly$weekly_market_return)
        ),
        Sharpe = c(
                mean(crypto_group_weekly$weekly_return_meme, na.rm = TRUE) /
                        stats::sd(crypto_group_weekly$weekly_return_meme, na.rm = TRUE),
                mean(crypto_group_weekly$weekly_return_conventional, na.rm = TRUE) /
                        stats::sd(crypto_group_weekly$weekly_return_conventional, na.rm = TRUE),
                mean(crypto_group_weekly$weekly_market_return, na.rm = TRUE) /
                        stats::sd(crypto_group_weekly$weekly_market_return, na.rm = TRUE)
        ),
        Skewness = c(
                safe_skewness(crypto_group_weekly$weekly_return_meme),
                safe_skewness(crypto_group_weekly$weekly_return_conventional),
                safe_skewness(crypto_group_weekly$weekly_market_return)
        ),
        Kurtosis = c(
                safe_kurtosis(crypto_group_weekly$weekly_return_meme),
                safe_kurtosis(crypto_group_weekly$weekly_return_conventional),
                safe_kurtosis(crypto_group_weekly$weekly_market_return)
        )
)

write.csv(group_summary, group_summary_file, row.names = FALSE)

save_kable_html(
        group_summary,
        "Grouped Return Characteristics: Meme Coins, Conventional Coins, and Coin Market",
        sub("\\.csv$", ".html", group_summary_file)
)

# ---------------------------
# 14. Export objects to global environment (optional)
# ---------------------------
assign("crypto_group_daily", crypto_group_daily, envir = .GlobalEnv)
assign("crypto_group_weekly", crypto_group_weekly, envir = .GlobalEnv)
assign("crypto_group_monthly", crypto_group_monthly, envir = .GlobalEnv)
assign("weekly_market_caps", weekly_market_caps, envir = .GlobalEnv)
assign("summary_stats", summary_stats, envir = .GlobalEnv)
assign("group_summary", group_summary, envir = .GlobalEnv)

message("05_memecoin_vs_conventional_analysis.R: completed successfully")