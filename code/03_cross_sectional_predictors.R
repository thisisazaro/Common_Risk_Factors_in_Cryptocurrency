# ============================================================
# 03_cross_sectional_predictors.R
# Purpose:
#   - Evaluate cross-sectional return predictability in crypto markets
#   - Construct quintile portfolios based on key characteristics
#   - Estimate size, momentum, volume, and volatility strategies
#   - Export formatted summary tables for thesis/reporting
# ============================================================

# ---------------------------
# 0. Setup
# ---------------------------
source("code/00_setup.R")

message("03_cross_sectional_predictors.R: started")

# ---------------------------
# 1. File paths
# ---------------------------
daily_file <- "data/processed/crypto_daily_clean.csv"

size_quintiles_file        <- "outputs/tables/size_strategy_quintiles.csv"
size_spreads_file          <- "outputs/tables/size_strategy_spreads.csv"
momentum_summary_file      <- "outputs/tables/momentum_strategy_summary.csv"
volume_summary_file        <- "outputs/tables/volume_strategy_summary.csv"
volatility_summary_file    <- "outputs/tables/volatility_strategy_summary.csv"
generic_quintile_plot_file <- "outputs/figures/generic_quintile_returns.png"
generic_spread_plot_file   <- "outputs/figures/generic_long_short_returns.png"

# ---------------------------
# 2. Helper functions
# ---------------------------
calculate_t_stat <- function(x) {
        x <- x[is.finite(x) & !is.na(x)]
        n <- length(x)
        if (n < 2) return(NA_real_)
        mean(x) / (stats::sd(x) / sqrt(n))
}

calculate_p_value <- function(t_value) {
        if (is.na(t_value)) return(NA_real_)
        2 * (1 - stats::pt(abs(t_value), df = Inf))
}

add_significance_stars <- function(t_value) {
        p_value <- calculate_p_value(t_value)
        dplyr::case_when(
                is.na(p_value)   ~ "",
                p_value < 0.01   ~ "***",
                p_value < 0.05   ~ "**",
                p_value < 0.10   ~ "*",
                TRUE             ~ ""
        )
}

format_estimate_with_stars <- function(value, t_value, digits = 6) {
        if (is.na(value) || is.na(t_value)) return("")
        paste0(round(value, digits), add_significance_stars(t_value))
}

safe_kable_html <- function(df, caption_text, output_file) {
        html_table <- knitr::kable(df, format = "html", caption = caption_text, escape = FALSE) %>%
                kableExtra::kable_styling(
                        full_width = FALSE,
                        bootstrap_options = c("striped", "hover", "condensed")
                )
        save_kable(html_table, file = output_file)
}

# Generic quintile sort by period
calculate_quintile_portfolios <- function(data, characteristic, return_col, period = "week") {
        period_col <- if (period == "week") "Period" else "Period"
        
        data_sorted <- data %>%
                dplyr::mutate(Period = lubridate::floor_date(Date, unit = period)) %>%
                dplyr::group_by(Period) %>%
                dplyr::mutate(
                        Quintile = dplyr::ntile(.data[[characteristic]], 5)
                ) %>%
                dplyr::ungroup()
        
        quintile_returns <- data_sorted %>%
                dplyr::group_by(Period, Quintile) %>%
                dplyr::summarise(
                        Mean_Return = mean(.data[[return_col]], na.rm = TRUE),
                        .groups = "drop"
                )
        
        long_short <- quintile_returns %>%
                dplyr::filter(Quintile %in% c(1, 5)) %>%
                tidyr::pivot_wider(names_from = Quintile, values_from = Mean_Return) %>%
                dplyr::mutate(
                        Long_Short = `5` - `1`
                )
        
        list(
                quintile_returns = quintile_returns,
                long_short = long_short
        )
}

# Size-style static quintile summary
calculate_quintiles_static <- function(data, variable) {
        data %>%
                dplyr::filter(!is.na(.data[[variable]]), is.finite(.data[[variable]])) %>%
                dplyr::group_by(Currency) %>%
                dplyr::mutate(Quintile = dplyr::ntile(.data[[variable]], 5)) %>%
                dplyr::ungroup() %>%
                dplyr::group_by(Quintile) %>%
                dplyr::summarise(
                        Mean = mean(ExcessReturn, na.rm = TRUE),
                        tMean = calculate_t_stat(ExcessReturn),
                        N = sum(is.finite(ExcessReturn)),
                        .groups = "drop"
                )
}

calculate_difference_row <- function(df, q_high, q_low, label) {
        low_row <- df %>% dplyr::filter(Quintile == q_low)
        high_row <- df %>% dplyr::filter(Quintile == q_high)
        
        if (nrow(low_row) == 0 || nrow(high_row) == 0) {
                return(tibble::tibble(
                        Quintile = label,
                        Mean = NA_real_,
                        tMean = NA_real_,
                        N = NA_real_
                ))
        }
        
        diff_mean <- high_row$Mean - low_row$Mean
        
        # Approximate standard error using reported t statistics
        diff_se <- sqrt(
                (low_row$Mean^2 / low_row$tMean^2) +
                        (high_row$Mean^2 / high_row$tMean^2)
        )
        
        diff_t <- diff_mean / diff_se
        
        tibble::tibble(
                Quintile = label,
                Mean = diff_mean,
                tMean = diff_t,
                N = NA_real_
        )
}

# Momentum summary across lookback windows
calculate_momentum_summary <- function(data, lookback_periods) {
        result_list <- lapply(lookback_periods, function(period) {
                tmp <- data %>%
                        dplyr::group_by(Currency) %>%
                        dplyr::arrange(Date, .by_group = TRUE) %>%
                        dplyr::mutate(
                                momentum_signal = dplyr::lag(ExcessReturn, period)
                        ) %>%
                        dplyr::ungroup() %>%
                        dplyr::filter(!is.na(momentum_signal), is.finite(momentum_signal)) %>%
                        dplyr::group_by(Date) %>%
                        dplyr::mutate(quintile = dplyr::ntile(momentum_signal, 5)) %>%
                        dplyr::ungroup() %>%
                        dplyr::group_by(Date, quintile) %>%
                        dplyr::summarise(
                                mean_return = mean(ExcessReturn, na.rm = TRUE),
                                .groups = "drop"
                        ) %>%
                        tidyr::pivot_wider(
                                names_from = quintile,
                                values_from = mean_return,
                                names_prefix = "Q"
                        ) %>%
                        dplyr::mutate(
                                `5-1` = Q5 - Q1,
                                Lookback_Period = period
                        )
                
                tmp
        })
        
        combined <- dplyr::bind_rows(result_list)
        
        combined %>%
                dplyr::group_by(Lookback_Period) %>%
                dplyr::summarise(
                        Mean_Q1 = mean(Q1, na.rm = TRUE),
                        t_Q1 = calculate_t_stat(Q1),
                        Mean_Q2 = mean(Q2, na.rm = TRUE),
                        t_Q2 = calculate_t_stat(Q2),
                        Mean_Q3 = mean(Q3, na.rm = TRUE),
                        t_Q3 = calculate_t_stat(Q3),
                        Mean_Q4 = mean(Q4, na.rm = TRUE),
                        t_Q4 = calculate_t_stat(Q4),
                        Mean_Q5 = mean(Q5, na.rm = TRUE),
                        t_Q5 = calculate_t_stat(Q5),
                        Mean_5_1 = mean(`5-1`, na.rm = TRUE),
                        t_5_1 = calculate_t_stat(`5-1`),
                        .groups = "drop"
                )
}

# Volume strategy
calculate_volume_strategy <- function(data, volume_column) {
        data %>%
                dplyr::filter(!is.na(.data[[volume_column]]), is.finite(.data[[volume_column]])) %>%
                dplyr::group_by(Date) %>%
                dplyr::mutate(
                        quintile = dplyr::ntile(.data[[volume_column]], 5)
                ) %>%
                dplyr::ungroup() %>%
                dplyr::group_by(Date, quintile) %>%
                dplyr::summarise(
                        mean_return = mean(ExcessReturn, na.rm = TRUE),
                        .groups = "drop"
                ) %>%
                tidyr::pivot_wider(
                        names_from = quintile,
                        values_from = mean_return,
                        names_prefix = "Q"
                ) %>%
                dplyr::mutate(
                        `5-1` = Q1 - Q5
                )
}

# Volatility strategy based on rolling SD of volume
calculate_volatility_strategy <- function(data, window = 7) {
        data %>%
                dplyr::group_by(Currency) %>%
                dplyr::arrange(Date, .by_group = TRUE) %>%
                dplyr::mutate(
                        std_volume = zoo::rollapply(
                                Volume,
                                width = window,
                                FUN = stats::sd,
                                fill = NA,
                                align = "right"
                        )
                ) %>%
                dplyr::ungroup() %>%
                dplyr::filter(!is.na(std_volume), is.finite(std_volume)) %>%
                dplyr::group_by(Date) %>%
                dplyr::mutate(
                        quintile = dplyr::ntile(std_volume, 5)
                ) %>%
                dplyr::ungroup() %>%
                dplyr::group_by(Date, quintile) %>%
                dplyr::summarise(
                        mean_return = mean(Return, na.rm = TRUE),
                        .groups = "drop"
                ) %>%
                tidyr::pivot_wider(
                        names_from = quintile,
                        values_from = mean_return,
                        names_prefix = "Q"
                ) %>%
                dplyr::mutate(
                        `5-1` = Q1 - Q5
                )
}

summarise_strategy_table <- function(strategy_df) {
        strategy_df %>%
                dplyr::summarise(
                        Mean_Q1 = mean(Q1, na.rm = TRUE),
                        t_Q1 = calculate_t_stat(Q1),
                        Mean_Q2 = mean(Q2, na.rm = TRUE),
                        t_Q2 = calculate_t_stat(Q2),
                        Mean_Q3 = mean(Q3, na.rm = TRUE),
                        t_Q3 = calculate_t_stat(Q3),
                        Mean_Q4 = mean(Q4, na.rm = TRUE),
                        t_Q4 = calculate_t_stat(Q4),
                        Mean_Q5 = mean(Q5, na.rm = TRUE),
                        t_Q5 = calculate_t_stat(Q5),
                        Mean_5_1 = mean(`5-1`, na.rm = TRUE),
                        t_5_1 = calculate_t_stat(`5-1`)
                )
}

format_strategy_summary <- function(summary_df) {
        summary_df %>%
                dplyr::mutate(
                        Q1 = mapply(format_estimate_with_stars, Mean_Q1, t_Q1, MoreArgs = list(digits = 3)),
                        Q2 = mapply(format_estimate_with_stars, Mean_Q2, t_Q2, MoreArgs = list(digits = 3)),
                        Q3 = mapply(format_estimate_with_stars, Mean_Q3, t_Q3, MoreArgs = list(digits = 3)),
                        Q4 = mapply(format_estimate_with_stars, Mean_Q4, t_Q4, MoreArgs = list(digits = 3)),
                        Q5 = mapply(format_estimate_with_stars, Mean_Q5, t_Q5, MoreArgs = list(digits = 3)),
                        `5-1` = mapply(format_estimate_with_stars, Mean_5_1, t_5_1, MoreArgs = list(digits = 3))
                ) %>%
                dplyr::select(dplyr::any_of("Lookback_Period"), Q1, Q2, Q3, Q4, Q5, `5-1`)
}

# ---------------------------
# 3. Load processed dataset
# ---------------------------
if (!file.exists(daily_file)) stop("Missing file: ", daily_file)

crypto_daily <- read.csv(daily_file)
crypto_daily$Date <- as.Date(crypto_daily$Date)

required_cols <- c("Currency", "Date", "Close", "Volume", "Adjusted", "Return", "ExcessReturn")
missing_cols <- setdiff(required_cols, names(crypto_daily))
if (length(missing_cols) > 0) {
        stop("Missing columns in crypto_daily: ", paste(missing_cols, collapse = ", "))
}

crypto_daily <- crypto_daily %>%
        dplyr::filter(
                !is.na(Return), is.finite(Return),
                !is.na(ExcessReturn), is.finite(ExcessReturn)
        )

# ---------------------------
# 4. Generic quintile portfolios (illustrative)
# ---------------------------
generic_result <- calculate_quintile_portfolios(
        data = crypto_daily,
        characteristic = "Close",
        return_col = "ExcessReturn",
        period = "week"
)

# Plot quintile returns
p_quintiles <- ggplot2::ggplot(
        generic_result$quintile_returns,
        ggplot2::aes(x = Period, y = Mean_Return, color = factor(Quintile))
) +
        ggplot2::geom_line() +
        ggplot2::labs(
                title = "Quintile Portfolio Returns",
                x = "Week",
                y = "Mean Excess Return",
                color = "Quintile"
        ) +
        ggplot2::theme_minimal()

ggplot2::ggsave(
        generic_quintile_plot_file,
        plot = p_quintiles,
        width = 10,
        height = 5
)

# Plot long-short spread
p_spread <- ggplot2::ggplot(
        generic_result$long_short,
        ggplot2::aes(x = Period, y = Long_Short)
) +
        ggplot2::geom_line(color = "steelblue") +
        ggplot2::labs(
                title = "Long-Short (Q5 - Q1) Excess Return",
                x = "Week",
                y = "Long-Short Return"
        ) +
        ggplot2::theme_minimal()

ggplot2::ggsave(
        generic_spread_plot_file,
        plot = p_spread,
        width = 10,
        height = 5
)

# ---------------------------
# 5. Size characteristics
# ---------------------------
# Preserving the original project logic:
# - Volume used as MCAP-like proxy
# - Close used as price proxy
# - Adjusted used as MAXDPRC-like characteristic in the original script

mcap_quintiles <- calculate_quintiles_static(crypto_daily, "Volume") %>%
        dplyr::mutate(Measure = "MCAP")

prc_quintiles <- calculate_quintiles_static(crypto_daily, "Close") %>%
        dplyr::mutate(Measure = "PRC")

maxdprc_quintiles <- calculate_quintiles_static(crypto_daily, "Adjusted") %>%
        dplyr::mutate(Measure = "MAXDPRC")

size_quintiles <- dplyr::bind_rows(
        mcap_quintiles,
        prc_quintiles,
        maxdprc_quintiles
) %>%
        dplyr::mutate(
                Mean_with_stars = mapply(format_estimate_with_stars, Mean, tMean)
        )

write.csv(size_quintiles, size_quintiles_file, row.names = FALSE)

safe_kable_html(
        size_quintiles %>% dplyr::select(Measure, Quintile, Mean_with_stars, tMean, N),
        "Size Strategy Returns by Quintile",
        sub("\\.csv$", ".html", size_quintiles_file)
)

# Spreads: 5-1 and 3-1
mcap_spreads <- dplyr::bind_rows(
        calculate_difference_row(mcap_quintiles, 5, 1, "5-1"),
        calculate_difference_row(mcap_quintiles, 3, 1, "3-1")
) %>% dplyr::mutate(Measure = "MCAP")

prc_spreads <- dplyr::bind_rows(
        calculate_difference_row(prc_quintiles, 5, 1, "5-1"),
        calculate_difference_row(prc_quintiles, 3, 1, "3-1")
) %>% dplyr::mutate(Measure = "PRC")

maxdprc_spreads <- dplyr::bind_rows(
        calculate_difference_row(maxdprc_quintiles, 5, 1, "5-1"),
        calculate_difference_row(maxdprc_quintiles, 3, 1, "3-1")
) %>% dplyr::mutate(Measure = "MAXDPRC")

size_spreads <- dplyr::bind_rows(
        mcap_spreads,
        prc_spreads,
        maxdprc_spreads
) %>%
        dplyr::mutate(
                Mean_with_stars = mapply(format_estimate_with_stars, Mean, tMean)
        )

write.csv(size_spreads, size_spreads_file, row.names = FALSE)

safe_kable_html(
        size_spreads %>% dplyr::select(Measure, Quintile, Mean_with_stars, tMean),
        "Size Strategy Long-Short Spreads",
        sub("\\.csv$", ".html", size_spreads_file)
)

# ---------------------------
# 6. Momentum characteristics
# ---------------------------
lookback_periods <- c(7, 14, 21, 28)

momentum_summary_raw <- calculate_momentum_summary(crypto_daily, lookback_periods)

momentum_summary <- momentum_summary_raw %>%
        format_strategy_summary()

write.csv(momentum_summary_raw, momentum_summary_file, row.names = FALSE)

safe_kable_html(
        momentum_summary,
        "Momentum Strategy Returns",
        sub("\\.csv$", ".html", momentum_summary_file)
)

# ---------------------------
# 7. Volume characteristics
# ---------------------------
volume_results <- calculate_volume_strategy(crypto_daily, "Volume")
volume_summary_raw <- summarise_strategy_table(volume_results)
volume_summary <- format_strategy_summary(volume_summary_raw)

write.csv(volume_summary_raw, volume_summary_file, row.names = FALSE)

safe_kable_html(
        volume_summary,
        "Volume Strategy Returns",
        sub("\\.csv$", ".html", volume_summary_file)
)

# ---------------------------
# 8. Volatility characteristics
# ---------------------------
volatility_results <- calculate_volatility_strategy(crypto_daily, window = 7)
volatility_summary_raw <- summarise_strategy_table(volatility_results)
volatility_summary <- format_strategy_summary(volatility_summary_raw)

write.csv(volatility_summary_raw, volatility_summary_file, row.names = FALSE)

safe_kable_html(
        volatility_summary,
        "Volatility Strategy Returns",
        sub("\\.csv$", ".html", volatility_summary_file)
)

# ---------------------------
# 9. Export objects to global environment (optional)
# ---------------------------
assign("size_quintiles", size_quintiles, envir = .GlobalEnv)
assign("size_spreads", size_spreads, envir = .GlobalEnv)
assign("momentum_summary_raw", momentum_summary_raw, envir = .GlobalEnv)
assign("volume_summary_raw", volume_summary_raw, envir = .GlobalEnv)
assign("volatility_summary_raw", volatility_summary_raw, envir = .GlobalEnv)

message("03_cross_sectional_predictors.R: completed successfully")
