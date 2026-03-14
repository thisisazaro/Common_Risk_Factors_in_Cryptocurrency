# ============================================================
# 04_crypto_factor_model.R
# Purpose:
#   - Construct cryptocurrency pricing factors
#   - Estimate CMKT, CSMB, and CMOM factor returns
#   - Build characteristic-sorted long-short strategy portfolios
#   - Run factor regressions for test strategies
#   - Export tables and figures for thesis/reporting
# ============================================================

# ---------------------------
# 0. Setup
# ---------------------------
source("code/00_setup.R")

message("04_crypto_factor_model.R: started")

# ---------------------------
# 1. File paths
# ---------------------------
daily_file <- "data/processed/crypto_daily_clean.csv"

factor_returns_file         <- "outputs/tables/crypto_factor_returns.csv"
factor_correlations_file    <- "outputs/tables/crypto_factor_correlations.csv"
strategy_returns_file       <- "outputs/tables/strategy_test_portfolios.csv"
factor_model_results_file   <- "outputs/tables/crypto_factor_model_results.csv"

factor_plot_file            <- "outputs/figures/crypto_factor_cumulative_returns.png"

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
                is.na(p_value) ~ "",
                p_value < 0.01 ~ "***",
                p_value < 0.05 ~ "**",
                p_value < 0.10 ~ "*",
                TRUE ~ ""
        )
}

format_coef <- function(beta, t_stat, digits = 4) {
        if (is.na(beta) || is.na(t_stat)) return("")
        paste0(format(round(beta, digits), nsmall = digits), add_significance_stars(t_stat))
}

safe_weighted_mean <- function(x, w) {
        valid <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w)
        if (!any(valid)) return(NA_real_)
        sum(x[valid] * w[valid], na.rm = TRUE) / sum(w[valid], na.rm = TRUE)
}

`%||%` <- function(x, y) {
        if (length(x) == 0) y else x
}

save_kable_html <- function(df, caption_text, output_file) {
        html_table <- knitr::kable(df, format = "html", caption = caption_text, escape = FALSE) %>%
                kableExtra::kable_styling(
                        full_width = FALSE,
                        bootstrap_options = c("striped", "hover", "condensed")
                )
        save_kable(html_table, file = output_file)
}

# Build a long-short test strategy from a sorting signal
build_long_short_strategy <- function(data,
                                      signal_var,
                                      return_var = "ExcessReturn",
                                      ntiles = 5,
                                      high_minus_low = TRUE,
                                      strategy_name = "Strategy") {
        stopifnot(signal_var %in% names(data))
        stopifnot(return_var %in% names(data))
        
        strat <- data %>%
                dplyr::filter(
                        !is.na(.data[[signal_var]]), is.finite(.data[[signal_var]]),
                        !is.na(.data[[return_var]]), is.finite(.data[[return_var]])
                ) %>%
                dplyr::group_by(Date) %>%
                dplyr::mutate(
                        bucket = dplyr::ntile(.data[[signal_var]], ntiles)
                ) %>%
                dplyr::ungroup() %>%
                dplyr::group_by(Date, bucket) %>%
                dplyr::summarise(
                        bucket_return = mean(.data[[return_var]], na.rm = TRUE),
                        .groups = "drop"
                ) %>%
                tidyr::pivot_wider(
                        names_from = bucket,
                        values_from = bucket_return,
                        names_prefix = "Q"
                ) %>%
                dplyr::mutate(
                        Strategy = strategy_name,
                        LongShort = if (high_minus_low) Q5 - Q1 else Q1 - Q5
                ) %>%
                dplyr::select(Date, Strategy, LongShort)
        
        strat
}

run_factor_regressions <- function(strategy_df, factor_df) {
        merged <- strategy_df %>%
                dplyr::left_join(factor_df, by = "Date") %>%
                dplyr::filter(
                        !is.na(LongShort), is.finite(LongShort),
                        !is.na(CMKT), is.finite(CMKT),
                        !is.na(CSMB), is.finite(CSMB),
                        !is.na(CMOM), is.finite(CMOM)
                )
        
        if (nrow(merged) < 20) return(NULL)
        
        model_1 <- stats::lm(LongShort ~ CMKT, data = merged)
        model_2 <- stats::lm(LongShort ~ CMKT + CSMB, data = merged)
        model_3 <- stats::lm(LongShort ~ CMKT + CSMB + CMOM, data = merged)
        
        extract_model <- function(model, spec_name) {
                sm <- summary(model)
                coefs <- as.data.frame(sm$coefficients)
                coefs$Term <- rownames(coefs)
                rownames(coefs) <- NULL
                
                tibble::tibble(
                        Specification = spec_name,
                        Intercept = coefs$Estimate[coefs$Term == "(Intercept)"] %||% NA_real_,
                        Intercept_t = coefs$`t value`[coefs$Term == "(Intercept)"] %||% NA_real_,
                        CMKT = coefs$Estimate[coefs$Term == "CMKT"] %||% NA_real_,
                        CMKT_t = coefs$`t value`[coefs$Term == "CMKT"] %||% NA_real_,
                        CSMB = coefs$Estimate[coefs$Term == "CSMB"] %||% NA_real_,
                        CSMB_t = coefs$`t value`[coefs$Term == "CSMB"] %||% NA_real_,
                        CMOM = coefs$Estimate[coefs$Term == "CMOM"] %||% NA_real_,
                        CMOM_t = coefs$`t value`[coefs$Term == "CMOM"] %||% NA_real_,
                        R_squared = sm$r.squared,
                        Adj_R_squared = sm$adj.r.squared,
                        MAE = mean(abs(model$residuals), na.rm = TRUE),
                        N = length(model$residuals)
                )
        }
        
        dplyr::bind_rows(
                extract_model(model_1, "Model 1: CMKT"),
                extract_model(model_2, "Model 2: CMKT + CSMB"),
                extract_model(model_3, "Model 3: CMKT + CSMB + CMOM")
        )
}

# ---------------------------
# 3. Load processed data
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
                !is.na(ExcessReturn), is.finite(ExcessReturn),
                !is.na(Close), is.finite(Close),
                !is.na(Volume), is.finite(Volume),
                !is.na(Adjusted), is.finite(Adjusted)
        ) %>%
        dplyr::arrange(Currency, Date)

# ---------------------------
# 4. Construct working signals
# ---------------------------
# MarketCap_proxy is preserved from the original project logic:
# it is a price-volume proxy rather than true market capitalization.

crypto_daily <- crypto_daily %>%
        dplyr::group_by(Currency) %>%
        dplyr::arrange(Date, .by_group = TRUE) %>%
        dplyr::mutate(
                MarketCap_proxy = Close * Volume,
                
                Momentum_7 = dplyr::lag(
                        zoo::rollapply(Return, width = 7, FUN = sum, fill = NA, align = "right"),
                        1
                ),
                Momentum_14 = dplyr::lag(
                        zoo::rollapply(Return, width = 14, FUN = sum, fill = NA, align = "right"),
                        1
                ),
                Momentum_21 = dplyr::lag(
                        zoo::rollapply(Return, width = 21, FUN = sum, fill = NA, align = "right"),
                        1
                ),
                Momentum_28 = dplyr::lag(
                        zoo::rollapply(Return, width = 28, FUN = sum, fill = NA, align = "right"),
                        1
                ),
                
                STDVOL_7 = zoo::rollapply(
                        Volume,
                        width = 7,
                        FUN = stats::sd,
                        fill = NA,
                        align = "right"
                )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
                Momentum_Signal = Momentum_21
        )

# ---------------------------
# 5. Construct factor returns
# ---------------------------
factor_returns <- crypto_daily %>%
        dplyr::filter(
                !is.na(MarketCap_proxy), is.finite(MarketCap_proxy),
                !is.na(Momentum_Signal), is.finite(Momentum_Signal),
                !is.na(ExcessReturn), is.finite(ExcessReturn)
        ) %>%
        dplyr::group_by(Date) %>%
        dplyr::mutate(
                size_bucket = dplyr::ntile(MarketCap_proxy, 3),
                mom_bucket  = dplyr::ntile(Momentum_Signal, 3)
        ) %>%
        dplyr::summarise(
                CMKT = safe_weighted_mean(ExcessReturn, MarketCap_proxy),
                
                Small  = mean(ExcessReturn[size_bucket == 1], na.rm = TRUE),
                Big    = mean(ExcessReturn[size_bucket == 3], na.rm = TRUE),
                
                Winner = mean(ExcessReturn[mom_bucket == 3], na.rm = TRUE),
                Loser  = mean(ExcessReturn[mom_bucket == 1], na.rm = TRUE),
                
                .groups = "drop"
        ) %>%
        dplyr::mutate(
                CSMB = Small - Big,
                CMOM = Winner - Loser
        ) %>%
        dplyr::select(Date, CMKT, CSMB, CMOM) %>%
        dplyr::filter(
                !is.na(CMKT), is.finite(CMKT),
                !is.na(CSMB), is.finite(CSMB),
                !is.na(CMOM), is.finite(CMOM)
        ) %>%
        dplyr::arrange(Date)

write.csv(factor_returns, factor_returns_file, row.names = FALSE)

# ---------------------------
# 6. Factor diagnostics
# ---------------------------
factor_correlations <- factor_returns %>%
        dplyr::select(CMKT, CSMB, CMOM) %>%
        stats::cor(use = "pairwise.complete.obs") %>%
        as.data.frame()

write.csv(factor_correlations, factor_correlations_file, row.names = TRUE)

# ---------------------------
# 7. Plot cumulative factor returns
# ---------------------------
factor_plot_data <- factor_returns %>%
        dplyr::arrange(Date) %>%
        dplyr::mutate(
                CMKT_Cum = cumsum(CMKT),
                CSMB_Cum = cumsum(CSMB),
                CMOM_Cum = cumsum(CMOM)
        ) %>%
        tidyr::pivot_longer(
                cols = c(CMKT_Cum, CSMB_Cum, CMOM_Cum),
                names_to = "Factor",
                values_to = "CumulativeReturn"
        ) %>%
        dplyr::mutate(
                Factor = dplyr::recode(
                        Factor,
                        "CMKT_Cum" = "CMKT",
                        "CSMB_Cum" = "CSMB",
                        "CMOM_Cum" = "CMOM"
                )
        )

p_factors <- ggplot2::ggplot(
        factor_plot_data,
        ggplot2::aes(x = Date, y = CumulativeReturn, color = Factor)
) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::labs(
                title = "Cumulative Cryptocurrency Factor Returns",
                x = "Date",
                y = "Cumulative Return",
                color = "Factor"
        ) +
        ggplot2::theme_minimal()

ggplot2::ggsave(
        factor_plot_file,
        plot = p_factors,
        width = 10,
        height = 5
)

# ---------------------------
# 8. Build test strategy returns
# ---------------------------
# These test portfolios are separate from the factor construction step.
# They are characteristic-sorted long-short strategies to be explained by CMKT/CSMB/CMOM.

strategy_mcap <- build_long_short_strategy(
        data = crypto_daily,
        signal_var = "MarketCap_proxy",
        return_var = "ExcessReturn",
        high_minus_low = TRUE,
        strategy_name = "MCAP"
)

strategy_prc <- build_long_short_strategy(
        data = crypto_daily,
        signal_var = "Close",
        return_var = "ExcessReturn",
        high_minus_low = TRUE,
        strategy_name = "PRC"
)

strategy_adjusted <- build_long_short_strategy(
        data = crypto_daily,
        signal_var = "Adjusted",
        return_var = "ExcessReturn",
        high_minus_low = TRUE,
        strategy_name = "ADJUSTED"
)

strategy_mom_7 <- build_long_short_strategy(
        data = crypto_daily,
        signal_var = "Momentum_7",
        return_var = "ExcessReturn",
        high_minus_low = TRUE,
        strategy_name = "MOM_7"
)

strategy_mom_14 <- build_long_short_strategy(
        data = crypto_daily,
        signal_var = "Momentum_14",
        return_var = "ExcessReturn",
        high_minus_low = TRUE,
        strategy_name = "MOM_14"
)

strategy_mom_21 <- build_long_short_strategy(
        data = crypto_daily,
        signal_var = "Momentum_21",
        return_var = "ExcessReturn",
        high_minus_low = TRUE,
        strategy_name = "MOM_21"
)

strategy_mom_28 <- build_long_short_strategy(
        data = crypto_daily,
        signal_var = "Momentum_28",
        return_var = "ExcessReturn",
        high_minus_low = TRUE,
        strategy_name = "MOM_28"
)

strategy_volume <- build_long_short_strategy(
        data = crypto_daily,
        signal_var = "Volume",
        return_var = "ExcessReturn",
        high_minus_low = FALSE,
        strategy_name = "PRCVOL"
)

strategy_stdvol <- build_long_short_strategy(
        data = crypto_daily,
        signal_var = "STDVOL_7",
        return_var = "ExcessReturn",
        high_minus_low = FALSE,
        strategy_name = "STDPRCVOL"
)

strategy_returns <- dplyr::bind_rows(
        strategy_mcap,
        strategy_prc,
        strategy_adjusted,
        strategy_mom_7,
        strategy_mom_14,
        strategy_mom_21,
        strategy_mom_28,
        strategy_volume,
        strategy_stdvol
)

write.csv(strategy_returns, strategy_returns_file, row.names = FALSE)

# ---------------------------
# 9. Run factor model regressions
# ---------------------------
strategy_names <- unique(strategy_returns$Strategy)

factor_model_results_raw <- lapply(strategy_names, function(strategy_name) {
        tmp_strategy <- strategy_returns %>%
                dplyr::filter(Strategy == strategy_name)
        
        reg_out <- run_factor_regressions(tmp_strategy, factor_returns)
        
        if (is.null(reg_out)) return(NULL)
        
        reg_out %>%
                dplyr::mutate(Strategy = strategy_name, .before = 1)
}) %>%
        dplyr::bind_rows()

# ---------------------------
# 10. Format regression output
# ---------------------------
factor_model_results <- factor_model_results_raw %>%
        dplyr::mutate(
                Intercept_fmt = mapply(format_coef, Intercept, Intercept_t, MoreArgs = list(digits = 4)),
                CMKT_fmt      = mapply(format_coef, CMKT, CMKT_t, MoreArgs = list(digits = 4)),
                CSMB_fmt      = mapply(format_coef, CSMB, CSMB_t, MoreArgs = list(digits = 4)),
                CMOM_fmt      = mapply(format_coef, CMOM, CMOM_t, MoreArgs = list(digits = 4))
        ) %>%
        dplyr::select(
                Strategy,
                Specification,
                Intercept_fmt, Intercept_t,
                CMKT_fmt, CMKT_t,
                CSMB_fmt, CSMB_t,
                CMOM_fmt, CMOM_t,
                R_squared, Adj_R_squared, MAE, N
        ) %>%
        dplyr::rename(
                Intercept = Intercept_fmt,
                `t(Intercept)` = Intercept_t,
                CMKT = CMKT_fmt,
                `t(CMKT)` = CMKT_t,
                CSMB = CSMB_fmt,
                `t(CSMB)` = CSMB_t,
                CMOM = CMOM_fmt,
                `t(CMOM)` = CMOM_t
        )

write.csv(factor_model_results, factor_model_results_file, row.names = FALSE)

save_kable_html(
        factor_model_results,
        "Cryptocurrency Factor Model Regression Results",
        sub("\\.csv$", ".html", factor_model_results_file)
)

# ---------------------------
# 11. Export objects to global environment (optional)
# ---------------------------
assign("factor_returns", factor_returns, envir = .GlobalEnv)
assign("strategy_returns", strategy_returns, envir = .GlobalEnv)
assign("factor_model_results_raw", factor_model_results_raw, envir = .GlobalEnv)
assign("factor_model_results", factor_model_results, envir = .GlobalEnv)

message("04_crypto_factor_model.R: completed successfully")