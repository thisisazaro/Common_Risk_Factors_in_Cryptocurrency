# ============================================================
# 07_macro_sentiment_hashrate_models.R
# Purpose:
#   - Examine macro-financial, sentiment, and blockchain drivers
#     of cryptocurrency return dynamics
#   - Merge weekly crypto group returns with:
#       * S&P 500
#       * Dow Jones
#       * 10Y Treasury Yield
#       * Gold Futures
#       * Fear & Greed Index
#       * Bitcoin Hash Rate
#   - Estimate OLS models for:
#       * Meme coin returns
#       * Conventional coin returns
#       * Aggregate crypto market returns
#   - Export regression results, diagnostics, and figures
# ============================================================

# ---------------------------
# 0. Setup
# ---------------------------
source("code/00_setup.R")

message("07_macro_sentiment_hashrate_models.R: started")

# ---------------------------
# 1. File paths
# ---------------------------
weekly_file <- "data/processed/crypto_group_comparison_weekly.csv"

macro_weekly_file         <- "data/processed/macro_sentiment_hashrate_weekly.csv"
macro_model_results_file  <- "outputs/tables/macro_sentiment_hashrate_model_results.csv"
macro_summary_file        <- "outputs/tables/macro_sentiment_hashrate_summary.csv"
macro_correlation_file    <- "outputs/tables/macro_sentiment_hashrate_correlations.csv"

macro_heatmap_file        <- "outputs/figures/macro_sentiment_hashrate_correlation_heatmap.png"

# ---------------------------
# 2. Helper functions
# ---------------------------
save_kable_html <- function(df, caption_text, output_file) {
        html_table <- knitr::kable(df, format = "html", caption = caption_text, escape = FALSE) %>%
                kableExtra::kable_styling(
                        full_width = FALSE,
                        bootstrap_options = c("striped", "hover", "condensed")
                )
        save_kable(html_table, file = output_file)
}

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

add_significance_stars <- function(p_value) {
        dplyr::case_when(
                is.na(p_value) ~ "",
                p_value < 0.01 ~ "***",
                p_value < 0.05 ~ "**",
                p_value < 0.10 ~ "*",
                TRUE ~ ""
        )
}

format_estimate <- function(estimate, p_value, digits = 4) {
        if (is.na(estimate)) return("")
        paste0(format(round(estimate, digits), nsmall = digits), add_significance_stars(p_value))
}

safe_scale <- function(x) {
        x <- as.numeric(x)
        s <- stats::sd(x, na.rm = TRUE)
        m <- mean(x, na.rm = TRUE)
        if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
        (x - m) / s
}

extract_model_results <- function(model, dependent_var, specification) {
        td <- broom::tidy(model)
        gl <- broom::glance(model)
        
        td %>%
                dplyr::mutate(
                        dependent_variable = dependent_var,
                        specification = specification,
                        estimate_fmt = mapply(format_estimate, estimate, p.value, MoreArgs = list(digits = 4)),
                        r_squared = gl$r.squared,
                        adj_r_squared = gl$adj.r.squared,
                        aic = gl$AIC,
                        bic = gl$BIC,
                        n = stats::nobs(model),
                        .before = 1
                ) %>%
                dplyr::select(
                        dependent_variable,
                        specification,
                        term,
                        estimate_fmt,
                        estimate,
                        std.error,
                        statistic,
                        p.value,
                        r_squared,
                        adj_r_squared,
                        aic,
                        bic,
                        n
                )
}

build_summary_table <- function(df, vars) {
        out <- lapply(vars, function(v) {
                x <- df[[v]]
                tibble::tibble(
                        Variable = v,
                        Mean = mean(x, na.rm = TRUE),
                        Median = stats::median(x, na.rm = TRUE),
                        SD = stats::sd(x, na.rm = TRUE),
                        t_Stat = calculate_t_stat(x),
                        Skewness = safe_skewness(x),
                        Kurtosis = safe_kurtosis(x),
                        N = sum(is.finite(x))
                )
        })
        dplyr::bind_rows(out)
}

# ---------------------------
# 3. Load weekly crypto dataset
# ---------------------------
if (!file.exists(weekly_file)) stop("Missing file: ", weekly_file)

crypto_group_weekly <- read.csv(weekly_file)
crypto_group_weekly$week <- as.Date(crypto_group_weekly$week)

required_cols <- c(
        "week",
        "weekly_market_return",
        "weekly_return_conventional",
        "weekly_return_meme"
)

missing_cols <- setdiff(required_cols, names(crypto_group_weekly))
if (length(missing_cols) > 0) {
        stop("Missing columns in weekly crypto dataset: ", paste(missing_cols, collapse = ", "))
}

# ---------------------------
# 4. Download macro-financial series
# ---------------------------
symbols <- c("^GSPC", "^DJI", "^TNX", "GC=F")

suppressWarnings(
        quantmod::getSymbols(symbols, src = "yahoo", from = "2018-01-01", auto.assign = TRUE)
)

GSPC_Close <- quantmod::Cl(GSPC)
DJI_Close  <- quantmod::Cl(DJI)
TNX_Close  <- quantmod::Cl(TNX)
Gold_Close <- quantmod::Cl(`GC=F`)

colnames(GSPC_Close) <- "SP500"
colnames(DJI_Close)  <- "DowJones"
colnames(TNX_Close)  <- "TreasuryYield10Y"
colnames(Gold_Close) <- "GoldFutures"

market_data <- merge(GSPC_Close, DJI_Close, TNX_Close, Gold_Close, all = TRUE)
market_data <- zoo::na.locf(market_data, na.rm = FALSE)
market_data <- zoo::na.locf(market_data, fromLast = TRUE, na.rm = FALSE)

market_data_tbl <- tibble::tibble(
        Date = as.Date(zoo::index(market_data)),
        SP500 = as.numeric(market_data[, "SP500"]),
        DowJones = as.numeric(market_data[, "DowJones"]),
        TreasuryYield10Y = as.numeric(market_data[, "TreasuryYield10Y"]),
        GoldFutures = as.numeric(market_data[, "GoldFutures"])
)

market_weekly <- market_data_tbl %>%
        dplyr::mutate(week = lubridate::floor_date(Date, "week")) %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(
                SP500 = mean(SP500, na.rm = TRUE),
                DowJones = mean(DowJones, na.rm = TRUE),
                TreasuryYield10Y = mean(TreasuryYield10Y, na.rm = TRUE),
                GoldFutures = mean(GoldFutures, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        dplyr::arrange(week)

# ---------------------------
# 5. Download Fear & Greed Index
# ---------------------------
fear_greed_url <- "https://api.alternative.me/fng/?limit=0"

fear_greed_raw <- jsonlite::fromJSON(fear_greed_url)$data

fear_greed_df <- data.frame(
        Date = as.Date(as.POSIXct(as.numeric(fear_greed_raw$timestamp), origin = "1970-01-01", tz = "UTC")),
        FearGreed = as.numeric(fear_greed_raw$value)
) %>%
        dplyr::filter(Date >= as.Date("2018-01-01")) %>%
        dplyr::arrange(Date)

fear_greed_weekly <- fear_greed_df %>%
        dplyr::mutate(week = lubridate::floor_date(Date, "week")) %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(
                FearGreed = mean(FearGreed, na.rm = TRUE),
                .groups = "drop"
        )

# ---------------------------
# 6. Load Bitcoin hash rate
# ---------------------------
# Expects a local JSON file in data/raw named hash-rate.json
# with a structure similar to blockchain-style exports.

hashrate_api_url <- "https://api.blockchain.info/charts/hash-rate?timespan=all&sampled=false&metadata=false&cors=true&format=json"

hash_rate_json <- jsonlite::fromJSON(hashrate_api_url)

if (!"values" %in% names(hash_rate_json)) {
        stop("Blockchain hash rate API response does not contain a 'values' field.")
}
write(jsonlite::toJSON(hash_rate_json, auto_unbox = TRUE, pretty = TRUE),
      "data/raw/hash-rate.json")

hash_rate_df <- hash_rate_json$values %>%
        dplyr::rename(timestamp = x, hash_rate = y) %>%
        dplyr::mutate(
                Date = as.Date(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"))
        ) %>%
        dplyr::select(Date, hash_rate) %>%
        dplyr::arrange(Date)

hash_rate_weekly <- hash_rate_df %>%
        dplyr::mutate(week = lubridate::floor_date(Date, "week")) %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(
                hash_rate = mean(hash_rate, na.rm = TRUE),
                .groups = "drop"
        )

# ---------------------------
# 7. Merge all weekly datasets
# ---------------------------
macro_weekly <- crypto_group_weekly %>%
        dplyr::left_join(market_weekly, by = "week") %>%
        dplyr::left_join(fear_greed_weekly, by = "week") %>%
        dplyr::left_join(hash_rate_weekly, by = "week") %>%
        dplyr::arrange(week)

# ---------------------------
# 8. Transform and clean
# ---------------------------
macro_weekly <- macro_weekly %>%
        dplyr::mutate(
                log_hash_rate = log(hash_rate + 1)
        ) %>%
        dplyr::mutate(
                across(
                        c(SP500, DowJones, TreasuryYield10Y, GoldFutures, FearGreed, log_hash_rate),
                        ~ ifelse(is.infinite(.x), NA, .x)
                )
        ) %>%
        tidyr::drop_na(
                weekly_return_meme,
                weekly_return_conventional,
                weekly_market_return,
                SP500,
                DowJones,
                TreasuryYield10Y,
                GoldFutures,
                FearGreed,
                log_hash_rate
        )

write.csv(macro_weekly, macro_weekly_file, row.names = FALSE)

# ---------------------------
# 9. Summary table
# ---------------------------
summary_vars <- c(
        "weekly_return_meme",
        "weekly_return_conventional",
        "weekly_market_return",
        "SP500",
        "DowJones",
        "TreasuryYield10Y",
        "GoldFutures",
        "FearGreed",
        "log_hash_rate"
)

macro_summary <- build_summary_table(macro_weekly, summary_vars)

write.csv(macro_summary, macro_summary_file, row.names = FALSE)

save_kable_html(
        macro_summary,
        "Summary Statistics: Macro, Sentiment, Hash Rate, and Crypto Weekly Variables",
        sub("\\.csv$", ".html", macro_summary_file)
)

# ---------------------------
# 10. Correlation matrix
# ---------------------------
correlation_df <- macro_weekly %>%
        dplyr::select(all_of(summary_vars))

correlation_matrix <- stats::cor(correlation_df, use = "pairwise.complete.obs")

write.csv(correlation_matrix, macro_correlation_file, row.names = TRUE)

cor_plot_data <- as.data.frame(as.table(correlation_matrix))
names(cor_plot_data) <- c("Var1", "Var2", "Correlation")

p_heatmap <- ggplot2::ggplot(cor_plot_data, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)), size = 3.5) +
        ggplot2::scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
        ggplot2::labs(
                title = "Correlation Heatmap: Macro, Sentiment, Hash Rate, and Crypto Returns",
                x = NULL,
                y = NULL,
                fill = "Correlation"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )

ggplot2::ggsave(
        macro_heatmap_file,
        plot = p_heatmap,
        width = 10,
        height = 8
)

# ---------------------------
# 11. Estimate baseline OLS models
# ---------------------------
model_meme <- stats::lm(
        weekly_return_meme ~ SP500 + DowJones + TreasuryYield10Y + GoldFutures + FearGreed + log_hash_rate,
        data = macro_weekly
)

model_conventional <- stats::lm(
        weekly_return_conventional ~ SP500 + DowJones + TreasuryYield10Y + GoldFutures + FearGreed + log_hash_rate,
        data = macro_weekly
)

model_market <- stats::lm(
        weekly_market_return ~ SP500 + DowJones + TreasuryYield10Y + GoldFutures + FearGreed + log_hash_rate,
        data = macro_weekly
)

model_results <- dplyr::bind_rows(
        extract_model_results(model_meme, "Meme Coins", "Baseline macro-sentiment-hashrate model"),
        extract_model_results(model_conventional, "Conventional Coins", "Baseline macro-sentiment-hashrate model"),
        extract_model_results(model_market, "Coin Market", "Baseline macro-sentiment-hashrate model")
)

write.csv(model_results, macro_model_results_file, row.names = FALSE)

save_kable_html(
        model_results,
        "OLS Models: Macro, Sentiment, and Hash Rate Drivers of Crypto Returns",
        sub("\\.csv$", ".html", macro_model_results_file)
)

# ---------------------------
# 12. Export objects to global environment (optional)
# ---------------------------
assign("macro_weekly", macro_weekly, envir = .GlobalEnv)
assign("macro_summary", macro_summary, envir = .GlobalEnv)
assign("correlation_matrix", correlation_matrix, envir = .GlobalEnv)
assign("model_results", model_results, envir = .GlobalEnv)

message("07_macro_sentiment_hashrate_models.R: completed successfully")
