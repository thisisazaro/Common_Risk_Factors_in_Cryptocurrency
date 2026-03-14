# ============================================================
# 08_network_factors_models.R
# Purpose:
#   - Build Bitcoin network activity factors from raw JSON files
#   - Aggregate them to weekly frequency
#   - Construct a composite network factor using PCA
#   - Estimate univariate and multivariate models for:
#       * Meme coin returns
#       * Conventional coin returns
#       * Aggregate crypto market returns
#   - Export processed datasets, tables, and figures
# ============================================================

# ---------------------------
# 0. Setup
# ---------------------------
source("code/00_setup.R")

message("08_network_factors_models.R: started")

# ---------------------------
# 1. File paths
# ---------------------------
weekly_file <- "data/processed/crypto_group_comparison_weekly.csv"

unique_addresses_file <- "data/raw/n-unique-addresses.json"
transactions_file     <- "data/raw/n-transactions.json"
payments_file         <- "data/raw/n-payments.json"
output_volume_file    <- "data/raw/output-volume.json"

raw_network_csv_file        <- "data/raw/network_factors_Bitcoin.csv"
processed_network_weekly    <- "data/processed/network_factors_weekly.csv"
processed_network_merged    <- "data/processed/network_crypto_merged_weekly.csv"

network_summary_file        <- "outputs/tables/network_factor_summary.csv"
network_corr_file           <- "outputs/tables/network_factor_correlations.csv"
network_univariate_file     <- "outputs/tables/network_univariate_model_results.csv"
network_multivariate_file   <- "outputs/tables/network_multivariate_model_results.csv"
network_pca_loadings_file   <- "outputs/tables/network_pca_loadings.csv"
network_granger_file        <- "outputs/tables/network_granger_results.csv"

network_pca_plot_file       <- "outputs/figures/network_pca_factor_vs_returns.png"
network_heatmap_file        <- "outputs/figures/network_factor_correlation_heatmap.png"

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

fill_na_with_moving_avg <- function(x, window_size = 7) {
        x <- zoo::na.approx(x, rule = 2, na.rm = FALSE)
        rollmean_filled <- zoo::rollapply(
                x,
                width = window_size,
                FUN = mean,
                na.rm = TRUE,
                fill = NA,
                align = "center"
        )
        x[is.na(x)] <- rollmean_filled[is.na(x)]
        x <- zoo::na.locf(x, na.rm = FALSE)
        x <- zoo::na.locf(x, fromLast = TRUE, na.rm = FALSE)
        x
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

read_blockchain_chart_json <- function(filepath, value_name) {
        if (!file.exists(filepath)) stop("Missing file: ", filepath)
        
        json_data <- jsonlite::fromJSON(filepath)
        
        if ("values" %in% names(json_data)) {
                values_df <- json_data$values
        } else if (value_name %in% names(json_data)) {
                values_df <- json_data[[value_name]]
        } else {
                stop("Could not find chart values in file: ", filepath)
        }
        
        if (!all(c("x", "y") %in% names(values_df))) {
                stop("JSON file does not contain x/y structure: ", filepath)
        }
        
        timestamp_raw <- as.numeric(values_df$x)
        timestamp_sec <- ifelse(timestamp_raw > 1e12, timestamp_raw / 1000, timestamp_raw)
        
        result <- tibble::tibble(
                Date = as.Date(as.POSIXct(timestamp_sec, origin = "1970-01-01", tz = "UTC")),
                value = as.numeric(values_df$y)
        ) %>%
                dplyr::arrange(Date)
        
        if (min(result$Date, na.rm = TRUE) > as.Date("2030-01-01")) {
                stop("Parsed network dates are implausibly far in the future. Check timestamp units in: ", filepath)
        }
        
        result
}

# ---------------------------
# 3. Load weekly crypto dataset
# ---------------------------
if (!file.exists(weekly_file)) stop("Missing weekly dataset: ", weekly_file)

crypto_group_weekly <- read.csv(weekly_file)
crypto_group_weekly$week <- as.Date(crypto_group_weekly$week)

required_crypto_cols <- c(
        "week",
        "weekly_return_meme",
        "weekly_return_conventional",
        "weekly_market_return"
)

missing_crypto_cols <- setdiff(required_crypto_cols, names(crypto_group_weekly))
if (length(missing_crypto_cols) > 0) {
        stop("Missing columns in weekly crypto dataset: ", paste(missing_crypto_cols, collapse = ", "))
}

# ---------------------------
# 4. Load raw network JSON files
# ---------------------------
unique_addresses_df <- read_blockchain_chart_json(unique_addresses_file, "n-unique-addresses") %>%
        dplyr::rename(UniqueAddresses = value)

transactions_df <- read_blockchain_chart_json(transactions_file, "n-transactions") %>%
        dplyr::rename(Transactions = value)

payments_df <- read_blockchain_chart_json(payments_file, "n-payments") %>%
        dplyr::rename(Payments = value)

output_volume_df <- read_blockchain_chart_json(output_volume_file, "output-volume") %>%
        dplyr::rename(TransactionVolumeUSD = value)

# ---------------------------
# 5. Merge raw network series
# ---------------------------
network_daily <- unique_addresses_df %>%
        dplyr::full_join(transactions_df, by = "Date") %>%
        dplyr::full_join(payments_df, by = "Date") %>%
        dplyr::full_join(output_volume_df, by = "Date") %>%
        dplyr::arrange(Date)

# Save assembled raw panel
write.csv(network_daily, raw_network_csv_file, row.names = FALSE)

# ---------------------------
# 6. Clean network data
# ---------------------------
network_daily <- network_daily %>%
        dplyr::mutate(
                UniqueAddresses = as.numeric(UniqueAddresses),
                Transactions = as.numeric(Transactions),
                Payments = as.numeric(Payments),
                TransactionVolumeUSD = as.numeric(TransactionVolumeUSD)
        ) %>%
        dplyr::mutate(
                UniqueAddresses = fill_na_with_moving_avg(UniqueAddresses),
                Transactions = fill_na_with_moving_avg(Transactions),
                Payments = fill_na_with_moving_avg(Payments),
                TransactionVolumeUSD = fill_na_with_moving_avg(TransactionVolumeUSD)
        ) %>%
        dplyr::filter(
                !is.na(Date),
                dplyr::if_all(
                        c(UniqueAddresses, Transactions, Payments, TransactionVolumeUSD),
                        ~ is.finite(.x)
                )
        )

# ---------------------------
# 7. Construct transformed network variables
# ---------------------------
network_daily <- network_daily %>%
        dplyr::arrange(Date) %>%
        dplyr::mutate(
                Delta_Users = c(NA, diff(UniqueAddresses)),
                Delta_Transactions = c(NA, diff(Transactions)),
                Delta_Payments = c(NA, diff(Payments)),
                Delta_TransactionVolume = c(NA, diff(TransactionVolumeUSD))
        ) %>%
        tidyr::drop_na(
                Delta_Users,
                Delta_Transactions,
                Delta_Payments,
                Delta_TransactionVolume
        )

# ---------------------------
# 8. PCA-based composite network factor
# ---------------------------
pca_input <- network_daily %>%
        dplyr::select(
                Delta_Users,
                Delta_Transactions,
                Delta_Payments,
                Delta_TransactionVolume
        )

pca_model <- psych::principal(
        pca_input,
        nfactors = 1,
        rotate = "none",
        scores = TRUE
)

network_daily$PC_network <- as.numeric(pca_model$scores[, 1])

pca_loadings <- tibble::tibble(
        Variable = rownames(pca_model$loadings),
        Loading = as.numeric(pca_model$loadings[, 1])
)

write.csv(pca_loadings, network_pca_loadings_file, row.names = FALSE)

# ---------------------------
# 9. Aggregate to weekly frequency
# ---------------------------
network_weekly <- network_daily %>%
        dplyr::mutate(week = lubridate::floor_date(Date, "week")) %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(
                UniqueAddresses = dplyr::last(UniqueAddresses),
                Transactions = sum(Delta_Transactions, na.rm = TRUE),
                Payments = sum(Delta_Payments, na.rm = TRUE),
                TransactionVolumeUSD = sum(Delta_TransactionVolume, na.rm = TRUE),
                PC_network = mean(PC_network, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        dplyr::arrange(week)

write.csv(network_weekly, processed_network_weekly, row.names = FALSE)

# ---------------------------
# 10. Merge network data with weekly crypto returns
# ---------------------------
network_crypto_weekly <- crypto_group_weekly %>%
        dplyr::left_join(network_weekly, by = "week") %>%
        dplyr::filter(
                !is.na(weekly_return_meme),
                !is.na(weekly_return_conventional),
                !is.na(weekly_market_return),
                !is.na(UniqueAddresses),
                !is.na(Transactions),
                !is.na(Payments),
                !is.na(TransactionVolumeUSD),
                !is.na(PC_network)
        ) %>%
        dplyr::mutate(
                log_UniqueAddresses = log(UniqueAddresses + 1),
                log_Transactions = log(abs(Transactions) + 1),
                log_Payments = log(abs(Payments) + 1),
                log_TransactionVolumeUSD = log(abs(TransactionVolumeUSD) + 1)
        )

write.csv(network_crypto_weekly, processed_network_merged, row.names = FALSE)

# ---------------------------
# 11. Summary table
# ---------------------------
summary_vars <- c(
        "weekly_return_meme",
        "weekly_return_conventional",
        "weekly_market_return",
        "UniqueAddresses",
        "Transactions",
        "Payments",
        "TransactionVolumeUSD",
        "PC_network",
        "log_UniqueAddresses",
        "log_Transactions",
        "log_Payments",
        "log_TransactionVolumeUSD"
)

network_summary <- build_summary_table(network_crypto_weekly, summary_vars)

write.csv(network_summary, network_summary_file, row.names = FALSE)

save_kable_html(
        network_summary,
        "Summary Statistics: Crypto Returns and Blockchain Network Factors",
        sub("\\.csv$", ".html", network_summary_file)
)

# ---------------------------
# 12. Correlation matrix
# ---------------------------
correlation_vars <- c(
        "weekly_return_meme",
        "weekly_return_conventional",
        "weekly_market_return",
        "UniqueAddresses",
        "Transactions",
        "Payments",
        "TransactionVolumeUSD",
        "PC_network"
)

correlation_matrix <- network_crypto_weekly %>%
        dplyr::select(all_of(correlation_vars)) %>%
        stats::cor(use = "pairwise.complete.obs")

write.csv(correlation_matrix, network_corr_file, row.names = TRUE)

cor_plot_data <- as.data.frame(as.table(correlation_matrix))
names(cor_plot_data) <- c("Var1", "Var2", "Correlation")

p_heatmap <- ggplot2::ggplot(cor_plot_data, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)), size = 3.5) +
        ggplot2::scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
        ggplot2::labs(
                title = "Correlation Heatmap: Crypto Returns and Network Factors",
                x = NULL,
                y = NULL,
                fill = "Correlation"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )

ggplot2::ggsave(
        network_heatmap_file,
        plot = p_heatmap,
        width = 10,
        height = 8
)

# ---------------------------
# 13. Univariate OLS models
# ---------------------------
network_vars <- c(
        "UniqueAddresses",
        "Transactions",
        "Payments",
        "TransactionVolumeUSD",
        "PC_network"
)

dependent_vars <- c(
        "weekly_return_meme",
        "weekly_return_conventional",
        "weekly_market_return"
)

univariate_results <- list()

for (dep in dependent_vars) {
        for (net_var in network_vars) {
                formula_str <- paste(dep, "~", net_var)
                model <- stats::lm(stats::as.formula(formula_str), data = network_crypto_weekly)
                
                univariate_results[[paste(dep, net_var, sep = "__")]] <-
                        extract_model_results(
                                model,
                                dependent_var = dep,
                                specification = paste("Univariate:", net_var)
                        )
        }
}

univariate_results_tbl <- dplyr::bind_rows(univariate_results)

write.csv(univariate_results_tbl, network_univariate_file, row.names = FALSE)

save_kable_html(
        univariate_results_tbl,
        "Univariate OLS Models: Network Factors and Crypto Returns",
        sub("\\.csv$", ".html", network_univariate_file)
)

# ---------------------------
# 14. Multivariate OLS models
# ---------------------------
model_meme <- stats::lm(
        weekly_return_meme ~ log_UniqueAddresses + log_Transactions + log_Payments + log_TransactionVolumeUSD,
        data = network_crypto_weekly
)

model_conventional <- stats::lm(
        weekly_return_conventional ~ log_UniqueAddresses + log_Transactions + log_Payments + log_TransactionVolumeUSD,
        data = network_crypto_weekly
)

model_market <- stats::lm(
        weekly_market_return ~ log_UniqueAddresses + log_Transactions + log_Payments + log_TransactionVolumeUSD,
        data = network_crypto_weekly
)

model_pca_meme <- stats::lm(
        weekly_return_meme ~ PC_network,
        data = network_crypto_weekly
)

model_pca_conventional <- stats::lm(
        weekly_return_conventional ~ PC_network,
        data = network_crypto_weekly
)

model_pca_market <- stats::lm(
        weekly_market_return ~ PC_network,
        data = network_crypto_weekly
)

multivariate_results_tbl <- dplyr::bind_rows(
        extract_model_results(
                model_meme,
                dependent_var = "weekly_return_meme",
                specification = "Multivariate: log network variables"
        ),
        extract_model_results(
                model_conventional,
                dependent_var = "weekly_return_conventional",
                specification = "Multivariate: log network variables"
        ),
        extract_model_results(
                model_market,
                dependent_var = "weekly_market_return",
                specification = "Multivariate: log network variables"
        ),
        extract_model_results(
                model_pca_meme,
                dependent_var = "weekly_return_meme",
                specification = "Univariate PCA factor"
        ),
        extract_model_results(
                model_pca_conventional,
                dependent_var = "weekly_return_conventional",
                specification = "Univariate PCA factor"
        ),
        extract_model_results(
                model_pca_market,
                dependent_var = "weekly_market_return",
                specification = "Univariate PCA factor"
        )
)

write.csv(multivariate_results_tbl, network_multivariate_file, row.names = FALSE)

save_kable_html(
        multivariate_results_tbl,
        "Multivariate OLS Models: Network Factors and Crypto Returns",
        sub("\\.csv$", ".html", network_multivariate_file)
)

# ---------------------------
# 15. Optional Granger causality test
# ---------------------------
granger_results <- tryCatch({
        gt_meme <- lmtest::grangertest(
                weekly_return_meme ~ PC_network,
                order = 2,
                data = network_crypto_weekly
        )
        
        gt_conventional <- lmtest::grangertest(
                weekly_return_conventional ~ PC_network,
                order = 2,
                data = network_crypto_weekly
        )
        
        gt_market <- lmtest::grangertest(
                weekly_market_return ~ PC_network,
                order = 2,
                data = network_crypto_weekly
        )
        
        tibble::bind_rows(
                broom::tidy(gt_meme) %>% dplyr::mutate(dependent_variable = "weekly_return_meme"),
                broom::tidy(gt_conventional) %>% dplyr::mutate(dependent_variable = "weekly_return_conventional"),
                broom::tidy(gt_market) %>% dplyr::mutate(dependent_variable = "weekly_market_return")
        )
}, error = function(e) {
        tibble::tibble(
                dependent_variable = NA_character_,
                statistic = NA_real_,
                p.value = NA_real_,
                method = paste("Granger test failed:", e$message)
        )
})

write.csv(granger_results, network_granger_file, row.names = FALSE)

# ---------------------------
# 16. Plot PCA factor vs crypto returns
# ---------------------------
plot_df <- network_crypto_weekly %>%
        dplyr::select(week, PC_network, weekly_return_meme, weekly_return_conventional, weekly_market_return) %>%
        dplyr::mutate(
                PC_network_std = safe_scale(PC_network),
                Meme_std = safe_scale(weekly_return_meme),
                Conventional_std = safe_scale(weekly_return_conventional),
                Market_std = safe_scale(weekly_market_return)
        ) %>%
        dplyr::select(week, PC_network_std, Meme_std, Conventional_std, Market_std) %>%
        tidyr::pivot_longer(-week, names_to = "Series", values_to = "Value") %>%
        dplyr::mutate(
                Series = dplyr::recode(
                        Series,
                        "PC_network_std" = "Network PCA Factor",
                        "Meme_std" = "Meme Coins",
                        "Conventional_std" = "Conventional Coins",
                        "Market_std" = "Coin Market"
                )
        )

p_pca <- ggplot2::ggplot(plot_df, ggplot2::aes(x = week, y = Value, color = Series)) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::labs(
                title = "Standardized Network PCA Factor vs Crypto Return Series",
                x = "Week",
                y = "Standardized Value",
                color = "Series"
        ) +
        ggplot2::theme_minimal()

ggplot2::ggsave(
        network_pca_plot_file,
        plot = p_pca,
        width = 10,
        height = 5
)

# ---------------------------
# 17. Export objects to global environment (optional)
# ---------------------------
assign("network_daily", network_daily, envir = .GlobalEnv)
assign("network_weekly", network_weekly, envir = .GlobalEnv)
assign("network_crypto_weekly", network_crypto_weekly, envir = .GlobalEnv)
assign("pca_loadings", pca_loadings, envir = .GlobalEnv)
assign("correlation_matrix", correlation_matrix, envir = .GlobalEnv)
assign("univariate_results_tbl", univariate_results_tbl, envir = .GlobalEnv)
assign("multivariate_results_tbl", multivariate_results_tbl, envir = .GlobalEnv)
assign("granger_results", granger_results, envir = .GlobalEnv)

message("08_network_factors_models.R: completed successfully")