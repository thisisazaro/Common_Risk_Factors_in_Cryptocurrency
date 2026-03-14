# ============================================================
# 06_google_trends_attention_models.R
# Purpose:
#   - Study the relationship between Google search attention
#     and cryptocurrency return dynamics
#   - Run comparable lag-augmented regression models across
#     multiple Google Trends keywords
#   - Compare attention sensitivity for:
#       * Coin Market
#       * Conventional Coins
#       * Meme Coins
#       * Optional individual assets
#   - Export regression tables and model-fit summaries
# ============================================================

# ---------------------------
# 0. Setup
# ---------------------------
source("code/00_setup.R")

message("06_google_trends_attention_models.R: started")

# ---------------------------
# 1. File paths
# ---------------------------
weekly_file <- "data/processed/crypto_group_comparison_weekly.csv"

trends_dir <- "data/raw"

attention_results_file <- "outputs/tables/google_trends_attention_model_results.csv"
attention_r2_file      <- "outputs/tables/google_trends_attention_r_squared.csv"
attention_summary_file <- "outputs/tables/google_trends_keyword_summary.csv"

attention_plot_dir <- "outputs/figures/google_trends"

dir.create(attention_plot_dir, recursive = TRUE, showWarnings = FALSE)

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

safe_scale <- function(x) {
        x <- as.numeric(x)
        s <- stats::sd(x, na.rm = TRUE)
        m <- mean(x, na.rm = TRUE)
        if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
        (x - m) / s
}

clean_trend_value <- function(x) {
        x <- as.character(x)
        x <- gsub("<", "", x)
        x <- gsub(",", ".", x)
        suppressWarnings(as.numeric(x))
}

calculate_t_stat <- function(x) {
        x <- x[is.finite(x) & !is.na(x)]
        n <- length(x)
        if (n < 2) return(NA_real_)
        mean(x) / (stats::sd(x) / sqrt(n))
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

extract_model_output <- function(model, keyword, target_series, spec_name) {
        td <- broom::tidy(model)
        
        td %>%
                dplyr::mutate(
                        keyword = keyword,
                        target_series = target_series,
                        specification = spec_name,
                        estimate_fmt = mapply(format_estimate, estimate, p.value, MoreArgs = list(digits = 4)),
                        .before = 1
                ) %>%
                dplyr::select(
                        keyword,
                        target_series,
                        specification,
                        term,
                        estimate_fmt,
                        estimate,
                        std.error,
                        statistic,
                        p.value
                )
}

extract_model_fit <- function(model, keyword, target_series, spec_name) {
        sm <- summary(model)
        tibble::tibble(
                keyword = keyword,
                target_series = target_series,
                specification = spec_name,
                r_squared = sm$r.squared,
                adj_r_squared = sm$adj.r.squared,
                sigma = sm$sigma,
                n = stats::nobs(model)
        )
}

plot_attention_series <- function(df, keyword_label, output_file) {
        p <- ggplot2::ggplot(df, ggplot2::aes(x = week, y = Google_t)) +
                ggplot2::geom_line(linewidth = 0.9) +
                ggplot2::labs(
                        title = paste("Google Trends:", keyword_label),
                        x = "Week",
                        y = "Standardized Search Interest"
                ) +
                ggplot2::theme_minimal()
        
        ggplot2::ggsave(output_file, plot = p, width = 9, height = 4.5)
}

prepare_attention_data <- function(base_data, trend_data, return_var) {
        stopifnot(return_var %in% names(base_data))
        
        base_data %>%
                dplyr::left_join(trend_data, by = "week") %>%
                dplyr::arrange(week) %>%
                dplyr::mutate(
                        R_t_0 = .data[[return_var]],
                        R_t_1 = dplyr::lag(.data[[return_var]], 1),
                        R_t_2 = dplyr::lag(.data[[return_var]], 2),
                        R_t_3 = dplyr::lag(.data[[return_var]], 3),
                        R_t_4 = dplyr::lag(.data[[return_var]], 4)
                ) %>%
                dplyr::filter(
                        !is.na(Google_t), is.finite(Google_t),
                        !is.na(R_t_0), is.finite(R_t_0)
                )
}

run_attention_models <- function(df, keyword, target_series) {
        model_1 <- stats::lm(Google_t ~ R_t_0, data = df)
        
        model_2 <- df %>%
                dplyr::filter(!is.na(R_t_1), is.finite(R_t_1)) %>%
                {stats::lm(Google_t ~ R_t_0 + R_t_1, data = .)}
        
        model_3 <- df %>%
                dplyr::filter(!is.na(R_t_1), !is.na(R_t_2), is.finite(R_t_1), is.finite(R_t_2)) %>%
                {stats::lm(Google_t ~ R_t_0 + R_t_1 + R_t_2, data = .)}
        
        model_4 <- df %>%
                dplyr::filter(!is.na(R_t_1), !is.na(R_t_2), !is.na(R_t_3),
                              is.finite(R_t_1), is.finite(R_t_2), is.finite(R_t_3)) %>%
                {stats::lm(Google_t ~ R_t_0 + R_t_1 + R_t_2 + R_t_3, data = .)}
        
        model_5 <- df %>%
                dplyr::filter(!is.na(R_t_1), !is.na(R_t_2), !is.na(R_t_3), !is.na(R_t_4),
                              is.finite(R_t_1), is.finite(R_t_2), is.finite(R_t_3), is.finite(R_t_4)) %>%
                {stats::lm(Google_t ~ R_t_0 + R_t_1 + R_t_2 + R_t_3 + R_t_4, data = .)}
        
        models <- list(
                "Model 1: current return" = model_1,
                "Model 2: + 1 lag" = model_2,
                "Model 3: + 2 lags" = model_3,
                "Model 4: + 3 lags" = model_4,
                "Model 5: + 4 lags" = model_5
        )
        
        coef_tbl <- purrr::imap_dfr(
                models,
                ~ extract_model_output(.x, keyword = keyword, target_series = target_series, spec_name = .y)
        )
        
        fit_tbl <- purrr::imap_dfr(
                models,
                ~ extract_model_fit(.x, keyword = keyword, target_series = target_series, spec_name = .y)
        )
        
        list(coefficients = coef_tbl, fit = fit_tbl)
}

read_google_trends_file <- function(filepath, keyword_label) {
        ext <- tools::file_ext(filepath)
        
        if (tolower(ext) %in% c("csv")) {
                df <- utils::read.csv(filepath, skip = 1, header = TRUE, stringsAsFactors = FALSE)
        } else if (tolower(ext) %in% c("xlsx", "xls")) {
                df <- readxl::read_excel(filepath)
                df <- as.data.frame(df)
        } else {
                stop("Unsupported Google Trends file type: ", filepath)
        }
        
        if (ncol(df) < 2) {
                stop("Google Trends file must contain at least 2 columns: ", filepath)
        }
        
        names(df)[1:2] <- c("Week", "Google_t_raw")
        
        # Handle common Google Trends export quirks
        df <- df %>%
                dplyr::filter(!is.na(Week)) %>%
                dplyr::mutate(
                        Week = as.character(Week)
                )
        
        # Try multiple date parses
        parsed_week <- suppressWarnings(as.Date(df$Week))
        idx_na <- is.na(parsed_week)
        
        if (any(idx_na)) {
                parsed_week[idx_na] <- suppressWarnings(as.Date(df$Week[idx_na], format = "%m/%d/%Y"))
        }
        
        if (any(is.na(parsed_week))) {
                parsed_week <- as.Date(parsed_week)
        }
        
        df <- df %>%
                dplyr::mutate(
                        week = parsed_week,
                        Google_t_raw = clean_trend_value(Google_t_raw)
                ) %>%
                dplyr::filter(!is.na(week)) %>%
                dplyr::mutate(
                        Google_t = safe_scale(Google_t_raw),
                        keyword = keyword_label
                ) %>%
                dplyr::select(week, Google_t_raw, Google_t, keyword)
        
        df
}

# ---------------------------
# 3. Load weekly crypto group dataset
# ---------------------------
if (!file.exists(weekly_file)) stop("Missing file: ", weekly_file)

crypto_group_weekly <- read.csv(weekly_file)
crypto_group_weekly$week <- as.Date(crypto_group_weekly$week)

required_cols <- c(
        "week",
        "weekly_market_return",
        "weekly_return_conventional",
        "weekly_return_meme",
        "weekly_return_BTC",
        "weekly_return_ETH",
        "weekly_return_XRP",
        "weekly_return_DOGE",
        "weekly_return_SHIB"
)

missing_cols <- setdiff(required_cols, names(crypto_group_weekly))
if (length(missing_cols) > 0) {
        stop("Missing columns in weekly dataset: ", paste(missing_cols, collapse = ", "))
}

# ---------------------------
# 4. Define Google Trends inputs
# ---------------------------
# You can edit this table as your raw files evolve.
trend_files <- tibble::tribble(
        ~keyword,           ~file,
        "Crypto",           file.path(trends_dir, "multiTimelineCrypto.csv"),
        "Bitcoin",          file.path(trends_dir, "multiTimelineBitcoin.csv"),
        "CryptoReg",        file.path(trends_dir, "multiTimelineCryptoReg.csv"),
        "Hack",             file.path(trends_dir, "multiTimelineHack.csv"),
        "Doge",             file.path(trends_dir, "multiTimeline_doge_coin.csv"),
        "Cryptocurrency",   file.path(trends_dir, "multiTimelineCryptocurrency.csv"),
        "BitcoinBan",       file.path(trends_dir, "multiTimelineBitcoinBan.csv"),
        "Bitget",           file.path(trends_dir, "multiTimelineBitget.csv"),
        "Cryptoban",        file.path(trends_dir, "multiTimelinecryptoban.csv"),
        "SHIB",             file.path(trends_dir, "multiTimeline_shib.xlsx")
) %>%
        dplyr::filter(file.exists(file))

if (nrow(trend_files) == 0) {
        stop("No Google Trends files found in data/raw.")
}

# ---------------------------
# 5. Define target return series
# ---------------------------
target_series_map <- tibble::tribble(
        ~target_label,           ~return_var,
        "Coin Market",           "weekly_market_return",
        "Conventional Coins",    "weekly_return_conventional",
        "Meme Coins",            "weekly_return_meme",
        "Bitcoin",               "weekly_return_BTC",
        "Ethereum",              "weekly_return_ETH",
        "Ripple",                "weekly_return_XRP",
        "Dogecoin",              "weekly_return_DOGE",
        "Shiba Inu",             "weekly_return_SHIB"
)

# ---------------------------
# 6. Run all keyword × target models
# ---------------------------
all_coefficients <- list()
all_fit <- list()
keyword_summary <- list()

for (i in seq_len(nrow(trend_files))) {
        keyword_i <- trend_files$keyword[i]
        file_i <- trend_files$file[i]
        
        trend_df <- read_google_trends_file(file_i, keyword_i)
        
        # Plot attention series
        plot_attention_series(
                trend_df,
                keyword_label = keyword_i,
                output_file = file.path(
                        attention_plot_dir,
                        paste0("google_trends_", gsub("[^A-Za-z0-9]+", "_", tolower(keyword_i)), ".png")
                )
        )
        
        keyword_summary[[keyword_i]] <- tibble::tibble(
                keyword = keyword_i,
                start_week = min(trend_df$week, na.rm = TRUE),
                end_week = max(trend_df$week, na.rm = TRUE),
                n_obs = nrow(trend_df),
                mean_raw = mean(trend_df$Google_t_raw, na.rm = TRUE),
                sd_raw = stats::sd(trend_df$Google_t_raw, na.rm = TRUE),
                mean_standardized = mean(trend_df$Google_t, na.rm = TRUE),
                sd_standardized = stats::sd(trend_df$Google_t, na.rm = TRUE)
        )
        
        for (j in seq_len(nrow(target_series_map))) {
                target_label_j <- target_series_map$target_label[j]
                return_var_j <- target_series_map$return_var[j]
                
                model_df <- prepare_attention_data(
                        base_data = crypto_group_weekly,
                        trend_data = trend_df %>% dplyr::select(week, Google_t),
                        return_var = return_var_j
                )
                
                # Need enough rows to run lag models meaningfully
                if (nrow(model_df) < 30) next
                
                model_out <- run_attention_models(
                        df = model_df,
                        keyword = keyword_i,
                        target_series = target_label_j
                )
                
                all_coefficients[[paste(keyword_i, target_label_j, sep = "__")]] <- model_out$coefficients
                all_fit[[paste(keyword_i, target_label_j, sep = "__")]] <- model_out$fit
        }
}

attention_coefficients <- dplyr::bind_rows(all_coefficients)
attention_fit <- dplyr::bind_rows(all_fit)
keyword_summary_tbl <- dplyr::bind_rows(keyword_summary)

# ---------------------------
# 7. Save outputs
# ---------------------------
write.csv(attention_coefficients, attention_results_file, row.names = FALSE)
write.csv(attention_fit, attention_r2_file, row.names = FALSE)
write.csv(keyword_summary_tbl, attention_summary_file, row.names = FALSE)

save_kable_html(
        attention_fit,
        "Google Trends Attention Models: R-squared and Model Fit",
        sub("\\.csv$", ".html", attention_r2_file)
)

save_kable_html(
        keyword_summary_tbl,
        "Google Trends Keyword Summary",
        sub("\\.csv$", ".html", attention_summary_file)
)

# ---------------------------
# 8. Create compact summary table
# ---------------------------
best_fit_summary <- attention_fit %>%
        dplyr::group_by(keyword, target_series) %>%
        dplyr::slice_max(order_by = r_squared, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(keyword, dplyr::desc(r_squared))

write.csv(
        best_fit_summary,
        "outputs/tables/google_trends_attention_best_fit_summary.csv",
        row.names = FALSE
)

save_kable_html(
        best_fit_summary,
        "Best-Fitting Google Trends Attention Model by Keyword and Target Series",
        "outputs/tables/google_trends_attention_best_fit_summary.html"
)

# ---------------------------
# 9. Export objects to global environment (optional)
# ---------------------------
assign("attention_coefficients", attention_coefficients, envir = .GlobalEnv)
assign("attention_fit", attention_fit, envir = .GlobalEnv)
assign("keyword_summary_tbl", keyword_summary_tbl, envir = .GlobalEnv)
assign("best_fit_summary", best_fit_summary, envir = .GlobalEnv)

message("06_google_trends_attention_models.R: completed successfully")