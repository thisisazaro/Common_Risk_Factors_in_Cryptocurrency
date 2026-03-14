# ============================================================
# 00_setup.R
# Purpose:
#   - Load required packages
#   - Create project folders if needed
#   - Set global options
#   - Define lightweight project-wide helper utilities
# ============================================================

message("00_setup.R: loading packages and preparing project environment")

# ---------------------------
# 1. Required packages
# ---------------------------
required_packages <- c(
        "dplyr",
        "tidyr",
        "lubridate",
        "ggplot2",
        "gridExtra",
        "moments",
        "broom",
        "zoo",
        "readxl",
        "quantmod",
        "e1071",
        "kableExtra",
        "purrr",
        "jsonlite",
        "xts",
        "psych",
        "lmtest",
        "knitr",
        "tibble"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
        stop(
                "Missing required packages: ",
                paste(missing_packages, collapse = ", "),
                "\nPlease install them before running the pipeline."
        )
}

# Load packages
invisible(lapply(required_packages, library, character.only = TRUE))

# ---------------------------
# 2. Create project folders
# ---------------------------
project_dirs <- c(
        "data",
        "data/raw",
        "data/processed",
        "code",
        "outputs",
        "outputs/figures",
        "outputs/tables",
        "outputs/models",
        "outputs/logs",
        "paper",
        "memo",
        "env",
        "assets"
)

invisible(lapply(project_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# ---------------------------
# 3. Global options
# ---------------------------
options(stringsAsFactors = FALSE)
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# ---------------------------
# 4. Lightweight helper utilities
# ---------------------------

# Null-coalescing helper
`%||%` <- function(x, y) {
        if (length(x) == 0 || is.null(x)) y else x
}

# Safe weighted mean
safe_weighted_mean <- function(x, w) {
        valid <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w)
        if (!any(valid)) return(NA_real_)
        sum(x[valid] * w[valid], na.rm = TRUE) / sum(w[valid], na.rm = TRUE)
}

# Safe t-statistic
calculate_t_stat <- function(x) {
        x <- x[is.finite(x) & !is.na(x)]
        n <- length(x)
        if (n < 2) return(NA_real_)
        mean(x) / (stats::sd(x) / sqrt(n))
}

# Safe skewness
safe_skewness <- function(x) {
        x <- x[is.finite(x) & !is.na(x)]
        if (length(x) < 3) return(NA_real_)
        moments::skewness(x)
}

# Safe kurtosis
safe_kurtosis <- function(x) {
        x <- x[is.finite(x) & !is.na(x)]
        if (length(x) < 4) return(NA_real_)
        moments::kurtosis(x)
}

# Significance stars
add_significance_stars <- function(p_value) {
        dplyr::case_when(
                is.na(p_value) ~ "",
                p_value < 0.01 ~ "***",
                p_value < 0.05 ~ "**",
                p_value < 0.10 ~ "*",
                TRUE ~ ""
        )
}

# Formatted coefficient with stars
format_estimate <- function(estimate, p_value, digits = 4) {
        if (is.na(estimate)) return("")
        paste0(format(round(estimate, digits), nsmall = digits), add_significance_stars(p_value))
}

# Standardize safely
safe_scale <- function(x) {
        x <- as.numeric(x)
        s <- stats::sd(x, na.rm = TRUE)
        m <- mean(x, na.rm = TRUE)
        if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
        (x - m) / s
}

# Save HTML table
save_kable_html <- function(df, caption_text, output_file) {
        html_table <- knitr::kable(df, format = "html", caption = caption_text, escape = FALSE) %>%
                kableExtra::kable_styling(
                        full_width = FALSE,
                        bootstrap_options = c("striped", "hover", "condensed")
                )
        save_kable(html_table, file = output_file)
}

# Simple file existence checker
assert_file_exists <- function(path) {
        if (!file.exists(path)) stop("Missing file: ", path)
}

# Required columns checker
assert_required_columns <- function(data, required_cols, object_name = "data") {
        missing_cols <- setdiff(required_cols, names(data))
        if (length(missing_cols) > 0) {
                stop(
                        "Missing required columns in ", object_name, ": ",
                        paste(missing_cols, collapse = ", ")
                )
        }
}

# Safe interpolation
safe_na_approx <- function(x) {
        zoo::na.approx(x, na.rm = FALSE)
}

# Fill missing values using interpolation + LOCF/NOCB
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

# Convert return series to index starting at 100
to_index_100 <- function(ret) {
        100 * exp(cumsum(dplyr::coalesce(dplyr::lag(ret), 0)))
}

# Compute drawdown
compute_drawdown <- function(index_series) {
        running_max <- cummax(index_series)
        (index_series / running_max) - 1
}

message("00_setup.R: ready")