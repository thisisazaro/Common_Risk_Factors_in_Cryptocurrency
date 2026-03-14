# ============================================================
# 09_export_tables_and_figures.R
# Purpose:
#   - Audit all generated project outputs
#   - Build a clean manifest of tables, figures, and processed data
#   - Create handoff-ready summary files for portfolio / thesis / GitHub
#   - Export reproducibility and deliverables metadata
# ============================================================

# ---------------------------
# 0. Setup
# ---------------------------
source("code/00_setup.R")

message("09_export_tables_and_figures.R: started")

# ---------------------------
# 1. File paths
# ---------------------------
manifest_csv_file        <- "outputs/export_manifest.csv"
manifest_html_file       <- "outputs/export_manifest.html"
deliverables_md_file     <- "outputs/DELIVERABLES_INDEX.md"
project_inventory_file   <- "outputs/project_inventory.csv"
reproducibility_file     <- "outputs/reproducibility_summary.csv"
portfolio_summary_file   <- "outputs/portfolio_summary.md"

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

safe_file_info <- function(path) {
        info <- file.info(path)
        
        tibble::tibble(
                path = path,
                exists = file.exists(path),
                size_bytes = ifelse(file.exists(path), info$size, NA_real_),
                modified_time = ifelse(file.exists(path), as.character(info$mtime), NA_character_)
        )
}

classify_output_type <- function(path) {
        ext <- tolower(tools::file_ext(path))
        
        dplyr::case_when(
                grepl("^data/processed/", path) ~ "processed_data",
                grepl("^outputs/tables/", path) & ext == "csv" ~ "table_csv",
                grepl("^outputs/tables/", path) & ext == "html" ~ "table_html",
                grepl("^outputs/figures/", path) ~ "figure",
                grepl("^outputs/", path) & ext == "md" ~ "markdown",
                grepl("^outputs/", path) & ext == "csv" ~ "metadata_csv",
                grepl("^outputs/", path) & ext == "html" ~ "metadata_html",
                TRUE ~ "other"
        )
}

classify_module <- function(path) {
        dplyr::case_when(
                grepl("crypto_daily_clean|crypto_weekly|crypto_monthly|market_index", path) ~ "01_load_and_clean_data",
                grepl("summary_statistics|extreme_return|panel_a|panel_b|crypto_vs_coin_market|return_distributions", path) ~ "02_descriptive_statistics",
                grepl("size_strategy|momentum_strategy|volume_strategy|volatility_strategy|generic_quintile", path) ~ "03_cross_sectional_predictors",
                grepl("crypto_factor|strategy_test_portfolios", path) ~ "04_crypto_factor_model",
                grepl("memecoin|conventional|crypto_group_comparison|weekly_market_caps|drawdowns", path) ~ "05_memecoin_vs_conventional_analysis",
                grepl("google_trends|attention_model", path) ~ "06_google_trends_attention_models",
                grepl("macro_sentiment_hashrate", path) ~ "07_macro_sentiment_hashrate_models",
                grepl("network_factor|network_pca|network_crypto_merged", path) ~ "08_network_factors_models",
                TRUE ~ "project_level"
        )
}

describe_output <- function(path) {
        dplyr::case_when(
                grepl("crypto_daily_clean.csv", path) ~ "Cleaned daily cryptocurrency panel",
                grepl("crypto_weekly.csv", path) ~ "Weekly cryptocurrency panel",
                grepl("crypto_monthly.csv", path) ~ "Monthly cryptocurrency panel",
                grepl("risk_free_daily.csv", path) ~ "Daily risk-free rate series",
                grepl("market_index_daily.csv", path) ~ "Daily crypto market proxy index",
                grepl("summary_statistics_by_frequency.csv", path) ~ "Summary statistics by frequency",
                grepl("selected_assets_summary_statistics.csv", path) ~ "Selected assets and market summary statistics",
                grepl("extreme_return_events.csv", path) ~ "Extreme return event frequencies",
                grepl("panel_a_market_characteristics.csv", path) ~ "Panel A market characteristics",
                grepl("panel_b_return_characteristics.csv", path) ~ "Panel B return characteristics",
                grepl("return_distributions", path) ~ "Return distribution plots",
                grepl("crypto_vs_coin_market", path) ~ "Asset vs crypto market comparison plots",
                grepl("size_strategy_quintiles.csv", path) ~ "Size strategy quintile table",
                grepl("size_strategy_spreads.csv", path) ~ "Size strategy spread table",
                grepl("momentum_strategy_summary.csv", path) ~ "Momentum strategy summary table",
                grepl("volume_strategy_summary.csv", path) ~ "Volume strategy summary table",
                grepl("volatility_strategy_summary.csv", path) ~ "Volatility strategy summary table",
                grepl("crypto_factor_returns.csv", path) ~ "Estimated crypto factor return series",
                grepl("crypto_factor_model_results.csv", path) ~ "Factor model regression results",
                grepl("strategy_test_portfolios.csv", path) ~ "Long-short test strategy returns",
                grepl("crypto_factor_cumulative_returns", path) ~ "Cumulative crypto factor plot",
                grepl("crypto_group_comparison_weekly.csv", path) ~ "Weekly grouped crypto comparison dataset",
                grepl("memecoin_vs_conventional_summary_statistics.csv", path) ~ "Grouped crypto summary statistics",
                grepl("meme_vs_conventional_vs_market", path) ~ "Grouped performance comparison plot",
                grepl("google_trends_attention_model_results.csv", path) ~ "Google Trends attention model coefficients",
                grepl("google_trends_attention_r_squared.csv", path) ~ "Google Trends model fit table",
                grepl("macro_sentiment_hashrate_model_results.csv", path) ~ "Macro, sentiment, and hash-rate model results",
                grepl("macro_sentiment_hashrate_correlation_heatmap", path) ~ "Macro/sentiment/hash-rate correlation heatmap",
                grepl("network_univariate_model_results.csv", path) ~ "Univariate network-factor model results",
                grepl("network_multivariate_model_results.csv", path) ~ "Multivariate network-factor model results",
                grepl("network_pca_loadings.csv", path) ~ "PCA loadings for network factor",
                grepl("network_pca_factor_vs_returns", path) ~ "Network PCA factor comparison plot",
                TRUE ~ "Project output artifact"
        )
}

# ---------------------------
# 3. Define expected outputs
# ---------------------------
expected_outputs <- c(
        # processed data
        "data/processed/crypto_daily_clean.csv",
        "data/processed/crypto_weekly.csv",
        "data/processed/crypto_monthly.csv",
        "data/processed/market_index_daily.csv",
        "data/processed/risk_free_daily.csv",
        "data/processed/crypto_group_comparison_daily.csv",
        "data/processed/crypto_group_comparison_weekly.csv",
        "data/processed/crypto_group_comparison_monthly.csv",
        "data/processed/weekly_market_caps.csv",
        "data/processed/macro_sentiment_hashrate_weekly.csv",
        "data/processed/network_factors_weekly.csv",
        "data/processed/network_crypto_merged_weekly.csv",
        
        # 02
        "outputs/tables/summary_statistics_by_frequency.csv",
        "outputs/tables/selected_assets_summary_statistics.csv",
        "outputs/tables/extreme_return_events.csv",
        "outputs/tables/panel_a_market_characteristics.csv",
        "outputs/tables/panel_b_return_characteristics.csv",
        "outputs/figures/return_distributions.png",
        "outputs/figures/crypto_vs_coin_market.png",
        "outputs/figures/crypto_vs_coin_market_sp500.png",
        
        # 03
        "outputs/tables/size_strategy_quintiles.csv",
        "outputs/tables/size_strategy_spreads.csv",
        "outputs/tables/momentum_strategy_summary.csv",
        "outputs/tables/volume_strategy_summary.csv",
        "outputs/tables/volatility_strategy_summary.csv",
        "outputs/figures/generic_quintile_returns.png",
        "outputs/figures/generic_long_short_returns.png",
        
        # 04
        "outputs/tables/crypto_factor_returns.csv",
        "outputs/tables/crypto_factor_correlations.csv",
        "outputs/tables/strategy_test_portfolios.csv",
        "outputs/tables/crypto_factor_model_results.csv",
        "outputs/figures/crypto_factor_cumulative_returns.png",
        
        # 05
        "outputs/tables/memecoin_vs_conventional_summary_statistics.csv",
        "outputs/tables/group_return_summary_statistics.csv",
        "outputs/figures/conventional_assets_vs_coin_market.png",
        "outputs/figures/meme_vs_conventional_vs_market.png",
        "outputs/figures/meme_vs_conventional_drawdowns.png",
        
        # 06
        "outputs/tables/google_trends_attention_model_results.csv",
        "outputs/tables/google_trends_attention_r_squared.csv",
        "outputs/tables/google_trends_keyword_summary.csv",
        "outputs/tables/google_trends_attention_best_fit_summary.csv",
        
        # 07
        "outputs/tables/macro_sentiment_hashrate_model_results.csv",
        "outputs/tables/macro_sentiment_hashrate_summary.csv",
        "outputs/tables/macro_sentiment_hashrate_correlations.csv",
        "outputs/figures/macro_sentiment_hashrate_correlation_heatmap.png",
        
        # 08
        "outputs/tables/network_factor_summary.csv",
        "outputs/tables/network_factor_correlations.csv",
        "outputs/tables/network_univariate_model_results.csv",
        "outputs/tables/network_multivariate_model_results.csv",
        "outputs/tables/network_pca_loadings.csv",
        "outputs/tables/network_granger_results.csv",
        "outputs/figures/network_pca_factor_vs_returns.png",
        "outputs/figures/network_factor_correlation_heatmap.png"
)

# ---------------------------
# 4. Build export manifest
# ---------------------------
manifest <- lapply(expected_outputs, safe_file_info) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(
                output_type = vapply(path, classify_output_type, character(1)),
                module = vapply(path, classify_module, character(1)),
                description = vapply(path, describe_output, character(1))
        ) %>%
        dplyr::arrange(module, output_type, path)

write.csv(manifest, manifest_csv_file, row.names = FALSE)

save_kable_html(
        manifest,
        "Project Export Manifest",
        manifest_html_file
)

# ---------------------------
# 5. Build project inventory
# ---------------------------
inventory <- manifest %>%
        dplyr::group_by(module, output_type) %>%
        dplyr::summarise(
                expected_files = dplyr::n(),
                existing_files = sum(exists, na.rm = TRUE),
                missing_files = expected_files - existing_files,
                total_size_bytes = sum(size_bytes, na.rm = TRUE),
                .groups = "drop"
        ) %>%
        dplyr::arrange(module, output_type)

write.csv(inventory, project_inventory_file, row.names = FALSE)

# ---------------------------
# 6. Build reproducibility summary
# ---------------------------
reproducibility_summary <- tibble::tibble(
        Item = c(
                "Project working directory",
                "R version",
                "Total expected outputs",
                "Total existing outputs",
                "Total missing outputs",
                "Processed data files found",
                "Table CSV files found",
                "Table HTML files found",
                "Figure files found"
        ),
        Value = c(
                getwd(),
                R.version.string,
                nrow(manifest),
                sum(manifest$exists, na.rm = TRUE),
                sum(!manifest$exists, na.rm = TRUE),
                sum(manifest$output_type == "processed_data" & manifest$exists, na.rm = TRUE),
                sum(manifest$output_type == "table_csv" & manifest$exists, na.rm = TRUE),
                sum(manifest$output_type == "table_html" & manifest$exists, na.rm = TRUE),
                sum(manifest$output_type == "figure" & manifest$exists, na.rm = TRUE)
        )
)

write.csv(reproducibility_summary, reproducibility_file, row.names = FALSE)

# ---------------------------
# 7. Create deliverables markdown index
# ---------------------------
deliverables_lines <- c(
        "# Project Deliverables Index",
        "",
        "This file summarizes the key outputs generated by the cryptocurrency research pipeline.",
        "",
        "## Modules",
        ""
)

modules <- unique(manifest$module)

for (mod in modules) {
        deliverables_lines <- c(
                deliverables_lines,
                paste0("### ", mod),
                ""
        )
        
        mod_rows <- manifest %>%
                dplyr::filter(module == mod)
        
        if (nrow(mod_rows) == 0) {
                deliverables_lines <- c(deliverables_lines, "- No outputs registered.", "")
        } else {
                for (i in seq_len(nrow(mod_rows))) {
                        status <- ifelse(mod_rows$exists[i], "✅", "❌")
                        line_i <- paste0(
                                "- ", status, " `", mod_rows$path[i], "` — ", mod_rows$description[i]
                        )
                        deliverables_lines <- c(deliverables_lines, line_i)
                }
                deliverables_lines <- c(deliverables_lines, "")
        }
}

writeLines(deliverables_lines, deliverables_md_file)

# ---------------------------
# 8. Create portfolio summary markdown
# ---------------------------
existing_figures <- manifest %>%
        dplyr::filter(output_type == "figure", exists) %>%
        dplyr::pull(path)

existing_tables <- manifest %>%
        dplyr::filter(output_type %in% c("table_csv", "table_html"), exists) %>%
        dplyr::pull(path)

portfolio_lines <- c(
        "# Portfolio Summary",
        "",
        "## Project Scope",
        "",
        "This repository contains a reproducible empirical research pipeline for cryptocurrency return analysis.",
        "",
        "The project includes:",
        "- data ingestion and cleaning,",
        "- descriptive return analysis,",
        "- cross-sectional predictors,",
        "- crypto factor models,",
        "- meme coin vs conventional coin comparisons,",
        "- Google Trends attention models,",
        "- macro / sentiment / hash-rate models,",
        "- blockchain network factor models.",
        "",
        "## Key Output Counts",
        "",
        paste0("- Existing artifacts: ", sum(manifest$exists, na.rm = TRUE)),
        paste0("- Figures: ", length(existing_figures)),
        paste0("- Tables: ", length(existing_tables)),
        "",
        "## Suggested Highlights for README / Portfolio",
        "",
        "- Descriptive return distributions and benchmark comparisons",
        "- Meme vs conventional performance and drawdown comparison",
        "- Cross-sectional strategy tables",
        "- Crypto factor return series and regression outputs",
        "- Google Trends attention regressions",
        "- Macro-sentiment-hashrate model results",
        "- Blockchain network PCA factor and return models",
        "",
        "## Reproducibility Note",
        "",
        "All outputs listed in the export manifest are generated from modular R scripts and saved in structured project folders."
)

writeLines(portfolio_lines, portfolio_summary_file)

# ---------------------------
# 9. Export objects to global environment (optional)
# ---------------------------
assign("manifest", manifest, envir = .GlobalEnv)
assign("inventory", inventory, envir = .GlobalEnv)
assign("reproducibility_summary", reproducibility_summary, envir = .GlobalEnv)

message("09_export_tables_and_figures.R: completed successfully")
