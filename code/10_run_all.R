# ============================================================
# 10_run_all.R
# Purpose:
#   - Execute the full cryptocurrency research pipeline
#   - Run all project modules in the correct order
#   - Log execution status, timing, and failures
#   - Produce a final run summary for reproducibility
# ============================================================

# ---------------------------
# 0. Global options
# ---------------------------
options(stringsAsFactors = FALSE)
options(scipen = 999)

cat("10_run_all.R: pipeline started\n")

# ---------------------------
# 1. Create logs folder if needed
# ---------------------------
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)

run_log_file <- file.path("outputs/logs", "pipeline_run_log.csv")
run_summary_file <- file.path("outputs/logs", "pipeline_run_summary.txt")

# ---------------------------
# 2. Define pipeline modules
# ---------------------------
pipeline_modules <- c(
        "code/01_load_and_clean_data.R",
        "code/02_descriptive_statistics.R",
        "code/03_cross_sectional_predictors.R",
        "code/04_crypto_factor_model.R",
        "code/05_memecoin_vs_conventional_analysis.R",
        "code/06_google_trends_attention_models.R",
        "code/07_macro_sentiment_hashrate_models.R",
        "code/08_network_factors_models.R",
        "code/09_export_tables_and_figures.R"
)

# ---------------------------
# 3. Helper function to run one module
# ---------------------------
run_module <- function(module_path) {
        start_time <- Sys.time()
        
        if (!file.exists(module_path)) {
                return(tibble::tibble(
                        module = module_path,
                        status = "missing",
                        start_time = as.character(start_time),
                        end_time = as.character(Sys.time()),
                        duration_seconds = NA_real_,
                        error_message = "File does not exist"
                ))
        }
        
        cat("Running:", module_path, "\n")
        
        result <- tryCatch(
                {
                        source(module_path, local = FALSE, echo = FALSE)
                        end_time <- Sys.time()
                        
                        tibble::tibble(
                                module = module_path,
                                status = "success",
                                start_time = as.character(start_time),
                                end_time = as.character(end_time),
                                duration_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
                                error_message = NA_character_
                        )
                },
                error = function(e) {
                        end_time <- Sys.time()
                        
                        tibble::tibble(
                                module = module_path,
                                status = "failed",
                                start_time = as.character(start_time),
                                end_time = as.character(end_time),
                                duration_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
                                error_message = as.character(e$message)
                        )
                }
        )
        
        result
}

# ---------------------------
# 4. Run pipeline sequentially
# ---------------------------
run_results <- list()

for (i in seq_along(pipeline_modules)) {
        module_i <- pipeline_modules[i]
        module_result <- run_module(module_i)
        
        run_results[[i]] <- module_result
        
        # Save log progressively after each step
        current_log <- dplyr::bind_rows(run_results)
        utils::write.csv(current_log, run_log_file, row.names = FALSE)
        
        # Stop pipeline immediately if a module fails
        if (module_result$status[1] != "success") {
                cat("Pipeline stopped at:", module_i, "\n")
                cat("Reason:", module_result$error_message[1], "\n")
                break
        }
}

run_log <- dplyr::bind_rows(run_results)

# ---------------------------
# 5. Build final summary
# ---------------------------
total_modules <- length(pipeline_modules)
successful_modules <- sum(run_log$status == "success", na.rm = TRUE)
failed_modules <- sum(run_log$status == "failed", na.rm = TRUE)
missing_modules <- sum(run_log$status == "missing", na.rm = TRUE)

total_runtime <- sum(run_log$duration_seconds, na.rm = TRUE)

summary_lines <- c(
        "CRYPTO RESEARCH PIPELINE RUN SUMMARY",
        "===================================",
        "",
        paste0("Run timestamp: ", Sys.time()),
        paste0("Working directory: ", getwd()),
        "",
        paste0("Total modules configured: ", total_modules),
        paste0("Modules completed successfully: ", successful_modules),
        paste0("Modules failed: ", failed_modules),
        paste0("Modules missing: ", missing_modules),
        paste0("Total runtime (seconds): ", round(total_runtime, 2)),
        ""
)

if (failed_modules > 0) {
        failed_rows <- run_log[run_log$status == "failed", , drop = FALSE]
        summary_lines <- c(summary_lines, "FAILED MODULES", "--------------")
        
        for (i in seq_len(nrow(failed_rows))) {
                summary_lines <- c(
                        summary_lines,
                        paste0("* ", failed_rows$module[i]),
                        paste0("  Error: ", failed_rows$error_message[i]),
                        ""
                )
        }
}

if (missing_modules > 0) {
        missing_rows <- run_log[run_log$status == "missing", , drop = FALSE]
        summary_lines <- c(summary_lines, "MISSING MODULES", "---------------")
        
        for (i in seq_len(nrow(missing_rows))) {
                summary_lines <- c(
                        summary_lines,
                        paste0("* ", missing_rows$module[i]),
                        ""
                )
        }
}

if (successful_modules > 0) {
        ok_rows <- run_log[run_log$status == "success", , drop = FALSE]
        summary_lines <- c(summary_lines, "SUCCESSFUL MODULES", "------------------")
        
        for (i in seq_len(nrow(ok_rows))) {
                summary_lines <- c(
                        summary_lines,
                        paste0("* ", ok_rows$module[i], " (", round(ok_rows$duration_seconds[i], 2), " sec)")
                )
        }
        
        summary_lines <- c(summary_lines, "")
}

writeLines(summary_lines, run_summary_file)

# ---------------------------
# 6. Final console output
# ---------------------------
utils::write.csv(run_log, run_log_file, row.names = FALSE)

cat("\n10_run_all.R: pipeline finished\n")
cat("Execution log saved to:", run_log_file, "\n")
cat("Execution summary saved to:", run_summary_file, "\n")

if (failed_modules > 0 || missing_modules > 0) {
        stop("Pipeline did not complete successfully. See outputs/logs/ for details.")
} else {
        cat("All modules completed successfully.\n")
}