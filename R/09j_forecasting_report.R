
# ============================================================
# STEP 09J: FORECASTING REPORT & RESULTS DOCUMENTATION
# Project: Campus Energy Consumption Analytics
# ============================================================

cat("\n============================================\n")
cat("STEP 09J: FORECASTING REPORT\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 1. CREATE OUTPUT FOLDER
# ------------------------------------------------------------

if (!dir.exists("reports")) {
  dir.create("reports", recursive = TRUE)
}

# ------------------------------------------------------------
# 2. LOAD STEP 09 RESULTS
# ------------------------------------------------------------

cat("Loading forecasting results...\n")

required_files <- c(
  "results/forecasting_final_model_comparison.csv",
  "results/forecasting_moving_block_bootstrap.csv",
  "results/forecasting_final_model_selection.csv",
  "results/forecasting_error_investigation.csv",
  "results/forecasting_daily_model_comparison.csv",
  "results/forecasting_robustness_comparison.csv"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "The following required files are missing:\n",
    paste(missing_files, collapse = "\n")
  )
}

final_comparison <- read.csv(
  "results/forecasting_final_model_comparison.csv"
)

bootstrap_results <- read.csv(
  "results/forecasting_moving_block_bootstrap.csv"
)

selection_results <- read.csv(
  "results/forecasting_final_model_selection.csv"
)

error_investigation <- read.csv(
  "results/forecasting_error_investigation.csv"
)

daily_comparison <- read.csv(
  "results/forecasting_daily_model_comparison.csv"
)

robustness_comparison <- read.csv(
  "results/forecasting_robustness_comparison.csv"
)

# ------------------------------------------------------------
# 3. VALIDATE REQUIRED COLUMNS
# ------------------------------------------------------------

required_columns <- list(
  final_comparison = c(
    "Model", "Observations", "MAE", "RMSE", "Bias"
  ),
  bootstrap_results = c(
    "Block_Length_Days",
    "CI_95_Lower",
    "CI_95_Upper",
    "Proportion_Positive"
  ),
  daily_comparison = c(
    "Date",
    "Naive_Daily_MAE",
    "Lag_Daily_MAE",
    "MAE_Improvement"
  ),
  error_investigation = c(
    "Model",
    "Median_Absolute_Error",
    "Error_95th_Percentile",
    "Maximum_Absolute_Error"
  )
)

data_objects <- list(
  final_comparison = final_comparison,
  bootstrap_results = bootstrap_results,
  daily_comparison = daily_comparison,
  error_investigation = error_investigation
)

for (object_name in names(required_columns)) {
  
  missing_columns <- setdiff(
    required_columns[[object_name]],
    names(data_objects[[object_name]])
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing columns in ", object_name, ": ",
      paste(missing_columns, collapse = ", ")
    )
  }
}

# ------------------------------------------------------------
# 4. IDENTIFY FINAL MODEL RESULTS
# ------------------------------------------------------------

final_comparison <- final_comparison[
  order(final_comparison$MAE),
]

best_mae_model <- final_comparison$Model[1]
best_rmse_model <- final_comparison$Model[
  which.min(final_comparison$RMSE)
]

lag_row <- error_investigation[
  error_investigation$Model == "Lag-Based Regression",
]

naive_row <- error_investigation[
  error_investigation$Model == "Naive Previous Hour",
]

if (nrow(lag_row) != 1 || nrow(naive_row) != 1) {
  stop("Could not uniquely identify the two models in error investigation.")
}

best_median_model <- error_investigation$Model[
  which.min(error_investigation$Median_Absolute_Error)
]

lag_mae <- final_comparison$MAE[
  final_comparison$Model == "Lag-Based Regression"
]

naive_mae <- final_comparison$MAE[
  final_comparison$Model == "Naive Previous Hour"
]

lag_rmse <- final_comparison$RMSE[
  final_comparison$Model == "Lag-Based Regression"
]

naive_rmse <- final_comparison$RMSE[
  final_comparison$Model == "Naive Previous Hour"
]

mae_improvement <- naive_mae - lag_mae

mae_improvement_percent <- (
  mae_improvement / naive_mae
) * 100

rmse_improvement_percent <- (
  (naive_rmse - lag_rmse) / naive_rmse
) * 100

# ------------------------------------------------------------
# 5. IDENTIFY BOOTSTRAP INTERPRETATION
# ------------------------------------------------------------

bootstrap_7day <- bootstrap_results[
  bootstrap_results$Block_Length_Days == 7,
]

if (nrow(bootstrap_7day) != 1) {
  stop("Could not uniquely identify the 7-day bootstrap result.")
}

if (
  bootstrap_7day$CI_95_Lower <= 0 &&
  bootstrap_7day$CI_95_Upper >= 0
) {
  bootstrap_interpretation <-
    paste(
      "The 7-day moving-block bootstrap interval includes zero.",
      "The daily improvement is not conclusive under this",
      "bootstrap analysis."
    )
} else if (bootstrap_7day$CI_95_Lower > 0) {
  bootstrap_interpretation <-
    paste(
      "The 7-day moving-block bootstrap interval is entirely",
      "above zero, supporting a positive daily MAE improvement",
      "under this analysis."
    )
} else {
  bootstrap_interpretation <-
    paste(
      "The 7-day moving-block bootstrap interval is entirely",
      "below zero, favouring the Naive Previous Hour model",
      "under this analysis."
    )
}

# ------------------------------------------------------------
# 6. CREATE MARKDOWN TABLES
# ------------------------------------------------------------

format_number <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    "NA",
    formatC(x, format = "f", digits = digits)
  )
}

make_markdown_table <- function(data, digits = 3) {
  
  data <- as.data.frame(data)
  
  header <- paste0(
    "| ",
    paste(names(data), collapse = " | "),
    " |"
  )
  
  separator <- paste0(
    "| ",
    paste(rep("---", ncol(data)), collapse = " | "),
    " |"
  )
  
  rows <- apply(data, 1, function(row) {
    paste0("| ", paste(row, collapse = " | "), " |")
  })
  
  c(header, separator, rows)
}

# Overall model comparison table
comparison_table <- data.frame(
  Model = final_comparison$Model,
  Observations = final_comparison$Observations,
  MAE_kWh = format_number(final_comparison$MAE),
  RMSE_kWh = format_number(final_comparison$RMSE),
  Bias_kWh = format_number(final_comparison$Bias),
  stringsAsFactors = FALSE
)

# Error diagnostic table for the two investigated models
error_table <- data.frame(
  Model = c(
    "Lag-Based Regression",
    "Naive Previous Hour"
  ),
  Median_Absolute_Error = format_number(
    c(
      lag_row$Median_Absolute_Error,
      naive_row$Median_Absolute_Error
    )
  ),
  Error_95th_Percentile = format_number(
    c(
      lag_row$Error_95th_Percentile,
      naive_row$Error_95th_Percentile
    )
  ),
  Maximum_Absolute_Error = format_number(
    c(
      lag_row$Maximum_Absolute_Error,
      naive_row$Maximum_Absolute_Error
    )
  ),
  stringsAsFactors = FALSE
)

# Bootstrap table
bootstrap_table <- data.frame(
  Block_Length_Days = bootstrap_results$Block_Length_Days,
  Replications = bootstrap_results$Bootstrap_Replications,
  CI_95_Lower = format_number(
    bootstrap_results$CI_95_Lower
  ),
  CI_95_Upper = format_number(
    bootstrap_results$CI_95_Upper
  ),
  Proportion_Positive = format_number(
    bootstrap_results$Proportion_Positive,
    digits = 4
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 7. CALCULATE DAILY COMPARISON SUMMARY
# ------------------------------------------------------------

daily_comparison$Date <- as.Date(daily_comparison$Date)

number_of_days <- nrow(daily_comparison)

lag_better_days <- sum(
  daily_comparison$MAE_Improvement > 0,
  na.rm = TRUE
)

naive_better_days <- sum(
  daily_comparison$MAE_Improvement < 0,
  na.rm = TRUE
)

equal_days <- sum(
  daily_comparison$MAE_Improvement == 0,
  na.rm = TRUE
)

mean_daily_improvement <- mean(
  daily_comparison$MAE_Improvement,
  na.rm = TRUE
)

# ------------------------------------------------------------
# 8. GENERATE REPORT CONTENT
# ------------------------------------------------------------

report_lines <- c(
  
  "# Campus Energy Consumption Analytics",
  
  "## Step 09J: Forecasting Report",
  
  "",
  
  "**Analysis type:** Historical hourly energy forecasting",
  
  "**Dataset:** I-BLEND public dataset",
  
  "**Selected forecasting model:** Lag-Based Regression",
  
  "",
  
  "---",
  
  "",
  
  "# 1. Executive Summary",
  
  "",
  
  "This phase evaluated multiple forecasting approaches for",
  "historical hourly campus energy consumption data.",
  
  "",
  
  "Five forecasting models were compared using chronological",
  "training and testing data. The evaluation included baseline",
  "models, regression models, residual diagnostics, robustness",
  "analysis, and moving-block bootstrap validation.",
  
  "",
  
  paste0(
    "The Lag-Based Regression model achieved the lowest overall",
    " MAE of ",
    format_number(lag_mae),
    " kWh and the lowest RMSE of ",
    format_number(lag_rmse),
    " kWh."
  ),
  
  "",
  
  paste0(
    "Compared with the Naive Previous Hour baseline, its MAE",
    " was lower by ",
    format_number(mae_improvement),
    " kWh (",
    format_number(mae_improvement_percent, 2),
    "%)."
  ),
  
  "",
  
  "However, the Naive Previous Hour model achieved a lower",
  "median absolute error. The moving-block bootstrap interval",
  "also included zero, so the improvement is not conclusive",
  "under that validation approach.",
  
  "",
  
  "---",
  
  "",
  
  "# 2. Project Objective",
  
  "",
  
  "The objective was to investigate whether historical hourly",
  "energy consumption patterns could be used to estimate",
  "subsequent hourly energy consumption.",
  
  "",
  
  "The analysis compared simple forecasting baselines with",
  "regression-based approaches and examined their prediction",
  "errors across time.",
  
  "",
  
  "# 3. Dataset and Preparation",
  
  "",
  
  "The project uses the publicly available I-BLEND dataset.",
  "It is a historical case study and does not represent",
  "measurements collected from Heritage Institute of Technology",
  "or any current campus monitoring system.",
  
  "",
  
  "The hourly forecasting target was constructed using building",
  "energy readings and the project coverage rules. Building",
  "readings were considered eligible when hourly meter coverage",
  "was at least 75%. The campus estimate required at least five",
  "eligible buildings.",
  
  "",
  
  "Missing target observations were not replaced with zero.",
  "The forecasting analysis selected the longest continuous",
  "period of observed hourly campus energy values.",
  
  "",
  
  "**Selected continuous period:** 10 June 2016, 09:00 to",
  "5 July 2017, 18:00.",
  
  "",
  
  "**Selected observations:** 9,370 hourly records.",
  
  "**Training observations:** 7,496.",
  
  "**Testing observations:** 1,874.",
  
  "",
  
  "The split was chronological: earlier observations were used",
  "for training and later observations for testing.",
  
  "",
  
  "# 4. Forecasting Methodology",
  
  "",
  
  "The following five models were evaluated:",
  
  "",
  
  "1. **Naive Previous Hour:** Uses the previous hour's actual",
  "   energy consumption as the forecast.",
  
  "2. **Seasonal Naive (24 Hours):** Uses the actual value from",
  "   the same hour on the previous day.",
  
  "3. **Linear Trend Regression:** Models energy consumption",
  "   using a time index.",
  
  "4. **Calendar Regression:** Uses hour, day of week, and",
  "   weekend information.",
  
  "5. **Lag-Based Regression:** Uses previous-hour energy,",
  "   previous-24-hour energy, hour, and day of week.",
  
  "",
  
  "The evaluation used rolling one-step-ahead predictions.",
  "For test-period predictions, the models could use actual",
  "previous observations from the test period when calculating",
  "subsequent forecasts. This is not the same as producing a",
  "fixed-origin forecast for the entire test horizon.",
  
  "",
  
  "# 5. Model Performance",
  
  "",
  
  "The models were evaluated using:",
  
  "",
  
  "- **MAE:** Mean absolute error, measuring average absolute",
  "  prediction error.",
  
  "- **RMSE:** Root mean squared error, giving greater weight",
  "  to larger prediction errors.",
  
  "- **Bias:** Mean predicted value minus actual value. Positive",
  "  bias indicates average overprediction; negative bias",
  "  indicates average underprediction.",
  
  "",
  
  make_markdown_table(comparison_table),
  
  "",
  
  paste0(
    "The best model by MAE was **",
    best_mae_model,
    "**."
  ),
  
  paste0(
    "The best model by RMSE was **",
    best_rmse_model,
    "**."
  ),
  
  paste0(
    "The best median absolute error among the two models with",
    " detailed error diagnostics was **",
    best_median_model,
    "**."
  ),
  
  "",
  
  "# 6. Detailed Error Investigation",
  
  "",
  
  "Detailed error diagnostics were conducted for the Lag-Based",
  "Regression model and the Naive Previous Hour baseline.",
  
  "",
  
  make_markdown_table(error_table),
  
  "",
  
  paste0(
    "Across ",
    number_of_days,
    " evaluated days, Lag-Based Regression had lower daily MAE",
    " on ",
    lag_better_days,
    " days, while Naive Previous Hour had lower daily MAE on ",
    naive_better_days,
    " days. There were ",
    equal_days,
    " days with equal daily MAE."
  ),
  
  "",
  
  paste0(
    "The mean daily MAE improvement was ",
    format_number(mean_daily_improvement),
    " kWh."
  ),
  
  "",
  
  "The results show that the Lag-Based Regression model",
  "performed better on average, but its advantage was not",
  "consistent on every day or across every error metric.",
  
  "",
  
  "# 7. Robustness and Bootstrap Validation",
  
  "",
  
  "A moving-block bootstrap was used to examine daily MAE",
  "improvement while resampling consecutive blocks of days.",
  "This approach retains some local time dependence within",
  "the sampled blocks.",
  
  "",
  
  make_markdown_table(bootstrap_table),
  
  "",
  
  paste0(
    "**7-day bootstrap interpretation:** ",
    bootstrap_interpretation
  ),
  
  "",
  
  "The proportion of positive bootstrap estimates is a",
  "description of the bootstrap distribution, not a p-value.",
  "The confidence interval should be considered alongside",
  "the observed MAE improvement.",
  
  "",
  
  "# 8. Final Model Selection",
  
  "",
  
  "Lag-Based Regression was selected as the recommended model",
  "because it achieved the lowest overall MAE and RMSE among",
  "the five evaluated models.",
  
  "",
  
  paste0(
    "- MAE: ",
    format_number(lag_mae),
    " kWh"
  ),
  
  paste0(
    "- RMSE: ",
    format_number(lag_rmse),
    " kWh"
  ),
  
  paste0(
    "- MAE improvement over Naive Previous Hour: ",
    format_number(mae_improvement_percent, 2),
    "%"
  ),
  
  paste0(
    "- RMSE improvement over Naive Previous Hour: ",
    format_number(rmse_improvement_percent, 2),
    "%"
  ),
  
  "",
  
  "This is a practical model-selection decision based on the",
  "observed test-period metrics. It should not be interpreted",
  "as proof that the model will outperform the baseline on",
  "all future datasets or operating conditions.",
  
  "",
  
  "# 9. Limitations",
  
  "",
  
  "1. The analysis uses historical public data and does not",
  "   measure current energy consumption.",
  
  "2. The selected continuous period is limited to June 2016",
  "   through July 2017.",
  
  "3. The campus target depends on building meter coverage",
  "   and the eligibility rules used during preparation.",
  
  "4. The longest continuous observed period was selected;",
  "   results may differ for other time periods.",
  
  "5. Rolling one-step-ahead evaluation uses actual previous",
  "   observations during the test period. Performance may",
  "   differ for longer fixed-origin forecasts.",
  
  "6. The bootstrap confidence intervals include zero, so",
  "   the observed daily improvement is not conclusive under",
  "   this bootstrap analysis.",
  
  "7. Forecasting errors remain substantial for some hours.",
  "   The largest observed absolute error for the selected",
  "   model was approximately 66.975 kWh.",
  
  "8. Historical patterns may not represent future occupancy,",
  "   weather, schedules, equipment, or campus operations.",
  
  "",
  
  "# 10. Conclusion",
  
  "",
  
  "The forecasting analysis found that Lag-Based Regression",
  "provided the best overall MAE and RMSE among the five",
  "evaluated models on the selected historical test period.",
  
  "",
  
  "Its improvement over the Naive Previous Hour baseline was",
  "modest: approximately 3.98% in MAE and 9.97% in RMSE.",
  "The Naive baseline nevertheless achieved a lower median",
  "absolute error, and the moving-block bootstrap analysis",
  "did not establish a conclusive positive daily improvement.",
  
  "",
  
  "Therefore, Lag-Based Regression is recommended as the",
  "best-performing model in this experiment, while the",
  "Naive Previous Hour model remains a useful and competitive",
  "baseline.",
  
  "",
  
  "Future work could evaluate additional continuous periods,",
  "weather and occupancy features with correctly aligned",
  "timestamps, and fixed-origin multi-step forecasting.",
  
  "",
  
  "---",
  
  "",
  
  "# 11. Supporting Figures",
  
  "",
  
  "The following plots were generated during the forecasting",
  "analysis and are saved in the project's `plots/` directory:",
  
  "",
  
  "- `forecasting_final_model_mae.png`",
  
  "- `forecasting_moving_block_bootstrap.png`",
  
  "- `actual_vs_predicted.png`",
  
  "- `forecast_residuals_over_time.png`",
  
  "- `forecast_residual_distribution.png`",
  
  "- `residuals_vs_predicted.png`",
  
  "- `forecast_error_by_hour.png`",
  
  "- `forecast_error_by_weekday.png`",
  
  "- `forecasting_weekly_robustness.png`",
  
  "- `forecasting_hourly_robustness.png`",
  
  "- `forecasting_weekday_robustness.png`",
  
  "- `forecasting_cumulative_mae.png`",
  
  "- `forecasting_paired_error_improvement.png`",
  
  "- `forecasting_error_timeline.png`",
  
  "- `forecasting_daily_mae_comparison.png`",
  
  "- `forecasting_daily_improvement_distribution.png`",
  
  "",
  
  "**End of Step 09 Forecasting Report.**"
)

# ------------------------------------------------------------
# 9. SAVE MARKDOWN REPORT
# ------------------------------------------------------------

report_file <- "reports/09_forecasting_report.md"

writeLines(
  report_lines,
  con = report_file,
  useBytes = TRUE
)

cat("\nReport saved:", report_file, "\n")

# ------------------------------------------------------------
# 10. SAVE KEY FINDINGS CSV
# ------------------------------------------------------------

key_findings <- data.frame(
  Finding = c(
    "Recommended Model",
    "Best Model by MAE",
    "Best Model by RMSE",
    "Best Median Error Model (Detailed Diagnostics)",
    "Lag Model MAE",
    "Naive Model MAE",
    "Lag Model RMSE",
    "Naive Model RMSE",
    "MAE Improvement Percentage",
    "RMSE Improvement Percentage",
    "Evaluated Test Observations",
    "Evaluated Days",
    "Lag Better Days",
    "Naive Better Days",
    "Mean Daily MAE Improvement",
    "7-Day Bootstrap Lower CI",
    "7-Day Bootstrap Upper CI",
    "Bootstrap Conclusion"
  ),
  
  Result = c(
    "Lag-Based Regression",
    best_mae_model,
    best_rmse_model,
    best_median_model,
    format_number(lag_mae),
    format_number(naive_mae),
    format_number(lag_rmse),
    format_number(naive_rmse),
    format_number(mae_improvement_percent, 2),
    format_number(rmse_improvement_percent, 2),
    as.character(final_comparison$Observations[1]),
    as.character(number_of_days),
    as.character(lag_better_days),
    as.character(naive_better_days),
    format_number(mean_daily_improvement),
    format_number(bootstrap_7day$CI_95_Lower),
    format_number(bootstrap_7day$CI_95_Upper),
    bootstrap_interpretation
  ),
  
  stringsAsFactors = FALSE
)

write.csv(
  key_findings,
  "results/forecasting_report_key_findings.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 11. FINAL VERIFICATION
# ------------------------------------------------------------

expected_outputs <- c(
  "reports/09_forecasting_report.md",
  "results/forecasting_report_key_findings.csv"
)

verification <- data.frame(
  File = expected_outputs,
  Exists = file.exists(expected_outputs)
)

cat("\nOUTPUT VERIFICATION\n")
print(verification)

cat(
  "\nFiles generated:",
  sum(verification$Exists),
  "out of",
  nrow(verification),
  "\n"
)

if (!all(verification$Exists)) {
  stop("Step 09J output verification failed.")
}

cat("\n============================================\n")
cat("STEP 09J COMPLETED SUCCESSFULLY\n")
cat("============================================\n")

cat("\nReport:", report_file, "\n")
cat("Recommended model:", best_mae_model, "\n")
cat(
  "MAE improvement:",
  format_number(mae_improvement_percent, 2),
  "%\n"
)