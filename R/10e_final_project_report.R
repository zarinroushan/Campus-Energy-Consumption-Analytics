
# ============================================================
# CAMPUS ENERGY CONSUMPTION ANALYTICS
# STEP 10E: FINAL PROJECT REPORT PREPARATION
# ============================================================

cat("\n============================================\n")
cat("STEP 10E: FINAL PROJECT REPORT PREPARATION\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES
# ------------------------------------------------------------

required_packages <- c("dplyr", "readr", "knitr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(dplyr)

# ------------------------------------------------------------
# 2. CREATE REPORT DIRECTORY
# ------------------------------------------------------------

if (!dir.exists("reports")) {
  dir.create("reports", recursive = TRUE)
}

# ------------------------------------------------------------
# 3. CHECK REQUIRED INPUT FILES
# ------------------------------------------------------------

required_files <- c(
  "results/final_forecasting_model_comparison.csv",
  "results/forecasting_error_investigation.csv",
  "results/forecasting_moving_block_bootstrap.csv",
  "results/forecasting_daily_model_comparison.csv",
  "results/forecasting_final_model_selection.csv",
  "results/forecasting_final_model_selection_summary.txt",
  "results/daily_energy_trend_refined.csv",
  "results/monthly_energy_trend_refined.csv",
  "results/forecast_predictions_refined.csv"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  cat("MISSING REQUIRED FILES:\n")
  print(missing_files)
  stop("Please generate the missing files before creating the report.")
}

cat("All required input files are available.\n")

# ------------------------------------------------------------
# 4. LOAD ANALYSIS RESULTS
# ------------------------------------------------------------

model_comparison <- read.csv(
  "results/final_forecasting_model_comparison.csv"
)

error_investigation <- read.csv(
  "results/forecasting_error_investigation.csv"
)

bootstrap_results <- read.csv(
  "results/forecasting_moving_block_bootstrap.csv"
)

daily_comparison <- read.csv(
  "results/forecasting_daily_model_comparison.csv"
)

final_selection <- read.csv(
  "results/forecasting_final_model_selection.csv"
)

daily_energy <- read.csv(
  "results/daily_energy_trend_refined.csv"
)

monthly_energy <- read.csv(
  "results/monthly_energy_trend_refined.csv"
)

forecast_predictions <- read.csv(
  "results/forecast_predictions_refined.csv"
)

selection_summary <- readLines(
  "results/forecasting_final_model_selection_summary.txt",
  warn = FALSE
)

# ------------------------------------------------------------
# 5. VALIDATE FORECASTING RESULTS
# ------------------------------------------------------------

required_model_columns <- c(
  "Model", "Observations", "MAE", "RMSE", "Bias"
)

if (!all(required_model_columns %in% names(model_comparison))) {
  stop("Required forecasting model columns are missing.")
}

if (nrow(model_comparison) != 5) {
  stop("Expected five forecasting models in the comparison.")
}

best_model <- model_comparison %>%
  arrange(MAE) %>%
  slice(1)

naive_model <- model_comparison %>%
  filter(Model == "Naive Previous Hour")

lag_model <- model_comparison %>%
  filter(Model == "Lag-Based Regression")

if (nrow(naive_model) != 1 || nrow(lag_model) != 1) {
  stop("Could not identify both forecasting comparison models.")
}

mae_improvement <- (
  naive_model$MAE - lag_model$MAE
) / naive_model$MAE * 100

rmse_improvement <- (
  naive_model$RMSE - lag_model$RMSE
) / naive_model$RMSE * 100

# ------------------------------------------------------------
# 6. EXTRACT DAILY AND MONTHLY COVERAGE
# ------------------------------------------------------------

daily_rows <- nrow(daily_energy)
monthly_rows <- nrow(monthly_energy)
forecast_rows <- nrow(forecast_predictions)

cat("\nDATASET SUMMARY\n")
cat("Daily trend rows:", daily_rows, "\n")
cat("Monthly trend rows:", monthly_rows, "\n")
cat("Forecast prediction rows:", forecast_rows, "\n")

# ------------------------------------------------------------
# 7. BUILD REPORT CONTENT
# ------------------------------------------------------------

report <- c(
  
  "# CAMPUS ENERGY CONSUMPTION ANALYTICS",
  
  "",
  
  "## 1. Executive Summary",
  
  "This project presents an exploratory and forecasting-based analysis of historical campus energy consumption using the public I-BLEND dataset from IIIT-Delhi.",
  
  "The analysis examines energy consumption patterns, building-level differences, temporal trends, data availability and the performance of several forecasting approaches.",
  
  "The forecasting experiments identify Lag-Based Regression as the best-performing model by Mean Absolute Error among the five evaluated models. However, its improvement over the previous-hour naive baseline is modest.",
  
  "",
  
  "## 2. Project Objectives",
  
  "- Examine historical campus energy consumption patterns.",
  "- Assess the availability and quality of energy meter observations.",
  "- Compare energy consumption across monitored buildings.",
  "- Investigate hourly, daily, monthly and weekday patterns.",
  "- Develop baseline and regression-based forecasting models.",
  "- Evaluate model performance using MAE, RMSE and bias.",
  "- Investigate forecasting errors and assess robustness.",
  
  "",
  
  "## 3. Dataset Description",
  
  "The project uses the public I-BLEND dataset associated with IIIT-Delhi.",
  
  "The dataset contains historical energy meter readings and supplementary information. The processed hourly energy dataset covers 10 August 2013 to 31 December 2017.",
  
  "Important dataset considerations:",
  
  "- The dataset does not represent measurements from Heritage Institute of Technology.",
  "- Historical energy observations contain missing and irregular coverage.",
  "- Occupancy readings are Wi-Fi-based estimates rather than exact headcounts.",
  "- Building-level comparisons depend on available meter coverage.",
  "- Supplementary weather observations from 2018 were not treated as contemporaneous with the 2013–2017 energy observations.",
  
  "",
  
  "## 4. Data Preparation and Cleaning",
  
  "The raw energy readings were cleaned and converted into hourly observations.",
  
  "Key preparation activities included:",
  
  "- Date-time standardization using the Asia/Kolkata time zone.",
  "- Conversion of power readings into hourly energy estimates.",
  "- Aggregation of minute-level readings into hourly observations.",
  "- Calculation of meter coverage percentages.",
  "- Creation of calendar and temporal features.",
  "- Integration of available occupancy information.",
  "- Preservation of missing observations rather than automatically replacing them with zero.",
  
  "The hourly processed dataset contains 38,520 observations.",
  
  "",
  
  "## 5. Exploratory Data Analysis",
  
  "The exploratory analysis investigated overall energy trends, building-level consumption and temporal patterns.",
  
  "### 5.1 Energy Data Coverage",
  
  "Meter coverage varied across buildings and time periods. Consequently, campus-level energy estimates were calculated using explicit coverage criteria rather than assuming every building had complete observations.",
  
  "### 5.2 Building-Level Analysis",
  
  "Average hourly energy consumption and distributions were compared across monitored buildings.",
  
  "### 5.3 Temporal Analysis",
  
  "Hourly, daily, monthly and weekday patterns were examined to understand changes in energy consumption over time.",
  
  "Daily averages were calculated using a minimum of 18 qualifying hourly observations per day. Gaps were preserved rather than connected as continuous observations.",
  
  "",
  
  "## 6. Forecasting Methodology",
  
  "The forecasting dataset was constructed from the longest continuous period of eligible campus energy observations.",
  
  "Selected continuous period: 9,370 hourly observations.",
  
  "Training observations: 7,496.",
  
  "Testing observations: 1,874.",
  
  "Training and testing were separated chronologically to avoid using future observations to train the models.",
  
  "The training period ended on 18 April 2017 at 16:00, and the testing period began on 18 April 2017 at 17:00.",
  
  "All evaluated models used rolling one-step-ahead predictions. Actual previous observations from the test period were available when generating subsequent predictions.",
  
  "",
  
  "## 7. Forecasting Models",
  
  "The following five models were evaluated:",
  
  "1. Naive Previous Hour",
  "2. Seasonal Naive (24 Hours)",
  "3. Linear Trend Regression",
  "4. Calendar Regression",
  "5. Lag-Based Regression",
  
  "",
  
  "## 8. Forecasting Model Comparison",
  
  "```",
  
  paste(
    capture.output(
      print(
        model_comparison %>%
          select(Model, Observations, MAE, RMSE, Bias),
        row.names = FALSE
      )
    ),
    collapse = "\n"
  ),
  
  "```",
  
  paste0(
    "The best-performing model by MAE was ",
    best_model$Model,
    "."
  ),
  
  paste0(
    "Its MAE was ",
    round(best_model$MAE, 3),
    " kWh and its RMSE was ",
    round(best_model$RMSE, 3),
    " kWh."
  ),
  
  "",
  
  "## 9. Comparison with the Naive Baseline",
  
  paste0(
    "Lag-Based Regression achieved an MAE of ",
    round(lag_model$MAE, 3),
    " kWh, compared with ",
    round(naive_model$MAE, 3),
    " kWh for Naive Previous Hour."
  ),
  
  paste0(
    "This represents an MAE improvement of approximately ",
    round(mae_improvement, 2),
    "%."
  ),
  
  paste0(
    "The RMSE improvement was approximately ",
    round(rmse_improvement, 2),
    "%."
  ),
  
  "The improvement is measurable but modest. The naive baseline remains a strong comparison model.",
  
  "",
  
  "## 10. Forecasting Error Investigation",
  
  "The forecasting errors were investigated across time, hours, weekdays and individual observations.",
  
  "Important findings:",
  
  "- Lag-Based Regression achieved lower average absolute error.",
  "- Naive Previous Hour achieved a lower median absolute error.",
  "- Lag-Based Regression had lower 95th-percentile absolute error.",
  "- Both models produced occasional substantial forecasting errors.",
  "- Performance varied across hours and weekdays.",
  
  "The results demonstrate that a model performing better on average does not necessarily perform better for every individual observation.",
  
  "",
  
  "## 11. Statistical and Robustness Validation",
  
  "Daily model performance was compared using paired daily MAE differences.",
  
  "Moving-block bootstrap analysis was conducted using block lengths of 3, 7 and 14 days.",
  
  "The block bootstrap was used to examine uncertainty while retaining some short-term temporal dependence.",
  
  "The results should be interpreted as exploratory evidence rather than definitive proof of model superiority. Consecutive daily observations can remain serially correlated, and the analysis does not eliminate every source of dependence.",
  
  "",
  
  "## 12. Limitations",
  
  "- The dataset represents a historical public case study rather than the user's own college.",
  "- Missing meter observations limit the completeness of campus-level estimates.",
  "- Occupancy estimates are not exact headcounts.",
  "- The selected forecasting period is only one continuous historical segment.",
  "- The models use rolling one-step-ahead evaluation rather than fixed-origin long-horizon forecasting.",
  "- Weather information was not used as a contemporaneous explanatory variable for the historical energy period.",
  "- The forecasting improvement over the naive baseline is relatively small.",
  "- The findings should not be interpreted as evidence of current campus energy consumption or guaranteed future savings.",
  
  "",
  
  "## 13. Conclusion",
  
  "This project demonstrates a structured workflow for cleaning, exploring and forecasting historical campus energy consumption data.",
  
  "The exploratory analysis highlights variation in consumption across buildings and time periods, while the forecasting experiments demonstrate the importance of comparing more complex models against simple baselines.",
  
  "Lag-Based Regression achieved the lowest MAE among the evaluated models, but its improvement over the previous-hour baseline was limited. Further work could investigate additional explanatory variables, alternative time-series models and evaluation across multiple continuous historical periods.",
  
  "",
  
  "## 14. Future Scope",
  
  "- Investigate additional forecasting algorithms.",
  "- Explore weather variables when temporally aligned data is available.",
  "- Evaluate performance across multiple continuous historical periods.",
  "- Investigate building-specific forecasting models.",
  "- Explore longer-horizon forecasting.",
  "- Develop a dashboard for interactive energy analysis.",
  "- Adapt the methodology to a campus with its own verified energy meter data.",
  
  "",
  
  "## 15. Selected Project Outputs",
  
  "### Refined visualizations",
  
  "- detailed_daily_energy_trend_refined.png",
  "- detailed_monthly_energy_refined.png",
  "- actual_vs_predicted_refined.png",
  "- forecast_residuals_over_time_refined.png",
  
  "### Forecasting results",
  
  "- final_forecasting_model_comparison.csv",
  "- forecasting_error_investigation.csv",
  "- forecasting_moving_block_bootstrap.csv",
  "- forecasting_final_model_selection.csv",
  
  "",
  
  "## 16. Reproducibility",
  
  "All analysis stages are implemented in R scripts and their outputs are saved in the project directories. The report is generated from the existing processed datasets and results files.",
  
  ""
)

# ------------------------------------------------------------
# 8. SAVE REPORT
# ------------------------------------------------------------

report_file <- "reports/10e_final_project_report.md"

writeLines(
  report,
  report_file,
  useBytes = TRUE
)

# ------------------------------------------------------------
# 9. SAVE KEY FINDINGS
# ------------------------------------------------------------

key_findings <- data.frame(
  Metric = c(
    "Processed hourly observations",
    "Selected forecasting observations",
    "Training observations",
    "Testing observations",
    "Best model",
    "Best model MAE",
    "Naive baseline MAE",
    "MAE improvement percentage",
    "RMSE improvement percentage",
    "Daily trend records",
    "Monthly trend records",
    "Forecast prediction records"
  ),
  
  Value = c(
    38520,
    9370,
    7496,
    1874,
    best_model$Model,
    round(best_model$MAE, 4),
    round(naive_model$MAE, 4),
    round(mae_improvement, 2),
    round(rmse_improvement, 2),
    daily_rows,
    monthly_rows,
    forecast_rows
  ),
  
  stringsAsFactors = FALSE
)

write.csv(
  key_findings,
  "results/10e_key_findings.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 10. OUTPUT VERIFICATION
# ------------------------------------------------------------

expected_outputs <- c(
  "reports/10e_final_project_report.md",
  "results/10e_key_findings.csv"
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
  length(expected_outputs),
  "\n"
)

if (!all(verification$Exists)) {
  stop("Some Step 10E outputs were not generated.")
}

cat("\n============================================\n")
cat("STEP 10E COMPLETED SUCCESSFULLY\n")
cat("============================================\n")