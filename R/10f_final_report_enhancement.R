
# ============================================================
# STEP 10F: FINAL PROJECT REPORT ENHANCEMENT
# ============================================================

cat("\n============================================\n")
cat("STEP 10F: FINAL REPORT ENHANCEMENT\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 1. CHECK PROJECT DIRECTORIES
# ------------------------------------------------------------

if (!dir.exists("reports")) {
  dir.create("reports", recursive = TRUE)
}

if (!file.exists("reports/10e_final_project_report.md")) {
  stop("Step 10E report not found. Run Step 10E first.")
}

report <- readLines(
  "reports/10e_final_project_report.md",
  warn = FALSE,
  encoding = "UTF-8"
)

cat("Original report loaded successfully.\n")

# ------------------------------------------------------------
# 2. ADD TABLE OF CONTENTS
# ------------------------------------------------------------

toc <- c(
  "",
  "## Table of Contents",
  "",
  "1. Executive Summary",
  "2. Project Objectives",
  "3. Dataset Description",
  "4. Data Preparation and Cleaning",
  "5. Exploratory Data Analysis",
  "6. Forecasting Methodology",
  "7. Forecasting Models",
  "8. Forecasting Model Comparison",
  "9. Comparison with the Naive Baseline",
  "10. Forecasting Error Investigation",
  "11. Statistical and Robustness Validation",
  "12. Limitations",
  "13. Conclusion",
  "14. Future Scope",
  "15. Selected Project Outputs",
  "16. Reproducibility",
  "17. Visualization Appendix",
  "18. References",
  ""
)

# Insert the contents after the main title.
title_position <- grep(
  "^# CAMPUS ENERGY CONSUMPTION ANALYTICS$",
  report
)

if (length(title_position) == 0) {
  stop("Main report title could not be located.")
}

report <- append(
  report,
  toc,
  after = title_position[1]
)

# ------------------------------------------------------------
# 3. PREPARE VISUALIZATION APPENDIX
# ------------------------------------------------------------

cat("\nChecking available visualization files...\n")

figure_list <- list(
  
  list(
    file = "detailed_daily_energy_trend_refined.png",
    title = "Figure 1: Daily Average Hourly Campus Energy",
    description = paste(
      "This visualization shows the daily average hourly",
      "campus energy estimate over the historical dataset.",
      "Only days with at least 18 qualifying hourly",
      "observations are included. Missing periods are",
      "not treated as zero consumption."
    )
  ),
  
  list(
    file = "detailed_monthly_energy_refined.png",
    title = "Figure 2: Monthly Average Hourly Energy",
    description = paste(
      "This figure compares monthly average hourly energy",
      "consumption across the available years. Incomplete",
      "months should be interpreted cautiously."
    )
  ),
  
  list(
    file = "actual_vs_predicted_refined.png",
    title = "Figure 3: Actual vs Predicted Energy Consumption",
    description = paste(
      "This plot compares actual hourly energy values",
      "against predictions from the Lag-Based Regression",
      "model during the chronological testing period."
    )
  ),
  
  list(
    file = "forecast_residuals_over_time_refined.png",
    title = "Figure 4: Forecast Residuals Over Time",
    description = paste(
      "Residuals represent actual energy minus predicted",
      "energy. Values above zero indicate underprediction;",
      "values below zero indicate overprediction."
    )
  ),
  
  list(
    file = "actual_vs_predicted.png",
    title = "Figure 5: Forecast Evaluation",
    description = paste(
      "This figure provides an additional view of actual",
      "and predicted energy consumption during testing."
    )
  ),
  
  list(
    file = "forecast_residuals_over_time.png",
    title = "Figure 6: Forecast Residual Diagnostics",
    description = paste(
      "This visualization examines how forecasting",
      "residuals vary throughout the testing period."
    )
  ),
  
  list(
    file = "forecast_error_by_hour.png",
    title = "Figure 7: Forecast Error by Hour",
    description = paste(
      "This plot examines differences in forecasting",
      "error across hours of the day."
    )
  ),
  
  list(
    file = "forecast_error_by_weekday.png",
    title = "Figure 8: Forecast Error by Weekday",
    description = paste(
      "This plot compares forecasting errors across",
      "days of the week."
    )
  ),
  
  list(
    file = "forecasting_weekly_robustness.png",
    title = "Figure 9: Weekly Forecasting Robustness",
    description = paste(
      "This visualization compares model performance",
      "across weekly portions of the testing period."
    )
  ),
  
  list(
    file = "forecasting_hourly_robustness.png",
    title = "Figure 10: Hourly Forecasting Robustness",
    description = paste(
      "This figure compares forecasting performance",
      "across different hours."
    )
  ),
  
  list(
    file = "forecasting_weekday_robustness.png",
    title = "Figure 11: Weekday Forecasting Robustness",
    description = paste(
      "This figure examines how model performance",
      "varies across weekdays."
    )
  ),
  
  list(
    file = "forecasting_paired_error_improvement.png",
    title = "Figure 12: Paired Forecasting Error Improvement",
    description = paste(
      "This plot examines the difference in absolute",
      "forecasting errors between the Lag-Based Regression",
      "model and the previous-hour naive baseline."
    )
  ),
  
  list(
    file = "forecasting_daily_mae_comparison.png",
    title = "Figure 13: Daily MAE Comparison",
    description = paste(
      "This visualization compares daily mean absolute",
      "errors for the two principal forecasting models."
    )
  ),
  
  list(
    file = "forecasting_daily_improvement_distribution.png",
    title = "Figure 14: Distribution of Daily MAE Improvement",
    description = paste(
      "This figure displays the distribution of daily",
      "MAE improvement between the two models."
    )
  ),
  
  list(
    file = "forecasting_moving_block_bootstrap.png",
    title = "Figure 15: Moving-Block Bootstrap",
    description = paste(
      "This figure presents bootstrap estimates of daily",
      "MAE improvement for different block lengths.",
      "Intervals containing zero indicate uncertainty",
      "about the direction of the improvement."
    )
  )
  
)

# ------------------------------------------------------------
# 4. INCLUDE ONLY EXISTING FIGURES
# ------------------------------------------------------------

appendix <- c(
  "",
  "---",
  "",
  "# 17. Visualization Appendix",
  "",
  paste(
    "The following figures provide visual evidence",
    "supporting the exploratory analysis and forecasting",
    "results discussed in this report."
  ),
  ""
)

available_count <- 0
missing_figures <- character(0)

for (figure in figure_list) {
  
  figure_path <- file.path(
    "plots",
    figure$file
  )
  
  if (file.exists(figure_path)) {
    
    available_count <- available_count + 1
    
    appendix <- c(
      appendix,
      paste0("### ", figure$title),
      "",
      paste0(
        "![",
        figure$title,
        "](../plots/",
        figure$file,
        ")"
      ),
      "",
      figure$description,
      "",
      paste0(
        "**Source file:** `plots/",
        figure$file,
        "`"
      ),
      "",
      ""
    )
    
    cat("Included: ", figure$file, "\n")
    
  } else {
    
    missing_figures <- c(
      missing_figures,
      figure$file
    )
    
    cat("Not available: ", figure$file, "\n")
  }
}

appendix <- c(
  appendix,
  paste(
    "**Total figures included:**",
    available_count
  ),
  ""
)

# ------------------------------------------------------------
# 5. ADD REFERENCES SECTION
# ------------------------------------------------------------

references <- c(
  "",
  "---",
  "",
  "# 18. References",
  "",
  "1. IIIT-Delhi. I-BLEND public dataset.",
  "   Dataset used for the historical campus energy",
  "   consumption analysis. Add the exact dataset",
  "   landing-page URL and access date from the",
  "   original download source before submission.",
  "",
  "2. R Core Team. R: A Language and Environment",
  "   for Statistical Computing.",
  "   https://www.r-project.org/",
  "",
  "3. Tidyverse. Tools for Data Science in R.",
  "   https://www.tidyverse.org/",
  "",
  "**Citation note:** Verify the dataset's original",
  "publication details and cite the actual dataset",
  "source used in this project. Do not submit an",
  "unverified DOI or publication reference.",
  ""
)

# ------------------------------------------------------------
# 6. CREATE ENHANCED REPORT
# ------------------------------------------------------------

enhanced_report <- c(
  report,
  appendix,
  references
)

output_file <- file.path(
  "reports",
  "10f_final_project_report_enhanced.md"
)

writeLines(
  enhanced_report,
  output_file,
  useBytes = TRUE
)

# ------------------------------------------------------------
# 7. VERIFY OUTPUT
# ------------------------------------------------------------

cat("\n============================================\n")
cat("STEP 10F OUTPUT VERIFICATION\n")
cat("============================================\n")

verification <- data.frame(
  File = c(
    "reports/10e_final_project_report.md",
    "reports/10f_final_project_report_enhanced.md"
  ),
  Exists = file.exists(
    c(
      "reports/10e_final_project_report.md",
      "reports/10f_final_project_report_enhanced.md"
    )
  )
)

print(verification)

cat("\nFigures included:", available_count, "\n")
cat("Figures unavailable:", length(missing_figures), "\n")

if (length(missing_figures) > 0) {
  cat("\nMissing figure files:\n")
  print(missing_figures)
}

cat("\nEnhanced report saved to:\n")
cat(output_file, "\n")

cat("\n============================================\n")
cat("STEP 10F COMPLETED\n")
cat("============================================\n")