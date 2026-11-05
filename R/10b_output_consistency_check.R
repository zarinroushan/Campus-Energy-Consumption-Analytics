
# ============================================================
# STEP 10B: OUTPUT CONSISTENCY CHECK
# Project: Campus Energy Consumption Analytics
# ============================================================

cat("\n============================================\n")
cat("STEP 10B: OUTPUT CONSISTENCY CHECK\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 1. CHECK CORE PROCESSED DATA FILES
# ------------------------------------------------------------

processed_files <- c(
  "data/processed/energy_hourly_with_features.rds",
  "data/processed/forecasting_selected_period.rds",
  "data/processed/forecasting_train.rds",
  "data/processed/forecasting_test.rds"
)

processed_check <- data.frame(
  File = processed_files,
  Exists = file.exists(processed_files)
)

cat("PROCESSED DATA FILE CHECK\n")
print(processed_check)

# ------------------------------------------------------------
# 2. LOAD FORECASTING DATASETS
# ------------------------------------------------------------

if (all(file.exists(processed_files[-1]))) {
  
  selected_data <- readRDS(
    "data/processed/forecasting_selected_period.rds"
  )
  
  train_data <- readRDS(
    "data/processed/forecasting_train.rds"
  )
  
  test_data <- readRDS(
    "data/processed/forecasting_test.rds"
  )
  
  cat("\nFORECASTING DATASET DIMENSIONS\n")
  
  cat(
    "Selected period:",
    nrow(selected_data), "rows\n"
  )
  
  cat(
    "Training data:",
    nrow(train_data), "rows\n"
  )
  
  cat(
    "Testing data:",
    nrow(test_data), "rows\n"
  )
  
  # Verify row counts
  row_count_check <- data.frame(
    Dataset = c("Selected", "Training", "Testing"),
    Expected_Rows = c(9370, 7496, 1874),
    Actual_Rows = c(
      nrow(selected_data),
      nrow(train_data),
      nrow(test_data)
    )
  )
  
  row_count_check$Passed <-
    row_count_check$Expected_Rows ==
    row_count_check$Actual_Rows
  
  cat("\nROW COUNT VALIDATION\n")
  print(row_count_check)
  
  # Check chronological split
  if ("datetime" %in% names(train_data) &&
      "datetime" %in% names(test_data)) {
    
    train_end <- max(
      as.POSIXct(train_data$datetime),
      na.rm = TRUE
    )
    
    test_start <- min(
      as.POSIXct(test_data$datetime),
      na.rm = TRUE
    )
    
    cat("\nCHRONOLOGICAL SPLIT CHECK\n")
    cat("Training ends:", as.character(train_end), "\n")
    cat("Testing starts:", as.character(test_start), "\n")
    
    cat(
      "Training precedes testing:",
      train_end < test_start,
      "\n"
    )
    
  } else {
    cat(
      "\nDatetime column not named 'datetime'.",
      "Inspect dataset column names manually.\n"
    )
    
    cat("\nTraining columns:\n")
    print(names(train_data))
    
    cat("\nTesting columns:\n")
    print(names(test_data))
  }
  
} else {
  
  cat(
    "\nOne or more forecasting RDS files are missing.",
    "Dataset validation skipped.\n"
  )
}

# ------------------------------------------------------------
# 3. CHECK FORECASTING MODEL METRICS
# ------------------------------------------------------------

comparison_file <-
  "results/forecasting_final_model_comparison.csv"

if (file.exists(comparison_file)) {
  
  model_results <- read.csv(comparison_file)
  
  cat("\nFINAL MODEL METRICS\n")
  print(model_results)
  
  metric_check <- data.frame(
    Metric = c("MAE", "RMSE"),
    Lag_Model = c(
      model_results$MAE[
        model_results$Model == "Lag-Based Regression"
      ],
      model_results$RMSE[
        model_results$Model == "Lag-Based Regression"
      ]
    ),
    Naive_Model = c(
      model_results$MAE[
        model_results$Model == "Naive Previous Hour"
      ],
      model_results$RMSE[
        model_results$Model == "Naive Previous Hour"
      ]
    )
  )
  
  metric_check$Lag_Model_Better <-
    metric_check$Lag_Model < metric_check$Naive_Model
  
  cat("\nLAG MODEL VS NAIVE BASELINE\n")
  print(metric_check)
  
} else {
  cat("\nFinal model comparison file missing.\n")
}

# ------------------------------------------------------------
# 4. CHECK FORECASTING REPORT CONTENT
# ------------------------------------------------------------

report_file <- "reports/09_forecasting_report.md"

if (file.exists(report_file)) {
  
  report_content <- readLines(
    report_file,
    warn = FALSE
  )
  
  report_text <- paste(
    report_content,
    collapse = "\n"
  )
  
  report_terms <- c(
    "Lag-Based Regression",
    "Naive Previous Hour",
    "moving-block bootstrap",
    "Limitations",
    "rolling one-step-ahead"
  )
  
  report_check <- data.frame(
    Required_Term = report_terms,
    Present = vapply(
      report_terms,
      function(term) {
        grepl(
          tolower(term),
          tolower(report_text),
          fixed = TRUE
        )
      },
      logical(1)
    )
  )
  
  cat("\nFORECASTING REPORT CONTENT CHECK\n")
  print(report_check)
  
  if (!all(report_check$Present)) {
    warning("One or more required terms are missing from the report.")
  }
  
} else {
  cat("\nForecasting report missing.\n")
}

# ------------------------------------------------------------
# 5. CHECK UNUSUAL CSV FILENAMES
# ------------------------------------------------------------

result_files <- list.files(
  "results",
  full.names = FALSE
)

unusual_names <- result_files[
  grepl("\\\\", result_files)
]

cat("\nUNUSUAL RESULT FILENAMES\n")

if (length(unusual_names) == 0) {
  cat("No unusual backslash filenames detected.\n")
} else {
  print(unusual_names)
}

# ------------------------------------------------------------
# 6. SAVE AUDIT OUTPUTS
# ------------------------------------------------------------

write.csv(
  processed_check,
  "results/processed_data_consistency_check.csv",
  row.names = FALSE
)

if (exists("row_count_check")) {
  write.csv(
    row_count_check,
    "results/forecasting_row_count_check.csv",
    row.names = FALSE
  )
}

if (exists("metric_check")) {
  write.csv(
    metric_check,
    "results/forecasting_metric_consistency_check.csv",
    row.names = FALSE
  )
}

if (exists("report_check")) {
  write.csv(
    report_check,
    "results/forecasting_report_content_check.csv",
    row.names = FALSE
  )
}

cat("\n============================================\n")
cat("STEP 10B CHECK COMPLETED\n")
cat("============================================\n")