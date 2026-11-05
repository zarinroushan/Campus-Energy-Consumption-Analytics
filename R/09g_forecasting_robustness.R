
# ============================================================
# STEP 09G: FORECASTING MODEL ROBUSTNESS TESTING
# ============================================================

source("R/plot_utils.R")

if (!exists("save_and_show_plot")) {
  stop("save_and_show_plot() could not be loaded.")
}

cat("\nLoading forecasting predictions...\n")

# ------------------------------------------------------------
# 1. LOAD EXISTING PREDICTIONS
# ------------------------------------------------------------

baseline <- read.csv(
  "results/forecasting_baseline_predictions.csv",
  stringsAsFactors = FALSE
)

advanced <- read.csv(
  "results/advanced_forecasting_predictions.csv",
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 2. VALIDATE INPUT DATA
# ------------------------------------------------------------

if (nrow(baseline) == 0 || nrow(advanced) == 0) {
  stop("Prediction datasets are empty.")
}

required_baseline <- c(
  "datetime",
  "actual_kWh",
  "naive_previous_hour"
)

required_advanced <- c(
  "datetime",
  "actual_kWh",
  "lag_regression"
)

if (!all(required_baseline %in% names(baseline)) ||
    !all(required_advanced %in% names(advanced))) {
  stop("Required prediction columns are missing.")
}

if (anyDuplicated(baseline$datetime) ||
    anyDuplicated(advanced$datetime)) {
  stop("Duplicate timestamps detected.")
}

if (!identical(
  baseline$datetime,
  advanced$datetime
)) {
  stop("Baseline and advanced timestamps do not match.")
}

if (!isTRUE(all.equal(
  baseline$actual_kWh,
  advanced$actual_kWh
))) {
  stop("Actual energy observations do not match.")
}

# ------------------------------------------------------------
# 3. COMBINE THE TWO MODELS
# ------------------------------------------------------------

# Parse timestamps containing both date-time and date-only formats

parsed_datetime <- as.POSIXct(
  baseline$datetime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "Asia/Kolkata"
)

# Identify date-only timestamps (midnight observations)
date_only_rows <- grepl(
  "^\\d{4}-\\d{2}-\\d{2}$",
  baseline$datetime
)

# Parse date-only timestamps separately
parsed_datetime[date_only_rows] <- as.POSIXct(
  baseline$datetime[date_only_rows],
  format = "%Y-%m-%d",
  tz = "Asia/Kolkata"
)

# Validate datetime conversion
if (anyNA(parsed_datetime)) {
  stop("Datetime conversion failed. Check the original datetime values.")
}

evaluation_data <- data.frame(
  datetime = parsed_datetime,
  actual_kWh = baseline$actual_kWh,
  naive_prediction = baseline$naive_previous_hour,
  lag_prediction = advanced$lag_regression
)

if (anyNA(evaluation_data$datetime)) {
  stop("Datetime conversion failed.")
}

if (any(!is.finite(evaluation_data$actual_kWh)) ||
    any(!is.finite(evaluation_data$naive_prediction)) ||
    any(!is.finite(evaluation_data$lag_prediction))) {
  stop("Non-finite values found in prediction data.")
}

# Verify that every observation is exactly one hour apart
time_differences <- diff(
  as.numeric(evaluation_data$datetime)
)

if (any(abs(time_differences - 3600) > 1)) {
  stop("Evaluation timestamps are not continuous hourly.")
}

cat("Hourly timestamp continuity check passed.\n")

# ------------------------------------------------------------
# 4. CALCULATE ERRORS
# ------------------------------------------------------------

evaluation_data$naive_residual <-
  evaluation_data$actual_kWh -
  evaluation_data$naive_prediction

evaluation_data$lag_residual <-
  evaluation_data$actual_kWh -
  evaluation_data$lag_prediction

evaluation_data$naive_absolute_error <-
  abs(evaluation_data$naive_residual)

evaluation_data$lag_absolute_error <-
  abs(evaluation_data$lag_residual)

evaluation_data$lag_squared_error <-
  evaluation_data$lag_residual^2

evaluation_data$naive_squared_error <-
  evaluation_data$naive_residual^2

# Positive improvement means lag regression is better.
evaluation_data$absolute_error_improvement <-
  evaluation_data$naive_absolute_error -
  evaluation_data$lag_absolute_error

# ------------------------------------------------------------
# 5. OVERALL ROBUSTNESS COMPARISON
# ------------------------------------------------------------

calculate_metrics <- function(actual, predicted, model_name) {
  
  errors <- actual - predicted
  
  data.frame(
    Model = model_name,
    Observations = length(errors),
    MAE = mean(abs(errors)),
    RMSE = sqrt(mean(errors^2)),
    Bias = mean(predicted - actual)
  )
}

robustness_metrics <- rbind(
  
  calculate_metrics(
    evaluation_data$actual_kWh,
    evaluation_data$naive_prediction,
    "Naive Previous Hour"
  ),
  
  calculate_metrics(
    evaluation_data$actual_kWh,
    evaluation_data$lag_prediction,
    "Lag-Based Regression"
  )
)

cat("\nOVERALL ROBUSTNESS COMPARISON\n")
print(robustness_metrics)

mae_improvement <- (
  robustness_metrics$MAE[1] -
    robustness_metrics$MAE[2]
)

mae_improvement_percent <- (
  mae_improvement /
    robustness_metrics$MAE[1]
) * 100

cat(
  "\nLag model MAE improvement:",
  round(mae_improvement, 4), "kWh\n"
)

cat(
  "Percentage MAE improvement:",
  round(mae_improvement_percent, 2), "%\n"
)

# ------------------------------------------------------------
# 6. PAIRED PREDICTION COMPARISON
# ------------------------------------------------------------

lag_better <- sum(
  evaluation_data$absolute_error_improvement > 0
)

naive_better <- sum(
  evaluation_data$absolute_error_improvement < 0
)

equal_predictions <- sum(
  evaluation_data$absolute_error_improvement == 0
)

paired_comparison <- data.frame(
  Comparison = c(
    "Lag-Based Regression Better",
    "Naive Previous Hour Better",
    "Equal Absolute Error"
  ),
  
  Observations = c(
    lag_better,
    naive_better,
    equal_predictions
  ),
  
  Percentage = c(
    lag_better,
    naive_better,
    equal_predictions
  ) / nrow(evaluation_data) * 100
)

cat("\nPAIRED PREDICTION COMPARISON\n")
print(paired_comparison)

# ------------------------------------------------------------
# 7. CREATE TIME FEATURES
# ------------------------------------------------------------

datetime_lt <- as.POSIXlt(
  evaluation_data$datetime,
  tz = "Asia/Kolkata"
)

evaluation_data$hour <- datetime_lt$hour

evaluation_data$day_of_week <- factor(
  datetime_lt$wday,
  levels = 0:6,
  labels = c(
    "Sunday", "Monday", "Tuesday",
    "Wednesday", "Thursday", "Friday", "Saturday"
  )
)

evaluation_data$month <- format(
  evaluation_data$datetime,
  "%Y-%m"
)

# Monday-based weekly grouping
evaluation_data$week_start <-
  as.Date(evaluation_data$datetime) -
  ((datetime_lt$wday + 6) %% 7)

# ------------------------------------------------------------
# 8. WEEKLY PERFORMANCE ANALYSIS
# ------------------------------------------------------------

weekly_naive <- aggregate(
  cbind(
    naive_absolute_error,
    naive_squared_error
  ) ~ week_start,
  data = evaluation_data,
  FUN = mean
)

weekly_lag <- aggregate(
  cbind(
    lag_absolute_error,
    lag_squared_error
  ) ~ week_start,
  data = evaluation_data,
  FUN = mean
)

weekly_results <- data.frame(
  week_start = weekly_naive$week_start,
  
  naive_MAE = weekly_naive$naive_absolute_error,
  
  naive_RMSE = sqrt(weekly_naive$naive_squared_error),
  
  lag_MAE = weekly_lag$lag_absolute_error,
  
  lag_RMSE = sqrt(weekly_lag$lag_squared_error)
)

weekly_results$lag_better <-
  weekly_results$lag_MAE < weekly_results$naive_MAE

cat("\nWEEKLY PERFORMANCE SUMMARY\n")
print(head(weekly_results, 10))

cat(
  "\nWeeks where lag model has lower MAE:",
  sum(weekly_results$lag_better),
  "out of",
  nrow(weekly_results),
  "\n"
)

# ------------------------------------------------------------
# 9. MONTHLY PERFORMANCE ANALYSIS
# ------------------------------------------------------------

monthly_naive <- aggregate(
  cbind(
    naive_absolute_error,
    naive_squared_error
  ) ~ month,
  data = evaluation_data,
  FUN = mean
)

monthly_lag <- aggregate(
  cbind(
    lag_absolute_error,
    lag_squared_error
  ) ~ month,
  data = evaluation_data,
  FUN = mean
)

monthly_results <- data.frame(
  Month = monthly_naive$month,
  
  Naive_MAE = monthly_naive$naive_absolute_error,
  
  Naive_RMSE = sqrt(monthly_naive$naive_squared_error),
  
  Lag_MAE = monthly_lag$lag_absolute_error,
  
  Lag_RMSE = sqrt(monthly_lag$lag_squared_error)
)

cat("\nMONTHLY PERFORMANCE SUMMARY\n")
print(monthly_results)

# ------------------------------------------------------------
# 10. HOURLY ERROR ANALYSIS
# ------------------------------------------------------------

hourly_results <- aggregate(
  cbind(
    naive_absolute_error,
    lag_absolute_error
  ) ~ hour,
  data = evaluation_data,
  FUN = mean
)

names(hourly_results) <- c(
  "Hour",
  "Naive_MAE",
  "Lag_MAE"
)

hourly_results$lag_better <-
  hourly_results$Lag_MAE < hourly_results$Naive_MAE

# ------------------------------------------------------------
# 11. WEEKDAY ERROR ANALYSIS
# ------------------------------------------------------------

weekday_results <- aggregate(
  cbind(
    naive_absolute_error,
    lag_absolute_error
  ) ~ day_of_week,
  data = evaluation_data,
  FUN = mean
)

names(weekday_results) <- c(
  "Day",
  "Naive_MAE",
  "Lag_MAE"
)

weekday_results$lag_better <-
  weekday_results$Lag_MAE < weekday_results$Naive_MAE

# ------------------------------------------------------------
# 12. IDENTIFY LARGE FORECAST ERRORS
# ------------------------------------------------------------

residual_threshold <- quantile(
  evaluation_data$lag_absolute_error,
  probs = 0.95
)

large_errors <- evaluation_data[
  evaluation_data$lag_absolute_error >= residual_threshold,
  c(
    "datetime",
    "actual_kWh",
    "lag_prediction",
    "lag_residual",
    "lag_absolute_error"
  )
]

large_errors <- large_errors[
  order(-large_errors$lag_absolute_error),
]

cat("\n95TH PERCENTILE ABSOLUTE ERROR THRESHOLD:\n")
print(residual_threshold)

cat("\nLARGEST 20 FORECAST ERRORS:\n")
print(head(large_errors, 20))

# ------------------------------------------------------------
# 13. CUMULATIVE MAE ANALYSIS
# ------------------------------------------------------------

evaluation_data$cumulative_naive_MAE <-
  cumsum(evaluation_data$naive_absolute_error) /
  seq_len(nrow(evaluation_data))

evaluation_data$cumulative_lag_MAE <-
  cumsum(evaluation_data$lag_absolute_error) /
  seq_len(nrow(evaluation_data))

# ------------------------------------------------------------
# 14. SAVE ANALYSIS RESULTS
# ------------------------------------------------------------

dir.create("results", showWarnings = FALSE, recursive = TRUE)
dir.create("plots", showWarnings = FALSE, recursive = TRUE)

write.csv(
  robustness_metrics,
  "results/forecasting_robustness_comparison.csv",
  row.names = FALSE
)

write.csv(
  paired_comparison,
  "results/forecasting_paired_comparison.csv",
  row.names = FALSE
)

write.csv(
  weekly_results,
  "results/forecasting_weekly_robustness.csv",
  row.names = FALSE
)

write.csv(
  monthly_results,
  "results/forecasting_monthly_robustness.csv",
  row.names = FALSE
)

write.csv(
  hourly_results,
  "results/forecasting_hourly_robustness.csv",
  row.names = FALSE
)

write.csv(
  weekday_results,
  "results/forecasting_weekday_robustness.csv",
  row.names = FALSE
)

write.csv(
  large_errors,
  "results/forecasting_large_errors.csv",
  row.names = FALSE
)

write.csv(
  evaluation_data,
  "results/forecasting_robustness_predictions.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 15. PLOT 1: WEEKLY MAE COMPARISON
# ------------------------------------------------------------

weekly_plot_data <- rbind(
  data.frame(
    week_start = weekly_results$week_start,
    Model = "Naive Previous Hour",
    MAE = weekly_results$naive_MAE
  ),
  
  data.frame(
    week_start = weekly_results$week_start,
    Model = "Lag-Based Regression",
    MAE = weekly_results$lag_MAE
  )
)

weekly_plot <- ggplot2::ggplot(
  weekly_plot_data,
  ggplot2::aes(
    x = week_start,
    y = MAE,
    color = Model
  )
) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::labs(
    title = "Weekly Forecasting Performance",
    x = "Week Starting",
    y = "Weekly MAE (kWh)",
    color = "Model"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  weekly_plot,
  "forecasting_weekly_robustness.png",
  width = 14,
  height = 6
)

# ------------------------------------------------------------
# 16. PLOT 2: HOURLY MAE COMPARISON
# ------------------------------------------------------------

hourly_plot_data <- rbind(
  data.frame(
    Hour = hourly_results$Hour,
    Model = "Naive Previous Hour",
    MAE = hourly_results$Naive_MAE
  ),
  
  data.frame(
    Hour = hourly_results$Hour,
    Model = "Lag-Based Regression",
    MAE = hourly_results$Lag_MAE
  )
)

hourly_plot <- ggplot2::ggplot(
  hourly_plot_data,
  ggplot2::aes(
    x = Hour,
    y = MAE,
    fill = Model
  )
) +
  ggplot2::geom_col(
    position = "dodge"
  ) +
  ggplot2::scale_x_continuous(
    breaks = 0:23
  ) +
  ggplot2::labs(
    title = "Forecast Error by Hour",
    x = "Hour of Day",
    y = "Mean Absolute Error (kWh)",
    fill = "Model"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  hourly_plot,
  "forecasting_hourly_robustness.png"
)

# ------------------------------------------------------------
# 17. PLOT 3: WEEKDAY MAE COMPARISON
# ------------------------------------------------------------

weekday_plot_data <- rbind(
  data.frame(
    Day = weekday_results$Day,
    Model = "Naive Previous Hour",
    MAE = weekday_results$Naive_MAE
  ),
  
  data.frame(
    Day = weekday_results$Day,
    Model = "Lag-Based Regression",
    MAE = weekday_results$Lag_MAE
  )
)

weekday_plot <- ggplot2::ggplot(
  weekday_plot_data,
  ggplot2::aes(
    x = Day,
    y = MAE,
    fill = Model
  )
) +
  ggplot2::geom_col(
    position = "dodge"
  ) +
  ggplot2::labs(
    title = "Forecast Error by Weekday",
    x = "Day of Week",
    y = "Mean Absolute Error (kWh)",
    fill = "Model"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  weekday_plot,
  "forecasting_weekday_robustness.png"
)

# ------------------------------------------------------------
# 18. PLOT 4: CUMULATIVE MAE
# ------------------------------------------------------------

cumulative_plot_data <- rbind(
  data.frame(
    datetime = evaluation_data$datetime,
    Model = "Naive Previous Hour",
    Cumulative_MAE = evaluation_data$cumulative_naive_MAE
  ),
  
  data.frame(
    datetime = evaluation_data$datetime,
    Model = "Lag-Based Regression",
    Cumulative_MAE = evaluation_data$cumulative_lag_MAE
  )
)

cumulative_plot <- ggplot2::ggplot(
  cumulative_plot_data,
  ggplot2::aes(
    x = datetime,
    y = Cumulative_MAE,
    color = Model
  )
) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::labs(
    title = "Cumulative Forecasting MAE",
    x = "Date",
    y = "Cumulative MAE (kWh)",
    color = "Model"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  cumulative_plot,
  "forecasting_cumulative_mae.png",
  width = 14,
  height = 6
)

# ------------------------------------------------------------
# 19. PLOT 5: PAIRED ERROR IMPROVEMENT
# ------------------------------------------------------------

improvement_plot <- ggplot2::ggplot(
  evaluation_data,
  ggplot2::aes(
    x = datetime,
    y = absolute_error_improvement
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_line(
    linewidth = 0.45
  ) +
  ggplot2::labs(
    title = "Paired Absolute Error Improvement",
    subtitle = "Positive values mean lag regression has lower error",
    x = "Date",
    y = "Naive MAE - Lag MAE (kWh)"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  improvement_plot,
  "forecasting_paired_error_improvement.png",
  width = 14,
  height = 6
)

# ------------------------------------------------------------
# 20. COMPLETION SUMMARY
# ------------------------------------------------------------

cat("\n============================================\n")
cat("STEP 09G COMPLETED SUCCESSFULLY\n")
cat("============================================\n")

cat("\nLag model improved on:",
    lag_better, "of", nrow(evaluation_data),
    "test observations.\n")

cat("Naive model improved on:",
    naive_better, "observations.\n")

cat("Equal errors:",
    equal_predictions, "observations.\n")

cat("\nOverall MAE improvement:",
    round(mae_improvement_percent, 2), "%\n")