
# ============================================================
# STEP 09F: FORECAST EVALUATION AND RESIDUAL DIAGNOSTICS
# ============================================================

source("R/plot_utils.R")

if (!exists("save_and_show_plot")) {
  stop("save_and_show_plot() could not be loaded.")
}

cat("\nLoading forecasting predictions...\n")

# ------------------------------------------------------------
# 1. LOAD PREDICTION FILES
# ------------------------------------------------------------

baseline <- read.csv(
  "results/forecasting_baseline_predictions.csv"
)

advanced <- read.csv(
  "results/advanced_forecasting_predictions.csv"
)

# ------------------------------------------------------------
# 2. VALIDATE PREDICTION DATA
# ------------------------------------------------------------

if (nrow(baseline) == 0 || nrow(advanced) == 0) {
  stop("Prediction datasets are empty.")
}

if (anyDuplicated(baseline$datetime) ||
    anyDuplicated(advanced$datetime)) {
  stop("Duplicate timestamps found.")
}

if (!identical(
  as.character(baseline$datetime),
  as.character(advanced$datetime)
)) {
  stop("Baseline and advanced timestamps do not match.")
}

if (!isTRUE(all.equal(
  baseline$actual_kWh,
  advanced$actual_kWh
))) {
  stop("Actual energy values do not match.")
}

# ------------------------------------------------------------
# 3. COMBINE ALL MODEL PREDICTIONS
# ------------------------------------------------------------

evaluation_data <- data.frame(
  datetime = as.POSIXct(
    baseline$datetime,
    tz = "Asia/Kolkata"
  ),
  
  actual_kWh = baseline$actual_kWh,
  
  naive_previous_hour = baseline$naive_previous_hour,
  
  seasonal_naive_24h = baseline$seasonal_naive_24h,
  
  linear_trend = advanced$linear_trend,
  
  calendar_regression = advanced$calendar_regression,
  
  lag_regression = advanced$lag_regression
)

# ------------------------------------------------------------
# 4. CALCULATE MODEL PERFORMANCE
# ------------------------------------------------------------

calculate_metrics <- function(actual, predicted, model_name) {
  
  valid <- is.finite(actual) & is.finite(predicted)
  
  actual_valid <- actual[valid]
  predicted_valid <- predicted[valid]
  
  if (length(actual_valid) == 0) {
    stop(paste("No valid predictions for", model_name))
  }
  
  errors <- actual_valid - predicted_valid
  
  data.frame(
    Model = model_name,
    Observations = length(errors),
    MAE = mean(abs(errors)),
    RMSE = sqrt(mean(errors^2)),
    Bias = mean(predicted_valid - actual_valid)
  )
}

actual <- evaluation_data$actual_kWh

metrics <- rbind(
  
  calculate_metrics(
    actual,
    evaluation_data$naive_previous_hour,
    "Naive Previous Hour"
  ),
  
  calculate_metrics(
    actual,
    evaluation_data$seasonal_naive_24h,
    "Seasonal Naive (24 Hours)"
  ),
  
  calculate_metrics(
    actual,
    evaluation_data$linear_trend,
    "Linear Trend Regression"
  ),
  
  calculate_metrics(
    actual,
    evaluation_data$calendar_regression,
    "Calendar Regression"
  ),
  
  calculate_metrics(
    actual,
    evaluation_data$lag_regression,
    "Lag-Based Regression"
  )
)

metrics <- metrics[order(metrics$MAE), ]

cat("\nFINAL MODEL PERFORMANCE COMPARISON\n")
print(metrics)

# ------------------------------------------------------------
# 5. IDENTIFY BEST MODEL
# ------------------------------------------------------------

best_model <- metrics$Model[1]

cat("\nBest model based on MAE:", best_model, "\n")

# ------------------------------------------------------------
# 6. SAVE COMPARISON RESULTS
# ------------------------------------------------------------

dir.create("results", showWarnings = FALSE, recursive = TRUE)
dir.create("plots", showWarnings = FALSE, recursive = TRUE)

write.csv(
  metrics,
  "results/final_forecasting_model_comparison.csv",
  row.names = FALSE
)

write.csv(
  evaluation_data,
  "results/combined_forecasting_predictions.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 7. PREPARE BEST MODEL RESIDUALS
# ------------------------------------------------------------

best_prediction_column <- switch(
  best_model,
  
  "Naive Previous Hour" = "naive_previous_hour",
  
  "Seasonal Naive (24 Hours)" = "seasonal_naive_24h",
  
  "Linear Trend Regression" = "linear_trend",
  
  "Calendar Regression" = "calendar_regression",
  
  "Lag-Based Regression" = "lag_regression"
)

evaluation_data$predicted_kWh <- evaluation_data[[best_prediction_column]]

evaluation_data$residual <- (
  evaluation_data$actual_kWh -
    evaluation_data$predicted_kWh
)

evaluation_data$hour <- as.integer(
  format(evaluation_data$datetime, "%H")
)

evaluation_data$day_of_week <- weekdays(
  evaluation_data$datetime
)

# ------------------------------------------------------------
# 8. RESIDUAL SUMMARY
# ------------------------------------------------------------

residual_summary <- data.frame(
  Best_Model = best_model,
  Mean_Residual = mean(evaluation_data$residual),
  Median_Residual = median(evaluation_data$residual),
  SD_Residual = sd(evaluation_data$residual),
  Minimum_Residual = min(evaluation_data$residual),
  Maximum_Residual = max(evaluation_data$residual)
)

cat("\nRESIDUAL SUMMARY\n")
print(residual_summary)

write.csv(
  residual_summary,
  "results/forecasting_residual_summary.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 9. PLOT 1: ACTUAL VS PREDICTED
# ------------------------------------------------------------

actual_predicted_plot <- ggplot2::ggplot(
  evaluation_data,
  ggplot2::aes(x = datetime)
) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = actual_kWh, color = "Actual"),
    linewidth = 0.5
  ) +
  
  ggplot2::geom_line(
    ggplot2::aes(y = predicted_kWh, color = "Predicted"),
    linewidth = 0.5
  ) +
  
  ggplot2::labs(
    title = "Actual vs Predicted Energy Consumption",
    subtitle = paste("Best model:", best_model),
    x = "Date",
    y = "Energy Consumption (kWh)",
    color = "Series"
  ) +
  
  ggplot2::theme_minimal()

save_and_show_plot(
  actual_predicted_plot,
  "actual_vs_predicted.png",
  width = 14,
  height = 7
)

# ------------------------------------------------------------
# 10. PLOT 2: RESIDUALS OVER TIME
# ------------------------------------------------------------

residual_time_plot <- ggplot2::ggplot(
  evaluation_data,
  ggplot2::aes(x = datetime, y = residual)
) +
  
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  ggplot2::geom_line(
    linewidth = 0.45
  ) +
  
  ggplot2::labs(
    title = "Forecast Residuals Over Time",
    subtitle = paste("Best model:", best_model),
    x = "Date",
    y = "Residual (kWh)"
  ) +
  
  ggplot2::theme_minimal()

save_and_show_plot(
  residual_time_plot,
  "forecast_residuals_over_time.png"
)

# ------------------------------------------------------------
# 11. PLOT 3: RESIDUAL DISTRIBUTION
# ------------------------------------------------------------

residual_distribution_plot <- ggplot2::ggplot(
  evaluation_data,
  ggplot2::aes(x = residual)
) +
  
  ggplot2::geom_histogram(
    bins = 40,
    na.rm = TRUE
  ) +
  
  ggplot2::labs(
    title = "Distribution of Forecast Residuals",
    x = "Residual (kWh)",
    y = "Frequency"
  ) +
  
  ggplot2::theme_minimal()

save_and_show_plot(
  residual_distribution_plot,
  "forecast_residual_distribution.png"
)

# ------------------------------------------------------------
# 12. PLOT 4: RESIDUALS VS PREDICTED VALUES
# ------------------------------------------------------------

residual_prediction_plot <- ggplot2::ggplot(
  evaluation_data,
  ggplot2::aes(x = predicted_kWh, y = residual)
) +
  
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  ggplot2::geom_point(
    alpha = 0.35
  ) +
  
  ggplot2::labs(
    title = "Residuals vs Predicted Energy",
    x = "Predicted Energy (kWh)",
    y = "Residual (kWh)"
  ) +
  
  ggplot2::theme_minimal()

save_and_show_plot(
  residual_prediction_plot,
  "residuals_vs_predicted.png"
)

# ------------------------------------------------------------
# 13. HOURLY ERROR ANALYSIS
# ------------------------------------------------------------

hourly_error <- aggregate(
  abs(residual) ~ hour,
  data = evaluation_data,
  FUN = mean
)

names(hourly_error) <- c("Hour", "MAE")

write.csv(
  hourly_error,
  "results/hourly_forecasting_error.csv",
  row.names = FALSE
)

hourly_error_plot <- ggplot2::ggplot(
  hourly_error,
  ggplot2::aes(x = Hour, y = MAE)
) +
  
  ggplot2::geom_col() +
  
  ggplot2::labs(
    title = "Forecast Error by Hour of Day",
    x = "Hour of Day",
    y = "Mean Absolute Error (kWh)"
  ) +
  
  ggplot2::theme_minimal()

save_and_show_plot(
  hourly_error_plot,
  "forecast_error_by_hour.png"
)


# ------------------------------------------------------------
# 14. WEEKDAY ERROR ANALYSIS
# ------------------------------------------------------------

weekday_error <- aggregate(
  abs(residual) ~ day_of_week,
  data = evaluation_data,
  FUN = mean
)

names(weekday_error) <- c("Day", "MAE")

# Arrange weekdays in calendar order
weekday_order <- c(
  "Sunday", "Monday", "Tuesday",
  "Wednesday", "Thursday", "Friday", "Saturday"
)

weekday_error$Day <- factor(
  weekday_error$Day,
  levels = weekday_order
)

weekday_error <- weekday_error[
  order(weekday_error$Day),
]

write.csv(
  weekday_error,
  "results/weekday_forecasting_error.csv",
  row.names = FALSE
)

weekday_error_plot <- ggplot2::ggplot(
  weekday_error,
  ggplot2::aes(x = Day, y = MAE)
) +
  
  ggplot2::geom_col() +
  
  ggplot2::labs(
    title = "Forecast Error by Day of Week",
    x = "Day of Week",
    y = "Mean Absolute Error (kWh)"
  ) +
  
  ggplot2::theme_minimal()

save_and_show_plot(
  weekday_error_plot,
  "forecast_error_by_weekday.png"
)


# ------------------------------------------------------------
# 15. COMPLETION
# ------------------------------------------------------------

cat("\n============================================\n")
cat("STEP 09F COMPLETED SUCCESSFULLY\n")
cat("============================================\n")