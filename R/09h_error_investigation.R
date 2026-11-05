
# ============================================================
# STEP 09H: FORECASTING ERROR INVESTIGATION
#             AND STATISTICAL VALIDATION
# ============================================================

source("R/plot_utils.R")

if (!exists("save_and_show_plot")) {
  stop("save_and_show_plot() could not be loaded.")
}

cat("\nLoading forecasting robustness results...\n")

# ------------------------------------------------------------
# 1. LOAD ROBUSTNESS PREDICTIONS
# ------------------------------------------------------------

evaluation_data <- read.csv(
  "results/forecasting_robustness_predictions.csv",
  stringsAsFactors = FALSE
)

if (nrow(evaluation_data) == 0) {
  stop("Forecasting robustness dataset is empty.")
}

required_columns <- c(
  "datetime",
  "actual_kWh",
  "naive_prediction",
  "lag_prediction"
)

if (!all(required_columns %in% names(evaluation_data))) {
  stop("Required columns are missing from robustness predictions.")
}

# ------------------------------------------------------------
# 2. VALIDATE AND PARSE DATETIME
# ------------------------------------------------------------

parsed_datetime <- as.POSIXct(
  evaluation_data$datetime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "Asia/Kolkata"
)

date_only_rows <- grepl(
  "^\\d{4}-\\d{2}-\\d{2}$",
  evaluation_data$datetime
)

parsed_datetime[date_only_rows] <- as.POSIXct(
  evaluation_data$datetime[date_only_rows],
  format = "%Y-%m-%d",
  tz = "Asia/Kolkata"
)

if (anyNA(parsed_datetime)) {
  stop("Datetime conversion failed.")
}

evaluation_data$datetime <- parsed_datetime

if (anyDuplicated(evaluation_data$datetime)) {
  stop("Duplicate evaluation timestamps detected.")
}

if (any(diff(as.numeric(evaluation_data$datetime)) <= 0)) {
  stop("Timestamps are not in chronological order.")
}

if (any(!is.finite(evaluation_data$actual_kWh)) ||
    any(!is.finite(evaluation_data$naive_prediction)) ||
    any(!is.finite(evaluation_data$lag_prediction))) {
  stop("Non-finite values found in forecasting data.")
}

cat("Input validation passed.\n")

# ------------------------------------------------------------
# 3. CALCULATE MODEL ERRORS
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

evaluation_data$naive_squared_error <-
  evaluation_data$naive_residual^2

evaluation_data$lag_squared_error <-
  evaluation_data$lag_residual^2

evaluation_data$absolute_error_improvement <-
  evaluation_data$naive_absolute_error -
  evaluation_data$lag_absolute_error

# Positive improvement means lag regression is better.

# ------------------------------------------------------------
# 4. OVERALL ERROR INVESTIGATION
# ------------------------------------------------------------

error_investigation <- data.frame(
  Model = c(
    "Naive Previous Hour",
    "Lag-Based Regression"
  ),
  
  Mean_Absolute_Error = c(
    mean(evaluation_data$naive_absolute_error),
    mean(evaluation_data$lag_absolute_error)
  ),
  
  Median_Absolute_Error = c(
    median(evaluation_data$naive_absolute_error),
    median(evaluation_data$lag_absolute_error)
  ),
  
  Error_Standard_Deviation = c(
    sd(evaluation_data$naive_residual),
    sd(evaluation_data$lag_residual)
  ),
  
  Mean_Bias = c(
    mean(evaluation_data$naive_prediction -
           evaluation_data$actual_kWh),
    mean(evaluation_data$lag_prediction -
           evaluation_data$actual_kWh)
  ),
  
  Error_95th_Percentile = c(
    as.numeric(quantile(
      evaluation_data$naive_absolute_error,
      0.95
    )),
    as.numeric(quantile(
      evaluation_data$lag_absolute_error,
      0.95
    ))
  ),
  
  Maximum_Absolute_Error = c(
    max(evaluation_data$naive_absolute_error),
    max(evaluation_data$lag_absolute_error)
  ),
  
  Underestimation_Count = c(
    sum(evaluation_data$naive_residual > 0),
    sum(evaluation_data$lag_residual > 0)
  ),
  
  Overestimation_Count = c(
    sum(evaluation_data$naive_residual < 0),
    sum(evaluation_data$lag_residual < 0)
  )
)

cat("\nOVERALL ERROR INVESTIGATION\n")
print(error_investigation)

# ------------------------------------------------------------
# 5. ERROR ANALYSIS BY HOUR
# ------------------------------------------------------------

datetime_lt <- as.POSIXlt(
  evaluation_data$datetime,
  tz = "Asia/Kolkata"
)

evaluation_data$hour <- datetime_lt$hour

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

hourly_results$MAE_Improvement <-
  hourly_results$Naive_MAE -
  hourly_results$Lag_MAE

hourly_results$Better_Model <- ifelse(
  hourly_results$MAE_Improvement > 0,
  "Lag-Based Regression",
  ifelse(
    hourly_results$MAE_Improvement < 0,
    "Naive Previous Hour",
    "Equal"
  )
)

cat("\nERROR ANALYSIS BY HOUR\n")
print(hourly_results)

# ------------------------------------------------------------
# 6. ERROR ANALYSIS BY WEEKDAY
# ------------------------------------------------------------

evaluation_data$day_of_week <- factor(
  datetime_lt$wday,
  levels = 0:6,
  labels = c(
    "Sunday", "Monday", "Tuesday",
    "Wednesday", "Thursday", "Friday",
    "Saturday"
  )
)

weekday_results <- aggregate(
  cbind(
    naive_absolute_error,
    lag_absolute_error
  ) ~ day_of_week,
  data = evaluation_data,
  FUN = mean
)

names(weekday_results) <- c(
  "Weekday",
  "Naive_MAE",
  "Lag_MAE"
)

weekday_results$MAE_Improvement <-
  weekday_results$Naive_MAE -
  weekday_results$Lag_MAE

cat("\nERROR ANALYSIS BY WEEKDAY\n")
print(weekday_results)

# ------------------------------------------------------------
# 7. DAILY PAIRED MODEL COMPARISON
# ------------------------------------------------------------

evaluation_data$date <- as.Date(
  evaluation_data$datetime,
  tz = "Asia/Kolkata"
)

daily_results <- aggregate(
  cbind(
    naive_absolute_error,
    lag_absolute_error
  ) ~ date,
  data = evaluation_data,
  FUN = mean
)

names(daily_results) <- c(
  "Date",
  "Naive_Daily_MAE",
  "Lag_Daily_MAE"
)

daily_results$MAE_Improvement <-
  daily_results$Naive_Daily_MAE -
  daily_results$Lag_Daily_MAE

daily_results$Better_Model <- ifelse(
  daily_results$MAE_Improvement > 0,
  "Lag-Based Regression",
  ifelse(
    daily_results$MAE_Improvement < 0,
    "Naive Previous Hour",
    "Equal"
  )
)

cat("\nDAILY MODEL COMPARISON\n")
print(head(daily_results, 10))

cat(
  "\nNumber of evaluated days:",
  nrow(daily_results),
  "\n"
)

cat(
  "Days where lag model has lower MAE:",
  sum(daily_results$MAE_Improvement > 0),
  "\n"
)

cat(
  "Days where naive model has lower MAE:",
  sum(daily_results$MAE_Improvement < 0),
  "\n"
)

# ------------------------------------------------------------
# 8. STATISTICAL VALIDATION
# ------------------------------------------------------------

daily_improvement <- daily_results$MAE_Improvement

if (length(daily_improvement) < 2) {
  stop("Insufficient daily observations for statistical testing.")
}

# Positive difference means lag model has lower daily MAE.

paired_t_test <- t.test(
  daily_improvement,
  mu = 0,
  alternative = "greater"
)

wilcoxon_test <- wilcox.test(
  daily_improvement,
  mu = 0,
  alternative = "greater",
  exact = FALSE
)

statistical_tests <- data.frame(
  Test = c(
    "One-Sample Paired Daily MAE t-test",
    "Wilcoxon Signed-Rank Test on Daily MAE Differences"
  ),
  
  Sample_Size = length(daily_improvement),
  
  Mean_Daily_Improvement = mean(daily_improvement),
  
  Test_Statistic = c(
    unname(paired_t_test$statistic),
    unname(wilcoxon_test$statistic)
  ),
  
  P_Value = c(
    paired_t_test$p.value,
    wilcoxon_test$p.value
  ),
  
  Alternative = "Lag-Based Regression has lower daily MAE"
)

cat("\nSTATISTICAL TEST RESULTS\n")
print(statistical_tests)

cat("\nT-TEST CONFIDENCE INTERVAL:\n")
print(paired_t_test$conf.int)

cat(
  "\nIMPORTANT: These are exploratory tests on daily aggregates.",
  "\nSerial dependence between neighbouring days may still affect",
  "\nthe validity of the p-values. Do not interpret them as",
  "\ndefinitive evidence of forecasting superiority.\n"
)

# ------------------------------------------------------------
# 9. IDENTIFY EXTREME FORECAST ERROR EVENTS
# ------------------------------------------------------------

error_threshold <- quantile(
  evaluation_data$lag_absolute_error,
  probs = 0.95
)

extreme_errors <- evaluation_data[
  evaluation_data$lag_absolute_error >= error_threshold,
  c(
    "datetime",
    "actual_kWh",
    "lag_prediction",
    "lag_residual",
    "lag_absolute_error",
    "naive_absolute_error"
  )
]

extreme_errors <- extreme_errors[
  order(-extreme_errors$lag_absolute_error),
]

cat("\nEXTREME ERROR THRESHOLD:\n")
print(error_threshold)

cat("\nLARGEST FORECAST ERROR EVENTS:\n")
print(head(extreme_errors, 20))

# ------------------------------------------------------------
# 10. SAVE ANALYSIS RESULTS
# ------------------------------------------------------------

dir.create(
  "results",
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  "plots",
  showWarnings = FALSE,
  recursive = TRUE
)

write.csv(
  error_investigation,
  "results/forecasting_error_investigation.csv",
  row.names = FALSE
)

write.csv(
  hourly_results,
  "results/forecasting_error_by_hour.csv",
  row.names = FALSE
)

write.csv(
  weekday_results,
  "results/forecasting_error_by_weekday.csv",
  row.names = FALSE
)

write.csv(
  daily_results,
  "results/forecasting_daily_model_comparison.csv",
  row.names = FALSE
)

write.csv(
  statistical_tests,
  "results/forecasting_statistical_tests.csv",
  row.names = FALSE
)

write.csv(
  extreme_errors,
  "results/forecasting_extreme_error_events.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 11. PLOT 1: ERROR TIMELINE
# ------------------------------------------------------------

error_timeline_data <- rbind(
  data.frame(
    datetime = evaluation_data$datetime,
    Model = "Naive Previous Hour",
    Absolute_Error = evaluation_data$naive_absolute_error
  ),
  
  data.frame(
    datetime = evaluation_data$datetime,
    Model = "Lag-Based Regression",
    Absolute_Error = evaluation_data$lag_absolute_error
  )
)

error_timeline_plot <- ggplot2::ggplot(
  error_timeline_data,
  ggplot2::aes(
    x = datetime,
    y = Absolute_Error,
    color = Model
  )
) +
  ggplot2::geom_line(linewidth = 0.4) +
  ggplot2::labs(
    title = "Forecasting Absolute Error Over Time",
    x = "Date",
    y = "Absolute Error (kWh)",
    color = "Model"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  error_timeline_plot,
  "forecasting_error_timeline.png",
  width = 14,
  height = 6
)

# ------------------------------------------------------------
# 12. PLOT 2: ERROR BY HOUR
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
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_x_continuous(
    breaks = 0:23
  ) +
  ggplot2::labs(
    title = "Forecasting Error by Hour",
    x = "Hour of Day",
    y = "Mean Absolute Error (kWh)",
    fill = "Model"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  hourly_plot,
  "forecasting_error_by_hour.png"
)

# ------------------------------------------------------------
# 13. PLOT 3: DAILY MAE COMPARISON
# ------------------------------------------------------------

daily_plot_data <- rbind(
  data.frame(
    Date = daily_results$Date,
    Model = "Naive Previous Hour",
    MAE = daily_results$Naive_Daily_MAE
  ),
  
  data.frame(
    Date = daily_results$Date,
    Model = "Lag-Based Regression",
    MAE = daily_results$Lag_Daily_MAE
  )
)

daily_plot <- ggplot2::ggplot(
  daily_plot_data,
  ggplot2::aes(
    x = Date,
    y = MAE,
    color = Model
  )
) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::labs(
    title = "Daily Forecasting MAE Comparison",
    x = "Date",
    y = "Daily MAE (kWh)",
    color = "Model"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  daily_plot,
  "forecasting_daily_mae_comparison.png",
  width = 14,
  height = 6
)

# ------------------------------------------------------------
# 14. PLOT 4: DAILY IMPROVEMENT DISTRIBUTION
# ------------------------------------------------------------

improvement_plot <- ggplot2::ggplot(
  daily_results,
  ggplot2::aes(
    x = MAE_Improvement
  )
) +
  ggplot2::geom_histogram(
    bins = 20
  ) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Distribution of Daily MAE Improvement",
    subtitle = "Positive values favour Lag-Based Regression",
    x = "Naive Daily MAE - Lag Daily MAE (kWh)",
    y = "Number of Days"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  improvement_plot,
  "forecasting_daily_improvement_distribution.png"
)

# ------------------------------------------------------------
# 15. COMPLETION SUMMARY
# ------------------------------------------------------------

cat("\n============================================\n")
cat("STEP 09H COMPLETED SUCCESSFULLY\n")
cat("============================================\n")

cat(
  "\nAverage daily MAE improvement:",
  round(mean(daily_improvement), 4),
  "kWh\n"
)

cat(
  "Days favouring lag model:",
  sum(daily_improvement > 0),
  "\n"
)

cat(
  "Days favouring naive model:",
  sum(daily_improvement < 0),
  "\n"
)

cat(
  "\nPaired daily t-test p-value:",
  signif(paired_t_test$p.value, 4),
  "\n"
)

cat(
  "Wilcoxon test p-value:",
  signif(wilcoxon_test$p.value, 4),
  "\n"
)

cat("\nStep 09H outputs saved successfully.\n")
