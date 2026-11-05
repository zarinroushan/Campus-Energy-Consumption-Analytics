
# ============================================================
# STEP 09E: ADVANCED FORECASTING MODELS
# ============================================================

source("R/plot_utils.R")

if (!exists("save_and_show_plot")) {
  stop("save_and_show_plot() could not be loaded.")
}

cat("\nLoading forecasting datasets...\n")

training_data <- readRDS(
  "data/processed/forecasting_train.rds"
)

testing_data <- readRDS(
  "data/processed/forecasting_test.rds"
)

# ------------------------------------------------------------
# 1. INPUT VALIDATION
# ------------------------------------------------------------

required_columns <- c("datetime", "energy_kWh")

if (nrow(training_data) == 0 || nrow(testing_data) == 0) {
  stop("Training or testing dataset is empty.")
}

if (!all(required_columns %in% names(training_data)) ||
    !all(required_columns %in% names(testing_data))) {
  stop("Required columns are missing.")
}

if (!inherits(training_data$datetime, "POSIXct") ||
    !inherits(testing_data$datetime, "POSIXct")) {
  stop("Datetime columns must be POSIXct.")
}

if (anyNA(training_data$energy_kWh) ||
    anyNA(testing_data$energy_kWh)) {
  stop("Missing energy values detected.")
}

if (anyDuplicated(training_data$datetime) ||
    anyDuplicated(testing_data$datetime)) {
  stop("Duplicate timestamps detected.")
}

if (max(training_data$datetime) >= min(testing_data$datetime)) {
  stop("Training and testing periods overlap.")
}

if (any(diff(as.numeric(training_data$datetime)) != 3600) ||
    any(diff(as.numeric(testing_data$datetime)) != 3600)) {
  stop("Data is not continuous at hourly intervals.")
}

# ------------------------------------------------------------
# 2. COMBINE DATA
# ------------------------------------------------------------

combined_data <- rbind(
  training_data,
  testing_data
)

train_n <- nrow(training_data)

test_indices <- (train_n + 1):nrow(combined_data)

# ------------------------------------------------------------
# 3. FEATURE ENGINEERING
# ------------------------------------------------------------

combined_data$time_index <- seq_len(nrow(combined_data))

combined_data$hour <- as.integer(
  format(combined_data$datetime, "%H")
)

combined_data$day_of_week <- factor(
  as.POSIXlt(combined_data$datetime)$wday,
  levels = 0:6,
  labels = c("Sunday", "Monday", "Tuesday",
             "Wednesday", "Thursday", "Friday", "Saturday")
)

combined_data$month <- factor(
  format(combined_data$datetime, "%m"),
  levels = sprintf("%02d", 1:12),
  labels = month.name
)

# Use fixed weekday and month levels
combined_data$day_of_week <- factor(
  combined_data$day_of_week,
  levels = c(
    "Sunday", "Monday", "Tuesday",
    "Wednesday", "Thursday", "Friday", "Saturday"
  )
)

combined_data$month <- factor(
  combined_data$month,
  levels = month.name
)

combined_data$is_weekend <- as.integer(
  as.POSIXlt(combined_data$datetime)$wday %in% c(0, 6)
)

# Lag features
combined_data$lag_1 <- c(
  NA,
  head(combined_data$energy_kWh, -1)
)

combined_data$lag_24 <- c(
  rep(NA, 24),
  head(combined_data$energy_kWh, -24)
)

# ------------------------------------------------------------
# 4. CREATE TRAINING AND TESTING FEATURE SETS
# ------------------------------------------------------------

model_training <- combined_data[
  1:train_n,
]

model_testing <- combined_data[
  test_indices,
]

# Remove initial training rows without lag values
model_training <- model_training[
  complete.cases(
    model_training[, c("energy_kWh", "lag_1", "lag_24")]
  ),
]

# Check sufficient training observations
if (nrow(model_training) < 100) {
  stop("Insufficient training observations.")
}

# ------------------------------------------------------------
# 5. MODEL 1: LINEAR TREND REGRESSION
# ------------------------------------------------------------

model_trend <- lm(
  energy_kWh ~ time_index,
  data = model_training
)

forecast_trend <- predict(
  model_trend,
  newdata = model_testing
)

# ------------------------------------------------------------
# 6. MODEL 2: CALENDAR REGRESSION
# ------------------------------------------------------------

model_calendar <- lm(
  energy_kWh ~ hour + day_of_week + is_weekend,
  data = model_training
)

forecast_calendar <- predict(
  model_calendar,
  newdata = model_testing
)

# ------------------------------------------------------------
# 7. MODEL 3: LAG-BASED REGRESSION
# ------------------------------------------------------------

model_lag <- lm(
  energy_kWh ~ lag_1 + lag_24 +
    hour + day_of_week,
  data = model_training
)

forecast_lag <- predict(
  model_lag,
  newdata = model_testing
)

# ------------------------------------------------------------
# 8. EVALUATION FUNCTION
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

actual <- model_testing$energy_kWh

# ------------------------------------------------------------
# 9. MODEL COMPARISON
# ------------------------------------------------------------

metrics <- rbind(
  calculate_metrics(
    actual, forecast_trend,
    "Linear Trend Regression"
  ),
  calculate_metrics(
    actual, forecast_calendar,
    "Calendar Regression"
  ),
  calculate_metrics(
    actual, forecast_lag,
    "Lag-Based Regression"
  )
)

cat("\nADVANCED FORECASTING RESULTS\n")
print(metrics)

# ------------------------------------------------------------
# 10. SAVE MODEL RESULTS
# ------------------------------------------------------------

dir.create("results", showWarnings = FALSE, recursive = TRUE)
dir.create("plots", showWarnings = FALSE, recursive = TRUE)

write.csv(
  metrics,
  "results/advanced_forecasting_comparison.csv",
  row.names = FALSE
)

predictions <- data.frame(
  datetime = model_testing$datetime,
  actual_kWh = actual,
  linear_trend = forecast_trend,
  calendar_regression = forecast_calendar,
  lag_regression = forecast_lag
)

write.csv(
  predictions,
  "results/advanced_forecasting_predictions.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 11. VISUALIZE FORECAST COMPARISON
# ------------------------------------------------------------

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Please install ggplot2.")
}

if (!requireNamespace("tidyr", quietly = TRUE)) {
  stop("Please install tidyr.")
}

plot_data <- data.frame(
  datetime = predictions$datetime,
  Actual = predictions$actual_kWh,
  Linear_Trend = predictions$linear_trend,
  Calendar = predictions$calendar_regression,
  Lag_Regression = predictions$lag_regression
)

plot_long <- tidyr::pivot_longer(
  plot_data,
  cols = -datetime,
  names_to = "Model",
  values_to = "Energy_kWh"
)

advanced_plot <- ggplot2::ggplot(
  plot_long,
  ggplot2::aes(
    x = datetime,
    y = Energy_kWh,
    color = Model
  )
) +
  ggplot2::geom_line(
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  ggplot2::labs(
    title = "Advanced Forecasting Model Comparison",
    subtitle = "Actual consumption vs regression model predictions",
    x = "Date",
    y = "Energy Consumption (kWh)",
    color = "Model"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  plot = advanced_plot,
  filename = "advanced_forecasting_comparison.png",
  width = 14,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 12. COMPLETION
# ------------------------------------------------------------

cat("\n============================================\n")
cat("STEP 09E COMPLETED SUCCESSFULLY\n")
cat("============================================\n")