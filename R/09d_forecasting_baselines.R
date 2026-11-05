source("R/plot_utils.R")

if (!exists("save_and_show_plot")) {
  stop("save_and_show_plot() could not be loaded.")
}

# ============================================================
# STEP 09D: BASELINE FORECASTING MODELS
# ============================================================

cat("\nLoading forecasting datasets...\n")

training_data <- readRDS(
  "data/processed/forecasting_train.rds"
)

testing_data <- readRDS(
  "data/processed/forecasting_test.rds"
)


# ------------------------------------------------------------
# ADDITIONAL INPUT VALIDATION
# ------------------------------------------------------------

if (nrow(training_data) == 0 || nrow(testing_data) == 0) {
  stop("Training or testing dataset is empty.")
}

required_columns <- c("datetime", "energy_kWh")

if (!all(required_columns %in% names(training_data)) ||
    !all(required_columns %in% names(testing_data))) {
  stop("Required columns are missing.")
}

if (!inherits(training_data$datetime, "POSIXct") ||
    !inherits(testing_data$datetime, "POSIXct")) {
  stop("Datetime columns must be POSIXct.")
}


# ------------------------------------------------------------
# 1. VALIDATE DATA
# ------------------------------------------------------------

if (anyNA(training_data$energy_kWh) ||
    anyNA(testing_data$energy_kWh)) {
  stop("Missing energy values found in forecasting data.")
}

if (anyDuplicated(training_data$datetime) ||
    anyDuplicated(testing_data$datetime)) {
  stop("Duplicate timestamps found.")
}

if (max(training_data$datetime) >= min(testing_data$datetime)) {
  stop("Training and testing periods overlap.")
}

if (any(diff(as.numeric(training_data$datetime)) != 3600) ||
    any(diff(as.numeric(testing_data$datetime)) != 3600)) {
  stop("Data is not continuous at hourly intervals.")
}

# ------------------------------------------------------------
# 2. COMBINE TRAINING AND TESTING DATA
# ------------------------------------------------------------

combined_data <- rbind(
  training_data,
  testing_data
)

train_n <- nrow(training_data)

test_indices <- (train_n + 1):nrow(combined_data)

actual <- combined_data$energy_kWh[test_indices]

# ------------------------------------------------------------
# 3. NAIVE PREVIOUS-HOUR FORECAST
# ------------------------------------------------------------

naive_forecast <- combined_data$energy_kWh[
  test_indices - 1
]

# ------------------------------------------------------------
# 4. SEASONAL NAIVE (PREVIOUS 24 HOURS)
# ------------------------------------------------------------

seasonal_naive_forecast <- combined_data$energy_kWh[
  test_indices - 24
]

# ------------------------------------------------------------
# VALIDATE FORECAST LENGTHS
# ------------------------------------------------------------

if (length(naive_forecast) != length(actual) ||
    length(seasonal_naive_forecast) != length(actual)) {
  stop("Forecast and actual value lengths do not match.")
}

# ------------------------------------------------------------
# 5. EVALUATION FUNCTION
# ------------------------------------------------------------

calculate_metrics <- function(actual, predicted, model_name) {
  
  valid <- is.finite(actual) & is.finite(predicted)
  
  actual_valid <- actual[valid]
  predicted_valid <- predicted[valid]
  
  errors <- actual_valid - predicted_valid
  
  data.frame(
    Model = model_name,
    Observations = length(errors),
    MAE = mean(abs(errors)),
    RMSE = sqrt(mean(errors^2)),
    Bias = mean(predicted_valid - actual_valid)
  )
}

# ------------------------------------------------------------
# 6. COMPARE BASELINES
# ------------------------------------------------------------

metrics <- rbind(
  calculate_metrics(
    actual,
    naive_forecast,
    "Naive Previous Hour"
  ),
  calculate_metrics(
    actual,
    seasonal_naive_forecast,
    "Seasonal Naive (24 Hours)"
  )
)

cat("\nBASELINE FORECASTING RESULTS\n")
print(metrics)

# ------------------------------------------------------------
# 7. SAVE RESULTS
# ------------------------------------------------------------

dir.create("results", showWarnings = FALSE, recursive = TRUE)
dir.create("plots", showWarnings = FALSE, recursive = TRUE)

write.csv(
  metrics,
  "results/forecasting_baseline_comparison.csv",
  row.names = FALSE
)

predictions <- data.frame(
  datetime = combined_data$datetime[test_indices],
  actual_kWh = actual,
  naive_previous_hour = naive_forecast,
  seasonal_naive_24h = seasonal_naive_forecast
)

write.csv(
  predictions,
  "results/forecasting_baseline_predictions.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. VISUALIZE FORECASTS
# ------------------------------------------------------------

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Please install ggplot2: install.packages('ggplot2')")
}

library(ggplot2)

plot_data <- data.frame(
  datetime = predictions$datetime,
  Actual = predictions$actual_kWh,
  Naive = predictions$naive_previous_hour,
  Seasonal_Naive = predictions$seasonal_naive_24h
)

plot_long <- tidyr::pivot_longer(
  plot_data,
  cols = c("Actual", "Naive", "Seasonal_Naive"),
  names_to = "Model",
  values_to = "Energy_kWh"
)

baseline_plot <- ggplot(
  plot_long,
  aes(x = datetime, y = Energy_kWh, color = Model)
) +
  geom_line(linewidth = 0.45, na.rm = TRUE) +
  labs(
    title = "Baseline Forecasting Comparison",
    subtitle = "Actual energy consumption vs baseline predictions",
    x = "Date",
    y = "Energy Consumption (kWh)",
    color = "Series"
  ) +
  theme_minimal()

save_and_show_plot(
  plot = baseline_plot,
  filename = "forecasting_baseline_comparison.png",
  width = 12,
  height = 6,
  dpi = 300
)

cat("\n============================================\n")
cat("STEP 09D COMPLETED SUCCESSFULLY\n")
cat("============================================\n")