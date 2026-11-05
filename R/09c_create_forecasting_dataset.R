
# ============================================================
# STEP 09C: CREATE CHRONOLOGICAL FORECASTING DATASET
# ============================================================

library(dplyr)
library(readr)
library(ggplot2)

cat("\nLoading hourly analytical dataset...\n")

data <- readRDS("data/processed/energy_hourly_with_features.rds")


# Validate input dataset
if (nrow(data) == 0 || anyDuplicated(data$datetime)) {
  stop("Dataset is empty or contains duplicate timestamps.")
}

if (!inherits(data$datetime, "POSIXct")) {
  stop("datetime must be a POSIXct column.")
}

# Recreate the Step 08 campus energy target
forecast_data <- data %>%
  mutate(
    Boys_coverage = pmin(
      Boys_main_coverage_percent,
      Boys_backup_coverage_percent
    ),
    Girls_coverage = pmin(
      Girls_main_coverage_percent,
      Girls_backup_coverage_percent
    )
  ) %>%
  arrange(datetime)

energy_cols <- c(
  "Academic_energy_kWh",
  "Boys_total_energy_kWh",
  "Facilities_energy_kWh",
  "Girls_total_energy_kWh",
  "Lecture_energy_kWh",
  "Library_energy_kWh",
  "Mess_energy_kWh"
)

coverage_matrix <- cbind(
  Academic = forecast_data$Academic_coverage_percent,
  Boys = forecast_data$Boys_coverage,
  Facilities = forecast_data$Facilities_coverage_percent,
  Girls = forecast_data$Girls_coverage,
  Lecture = forecast_data$Lecture_coverage_percent,
  Library = forecast_data$Library_coverage_percent,
  Mess = forecast_data$Mess_coverage_percent
)

energy_matrix <- as.matrix(forecast_data[, energy_cols])

eligible <- !is.na(energy_matrix) &
  !is.na(coverage_matrix) &
  coverage_matrix >= 75

eligible_energy <- energy_matrix
eligible_energy[!eligible] <- NA_real_

building_count <- rowSums(!is.na(eligible_energy))

forecast_data$energy_kWh <- ifelse(
  building_count >= 5,
  rowSums(eligible_energy, na.rm = TRUE),
  NA_real_
)

# Identify consecutive observed periods
observed_flag <- !is.na(forecast_data$energy_kWh)

run_id <- cumsum(
  c(TRUE, diff(as.integer(observed_flag)) != 0)
)

forecast_data$run_id <- run_id

observed_runs <- forecast_data %>%
  filter(!is.na(energy_kWh)) %>%
  group_by(run_id) %>%
  summarise(
    start_datetime = min(datetime),
    end_datetime = max(datetime),
    duration_hours = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(duration_hours))

# Select the longest continuous observed period
best_run <- observed_runs %>%
  slice(1)

selected_data <- forecast_data %>%
  filter(run_id == best_run$run_id) %>%
  select(datetime, energy_kWh) %>%
  arrange(datetime)

# Verify continuity
intervals <- diff(as.numeric(selected_data$datetime)) / 3600

if (any(intervals != 1)) {
  stop("Selected period contains irregular hourly intervals.")
}

if (anyNA(selected_data$energy_kWh)) {
  stop("Selected period unexpectedly contains missing energy values.")
}

# Chronological 80/20 split
total_rows <- nrow(selected_data)

train_ratio <- 0.80

train_rows <- floor(total_rows * train_ratio)

training_data <- selected_data %>%
  slice_head(n = train_rows)

testing_data <- selected_data %>%
  slice((train_rows + 1):total_rows)


# Validate training and testing datasets
if (nrow(training_data) == 0 || nrow(testing_data) == 0) {
  stop("Training or testing dataset is empty.")
}

if (max(training_data$datetime) >= min(testing_data$datetime)) {
  stop("Chronological train-test split is invalid.")
}

# Summary
cat("\n============================================\n")
cat("FORECASTING DATASET SUMMARY\n")
cat("============================================\n")

cat("Selected period start:",
    format(min(selected_data$datetime), "%Y-%m-%d %H:%M:%S"), "\n")

cat("Selected period end:",
    format(max(selected_data$datetime), "%Y-%m-%d %H:%M:%S"), "\n")

cat("Total observations:", total_rows, "\n")
cat("Training observations:", nrow(training_data), "\n")
cat("Testing observations:", nrow(testing_data), "\n")

cat("\nTRAINING PERIOD\n")
cat("Start:", format(min(training_data$datetime), "%Y-%m-%d %H:%M:%S"), "\n")
cat("End:", format(max(training_data$datetime), "%Y-%m-%d %H:%M:%S"), "\n")

cat("\nTESTING PERIOD\n")
cat("Start:", format(min(testing_data$datetime), "%Y-%m-%d %H:%M:%S"), "\n")
cat("End:", format(max(testing_data$datetime), "%Y-%m-%d %H:%M:%S"), "\n")

cat("\nENERGY SUMMARY\n")
cat("Training mean:",
    round(mean(training_data$energy_kWh), 2), "kWh\n")

cat("Testing mean:",
    round(mean(testing_data$energy_kWh), 2), "kWh\n")

cat("Training standard deviation:",
    round(sd(training_data$energy_kWh), 2), "\n")

cat("Testing standard deviation:",
    round(sd(testing_data$energy_kWh), 2), "\n")

# Save datasets
saveRDS(
  selected_data,
  "data/processed/forecasting_selected_period.rds"
)

saveRDS(
  training_data,
  "data/processed/forecasting_train.rds"
)

saveRDS(
  testing_data,
  "data/processed/forecasting_test.rds"
)

write_csv(
  selected_data,
  "results/forecasting_selected_period.csv"
)

write_csv(
  training_data,
  "results/forecasting_train.csv"
)

write_csv(
  testing_data,
  "results/forecasting_test.csv"
)

# Visualize the split
split_plot <- ggplot() +
  geom_line(
    data = training_data,
    aes(x = datetime, y = energy_kWh),
    color = "steelblue",
    linewidth = 0.35
  ) +
  geom_line(
    data = testing_data,
    aes(x = datetime, y = energy_kWh),
    color = "darkorange",
    linewidth = 0.35
  ) +
  geom_vline(
    xintercept = min(testing_data$datetime),
    linetype = "dashed"
  ) +
  labs(
    title = "Chronological Forecasting Train-Test Split",
    subtitle = "Blue: training data | Orange: testing data",
    x = "Date",
    y = "Campus energy consumption (kWh)"
  ) +
  theme_minimal()

print(split_plot)

ggsave(
  "plots/forecasting_train_test_split.png",
  plot = split_plot,
  width = 12,
  height = 6,
  dpi = 300
)

cat("\n============================================\n")
cat("STEP 09C COMPLETED SUCCESSFULLY\n")
cat("============================================\n")