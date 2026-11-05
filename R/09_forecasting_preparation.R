
# ============================================================
# STEP 09: FORECASTING DATA PREPARATION
# Campus Energy Consumption Analytics
# ============================================================

library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. PROJECT PATHS
# ------------------------------------------------------------

project_dir <- getwd()

input_file <- file.path(
  project_dir,
  "data", "processed",
  "energy_hourly_with_features.rds"
)

results_dir <- file.path(project_dir, "results")
plots_dir <- file.path(project_dir, "plots")

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Hourly analytical dataset not found.")
}

# ------------------------------------------------------------
# 2. LOAD DATA
# ------------------------------------------------------------

cat("\nLoading hourly analytical dataset...\n")

data <- readRDS(input_file)

cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")

# ------------------------------------------------------------
# 3. VALIDATE REQUIRED COLUMNS
# ------------------------------------------------------------

building_columns <- c(
  "Academic_energy_kWh",
  "Boys_total_energy_kWh",
  "Facilities_energy_kWh",
  "Girls_total_energy_kWh",
  "Lecture_energy_kWh",
  "Library_energy_kWh",
  "Mess_energy_kWh"
)

coverage_columns <- c(
  "Academic_coverage_percent",
  "Boys_main_coverage_percent",
  "Boys_backup_coverage_percent",
  "Facilities_coverage_percent",
  "Girls_main_coverage_percent",
  "Girls_backup_coverage_percent",
  "Lecture_coverage_percent",
  "Library_coverage_percent",
  "Mess_coverage_percent"
)

required_columns <- c(
  "datetime",
  building_columns,
  coverage_columns
)

missing_columns <- setdiff(
  required_columns,
  names(data)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------
# 4. RECREATE STEP 08 COVERAGE FILTER
# ------------------------------------------------------------

# Apply the same 75% hourly meter coverage rule
# used in Step 08.

coverage_vectors <- list(
  data$Academic_coverage_percent,
  
  pmin(
    data$Boys_main_coverage_percent,
    data$Boys_backup_coverage_percent
  ),
  
  data$Facilities_coverage_percent,
  
  pmin(
    data$Girls_main_coverage_percent,
    data$Girls_backup_coverage_percent
  ),
  
  data$Lecture_coverage_percent,
  data$Library_coverage_percent,
  data$Mess_coverage_percent
)

eligible_energy <- as.data.frame(
  Map(
    function(energy, coverage) {
      ifelse(
        !is.na(energy) &
          !is.na(coverage) &
          coverage >= 75,
        energy,
        NA_real_
      )
    },
    data[building_columns],
    coverage_vectors
  )
)

names(eligible_energy) <- building_columns

available_buildings_75 <- rowSums(
  !is.na(eligible_energy)
)

# Require at least five qualifying buildings.

campus_energy_75 <- ifelse(
  available_buildings_75 >= 5,
  rowSums(eligible_energy, na.rm = TRUE),
  NA_real_
)

forecast_data <- data.frame(
  datetime = data$datetime,
  energy_kWh = campus_energy_75,
  available_buildings_75 = available_buildings_75
) %>%
  arrange(datetime)

cat(
  "\nHours meeting the Step 08 coverage rule:",
  sum(!is.na(forecast_data$energy_kWh)),
  "\n"
)

# ------------------------------------------------------------
# 5. FORECASTING DATA SUMMARY
# ------------------------------------------------------------

observed_energy <- forecast_data$energy_kWh[
  !is.na(forecast_data$energy_kWh)
]

forecast_summary <- data.frame(
  total_rows = nrow(forecast_data),
  observed_energy_hours = length(observed_energy),
  missing_energy_hours = sum(is.na(forecast_data$energy_kWh)),
  first_datetime = min(forecast_data$datetime, na.rm = TRUE),
  last_datetime = max(forecast_data$datetime, na.rm = TRUE),
  mean_energy_kWh = mean(observed_energy),
  median_energy_kWh = median(observed_energy),
  maximum_energy_kWh = max(observed_energy)
)

cat("\nFORECASTING DATA SUMMARY\n")
print(forecast_summary)

write.csv(
  forecast_summary,
  file.path(results_dir, "forecasting_data_summary.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 6. CHECK TIMESTAMP INTERVALS
# ------------------------------------------------------------

time_differences <- diff(
  as.numeric(forecast_data$datetime)
) / 3600

interval_summary <- data.frame(
  total_intervals = length(time_differences),
  one_hour_intervals = sum(
    time_differences == 1,
    na.rm = TRUE
  ),
  irregular_intervals = sum(
    time_differences != 1,
    na.rm = TRUE
  ),
  maximum_gap_hours = if (
    length(time_differences) > 0
  ) {
    max(time_differences, na.rm = TRUE)
  } else {
    NA_real_
  }
)

cat("\nTIMESTAMP INTERVAL SUMMARY\n")
print(interval_summary)

write.csv(
  interval_summary,
  file.path(results_dir, "forecasting_interval_summary.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 7. IDENTIFY MISSING ENERGY PERIODS
# ------------------------------------------------------------

missing_periods <- forecast_data %>%
  filter(is.na(energy_kWh))

cat(
  "\nMissing energy observations:",
  nrow(missing_periods),
  "\n"
)

write.csv(
  missing_periods,
  file.path(results_dir, "forecasting_missing_periods.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. VISUALIZE HISTORICAL ENERGY
# ------------------------------------------------------------

forecast_plot <- ggplot(
  forecast_data,
  aes(x = datetime, y = energy_kWh)
) +
  geom_line(
    color = "#3478A8",
    linewidth = 0.35,
    na.rm = TRUE
  ) +
  labs(
    title = "Historical Campus Energy Demand",
    subtitle = "75% meter coverage and at least five buildings",
    x = "Date",
    y = "Hourly energy consumption (kWh)"
  ) +
  theme_minimal(base_size = 12)

print(forecast_plot)

ggsave(
  file.path(plots_dir, "forecasting_historical_energy.png"),
  plot = forecast_plot,
  width = 12,
  height = 6,
  dpi = 300
)

cat("\nStep 09 preparation completed.\n")