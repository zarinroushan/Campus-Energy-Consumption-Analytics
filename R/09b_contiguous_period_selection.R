
# ============================================================
# STEP 09B: IDENTIFY CONTINUOUS FORECASTING PERIODS
# ============================================================

library(dplyr)
library(lubridate)
library(readr)

cat("\nLoading hourly analytical dataset...\n")

data <- readRDS("data/processed/energy_hourly_with_features.rds")

# Energy columns
building_cols <- c(
  "Academic_energy_kWh",
  "Boys_total_energy_kWh",
  "Facilities_energy_kWh",
  "Girls_total_energy_kWh",
  "Lecture_energy_kWh",
  "Library_energy_kWh",
  "Mess_energy_kWh"
)

# Actual coverage column names from your RDS
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

energy_matrix <- as.matrix(forecast_data[, building_cols])

# Apply the Step 08 coverage rule
eligible_matrix <- !is.na(energy_matrix) &
  !is.na(coverage_matrix) &
  coverage_matrix >= 75

eligible_energy <- energy_matrix
eligible_energy[!eligible_matrix] <- NA_real_

valid_building_count <- rowSums(!is.na(eligible_energy))

forecast_data$energy_kWh <- ifelse(
  valid_building_count >= 5,
  rowSums(eligible_energy, na.rm = TRUE),
  NA_real_
)

# Sort chronologically
forecast_data <- forecast_data %>%
  arrange(datetime)

# Identify consecutive observed and missing runs
observed_flag <- !is.na(forecast_data$energy_kWh)

run_id <- cumsum(
  c(TRUE, diff(as.integer(observed_flag)) != 0)
)

runs <- data.frame(
  run_id = run_id,
  observed = observed_flag,
  datetime = forecast_data$datetime
) %>%
  group_by(run_id, observed) %>%
  summarise(
    start_datetime = min(datetime),
    end_datetime = max(datetime),
    duration_hours = n(),
    .groups = "drop"
  )

# Extract continuous observed periods
observed_runs <- runs %>%
  filter(observed) %>%
  arrange(desc(duration_hours)) %>%
  mutate(
    duration_days = round(duration_hours / 24, 2),
    start_date = as.Date(start_datetime),
    end_date = as.Date(end_datetime)
  )

# Save results
write_csv(
  observed_runs,
  "results/forecasting_contiguous_observed_runs.csv"
)

cat("\n============================================\n")
cat("CONTINUOUS OBSERVED PERIOD SUMMARY\n")
cat("============================================\n")

cat("Number of continuous observed periods:",
    nrow(observed_runs), "\n")

cat("Longest continuous period:",
    max(observed_runs$duration_hours), "hours\n")

cat("Longest period in days:",
    round(max(observed_runs$duration_hours) / 24, 2), "\n")

cat("\nTOP 20 CONTINUOUS OBSERVED PERIODS\n")

print(
  observed_runs %>%
    select(
      start_datetime,
      end_datetime,
      duration_hours,
      duration_days
    ) %>%
    slice_head(n = 20),
  n = 20
)

cat("\nPERIODS LONGER THAN 30 DAYS\n")

print(
  observed_runs %>%
    filter(duration_hours >= 24 * 30) %>%
    select(
      start_datetime,
      end_datetime,
      duration_hours,
      duration_days
    ),
  n = Inf
)

# Monthly summary
monthly_summary <- forecast_data %>%
  mutate(
    year = year(datetime),
    month = month(datetime)
  ) %>%
  group_by(year, month) %>%
  summarise(
    total_hours = n(),
    observed_hours = sum(!is.na(energy_kWh)),
    missing_hours = sum(is.na(energy_kWh)),
    missing_percent = round(
      100 * missing_hours / total_hours, 2
    ),
    .groups = "drop"
  )

cat("\nFULL MONTHLY MISSING DATA SUMMARY\n")
print(monthly_summary, n = Inf)

cat("\n============================================\n")
cat("STEP 09B COMPLETED\n")
cat("============================================\n")