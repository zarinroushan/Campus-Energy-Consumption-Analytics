
# ============================================================
# STEP 09A: FORECASTING MISSING DATA DIAGNOSTICS
# Campus Energy Consumption Analytics
# ============================================================

library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------

project_dir <- getwd()

input_file <- file.path(
  project_dir,
  "data", "processed",
  "energy_hourly_with_features.rds"
)

results_dir <- file.path(project_dir, "results")
plots_dir <- file.path(project_dir, "plots")

data <- readRDS(input_file)

# ------------------------------------------------------------
# 2. RECREATE THE STEP 08 FILTERED TARGET
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

coverage_vectors <- list(
  data$Academic_coverage_percent,
  pmin(data$Boys_main_coverage_percent,
       data$Boys_backup_coverage_percent),
  data$Facilities_coverage_percent,
  pmin(data$Girls_main_coverage_percent,
       data$Girls_backup_coverage_percent),
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

available_buildings <- rowSums(!is.na(eligible_energy))

forecast_data <- data.frame(
  datetime = data$datetime,
  energy_kWh = ifelse(
    available_buildings >= 5,
    rowSums(eligible_energy, na.rm = TRUE),
    NA_real_
  )
) %>%
  arrange(datetime)

# ------------------------------------------------------------
# 3. IDENTIFY CONSECUTIVE MISSING PERIODS
# ------------------------------------------------------------

missing_flag <- is.na(forecast_data$energy_kWh)

run_id <- cumsum(
  c(TRUE, diff(as.integer(missing_flag)) != 0)
)

runs <- data.frame(
  run_id = run_id,
  missing = missing_flag,
  datetime = forecast_data$datetime
) %>%
  group_by(run_id, missing) %>%
  summarise(
    start_datetime = min(datetime),
    end_datetime = max(datetime),
    duration_hours = n(),
    .groups = "drop"
  )

missing_runs <- runs %>%
  filter(missing) %>%
  arrange(desc(duration_hours))

cat("\nMISSING PERIOD SUMMARY\n")
cat("Number of missing periods:", nrow(missing_runs), "\n")
cat(
  "Longest missing period:",
  if (nrow(missing_runs) > 0) {
    max(missing_runs$duration_hours)
  } else {
    0
  },
  "hours\n"
)

print(head(missing_runs, 15))

write.csv(
  missing_runs,
  file.path(results_dir, "forecasting_missing_runs.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 4. MISSING OBSERVATIONS BY YEAR AND MONTH
# ------------------------------------------------------------

monthly_summary <- forecast_data %>%
  mutate(
    year = format(datetime, "%Y"),
    month = format(datetime, "%m")
  ) %>%
  group_by(year, month) %>%
  summarise(
    total_hours = n(),
    observed_hours = sum(!is.na(energy_kWh)),
    missing_hours = sum(is.na(energy_kWh)),
    missing_percent = round(
      missing_hours / total_hours * 100,
      2
    ),
    .groups = "drop"
  )

cat("\nMONTHLY MISSING DATA SUMMARY\n")
print(monthly_summary, n = 30)

write.csv(
  monthly_summary,
  file.path(results_dir, "forecasting_monthly_missing_summary.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 5. VISUALIZE MONTHLY DATA AVAILABILITY
# ------------------------------------------------------------

availability_plot <- ggplot(
  monthly_summary,
  aes(
    x = month,
    y = year,
    fill = missing_percent
  )
) +
  geom_tile(color = "white") +
  scale_x_discrete(
    breaks = sprintf("%02d", 1:12)
  ) +
  labs(
    title = "Monthly Missing Energy Data",
    subtitle = "Percentage of hourly observations missing",
    x = "Month",
    y = "Year",
    fill = "Missing (%)"
  ) +
  theme_minimal()

print(availability_plot)

ggsave(
  file.path(plots_dir, "forecasting_missing_data_heatmap.png"),
  plot = availability_plot,
  width = 11,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 6. CHECK TIMESTAMP DUPLICATES
# ------------------------------------------------------------

duplicate_count <- sum(
  duplicated(forecast_data$datetime)
)

cat(
  "\nDuplicate timestamps:",
  duplicate_count,
  "\n"
)

# ------------------------------------------------------------
# 7. FINAL SUMMARY
# ------------------------------------------------------------

cat("\n============================================\n")
cat("STEP 09A DIAGNOSTICS COMPLETED\n")
cat("============================================\n")