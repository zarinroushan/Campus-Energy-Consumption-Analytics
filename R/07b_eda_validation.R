
# ============================================================
# STEP 07B: EDA VALIDATION AND CORRECTIONS
# Campus Energy Consumption Analytics
# ============================================================

library(dplyr)
library(tidyr)
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

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)


# ------------------------------------------------------------
# FUNCTION: DISPLAY PLOT IN RSTUDIO AND SAVE AS PNG
# ------------------------------------------------------------

save_and_show_plot <- function(plot_object, filename,
                               width = 10, height = 6) {

  # Display plot in RStudio Plots pane
  print(plot_object)

  # Save plot as high-resolution PNG
  ggsave(
    filename = file.path(plots_dir, filename),
    plot = plot_object,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )

  cat("Plot displayed in RStudio and saved:", filename, "\n")
}


if (!file.exists(input_file)) {
  stop("Input RDS file not found. Check your project directory.")
}

cat("\nLoading hourly analytical dataset...\n")

data <- readRDS(input_file)

cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")

# ------------------------------------------------------------
# 2. VALIDATE BUILDING ENERGY COLUMNS
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

missing_columns <- setdiff(
  building_columns,
  names(data)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing building columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# Count available building measurements per hour.
data <- data %>%
  mutate(
    available_buildings = rowSums(
      !is.na(across(all_of(building_columns)))
    )
  )

# ------------------------------------------------------------
# 3. CAMPUS COVERAGE SENSITIVITY ANALYSIS
# ------------------------------------------------------------

# These totals sum the building measurements actually
# available in each hour. They are not identical building
# sets when some measurements are missing.

data <- data %>%
  mutate(
    total_5plus = if_else(
      available_buildings >= 5,
      rowSums(
        across(all_of(building_columns)),
        na.rm = TRUE
      ),
      NA_real_
    ),

    total_6plus = if_else(
      available_buildings >= 6,
      rowSums(
        across(all_of(building_columns)),
        na.rm = TRUE
      ),
      NA_real_
    ),

    total_7of7 = if_else(
      available_buildings == 7,
      rowSums(
        across(all_of(building_columns)),
        na.rm = FALSE
      ),
      NA_real_
    )
  )

coverage_sensitivity <- bind_rows(
  data %>%
    summarise(
      threshold = "At least 5 buildings",
      qualifying_hours = sum(available_buildings >= 5),
      mean_hourly_energy_kWh = mean(total_5plus, na.rm = TRUE),
      median_hourly_energy_kWh = median(total_5plus, na.rm = TRUE)
    ),

  data %>%
    summarise(
      threshold = "At least 6 buildings",
      qualifying_hours = sum(available_buildings >= 6),
      mean_hourly_energy_kWh = mean(total_6plus, na.rm = TRUE),
      median_hourly_energy_kWh = median(total_6plus, na.rm = TRUE)
    ),

  data %>%
    summarise(
      threshold = "All 7 buildings",
      qualifying_hours = sum(available_buildings == 7),
      mean_hourly_energy_kWh = mean(total_7of7, na.rm = TRUE),
      median_hourly_energy_kWh = median(total_7of7, na.rm = TRUE)
    )
)

write.csv(
  coverage_sensitivity,
  file.path(results_dir, "campus_coverage_sensitivity.csv"),
  row.names = FALSE
)

cat("\nCAMPUS COVERAGE SENSITIVITY\n")
print(coverage_sensitivity)

# Distribution of available building counts.
building_availability <- data %>%
  count(available_buildings, name = "hours") %>%
  arrange(available_buildings)

write.csv(
  building_availability,
  file.path(results_dir, "building_availability_distribution.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 4. CORRECT OCCUPANCY SUMMARY
# ------------------------------------------------------------

occupancy_columns <- grep(
  "^occupancy_mean_[A-Z]+$",
  names(data),
  value = TRUE
)

if (length(occupancy_columns) == 0) {
  stop("No hourly occupancy mean columns were found.")
}

occupancy_long <- data %>%
  select(all_of(occupancy_columns)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "occupancy_dataset",
    values_to = "hourly_mean_occupancy"
  )

occupancy_summary_corrected <- occupancy_long %>%
  group_by(occupancy_dataset) %>%
  summarise(
    observed_hours = sum(!is.na(hourly_mean_occupancy)),
    missing_hours = sum(is.na(hourly_mean_occupancy)),
    mean_occupancy = mean(
      hourly_mean_occupancy,
      na.rm = TRUE
    ),
    median_occupancy = median(
      hourly_mean_occupancy,
      na.rm = TRUE
    ),
    maximum_hourly_mean = if (
      all(is.na(hourly_mean_occupancy))
    ) NA_real_ else max(
      hourly_mean_occupancy,
      na.rm = TRUE
    ),
    minimum_hourly_mean = if (
      all(is.na(hourly_mean_occupancy))
    ) NA_real_ else min(
      hourly_mean_occupancy,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

write.csv(
  occupancy_summary_corrected,
  file.path(results_dir, "occupancy_summary_corrected.csv"),
  row.names = FALSE
)

cat("\nCORRECTED OCCUPANCY SUMMARY\n")
print(occupancy_summary_corrected)

# ------------------------------------------------------------
# 5. FIX OVERALL ENERGY TREND PLOT
# ------------------------------------------------------------

if (!"date" %in% names(data)) {
  stop("Date column not found.")
}

if (!"Campus_total_energy_kWh" %in% names(data)) {
  stop("Campus_total_energy_kWh column not found.")
}

daily_energy <- data %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(
    daily_average_energy = mean(
      Campus_total_energy_kWh,
      na.rm = TRUE
    ),
    observed_hours = sum(
      !is.na(Campus_total_energy_kWh)
    ),
    .groups = "drop"
  ) %>%
  filter(
    observed_hours >= 18,
    is.finite(daily_average_energy)
  ) %>%
  arrange(date)

# Break the plotted line when consecutive retained
# dates are separated by more than one calendar day.
daily_energy <- daily_energy %>%
  mutate(
    segment = cumsum(
      c(TRUE, diff(as.numeric(date)) > 1)
    )
  )

trend_plot <- ggplot(
  daily_energy,
  aes(
    x = date,
    y = daily_average_energy,
    group = segment
  )
) +
  geom_line(linewidth = 0.35) +
  labs(
    title = "Daily Average Hourly Campus Energy",
    subtitle = "Days with at least 18 observed hourly values; gaps are not connected",
    x = "Date",
    y = "Average hourly energy (kWh)"
  ) +
  theme_minimal()

# Display in RStudio and save as PNG for GitHub

save_and_show_plot(
  trend_plot,
  "overall_energy_trend_corrected.png",
  width = 12,
  height = 6
)

# ------------------------------------------------------------
# 6. CHECK MISSING CAMPUS ENERGY VALUES
# ------------------------------------------------------------

campus_missing_check <- data %>%
  summarise(
    total_hours = n(),
    missing_campus_energy = sum(
      is.na(Campus_total_energy_kWh)
    ),
    available_campus_energy = sum(
      !is.na(Campus_total_energy_kWh)
    ),
    average_available_buildings = mean(
      available_buildings,
      na.rm = TRUE
    )
  )

write.csv(
  campus_missing_check,
  file.path(results_dir, "campus_energy_validation.csv"),
  row.names = FALSE
)

cat("\nCAMPUS ENERGY VALIDATION\n")
print(campus_missing_check)

# ------------------------------------------------------------
# 7. FINAL VALIDATION MESSAGE
# ------------------------------------------------------------

cat("\n============================================\n")
cat("EDA VALIDATION COMPLETED\n")
cat("============================================\n")

cat("\nNew result files:\n")
cat("- campus_coverage_sensitivity.csv\n")
cat("- building_availability_distribution.csv\n")
cat("- occupancy_summary_corrected.csv\n")
cat("- campus_energy_validation.csv\n")

cat("\nNew corrected plot:\n")
cat("- overall_energy_trend_corrected.png\n")

cat("\nOriginal EDA files have not been overwritten.\n")
