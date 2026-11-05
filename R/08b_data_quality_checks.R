# ============================================================
# STEP 08B: DATA QUALITY CHECKS
# Campus Energy Consumption Analytics
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. PROJECT PATHS AND DATA LOADING
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
  stop("Hourly analytical RDS file was not found.")
}

cat("\nLoading hourly analytical dataset...\n")

data <- readRDS(input_file)

cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")

# ------------------------------------------------------------
# 2. VALIDATE REQUIRED COLUMNS
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

required_columns <- c(
  "datetime",
  "date",
  "Campus_total_energy_kWh",
  building_columns
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
# 3. CONVERT BUILDING DATA INTO LONG FORMAT
# ------------------------------------------------------------

building_long <- data %>%
  select(
    datetime,
    date,
    all_of(building_columns)
  ) %>%
  pivot_longer(
    cols = all_of(building_columns),
    names_to = "building",
    values_to = "energy_kWh"
  )

# ------------------------------------------------------------
# 4. CHECK ZERO, NEGATIVE AND MISSING VALUES
# ------------------------------------------------------------

basic_quality_summary <- building_long %>%
  group_by(building) %>%
  summarise(
    total_hours = n(),

    observed_hours = sum(!is.na(energy_kWh)),

    missing_hours = sum(is.na(energy_kWh)),

    zero_readings = sum(
      energy_kWh == 0,
      na.rm = TRUE
    ),

    negative_readings = sum(
      energy_kWh < 0,
      na.rm = TRUE
    ),

    mean_energy_kWh = mean(
      energy_kWh,
      na.rm = TRUE
    ),

    median_energy_kWh = median(
      energy_kWh,
      na.rm = TRUE
    ),

    maximum_energy_kWh = if (
      all(is.na(energy_kWh))
    ) NA_real_ else max(
      energy_kWh,
      na.rm = TRUE
    ),

    .groups = "drop"
  )

write.csv(
  basic_quality_summary,
  file.path(results_dir, "building_basic_quality_summary.csv"),
  row.names = FALSE
)

cat("\nBASIC BUILDING DATA QUALITY SUMMARY\n")
print(basic_quality_summary)

# ------------------------------------------------------------
# 5. INVESTIGATE ZERO ENERGY READINGS
# ------------------------------------------------------------

zero_readings <- building_long %>%
  filter(
    !is.na(energy_kWh),
    energy_kWh == 0
  ) %>%
  arrange(building, datetime)

zero_reading_summary <- zero_readings %>%
  group_by(building) %>%
  summarise(
    zero_reading_count = n(),
    first_zero_datetime = min(datetime),
    last_zero_datetime = max(datetime),
    .groups = "drop"
  )

write.csv(
  zero_reading_summary,
  file.path(results_dir, "building_zero_reading_summary.csv"),
  row.names = FALSE
)

write.csv(
  zero_readings,
  file.path(results_dir, "building_zero_reading_details.csv"),
  row.names = FALSE
)

cat("\nZERO READING SUMMARY\n")
print(zero_reading_summary)

# ------------------------------------------------------------
# 6. IDENTIFY STATISTICAL OUTLIERS
# ------------------------------------------------------------

# Use the IQR method separately for each building.
#
# Special handling:
# Lecture has a large number of zero readings.
# Its outlier thresholds will therefore be calculated
# using positive readings only.
#
# Zero readings are NOT deleted from the original data.

outlier_reference <- building_long %>%
  filter(!is.na(energy_kWh)) %>%
  filter(
    building != "Lecture_energy_kWh" |
      energy_kWh > 0
  )

outlier_thresholds <- outlier_reference %>%
  group_by(building) %>%
  summarise(
    observations_used = n(),

    threshold_basis = if (
      first(building) == "Lecture_energy_kWh"
    ) {
      "Positive readings only"
    } else {
      "All observed readings"
    },

    q1 = quantile(
      energy_kWh,
      0.25,
      na.rm = TRUE
    ),

    median_energy = median(
      energy_kWh,
      na.rm = TRUE
    ),

    q3 = quantile(
      energy_kWh,
      0.75,
      na.rm = TRUE
    ),

    iqr = IQR(
      energy_kWh,
      na.rm = TRUE
    ),

    upper_outlier_limit = q3 + 1.5 * iqr,

    extreme_outlier_limit = q3 + 3 * iqr,

    .groups = "drop"
  )

flagged_outliers <- outlier_reference %>%
  left_join(
    outlier_thresholds,
    by = "building"
  ) %>%
  filter(
    energy_kWh > extreme_outlier_limit
  ) %>%
  mutate(
    outlier_type = "Extreme high value"
  ) %>%
  arrange(desc(energy_kWh))

write.csv(
  outlier_thresholds,
  file.path(results_dir, "building_outlier_thresholds.csv"),
  row.names = FALSE
)

write.csv(
  flagged_outliers,
  file.path(results_dir, "building_extreme_outliers.csv"),
  row.names = FALSE
)

cat("\nOUTLIER THRESHOLDS\n")
print(outlier_thresholds)

cat("\nEXTREME OUTLIER COUNTS\n")

print(
  flagged_outliers %>%
    count(
      building,
      name = "extreme_outlier_count"
    )
)

cat("\nTOP 20 EXTREME READINGS\n")

print(
  flagged_outliers %>%
    select(
      building,
      datetime,
      energy_kWh,
      extreme_outlier_limit
    ) %>%
    head(20)
)

# ------------------------------------------------------------
# 7. INVESTIGATE CAMPUS TOTAL COMPARABILITY
# ------------------------------------------------------------

data <- data %>%
  mutate(
    available_buildings = rowSums(
      !is.na(across(all_of(building_columns)))
    ),

    campus_total_7of7 = if_else(
      available_buildings == 7,
      rowSums(
        across(all_of(building_columns)),
        na.rm = FALSE
      ),
      NA_real_
    )
  )

campus_comparability <- data %>%
  summarise(
    total_hours = n(),

    hours_with_5_or_more_buildings =
      sum(available_buildings >= 5),

    hours_with_6_or_more_buildings =
      sum(available_buildings >= 6),

    hours_with_all_7_buildings =
      sum(available_buildings == 7),

    mean_existing_campus_total_kWh =
      mean(Campus_total_energy_kWh, na.rm = TRUE),

    mean_7_building_total_kWh =
      mean(campus_total_7of7, na.rm = TRUE),

    .groups = "drop"
  )

write.csv(
  campus_comparability,
  file.path(results_dir, "campus_total_comparability.csv"),
  row.names = FALSE
)

cat("\nCAMPUS TOTAL COMPARABILITY\n")
print(campus_comparability)

# ------------------------------------------------------------
# 8. PLOT: ZERO READINGS BY BUILDING
# ------------------------------------------------------------

zero_plot_data <- basic_quality_summary %>%
  mutate(
    building = gsub(
      "_energy_kWh",
      "",
      building
    )
  )

zero_plot <- ggplot(
  zero_plot_data,
  aes(
    x = reorder(building, zero_readings),
    y = zero_readings
  )
) +
  geom_col(fill = "#D65A5A") +
  coord_flip() +
  labs(
    title = "Zero Energy Readings by Building",
    subtitle = "Count of hourly readings equal to zero",
    x = "Building",
    y = "Number of zero readings"
  ) +
  theme_minimal()

print(zero_plot)

ggsave(
  file.path(plots_dir, "building_zero_readings.png"),
  zero_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 9. PLOT: BUILDING ENERGY DISTRIBUTIONS
# ------------------------------------------------------------

distribution_plot <- ggplot(
  building_long %>%
    filter(
      !is.na(energy_kWh),
      energy_kWh >= 0
    ),
  aes(
    x = building,
    y = energy_kWh
  )
) +
  geom_boxplot(
    fill = "#5B9BD5",
    outlier.alpha = 0.25
  ) +
  coord_flip() +
  labs(
    title = "Hourly Energy Distribution by Building",
    subtitle = "Boxplots help identify unusually high observations",
    x = "Building",
    y = "Hourly energy (kWh)"
  ) +
  theme_minimal()

print(distribution_plot)

ggsave(
  file.path(plots_dir, "building_energy_distributions.png"),
  distribution_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 10. PLOT: EXTREME OUTLIER COUNTS
# ------------------------------------------------------------

outlier_count_plot <- flagged_outliers %>%
  count(building, name = "extreme_outlier_count") %>%
  ggplot(
    aes(
      x = reorder(building, extreme_outlier_count),
      y = extreme_outlier_count
    )
  ) +
  geom_col(fill = "#E39B43") +
  coord_flip() +
  labs(
    title = "Extreme High Readings by Building",
    subtitle = "Values above Q3 + 3 × IQR",
    x = "Building",
    y = "Number of flagged readings"
  ) +
  theme_minimal()

print(outlier_count_plot)

ggsave(
  file.path(plots_dir, "building_extreme_outliers.png"),
  outlier_count_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 11. FINAL OUTPUT
# ------------------------------------------------------------

cat("\n============================================\n")
cat("STEP 08B DATA QUALITY CHECKS COMPLETED\n")
cat("============================================\n")

cat("\nNew result files:\n")
cat("- building_basic_quality_summary.csv\n")
cat("- building_zero_reading_summary.csv\n")
cat("- building_zero_reading_details.csv\n")
cat("- building_outlier_thresholds.csv\n")
cat("- building_extreme_outliers.csv\n")
cat("- campus_total_comparability.csv\n")

cat("\nNew plots:\n")
cat("- building_zero_readings.png\n")
cat("- building_energy_distributions.png\n")
cat("- building_extreme_outliers.png\n")

cat("\nNo original energy observations were modified or deleted.\n")
