
# ============================================================
# 07_eda.R
# Campus Energy Consumption Analytics
#
# Purpose:
#   1. Load the final hourly analytical dataset
#   2. Validate the dataset
#   3. Generate descriptive statistics
#   4. Analyze overall energy consumption
#   5. Analyze hourly, daily and monthly patterns
#   6. Compare working and non-working days
#   7. Compare building-level consumption
#   8. Explore occupancy-energy relationships
#   9. Visualize data coverage
#  10. Save plots and analytical summaries
# ============================================================


# ============================================================
# 1. Load required packages
# ============================================================

required_packages <- c(
  "ggplot2",
  "dplyr",
  "tidyr"
)

for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

library(ggplot2)
library(dplyr)
library(tidyr)


# ============================================================
# 2. Define project paths
# ============================================================

project_dir <- getwd()

processed_dir <- file.path(
  project_dir,
  "data",
  "processed"
)

results_dir <- file.path(
  project_dir,
  "results"
)

plots_dir <- file.path(
  project_dir,
  "plots"
)

if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}


# ============================================================
# FUNCTION: DISPLAY PLOT IN RSTUDIO AND SAVE AS PNG
# ============================================================

save_and_show_plot <- function(plot_object, filename,
                               width = 10, height = 6) {

  # Display plot in RStudio Plots pane
  print(plot_object)

  # Save high-resolution PNG for GitHub
  ggsave(
    filename = file.path(plots_dir, filename),
    plot = plot_object,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )

  cat("Plot displayed and saved:", filename, "\n")
}


# ============================================================
# 3. Load final analytical dataset
# ============================================================

cat("\nLoading final analytical dataset...\n")

data_file <- file.path(
  processed_dir,
  "energy_hourly_with_features.rds"
)

if (!file.exists(data_file)) {
  stop("Final analytical RDS file was not found.")
}

data <- readRDS(data_file)

cat(
  "Rows:",
  format(nrow(data), big.mark = ","),
  "\n"
)

cat(
  "Columns:",
  ncol(data),
  "\n"
)


# ============================================================
# 4. Validate important columns
# ============================================================

required_columns <- c(
  "datetime",
  "date",
  "hour",
  "day_of_week",
  "month",
  "year",
  "Campus_total_energy_kWh",
  "buildings_available",
  "working_day"
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

data$datetime <- as.POSIXct(
  data$datetime,
  tz = "Asia/Kolkata"
)

data$date <- as.Date(data$date)

data <- data %>%
  arrange(datetime)

if (anyDuplicated(data$datetime) > 0) {
  stop("Duplicate hourly timestamps found.")
}

cat("Dataset validation completed.\n")


# ============================================================
# 5. Define building-level energy columns
# ============================================================

building_columns <- c(
  "Academic_energy_kWh",
  "Boys_total_energy_kWh",
  "Facilities_energy_kWh",
  "Girls_total_energy_kWh",
  "Lecture_energy_kWh",
  "Library_energy_kWh",
  "Mess_energy_kWh"
)

building_columns <- intersect(
  building_columns,
  names(data)
)

if (length(building_columns) == 0) {
  stop("No building-level energy columns were found.")
}


# ============================================================
# 6. Dataset overview
# ============================================================

cat("\n============================================\n")
cat("DATASET OVERVIEW\n")
cat("============================================\n")

overview <- data.frame(
  Metric = c(
    "Total hourly records",
    "Total variables",
    "Start date",
    "End date",
    "Missing campus energy observations",
    "Average available buildings"
  ),

  Value = c(
    nrow(data),
    ncol(data),
    as.character(min(data$date, na.rm = TRUE)),
    as.character(max(data$date, na.rm = TRUE)),
    sum(is.na(data$Campus_total_energy_kWh)),
    round(
      mean(data$buildings_available, na.rm = TRUE),
      2
    )
  )
)

print(overview)

write.csv(
  overview,
  file.path(results_dir, "eda_dataset_overview.csv"),
  row.names = FALSE
)


# ============================================================
# 7. Descriptive statistics
# ============================================================

cat("\n============================================\n")
cat("CAMPUS ENERGY STATISTICS\n")
cat("============================================\n")

campus_energy <- data$Campus_total_energy_kWh

campus_statistics <- data.frame(
  Metric = c(
    "Mean",
    "Median",
    "Minimum",
    "Maximum",
    "Standard deviation",
    "Total observed energy",
    "Available observations"
  ),

  Value = c(
    mean(campus_energy, na.rm = TRUE),
    median(campus_energy, na.rm = TRUE),
    min(campus_energy, na.rm = TRUE),
    max(campus_energy, na.rm = TRUE),
    sd(campus_energy, na.rm = TRUE),
    sum(campus_energy, na.rm = TRUE),
    sum(!is.na(campus_energy))
  )
)

campus_statistics$Value <- round(
  campus_statistics$Value,
  3
)

print(campus_statistics)

write.csv(
  campus_statistics,
  file.path(results_dir, "eda_campus_energy_statistics.csv"),
  row.names = FALSE
)


# ============================================================
# 8. Overall energy consumption trend
# ============================================================

cat("\nCreating overall energy trend...\n")

daily_energy <- data %>%
  group_by(date) %>%
  summarise(
    average_hourly_energy_kWh = mean(
      Campus_total_energy_kWh,
      na.rm = TRUE
    ),

    available_hours = sum(
      !is.na(Campus_total_energy_kWh)
    ),

    .groups = "drop"
  ) %>%
  filter(
    available_hours >= 18
  )

p1 <- ggplot(
  daily_energy,
  aes(
    x = date,
    y = average_hourly_energy_kWh
  )
) +
  geom_line(linewidth = 0.4) +
  labs(
    title = "Daily Average Hourly Campus Energy",
    subtitle = "Days with at least 18 observed hourly values",
    x = "Date",
    y = "Average hourly energy (kWh)"
  ) +
  theme_minimal()

save_and_show_plot(
  p1,
  "overall_energy_trend.png",
  width = 12,
  height = 6
)


# ============================================================
# 9. Hour-of-day consumption pattern
# ============================================================

cat("\nAnalyzing hourly consumption...\n")

hourly_pattern <- data %>%
  group_by(hour) %>%
  summarise(
    average_energy_kWh = mean(
      Campus_total_energy_kWh,
      na.rm = TRUE
    ),

    median_energy_kWh = median(
      Campus_total_energy_kWh,
      na.rm = TRUE
    ),

    observations = sum(
      !is.na(Campus_total_energy_kWh)
    ),

    .groups = "drop"
  )

p2 <- ggplot(
  hourly_pattern,
  aes(
    x = hour,
    y = average_energy_kWh
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Average Energy Consumption by Hour",
    x = "Hour of day",
    y = "Average energy (kWh)"
  ) +
  theme_minimal()

save_and_show_plot(
  p2,
  "hourly_consumption.png",
  width = 10,
  height = 6
)

write.csv(
  hourly_pattern,
  file.path(results_dir, "hourly_consumption_summary.csv"),
  row.names = FALSE
)


# ============================================================
# 10. Day-of-week consumption pattern
# ============================================================

cat("\nAnalyzing day-of-week patterns...\n")

weekday_pattern <- data %>%
  group_by(day_of_week) %>%
  summarise(
    average_energy_kWh = mean(
      Campus_total_energy_kWh,
      na.rm = TRUE
    ),

    observations = sum(
      !is.na(Campus_total_energy_kWh)
    ),

    .groups = "drop"
  )

p3 <- ggplot(
  weekday_pattern,
  aes(
    x = day_of_week,
    y = average_energy_kWh
  )
) +
  geom_col() +
  labs(
    title = "Average Energy Consumption by Day of Week",
    x = "Day of week",
    y = "Average energy (kWh)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

save_and_show_plot(
  p3,
  "weekday_comparison.png",
  width = 10,
  height = 6
)

write.csv(
  weekday_pattern,
  file.path(results_dir, "weekday_consumption_summary.csv"),
  row.names = FALSE
)


# ============================================================
# 11. Working-day vs non-working-day comparison
# ============================================================

cat("\nComparing working and non-working days...\n")

working_pattern <- data %>%
  filter(
    !is.na(working_day)
  ) %>%
  mutate(
    day_type = ifelse(
      working_day == 1,
      "Working Day",
      "Non-working Day"
    )
  ) %>%
  group_by(day_type) %>%
  summarise(
    average_energy_kWh = mean(
      Campus_total_energy_kWh,
      na.rm = TRUE
    ),

    median_energy_kWh = median(
      Campus_total_energy_kWh,
      na.rm = TRUE
    ),

    observations = sum(
      !is.na(Campus_total_energy_kWh)
    ),

    .groups = "drop"
  )

p4 <- ggplot(
  working_pattern,
  aes(
    x = day_type,
    y = average_energy_kWh
  )
) +
  geom_col() +
  labs(
    title = "Working vs Non-working Day Energy",
    x = "Day type",
    y = "Average energy (kWh)"
  ) +
  theme_minimal()

save_and_show_plot(
  p4,
  "working_vs_nonworking.png",
  width = 8,
  height = 6
)

write.csv(
  working_pattern,
  file.path(results_dir, "working_day_comparison.csv"),
  row.names = FALSE
)


# ============================================================
# 12. Monthly consumption pattern
# ============================================================

cat("\nAnalyzing monthly consumption...\n")

monthly_pattern <- data %>%
  group_by(year, month) %>%
  summarise(
    average_energy_kWh = mean(
      Campus_total_energy_kWh,
      na.rm = TRUE
    ),

    observations = sum(
      !is.na(Campus_total_energy_kWh)
    ),

    .groups = "drop"
  )

p5 <- ggplot(
  monthly_pattern,
  aes(
    x = month,
    y = average_energy_kWh,
    color = factor(year),
    group = year
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point() +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Monthly Energy Consumption Pattern",
    x = "Month",
    y = "Average hourly energy (kWh)",
    color = "Year"
  ) +
  theme_minimal()

save_and_show_plot(
  p5,
  "monthly_consumption.png",
  width = 10,
  height = 6
)

write.csv(
  monthly_pattern,
  file.path(results_dir, "monthly_consumption_summary.csv"),
  row.names = FALSE
)


# ============================================================
# 13. Building-level energy comparison
# ============================================================

cat("\nComparing building-level energy...\n")

building_data <- data %>%
  select(
    all_of(building_columns)
  )

building_summary <- data.frame(
  building = building_columns,

  mean_hourly_energy_kWh = sapply(
    building_data,
    function(x) mean(x, na.rm = TRUE)
  ),

  median_hourly_energy_kWh = sapply(
    building_data,
    function(x) median(x, na.rm = TRUE)
  ),

  maximum_hourly_energy_kWh = sapply(
    building_data,
    function(x) max(x, na.rm = TRUE)
  ),

  available_hours = sapply(
    building_data,
    function(x) sum(!is.na(x))
  )
)

building_summary$building <- gsub(
  "_energy_kWh",
  "",
  building_summary$building
)

building_summary$mean_hourly_energy_kWh <- round(
  building_summary$mean_hourly_energy_kWh,
  3
)

building_summary <- building_summary %>%
  arrange(desc(mean_hourly_energy_kWh))

p6 <- ggplot(
  building_summary,
  aes(
    x = reorder(
      building,
      mean_hourly_energy_kWh
    ),
    y = mean_hourly_energy_kWh
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Hourly Energy by Building",
    x = "Building",
    y = "Average hourly energy (kWh)"
  ) +
  theme_minimal()

save_and_show_plot(
  p6,
  "building_comparison.png",
  width = 10,
  height = 6
)

write.csv(
  building_summary,
  file.path(results_dir, "building_energy_summary.csv"),
  row.names = FALSE
)


# ============================================================
# 14. Occupancy patterns
# ============================================================

cat("\nAnalyzing occupancy patterns...\n")

occupancy_columns <- grep(
  "^occupancy_mean_[A-Z]+$",
  names(data),
  value = TRUE
)

if (length(occupancy_columns) > 0) {

  occupancy_data <- data %>%
    select(
      datetime,
      all_of(occupancy_columns)
    ) %>%
    pivot_longer(
      cols = all_of(occupancy_columns),
      names_to = "occupancy_dataset",
      values_to = "mean_occupancy"
    )

  occupancy_summary <- occupancy_data %>%
    group_by(occupancy_dataset) %>%
    summarise(
      mean_occupancy = mean(
        mean_occupancy,
        na.rm = TRUE
      ),

      maximum_occupancy = max(
        mean_occupancy,
        na.rm = TRUE
      ),

      observations = sum(
        !is.na(mean_occupancy)
      ),

      .groups = "drop"
    )

  p7 <- ggplot(
    occupancy_summary,
    aes(
      x = reorder(
        occupancy_dataset,
        mean_occupancy
      ),
      y = mean_occupancy
    )
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Average Occupancy by Dataset",
      x = "Occupancy dataset",
      y = "Average occupancy"
    ) +
    theme_minimal()

  save_and_show_plot(
    p7,
    "occupancy_patterns.png",
    width = 10,
    height = 6
  )

  write.csv(
    occupancy_summary,
    file.path(results_dir, "occupancy_summary.csv"),
    row.names = FALSE
  )

  # Overall campus energy vs occupancy datasets.
  # This is an exploratory association, not a building-specific
  # relationship because occupancy codes are not yet mapped.

  relationship_data <- data %>%
    select(
      Campus_total_energy_kWh,
      all_of(occupancy_columns)
    ) %>%
    pivot_longer(
      cols = all_of(occupancy_columns),
      names_to = "occupancy_dataset",
      values_to = "mean_occupancy"
    ) %>%
    filter(
      !is.na(Campus_total_energy_kWh),
      !is.na(mean_occupancy)
    )

  p8 <- ggplot(
    relationship_data,
    aes(
      x = mean_occupancy,
      y = Campus_total_energy_kWh
    )
  ) +
    geom_point(alpha = 0.2) +
    geom_smooth(
      method = "lm",
      se = FALSE
    ) +
    facet_wrap(
      ~ occupancy_dataset,
      scales = "free_x"
    ) +
    labs(
      title = "Occupancy and Campus Energy Relationship",
      subtitle = "Exploratory association; occupancy codes are not building-mapped",
      x = "Hourly mean occupancy",
      y = "Campus energy (kWh)"
    ) +
    theme_minimal()

  save_and_show_plot(
    p8,
    "occupancy_energy_relationship.png",
    width = 12,
    height = 8
  )

} else {
  cat("No hourly mean occupancy columns found.\n")
}


# ============================================================
# 15. Energy data coverage visualization
# ============================================================

cat("\nVisualizing energy data coverage...\n")

coverage_columns <- grep(
  "_coverage_percent$",
  names(data),
  value = TRUE
)

if (length(coverage_columns) > 0) {

  coverage_data <- data %>%
    select(
      datetime,
      all_of(coverage_columns)
    ) %>%
    pivot_longer(
      cols = all_of(coverage_columns),
      names_to = "meter",
      values_to = "coverage_percent"
    )

  p9 <- ggplot(
    coverage_data,
    aes(
      x = datetime,
      y = meter,
      fill = coverage_percent
    )
  ) +
    geom_tile() +
    labs(
      title = "Hourly Energy Data Coverage",
      x = "Date",
      y = "Energy meter",
      fill = "Coverage (%)"
    ) +
    theme_minimal()

  save_and_show_plot(
    p9,
    "energy_data_coverage.png",
    width = 13,
    height = 7
  )

  coverage_summary <- data.frame(
    meter = coverage_columns,

    average_coverage_percent = sapply(
      data[coverage_columns],
      function(x) mean(x, na.rm = TRUE)
    ),

    hours_below_75_percent = sapply(
      data[coverage_columns],
      function(x) sum(x < 75, na.rm = TRUE)
    )
  )

  write.csv(
    coverage_summary,
    file.path(results_dir, "eda_coverage_summary.csv"),
    row.names = FALSE
  )
}


# ============================================================
# 16. Save EDA results
# ============================================================

cat("\n============================================\n")
cat("EDA STAGE COMPLETED\n")
cat("============================================\n")

cat("\nPlots directory:\n")
cat(plots_dir, "\n")

cat("\nResults directory:\n")
cat(results_dir, "\n")

cat("\nReview all generated plots and CSV files.\n")
