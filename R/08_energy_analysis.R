# ============================================================
# STEP 08: DETAILED ENERGY CONSUMPTION ANALYSIS
# Campus Energy Consumption Analytics
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. PROJECT PATHS AND DATA
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

if (!file.exists(input_file)) {
  stop("Hourly analytical dataset not found.")
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
  "hour",
  "day_of_week",
  "month",
  "year",
  building_columns
)

missing_columns <- setdiff(required_columns, names(data))

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------
# 3. APPLY HOURLY METER COVERAGE CHECK
# ------------------------------------------------------------

# A building's hourly energy is accepted only when its
# meter coverage is at least 75%.

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

missing_coverage <- setdiff(coverage_columns, names(data))

if (length(missing_coverage) > 0) {
  stop(
    paste(
      "Missing coverage columns:",
      paste(missing_coverage, collapse = ", ")
    )
  )
}

# For hostel totals, use the lower coverage of the
# main and backup meters as a conservative check.

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

# Keep energy only when coverage is at least 75%.

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

# Count how many buildings meet the coverage requirement.

available_buildings_75 <- rowSums(
  !is.na(eligible_energy)
)

# Create a quality-filtered campus energy estimate
# only when at least five buildings qualify.

campus_energy_75 <- ifelse(
  available_buildings_75 >= 5,
  rowSums(eligible_energy, na.rm = TRUE),
  NA_real_
)

analysis_data <- data %>%
  mutate(
    available_buildings_75 = available_buildings_75,
    Campus_energy_coverage75_kWh = campus_energy_75
  )

if (sum(!is.na(analysis_data$Campus_energy_coverage75_kWh)) == 0) {
  stop("No campus energy observations passed the coverage rule.")
}

cat("\nCoverage filtering completed.\n")
cat(
  "Hours meeting the 5-building, 75% coverage rule:",
  sum(!is.na(analysis_data$Campus_energy_coverage75_kWh)),
  "\n"
)

# ------------------------------------------------------------
# 4. BUILDING-LEVEL ENERGY ANALYSIS
# ------------------------------------------------------------

building_summary <- bind_rows(
  lapply(seq_along(building_columns), function(i) {

    energy <- data[[building_columns[i]]]
    coverage <- coverage_vectors[[i]]

    valid <- !is.na(energy) &
      !is.na(coverage) &
      coverage >= 75

    values <- energy[valid]

    data.frame(
      building = building_columns[i],
      qualifying_hours = length(values),
      mean_hourly_energy_kWh = if (length(values) > 0) {
        mean(values)
      } else {
        NA_real_
      },
      median_hourly_energy_kWh = if (length(values) > 0) {
        median(values)
      } else {
        NA_real_
      },
      standard_deviation = if (length(values) > 1) {
        sd(values)
      } else {
        NA_real_
      },
      percentile_95_kWh = if (length(values) > 0) {
        as.numeric(quantile(values, 0.95))
      } else {
        NA_real_
      },
      maximum_hourly_energy_kWh = if (length(values) > 0) {
        max(values)
      } else {
        NA_real_
      },
      qualifying_coverage_percent =
        length(values) / nrow(data) * 100
    )
  })
) %>%
  arrange(desc(mean_hourly_energy_kWh))

write.csv(
  building_summary,
  file.path(results_dir, "building_energy_analysis.csv"),
  row.names = FALSE
)

cat("\nBUILDING-LEVEL ENERGY ANALYSIS\n")
print(building_summary)

# ------------------------------------------------------------
# 5. HOURLY ENERGY DEMAND PROFILE
# ------------------------------------------------------------

hourly_profile <- analysis_data %>%
  group_by(hour) %>%
  summarise(
    mean_energy_kWh = mean(
      Campus_energy_coverage75_kWh,
      na.rm = TRUE
    ),
    median_energy_kWh = median(
      Campus_energy_coverage75_kWh,
      na.rm = TRUE
    ),
    percentile_95_kWh = if (
      all(is.na(Campus_energy_coverage75_kWh))
    ) NA_real_ else as.numeric(
      quantile(
        Campus_energy_coverage75_kWh,
        0.95,
        na.rm = TRUE
      )
    ),
    observed_hours = sum(
      !is.na(Campus_energy_coverage75_kWh)
    ),
    .groups = "drop"
  )

write.csv(
  hourly_profile,
  file.path(results_dir, "hourly_energy_profile.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 6. WEEKDAY ENERGY ANALYSIS
# ------------------------------------------------------------

weekday_order <- c(
  "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday", "Sunday"
)

weekday_analysis <- analysis_data %>%
  mutate(
    day_of_week = factor(
      as.character(day_of_week),
      levels = weekday_order
    )
  ) %>%
  group_by(day_of_week) %>%
  summarise(
    mean_energy_kWh = mean(
      Campus_energy_coverage75_kWh,
      na.rm = TRUE
    ),
    median_energy_kWh = median(
      Campus_energy_coverage75_kWh,
      na.rm = TRUE
    ),
    observed_hours = sum(
      !is.na(Campus_energy_coverage75_kWh)
    ),
    .groups = "drop"
  )

write.csv(
  weekday_analysis,
  file.path(results_dir, "weekday_energy_analysis.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 7. MONTHLY AND YEARLY ENERGY ANALYSIS
# ------------------------------------------------------------

monthly_analysis <- analysis_data %>%
  group_by(year, month) %>%
  summarise(
    mean_energy_kWh = mean(
      Campus_energy_coverage75_kWh,
      na.rm = TRUE
    ),
    median_energy_kWh = median(
      Campus_energy_coverage75_kWh,
      na.rm = TRUE
    ),
    peak_energy_kWh = if (
      all(is.na(Campus_energy_coverage75_kWh))
    ) NA_real_ else max(
      Campus_energy_coverage75_kWh,
      na.rm = TRUE
    ),
    observed_hours = sum(
      !is.na(Campus_energy_coverage75_kWh)
    ),
    .groups = "drop"
  )

write.csv(
  monthly_analysis,
  file.path(results_dir, "monthly_energy_analysis.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. DAILY ENERGY ANALYSIS
# ------------------------------------------------------------

daily_analysis <- analysis_data %>%
  group_by(date) %>%
  summarise(
    average_hourly_energy_kWh = mean(
      Campus_energy_coverage75_kWh,
      na.rm = TRUE
    ),
    peak_hourly_energy_kWh = if (
      all(is.na(Campus_energy_coverage75_kWh))
    ) NA_real_ else max(
      Campus_energy_coverage75_kWh,
      na.rm = TRUE
    ),
    observed_hours = sum(
      !is.na(Campus_energy_coverage75_kWh)
    ),
    .groups = "drop"
  ) %>%
  filter(
    observed_hours >= 18,
    is.finite(average_hourly_energy_kWh)
  ) %>%
  arrange(date)

write.csv(
  daily_analysis,
  file.path(results_dir, "daily_energy_analysis.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 9. HIGH-DEMAND AND PEAK-HOUR ANALYSIS
# ------------------------------------------------------------

valid_energy <- analysis_data$Campus_energy_coverage75_kWh
valid_energy <- valid_energy[is.finite(valid_energy)]

if (length(valid_energy) == 0) {
  stop("No valid energy values available for peak analysis.")
}

# Use the 95th percentile as a high-demand threshold.
# This identifies the highest 5% of observed hours.

high_demand_threshold <- as.numeric(
  quantile(valid_energy, 0.95)
)

analysis_data <- analysis_data %>%
  mutate(
    high_demand_hour = ifelse(
      !is.na(Campus_energy_coverage75_kWh) &
        Campus_energy_coverage75_kWh >= high_demand_threshold,
      TRUE,
      FALSE
    )
  )

high_demand_hours <- analysis_data %>%
  filter(high_demand_hour) %>%
  select(
    datetime,
    date,
    hour,
    Campus_energy_coverage75_kWh,
    available_buildings_75
  ) %>%
  arrange(desc(Campus_energy_coverage75_kWh))

write.csv(
  high_demand_hours,
  file.path(results_dir, "high_demand_hours.csv"),
  row.names = FALSE
)

top_peak_hours <- high_demand_hours %>%
  slice_head(n = 20)

write.csv(
  top_peak_hours,
  file.path(results_dir, "top_20_peak_hours.csv"),
  row.names = FALSE
)

peak_summary <- data.frame(
  metric = c(
    "High-demand threshold (95th percentile)",
    "Maximum observed hourly energy",
    "Number of high-demand hours",
    "Total qualifying energy observations"
  ),
  value = c(
    high_demand_threshold,
    max(valid_energy),
    nrow(high_demand_hours),
    length(valid_energy)
  )
)

write.csv(
  peak_summary,
  file.path(results_dir, "peak_demand_summary.csv"),
  row.names = FALSE
)

# Identify the hours of the day that most often
# appear in the high-demand group.

peak_hour_distribution <- high_demand_hours %>%
  count(hour, name = "high_demand_count") %>%
  arrange(desc(high_demand_count))

write.csv(
  peak_hour_distribution,
  file.path(results_dir, "peak_hour_distribution.csv"),
  row.names = FALSE
)

cat("\nPEAK DEMAND SUMMARY\n")
print(peak_summary)

cat("\nTOP 20 PEAK HOURS\n")
print(top_peak_hours)

# ------------------------------------------------------------
# 10. DISPLAY AND SAVE PLOTS
# ------------------------------------------------------------

save_and_show_plot <- function(plot_object, filename,
                               width = 10, height = 6) {

  print(plot_object)

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

# Plot 1: Building-level energy comparison

building_plot <- ggplot(
  building_summary,
  aes(
    x = reorder(building, mean_hourly_energy_kWh),
    y = mean_hourly_energy_kWh
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Average Hourly Energy by Building",
    subtitle = "Only hours with at least 75% meter coverage",
    x = "Building",
    y = "Average hourly energy (kWh)"
  ) +
  theme_minimal()

save_and_show_plot(
  building_plot,
  "building_energy_analysis.png"
)

# Plot 2: Hourly demand profile

hourly_plot <- ggplot(
  hourly_profile,
  aes(x = hour, y = mean_energy_kWh)
) +
  geom_line(linewidth = 0.9, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  labs(
    title = "Campus Energy Demand by Hour",
    subtitle = "Based on hours with at least five qualifying buildings",
    x = "Hour of day",
    y = "Average hourly energy (kWh)"
  ) +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal()

save_and_show_plot(
  hourly_plot,
  "detailed_hourly_energy_profile.png"
)

# Plot 3: Weekday comparison

weekday_plot <- ggplot(
  weekday_analysis,
  aes(x = day_of_week, y = mean_energy_kWh)
) +
  geom_col(fill = "darkseagreen4") +
  labs(
    title = "Energy Consumption by Day of Week",
    x = "Day of week",
    y = "Average hourly energy (kWh)"
  ) +
  theme_minimal()

save_and_show_plot(
  weekday_plot,
  "detailed_weekday_energy.png"
)

# Plot 4: Monthly consumption across years

monthly_plot <- ggplot(
  monthly_analysis,
  aes(
    x = month,
    y = mean_energy_kWh,
    color = factor(year),
    group = year
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Monthly Energy Consumption Across Years",
    x = "Month",
    y = "Average hourly energy (kWh)",
    color = "Year"
  ) +
  theme_minimal()

save_and_show_plot(
  monthly_plot,
  "detailed_monthly_energy.png"
)

# Plot 5: High-demand hours by time of day

peak_plot <- ggplot(
  peak_hour_distribution,
  aes(x = hour, y = high_demand_count)
) +
  geom_col(fill = "indianred3") +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Distribution of High-Demand Hours",
    subtitle = "Hours at or above the 95th percentile",
    x = "Hour of day",
    y = "Number of high-demand observations"
  ) +
  theme_minimal()

save_and_show_plot(
  peak_plot,
  "high_demand_hour_distribution.png"
)

# Plot 6: Daily average energy trend

daily_plot <- ggplot(
  daily_analysis,
  aes(x = as.Date(date), y = average_hourly_energy_kWh)
) +
  geom_line(linewidth = 0.4) +
  labs(
    title = "Daily Average Hourly Campus Energy",
    subtitle = "At least 18 qualifying hourly observations per day",
    x = "Date",
    y = "Average hourly energy (kWh)"
  ) +
  theme_minimal()

save_and_show_plot(
  daily_plot,
  "detailed_daily_energy_trend.png",
  width = 12,
  height = 6
)

# ------------------------------------------------------------
# 11. FINAL OUTPUT
# ------------------------------------------------------------

cat("\n============================================\n")
cat("STEP 08 ENERGY ANALYSIS COMPLETED\n")
cat("============================================\n")

cat("\nResults saved in:\n")
cat(results_dir, "\n")

cat("\nPlots saved in:\n")
cat(plots_dir, "\n")

cat("\nReview the generated CSV files and plots.\n")
