
# ============================================================
# STEP 10D: VISUALIZATION REFINEMENT
# Project: Campus Energy Consumption Analytics
# ============================================================

cat("\n============================================\n")
cat("STEP 10D: VISUALIZATION REFINEMENT\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

source("R/plot_utils.R")

if (!exists("save_and_show_plot")) {
  stop("save_and_show_plot() could not be loaded.")
}

dir.create("plots", showWarnings = FALSE, recursive = TRUE)
dir.create("results", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 2. LOAD HOURLY DATA AND RECREATE CAMPUS TARGET
# ------------------------------------------------------------

hourly_data <- readRDS(
  "data/processed/energy_hourly_with_features.rds"
)

if (!"datetime" %in% names(hourly_data)) {
  stop("The hourly dataset does not contain a datetime column.")
}

hourly_data$datetime <- as.POSIXct(
  hourly_data$datetime,
  tz = "Asia/Kolkata"
)

# Energy columns used in the campus estimate
energy_columns <- c(
  "Academic_energy_kWh",
  "Boys_total_energy_kWh",
  "Facilities_energy_kWh",
  "Girls_total_energy_kWh",
  "Lecture_energy_kWh",
  "Library_energy_kWh",
  "Mess_energy_kWh"
)

# Coverage columns used in the campus estimate
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

required_columns <- c(energy_columns, coverage_columns)

missing_columns <- setdiff(
  required_columns,
  names(hourly_data)
)

if (length(missing_columns) > 0) {
  stop(
    "Required columns are missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Hostel coverage is the lower of main and backup coverage
hourly_data <- hourly_data %>%
  mutate(
    Boys_coverage = pmin(
      Boys_main_coverage_percent,
      Boys_backup_coverage_percent,
      na.rm = FALSE
    ),
    Girls_coverage = pmin(
      Girls_main_coverage_percent,
      Girls_backup_coverage_percent,
      na.rm = FALSE
    )
  )

# Apply the 75% coverage eligibility rule
hourly_data <- hourly_data %>%
  mutate(
    Academic_eligible = if_else(
      !is.na(Academic_energy_kWh) &
        Academic_coverage_percent >= 75,
      Academic_energy_kWh, NA_real_
    ),
    Boys_eligible = if_else(
      !is.na(Boys_total_energy_kWh) &
        Boys_coverage >= 75,
      Boys_total_energy_kWh, NA_real_
    ),
    Facilities_eligible = if_else(
      !is.na(Facilities_energy_kWh) &
        Facilities_coverage_percent >= 75,
      Facilities_energy_kWh, NA_real_
    ),
    Girls_eligible = if_else(
      !is.na(Girls_total_energy_kWh) &
        Girls_coverage >= 75,
      Girls_total_energy_kWh, NA_real_
    ),
    Lecture_eligible = if_else(
      !is.na(Lecture_energy_kWh) &
        Lecture_coverage_percent >= 75,
      Lecture_energy_kWh, NA_real_
    ),
    Library_eligible = if_else(
      !is.na(Library_energy_kWh) &
        Library_coverage_percent >= 75,
      Library_energy_kWh, NA_real_
    ),
    Mess_eligible = if_else(
      !is.na(Mess_energy_kWh) &
        Mess_coverage_percent >= 75,
      Mess_energy_kWh, NA_real_
    )
  )

eligible_columns <- c(
  "Academic_eligible",
  "Boys_eligible",
  "Facilities_eligible",
  "Girls_eligible",
  "Lecture_eligible",
  "Library_eligible",
  "Mess_eligible"
)

hourly_data$Eligible_Buildings <- rowSums(
  !is.na(hourly_data[, eligible_columns])
)

hourly_data$Campus_energy_kWh <- rowSums(
  hourly_data[, eligible_columns],
  na.rm = TRUE
)

hourly_data$Campus_energy_kWh[
  hourly_data$Eligible_Buildings < 5
] <- NA_real_

hourly_data <- hourly_data %>%
  filter(!is.na(datetime)) %>%
  arrange(datetime)

cat("Hourly observations:", nrow(hourly_data), "\n")

cat(
  "Eligible campus observations:",
  sum(!is.na(hourly_data$Campus_energy_kWh)),
  "\n"
)

# ------------------------------------------------------------
# 3. DAILY TREND - BREAK LINES ACROSS MISSING DAYS
# ------------------------------------------------------------

daily_data <- hourly_data %>%
  mutate(Date = as.Date(datetime)) %>%
  group_by(Date) %>%
  summarise(
    Average_Hourly_Energy = if (
      sum(!is.na(Campus_energy_kWh)) >= 18
    ) {
      mean(Campus_energy_kWh, na.rm = TRUE)
    } else {
      NA_real_
    },
    Observed_Hours = sum(!is.na(Campus_energy_kWh)),
    .groups = "drop"
  ) %>%
  filter(!is.na(Average_Hourly_Energy)) %>%
  arrange(Date)

if (nrow(daily_data) > 0) {
  
  # Start a new line segment after every missing calendar day
  daily_data$Run_ID <- cumsum(
    c(TRUE, diff(daily_data$Date) != 1)
  )
  
  daily_plot <- ggplot(
    daily_data,
    aes(
      x = Date,
      y = Average_Hourly_Energy,
      group = Run_ID
    )
  ) +
    geom_line(linewidth = 0.35, colour = "black") +
    labs(
      title = "Daily Average Hourly Campus Energy",
      subtitle = paste(
        "At least 18 observed hourly values per day;",
        "gaps are not connected"
      ),
      x = "Date",
      y = "Average hourly energy (kWh)"
    ) +
    theme_minimal(base_size = 12)
  
  save_and_show_plot(
    daily_plot,
    "detailed_daily_energy_trend_refined.png"
  )
  
  write.csv(
    daily_data,
    "results/daily_energy_trend_refined.csv",
    row.names = FALSE
  )
  
} else {
  warning("No qualifying daily observations found.")
}

# ------------------------------------------------------------
# 4. MONTHLY TREND - POINTS ONLY
# ------------------------------------------------------------

monthly_data <- hourly_data %>%
  mutate(
    Year = year(datetime),
    Month = month(datetime),
    Month_Label = factor(
      month.abb[Month],
      levels = month.abb
    )
  ) %>%
  group_by(Year, Month, Month_Label) %>%
  summarise(
    Average_Hourly_Energy = mean(
      Campus_energy_kWh,
      na.rm = TRUE
    ),
    Observed_Hours = sum(!is.na(Campus_energy_kWh)),
    .groups = "drop"
  ) %>%
  filter(
    Observed_Hours > 0,
    is.finite(Average_Hourly_Energy)
  )

monthly_plot <- ggplot(
  monthly_data,
  aes(
    x = Month,
    y = Average_Hourly_Energy,
    colour = factor(Year)
  )
) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Average Hourly Energy Consumption",
    subtitle = "Points shown without connecting incomplete months",
    x = "Month",
    y = "Average hourly energy (kWh)",
    colour = "Year"
  ) +
  theme_minimal(base_size = 12)

save_and_show_plot(
  monthly_plot,
  "detailed_monthly_energy_refined.png"
)

write.csv(
  monthly_data,
  "results/monthly_energy_trend_refined.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 5. LOAD FORECAST PREDICTIONS
#    Preserve the original hourly POSIXct timestamps
# ------------------------------------------------------------

prediction_file <- "results/forecasting_robustness_predictions.csv"

if (!file.exists(prediction_file)) {
  stop("Missing prediction file: ", prediction_file)
}

# Load prediction results
prediction_data <- read.csv(
  prediction_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

cat("\nPREDICTION FILE COLUMNS\n")
print(names(prediction_data))

# Load original forecasting test dataset
original_test_data <- readRDS(
  "data/processed/forecasting_test.rds"
)

# ------------------------------------------------------------
# VALIDATE ROW COUNTS
# ------------------------------------------------------------

if (nrow(prediction_data) != nrow(original_test_data)) {
  stop(
    "Prediction and original test datasets have different row counts."
  )
}

# ------------------------------------------------------------
# VALIDATE ACTUAL ENERGY VALUES
# ------------------------------------------------------------

if (!"actual_kWh" %in% names(prediction_data)) {
  stop("actual_kWh column is missing from prediction data.")
}

if (!"lag_prediction" %in% names(prediction_data)) {
  stop("lag_prediction column is missing from prediction data.")
}

if (!isTRUE(all.equal(
  as.numeric(prediction_data$actual_kWh),
  as.numeric(original_test_data$energy_kWh),
  tolerance = 1e-8
))) {
  stop(
    "Actual energy values do not match the original test dataset."
  )
}

# ------------------------------------------------------------
# RESTORE ORIGINAL HOURLY TIMESTAMPS
# ------------------------------------------------------------

# Use the original POSIXct timestamps instead of parsing
# mixed-format timestamps from the CSV file.

prediction_data$datetime <- original_test_data$datetime

# Prepare plotting columns
prediction_data$Actual <- as.numeric(
  prediction_data$actual_kWh
)

prediction_data$Predicted <- as.numeric(
  prediction_data$lag_prediction
)

# ------------------------------------------------------------
# VALIDATE TIMESTAMP CONTINUITY
# ------------------------------------------------------------

if (anyNA(prediction_data$datetime)) {
  stop("Missing timestamps found in prediction data.")
}

if (any(diff(as.numeric(prediction_data$datetime)) != 3600)) {
  stop("Prediction timestamps are not exactly one hour apart.")
}

if (any(!is.finite(prediction_data$Actual)) ||
    any(!is.finite(prediction_data$Predicted))) {
  stop("Non-finite actual or predicted values found.")
}

# Do not convert datetime to Date here.
# Keep all 1,874 individual hourly timestamps.

cat("\nHOURLY TIMESTAMP VALIDATION\n")

cat(
  "Prediction observations:",
  nrow(prediction_data),
  "\n"
)

cat(
  "Unique calendar dates:",
  length(unique(as.Date(prediction_data$datetime))),
  "\n"
)

cat(
  "First timestamp:",
  as.character(prediction_data$datetime[1]),
  "\n"
)

cat(
  "Last timestamp:",
  as.character(tail(prediction_data$datetime, 1)),
  "\n"
)

cat(
  "All timestamps exactly one hour apart:",
  all(diff(as.numeric(prediction_data$datetime)) == 3600),
  "\n"
)

cat(
  "Datetime class:",
  paste(class(prediction_data$datetime), collapse = ", "),
  "\n"
)

  
# ----------------------------------------------------------
# 6. ACTUAL VS PREDICTED - FULL DATETIME
# ----------------------------------------------------------

prediction_long <- prediction_data %>%
  select(datetime, Actual, Predicted) %>%
  pivot_longer(
    cols = c(Actual, Predicted),
    names_to = "Series",
    values_to = "Energy"
  )

actual_predicted_plot <- ggplot(
  prediction_long,
  aes(
    x = datetime,
    y = Energy,
    colour = Series
  )
) +
  geom_line(linewidth = 0.35, alpha = 0.85) +
  labs(
    title = "Actual vs Predicted Energy Consumption",
    subtitle = "Lag-Based Regression; full hourly timestamps",
    x = "Date and time",
    y = "Energy consumption (kWh)",
    colour = "Series"
  ) +
  theme_minimal(base_size = 12)

save_and_show_plot(
  actual_predicted_plot,
  "actual_vs_predicted_refined.png"
)

# ----------------------------------------------------------
# 7. RESIDUALS OVER TIME - FULL DATETIME
# ----------------------------------------------------------

prediction_data$Residual <-
  prediction_data$Actual - prediction_data$Predicted

residual_plot <- ggplot(
  prediction_data,
  aes(
    x = datetime,
    y = Residual
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "red"
  ) +
  geom_point(
    size = 0.65,
    alpha = 0.55,
    colour = "black"
  ) +
  labs(
    title = "Forecast Residuals Over Time",
    subtitle = "Residual = actual energy minus predicted energy",
    x = "Date and time",
    y = "Residual (kWh)"
  ) +
  theme_minimal(base_size = 12)

save_and_show_plot(
  residual_plot,
  "forecast_residuals_over_time_refined.png"
)

write.csv(
  prediction_data,
  "results/forecast_predictions_refined.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. OUTPUT VERIFICATION
# ------------------------------------------------------------

expected_outputs <- c(
  "plots/detailed_daily_energy_trend_refined.png",
  "plots/detailed_monthly_energy_refined.png",
  "results/daily_energy_trend_refined.csv",
  "results/monthly_energy_trend_refined.csv",
  "plots/actual_vs_predicted_refined.png",
  "plots/forecast_residuals_over_time_refined.png",
  "results/forecast_predictions_refined.csv"
)

output_check <- data.frame(
  File = expected_outputs,
  Exists = file.exists(expected_outputs)
)

cat("\nOUTPUT VERIFICATION\n")
print(output_check)

cat(
  "\nFiles generated:",
  sum(output_check$Exists),
  "out of",
  length(expected_outputs),
  "\n"
)

cat("\n============================================\n")
cat("STEP 10D COMPLETED\n")
cat("============================================\n")