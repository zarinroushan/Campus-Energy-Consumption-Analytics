# ============================================================
# 05_hourly_aggregation.R
# Campus Energy Consumption Analytics
#
# Purpose:
#   1. Load cleaned minute-level energy data
#   2. Aggregate power to hourly energy
#   3. Calculate data coverage for each building
#   4. Preserve missing periods instead of inventing values
#   5. Save hourly analytical dataset
# ============================================================


# ============================================================
# 1. Project paths
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

if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}


# ============================================================
# 2. Load cleaned minute-level data
# ============================================================

rds_file <- file.path(
  processed_dir,
  "energy_clean_minute.rds"
)

cat("Loading cleaned minute-level dataset...\n")

energy <- readRDS(rds_file)

cat(
  "Rows loaded:",
  format(nrow(energy), big.mark = ","),
  "\n"
)


# ============================================================
# 3. Define building power columns
# ============================================================

building_columns <- c(
  "Academic",
  "Boys_main",
  "Boys_backup",
  "Facilities",
  "Girls_main",
  "Girls_backup",
  "Lecture",
  "Library",
  "Mess"
)


# ============================================================
# 4. Create hour identifier
# ============================================================

cat("Creating hourly groups...\n")

energy$hour_datetime <- as.POSIXct(
  format(
    energy$datetime,
    "%Y-%m-%d %H:00:00",
    tz = "Asia/Kolkata"
  ),
  tz = "Asia/Kolkata"
)


# ============================================================
# 5. Function for hourly aggregation
# ============================================================

aggregate_hourly <- function(data, column_name) {
  
  power <- data[[column_name]]
  
  valid <- !is.na(power)
  
  # ----------------------------------------------------------
  # Energy:
  # One minute power reading (W) × 1/60 hour = Wh
  # ----------------------------------------------------------
  
  energy_wh <- ifelse(
    valid,
    power / 60,
    NA
  )
  
  # ----------------------------------------------------------
  # Aggregate energy by hour
  # ----------------------------------------------------------
  
  hourly_energy <- aggregate(
    energy_wh,
    by = list(
      datetime = data$hour_datetime
    ),
    FUN = function(x) {
      
      if (all(is.na(x))) {
        return(NA_real_)
      }
      
      sum(
        x,
        na.rm = TRUE
      )
    }
  )
  
  names(hourly_energy)[2] <- paste0(
    column_name,
    "_energy_Wh"
  )
  
  
  # ----------------------------------------------------------
  # Count valid one-minute readings
  # ----------------------------------------------------------
  
  valid_count <- aggregate(
    valid,
    by = list(
      datetime = data$hour_datetime
    ),
    FUN = sum
  )
  
  names(valid_count)[2] <- paste0(
    column_name,
    "_valid_minutes"
  )
  
  
  # ----------------------------------------------------------
  # Coverage percentage
  # ----------------------------------------------------------
  
  valid_count[[2]] <- round(
    (
      valid_count[[2]] / 60
    ) * 100,
    2
  )
  
  names(valid_count)[2] <- paste0(
    column_name,
    "_coverage_percent"
  )
  
  
  # ----------------------------------------------------------
  # Average observed power
  # ----------------------------------------------------------
  
  average_power <- aggregate(
    power,
    by = list(
      datetime = data$hour_datetime
    ),
    FUN = function(x) {
      
      if (all(is.na(x))) {
        return(NA_real_)
      }
      
      mean(
        x,
        na.rm = TRUE
      )
    }
  )
  
  names(average_power)[2] <- paste0(
    column_name,
    "_average_power_W"
  )
  
  
  # ----------------------------------------------------------
  # Merge the three results
  # ----------------------------------------------------------
  
  result <- merge(
    hourly_energy,
    valid_count,
    by = "datetime"
  )
  
  result <- merge(
    result,
    average_power,
    by = "datetime"
  )
  
  return(result)
}


# ============================================================
# 6. Aggregate every meter
# ============================================================

cat("\nAggregating building meters...\n")

hourly_data <- NULL

for (column in building_columns) {
  
  cat(
    "Processing:",
    column,
    "\n"
  )
  
  current_result <- aggregate_hourly(
    energy,
    column
  )
  
  if (is.null(hourly_data)) {
    
    hourly_data <- current_result
    
  } else {
    
    hourly_data <- merge(
      hourly_data,
      current_result,
      by = "datetime",
      all = TRUE
    )
  }
}


# ============================================================
# 7. Sort chronologically
# ============================================================

hourly_data <- hourly_data[
  order(hourly_data$datetime),
]


# ============================================================
# 8. Convert Wh to kWh
# ============================================================

for (column in building_columns) {
  
  energy_column <- paste0(
    column,
    "_energy_Wh"
  )
  
  kwh_column <- paste0(
    column,
    "_energy_kWh"
  )
  
  hourly_data[[kwh_column]] <- round(
    hourly_data[[energy_column]] / 1000,
    4
  )
}


# ============================================================
# 9. Create dormitory totals
# ============================================================

# Boys dormitory:
# mains + backup

hourly_data$Boys_total_energy_kWh <- ifelse(
  !is.na(hourly_data$Boys_main_energy_kWh) &
    !is.na(hourly_data$Boys_backup_energy_kWh),
  
  hourly_data$Boys_main_energy_kWh +
    hourly_data$Boys_backup_energy_kWh,
  
  NA
)


# Girls dormitory:
# mains + backup

hourly_data$Girls_total_energy_kWh <- ifelse(
  !is.na(hourly_data$Girls_main_energy_kWh) &
    !is.na(hourly_data$Girls_backup_energy_kWh),
  
  hourly_data$Girls_main_energy_kWh +
    hourly_data$Girls_backup_energy_kWh,
  
  NA
)


# ============================================================
# 10. Create campus total carefully
# ============================================================

campus_columns <- c(
  "Academic_energy_kWh",
  "Boys_total_energy_kWh",
  "Facilities_energy_kWh",
  "Girls_total_energy_kWh",
  "Lecture_energy_kWh",
  "Library_energy_kWh",
  "Mess_energy_kWh"
)


# Number of building measurements available

hourly_data$buildings_available <- rowSums(
  !is.na(
    hourly_data[
      campus_columns
    ]
  )
)


# Campus total only when at least 5 of 7
# building-level measurements are available.
#
# We keep the number of available buildings so
# this decision is transparent.

hourly_data$Campus_total_energy_kWh <- ifelse(
  
  hourly_data$buildings_available >= 5,
  
  rowSums(
    hourly_data[
      campus_columns
    ],
    na.rm = TRUE
  ),
  
  NA
)


# ============================================================
# 11. Add calendar/time features
# ============================================================

hourly_data$date <- as.Date(
  hourly_data$datetime,
  tz = "Asia/Kolkata"
)

hourly_data$hour <- as.integer(
  format(
    hourly_data$datetime,
    "%H"
  )
)

hourly_data$day_of_week <- weekdays(
  hourly_data$datetime
)

hourly_data$month <- as.integer(
  format(
    hourly_data$datetime,
    "%m"
  )
)

hourly_data$year <- as.integer(
  format(
    hourly_data$datetime,
    "%Y"
  )
)


# ============================================================
# 12. Reorder important columns
# ============================================================

important_columns <- c(
  "datetime",
  "date",
  "year",
  "month",
  "day_of_week",
  "hour",
  
  "Academic_energy_kWh",
  
  "Boys_main_energy_kWh",
  "Boys_backup_energy_kWh",
  "Boys_total_energy_kWh",
  
  "Facilities_energy_kWh",
  
  "Girls_main_energy_kWh",
  "Girls_backup_energy_kWh",
  "Girls_total_energy_kWh",
  
  "Lecture_energy_kWh",
  "Library_energy_kWh",
  "Mess_energy_kWh",
  
  "Campus_total_energy_kWh",
  "buildings_available"
)

remaining_columns <- setdiff(
  names(hourly_data),
  important_columns
)

hourly_data <- hourly_data[
  c(
    important_columns,
    remaining_columns
  )
]


# ============================================================
# 13. Basic validation
# ============================================================

cat("\n============================================\n")
cat("HOURLY DATA VALIDATION\n")
cat("============================================\n")

cat(
  "Hourly rows:",
  format(nrow(hourly_data), big.mark = ","),
  "\n"
)

cat(
  "Start:",
  format(
    min(hourly_data$datetime),
    "%Y-%m-%d %H:%M:%S"
  ),
  "\n"
)

cat(
  "End:",
  format(
    max(hourly_data$datetime),
    "%Y-%m-%d %H:%M:%S"
  ),
  "\n"
)

cat(
  "Duplicate hourly timestamps:",
  sum(
    duplicated(hourly_data$datetime)
  ),
  "\n"
)


# ============================================================
# 14. Coverage summary
# ============================================================

coverage_columns <- grep(
  "_coverage_percent$",
  names(hourly_data),
  value = TRUE
)

coverage_summary <- data.frame(
  meter = coverage_columns,
  mean_coverage_percent = sapply(
    hourly_data[coverage_columns],
    mean,
    na.rm = TRUE
  ),
  hours_below_75_percent = sapply(
    hourly_data[coverage_columns],
    function(x)
      sum(
        x < 75,
        na.rm = TRUE
      )
  )
)

coverage_summary$mean_coverage_percent <- round(
  coverage_summary$mean_coverage_percent,
  2
)

cat("\nCoverage summary:\n")

print(
  coverage_summary
)

write.csv(
  coverage_summary,
  file.path(
    results_dir,
    "hourly_coverage_summary.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 15. Check negative energy values
# ============================================================

energy_columns <- grep(
  "_energy_kWh$",
  names(hourly_data),
  value = TRUE
)

cat("\nNegative hourly energy values:\n")

for (column in energy_columns) {
  
  negative_values <- sum(
    hourly_data[[column]] < 0,
    na.rm = TRUE
  )
  
  cat(
    column,
    ":",
    negative_values,
    "\n"
  )
}


# ============================================================
# 16. Save hourly RDS
# ============================================================

hourly_rds <- file.path(
  processed_dir,
  "energy_hourly.rds"
)

saveRDS(
  hourly_data,
  hourly_rds,
  compress = TRUE
)

cat(
  "\nHourly RDS saved:\n",
  hourly_rds,
  "\n"
)


# ============================================================
# 17. Save hourly CSV
# ============================================================

hourly_csv <- file.path(
  processed_dir,
  "energy_hourly.csv"
)

write.csv(
  hourly_data,
  hourly_csv,
  row.names = FALSE,
  na = ""
)

cat(
  "Hourly CSV saved:\n",
  hourly_csv,
  "\n"
)


# ============================================================
# 18. Final message
# ============================================================

cat("\n============================================\n")
cat("HOURLY AGGREGATION COMPLETE\n")
cat("============================================\n")

cat(
  "\nFiles created in data/processed:\n",
  " - energy_hourly.rds\n",
  " - energy_hourly.csv\n"
)

cat(
  "\nFile created in results:\n",
  " - hourly_coverage_summary.csv\n"
)