# ============================================================
# 06_merge_features.R
# Campus Energy Consumption Analytics
#
# Purpose:
#   1. Load hourly energy data
#   2. Load occupancy data
#   3. Aggregate occupancy from 10-min to hourly
#   4. Load institute calendar
#   5. Merge calendar + occupancy with energy
#
# IMPORTANT:
#   Occupancy building codes are kept as their original
#   filenames. We do NOT guess their building mapping here.
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

raw_dir <- file.path(
  project_dir,
  "data",
  "raw"
)

occupancy_dir <- file.path(
  raw_dir,
  "occupancy"
)

calendar_dir <- file.path(
  raw_dir,
  "calendar"
)

results_dir <- file.path(
  project_dir,
  "results"
)


# ============================================================
# 2. Load hourly energy data
# ============================================================

cat("Loading hourly energy data...\n")

hourly_file <- file.path(
  processed_dir,
  "energy_hourly.rds"
)

energy <- readRDS(
  hourly_file
)

cat(
  "Hourly rows:",
  format(nrow(energy), big.mark = ","),
  "\n"
)


# ============================================================
# 3. Load occupancy files
# ============================================================

cat("\nLoading occupancy datasets...\n")

occupancy_files <- list.files(
  occupancy_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

print(
  basename(occupancy_files)
)


# ============================================================
# 4. Function to process occupancy
# ============================================================

process_occupancy <- function(file) {
  
  building_code <- tools::file_path_sans_ext(
    basename(file)
  )
  
  cat(
    "\nProcessing occupancy:",
    building_code,
    "\n"
  )
  
  occ <- read.csv(
    file,
    stringsAsFactors = FALSE
  )
  
  # Convert timestamp
  occ$timestamp <- as.numeric(
    occ$timestamp
  )
  
  occ$datetime <- as.POSIXct(
    occ$timestamp,
    origin = "1970-01-01",
    tz = "Asia/Kolkata"
  )
  
  # Make sure occupancy is numeric
  occ$occupancy_count <- as.numeric(
    occ$occupancy_count
  )
  
  # Create hourly timestamp
  occ$hour_datetime <- as.POSIXct(
    format(
      occ$datetime,
      "%Y-%m-%d %H:00:00",
      tz = "Asia/Kolkata"
    ),
    tz = "Asia/Kolkata"
  )
  
  # ----------------------------------------------------------
  # Hourly occupancy
  #
  # Since source occupancy is already a 10-minute maximum
  # within each 10-minute window, we use the maximum observed
  # occupancy within each hour.
  # ----------------------------------------------------------
  
  # Hourly mean occupancy
  hourly_mean <- aggregate(
    occ$occupancy_count,
    by = list(datetime = occ$hour_datetime),
    FUN = function(x) {
      if (all(is.na(x))) return(NA_real_)
      mean(x, na.rm = TRUE)
    }
  )
  names(hourly_mean)[2] <- paste0("occupancy_mean_", building_code)
  
  # Hourly peak occupancy
  hourly_peak <- aggregate(
    occ$occupancy_count,
    by = list(datetime = occ$hour_datetime),
    FUN = function(x) {
      if (all(is.na(x))) return(NA_real_)
      max(x, na.rm = TRUE)
    }
  )
  names(hourly_peak)[2] <- paste0("occupancy_peak_", building_code)
  
  # ----------------------------------------------------------
  # Number of 10-minute observations available
  # ----------------------------------------------------------
  
  observation_count <- aggregate(
    !is.na(occ$occupancy_count),
    by = list(
      datetime = occ$hour_datetime
    ),
    FUN = sum
  )
  
  names(observation_count)[2] <- paste0(
    "occupancy_",
    building_code,
    "_observations"
  )
  
  # ----------------------------------------------------------
  # Merge occupancy and observation count
  # ----------------------------------------------------------
  
  result <- merge(hourly_mean, hourly_peak, by = "datetime", all = TRUE)
  result <- merge(result, observation_count, by = "datetime", all = TRUE)
  
  return(result)
}


# ============================================================
# 5. Process all occupancy files
# ============================================================

occupancy_hourly <- NULL

for (file in occupancy_files) {
  
  current <- process_occupancy(
    file
  )
  
  if (is.null(occupancy_hourly)) {
    
    occupancy_hourly <- current
    
  } else {
    
    occupancy_hourly <- merge(
      occupancy_hourly,
      current,
      by = "datetime",
      all = TRUE
    )
  }
}


# ============================================================
# 6. Sort occupancy data
# ============================================================

occupancy_hourly <- occupancy_hourly[
  order(
    occupancy_hourly$datetime
  ),
]


# ============================================================
# 7. Occupancy coverage summary
# ============================================================

occupancy_observation_columns <- grep(
  "_observations$",
  names(occupancy_hourly),
  value = TRUE
)

occupancy_coverage <- data.frame(
  occupancy_dataset = occupancy_observation_columns,
  mean_observations_per_hour = sapply(
    occupancy_hourly[
      occupancy_observation_columns
    ],
    mean,
    na.rm = TRUE
  ),
  hours_with_no_observations = sapply(
    occupancy_hourly[
      occupancy_observation_columns
    ],
    function(x)
      sum(
        x == 0,
        na.rm = TRUE
      )
  )
)

occupancy_coverage$mean_observations_per_hour <-
  round(
    occupancy_coverage$mean_observations_per_hour,
    2
  )


# ============================================================
# 8. Save occupancy coverage
# ============================================================

write.csv(
  occupancy_coverage,
  file.path(
    results_dir,
    "occupancy_hourly_coverage.csv"
  ),
  row.names = FALSE
)


# ============================================================
# 9. Load institute calendar
# ============================================================

cat("\nLoading institute calendar...\n")

calendar_files <- list.files(
  calendar_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

calendar_list <- list()

for (file in calendar_files) {
  
  cat(
    "Reading:",
    basename(file),
    "\n"
  )
  
  cal <- read.csv(
    file,
    stringsAsFactors = FALSE
  )
  
  calendar_list[[length(calendar_list) + 1]] <-
    cal
}


# Combine all years

calendar <- do.call(
  rbind,
  calendar_list
)


# ============================================================
# 10. Clean calendar
# ============================================================

calendar$Date <- as.Date(
  calendar$Date
)

calendar$working_day <- as.integer(
  calendar$working_day
)

calendar$activity <- as.character(
  calendar$activity
)


# Remove duplicate dates if any

calendar <- calendar[
  !duplicated(
    calendar$Date
  ),
]


# ============================================================
# 11. Create useful calendar variables
# ============================================================

calendar$is_working_day <- calendar$working_day

calendar$activity_level <- ifelse(
  calendar$activity == "H",
  "High",
  "Low"
)


# ============================================================
# 12. Merge calendar with hourly energy
# ============================================================

cat("\nMerging calendar with energy...\n")

energy$date <- as.Date(
  energy$datetime,
  tz = "Asia/Kolkata"
)

energy_calendar <- merge(
  energy,
  calendar[
    ,
    c(
      "Date",
      "working_day",
      "activity",
      "is_working_day",
      "activity_level"
    )
  ],
  by.x = "date",
  by.y = "Date",
  all.x = TRUE
)


# ============================================================
# 13. Merge occupancy
# ============================================================

cat("Merging occupancy with energy...\n")

merged_data <- merge(
  energy_calendar,
  occupancy_hourly,
  by = "datetime",
  all.x = TRUE
)


# ============================================================
# 14. Sort final dataset
# ============================================================

merged_data <- merged_data[
  order(
    merged_data$datetime
  ),
]


# ============================================================
# 15. Check date/calendar coverage
# ============================================================

cat("\n============================================\n")
cat("CALENDAR VALIDATION\n")
cat("============================================\n")

cat(
  "Hourly rows:",
  format(
    nrow(merged_data),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Missing calendar rows:",
  sum(
    is.na(
      merged_data$working_day
    )
  ),
  "\n"
)


# ============================================================
# 16. Check occupancy coverage
# ============================================================

cat("\n============================================\n")
cat("OCCUPANCY VALIDATION\n")
cat("============================================\n")

occupancy_columns <- grep(
  "^occupancy_(mean|peak)_[A-Z]+$",
  names(merged_data),
  value = TRUE
)

for (column in occupancy_columns) {
  
  cat(
    column,
    "missing:",
    sum(
      is.na(
        merged_data[[column]]
      )
    ),
    "\n"
  )
}


# ============================================================
# 17. Print occupancy code mapping
# ============================================================

cat("\n============================================\n")
cat("OCCUPANCY DATASET CODES\n")
cat("============================================\n")

cat(
  "\nThe following occupancy datasets were found:\n\n"
)

print(
  occupancy_columns
)

cat(
  "\nIMPORTANT: These codes have NOT been manually\n",
  "mapped to building names yet.\n",
  "We will verify the official mapping before using\n",
  "occupancy as a building-specific model feature.\n"
)


# ============================================================
# 18. Save merged dataset
# ============================================================

merged_rds <- file.path(
  processed_dir,
  "energy_hourly_with_features.rds"
)

saveRDS(
  merged_data,
  merged_rds,
  compress = TRUE
)

cat(
  "\nSaved RDS:\n",
  merged_rds,
  "\n"
)


# ============================================================
# 19. Save CSV
# ============================================================

merged_csv <- file.path(
  processed_dir,
  "energy_hourly_with_features.csv"
)

write.csv(
  merged_data,
  merged_csv,
  row.names = FALSE,
  na = ""
)

cat(
  "Saved CSV:\n",
  merged_csv,
  "\n"
)


# ============================================================
# 20. Final message
# ============================================================

cat("\n============================================\n")
cat("FEATURE MERGING COMPLETE\n")
cat("============================================\n")

cat(
  "\nCreated:\n",
  " - energy_hourly_with_features.rds\n",
  " - energy_hourly_with_features.csv\n",
  " - occupancy_hourly_coverage.csv\n"
)