# ============================================================
# 03_validate_data.R
# Campus Energy Consumption Analytics
# Purpose: Validate raw dataset structure, timestamps,
#          missingness and time intervals
# ============================================================

# -----------------------------
# 1. Project paths
# -----------------------------

project_dir <- getwd()

raw_dir <- file.path(project_dir, "data", "raw")
energy_dir <- file.path(raw_dir, "energy")
occupancy_dir <- file.path(raw_dir, "occupancy")
weather_dir <- file.path(raw_dir, "weather")
calendar_dir <- file.path(raw_dir, "calendar")

results_dir <- file.path(project_dir, "results")

if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

# -----------------------------
# 2. Helper function
# -----------------------------

validate_csv <- function(file, expected_interval = NA) {
  
  cat("\n============================================\n")
  cat("FILE:", basename(file), "\n")
  cat("============================================\n")
  
  # Read header
  header <- names(
    read.csv(
      file,
      nrows = 0,
      check.names = FALSE
    )
  )
  
  cat("Columns:", length(header), "\n")
  cat("Column names:\n")
  print(header)
  
  # Read data
  df <- read.csv(
    file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Basic information
  rows <- nrow(df)
  cols <- ncol(df)
  
  cat("\nRows:", format(rows, big.mark = ","), "\n")
  cat("Columns:", cols, "\n")
  
  # -----------------------------
  # Timestamp validation
  # -----------------------------
  
  if ("timestamp" %in% names(df)) {
    
    timestamps <- as.numeric(df$timestamp)
    
    valid_ts <- !is.na(timestamps)
    
    if (any(valid_ts)) {
      
      first_ts <- min(timestamps[valid_ts])
      last_ts  <- max(timestamps[valid_ts])
      
      first_time <- as.POSIXct(
        first_ts,
        origin = "1970-01-01",
        tz = "Asia/Kolkata"
      )
      
      last_time <- as.POSIXct(
        last_ts,
        origin = "1970-01-01",
        tz = "Asia/Kolkata"
      )
      
      cat("\nFirst timestamp:", first_time, "\n")
      cat("Last timestamp :", last_time, "\n")
      
      # Check chronological order
      ordered <- all(diff(timestamps[valid_ts]) >= 0)
      
      cat("Chronologically ordered:", ordered, "\n")
      
      # Check timestamp differences
      if (length(timestamps[valid_ts]) > 1) {
        
        differences <- diff(timestamps[valid_ts])
        
        cat(
          "Minimum interval (seconds):",
          min(differences, na.rm = TRUE),
          "\n"
        )
        
        cat(
          "Maximum interval (seconds):",
          max(differences, na.rm = TRUE),
          "\n"
        )
        
        if (!is.na(expected_interval)) {
          
          duplicate_count <- sum(
            differences == 0,
            na.rm = TRUE
          )
          
          gap_count <- sum(
            differences > expected_interval,
            na.rm = TRUE
          )
          
          wrong_interval_count <- sum(
            differences != expected_interval,
            na.rm = TRUE
          )
          
          cat("Duplicate timestamps:", duplicate_count, "\n")
          cat("Gaps:", gap_count, "\n")
          cat(
            "Intervals different from expected:",
            wrong_interval_count,
            "\n"
          )
        }
      }
    }
  }
  
  # -----------------------------
  # Missing values
  # -----------------------------
  
  missing_count <- sapply(
    df,
    function(x) sum(is.na(x) | x == "")
  )
  
  missing_percent <- round(
    (missing_count / rows) * 100,
    2
  )
  
  missing_table <- data.frame(
    column = names(missing_count),
    missing_values = as.integer(missing_count),
    missing_percent = missing_percent
  )
  
  cat("\nMissing values:\n")
  print(missing_table)
  
  # -----------------------------
  # Duplicate complete rows
  # -----------------------------
  
  duplicate_rows <- sum(duplicated(df))
  
  cat(
    "\nDuplicate complete rows:",
    duplicate_rows,
    "\n"
  )
  
  # -----------------------------
  # Numeric summary
  # -----------------------------
  
  numeric_columns <- names(
    df[
      sapply(df, is.numeric)
    ]
  )
  
  if (length(numeric_columns) > 0) {
    
    cat("\nNumeric summary:\n")
    
    for (column in numeric_columns) {
      
      x <- df[[column]]
      
      cat(
        column,
        " | min = ",
        min(x, na.rm = TRUE),
        " | max = ",
        max(x, na.rm = TRUE),
        " | mean = ",
        round(mean(x, na.rm = TRUE), 3),
        "\n",
        sep = ""
      )
    }
  }
  
  # Return summary
  return(
    list(
      file = basename(file),
      rows = rows,
      columns = cols,
      missing = missing_table,
      duplicate_rows = duplicate_rows
    )
  )
}

# ============================================================
# 3. Validate main building energy dataset
# ============================================================

energy_file <- file.path(
  energy_dir,
  "all_buildings_power.csv"
)

energy_result <- validate_csv(
  energy_file,
  expected_interval = 60
)

# ============================================================
# 4. Validate Academic building energy
# ============================================================

academic_file <- file.path(
  energy_dir,
  "acad_build_mains.csv"
)

academic_result <- validate_csv(
  academic_file,
  expected_interval = 60
)

# ============================================================
# 5. Validate occupancy datasets
# ============================================================

occupancy_files <- list.files(
  occupancy_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

occupancy_results <- list()

for (file in occupancy_files) {
  
  result <- validate_csv(
    file,
    expected_interval = 600
  )
  
  occupancy_results[[basename(file)]] <- result
}

# ============================================================
# 6. Validate calendar
# ============================================================

calendar_files <- list.files(
  calendar_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

for (file in calendar_files) {
  
  validate_csv(file)
}

# ============================================================
# 7. Validate weather
# ============================================================

weather_files <- list.files(
  weather_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

for (file in weather_files) {
  
  validate_csv(file)
}

# ============================================================
# 8. Save simple validation summary
# ============================================================

summary_table <- data.frame(
  dataset = c(
    "all_buildings_power",
    "acad_build_mains"
  ),
  rows = c(
    energy_result$rows,
    academic_result$rows
  ),
  columns = c(
    energy_result$columns,
    academic_result$columns
  ),
  duplicate_rows = c(
    energy_result$duplicate_rows,
    academic_result$duplicate_rows
  )
)

write.csv(
  summary_table,
  file.path(
    results_dir,
    "validation_summary.csv"
  ),
  row.names = FALSE
)

cat("\n\n============================================\n")
cat("VALIDATION COMPLETE\n")
cat("============================================\n")

cat(
  "\nSaved:",
  file.path(
    results_dir,
    "validation_summary.csv"
  ),
  "\n"
)