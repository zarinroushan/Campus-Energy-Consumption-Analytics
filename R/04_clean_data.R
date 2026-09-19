# ============================================================
# 04_clean_data.R
# Campus Energy Consumption Analytics
# Purpose:
#   - Clean main building-level energy data
#   - Convert Unix timestamps to Asia/Kolkata datetime
#   - Standardize numeric columns
#   - Create dormitory total power
#   - Preserve missing values
#   - Save processed minute-level dataset
# ============================================================

rm(list = ls())

# ============================================================
# 1. Project paths
# ============================================================

project_dir <- getwd()

raw_dir <- file.path(project_dir, "data", "raw")
energy_dir <- file.path(raw_dir, "energy")

processed_dir <- file.path(
  project_dir,
  "data",
  "processed"
)

if (!dir.exists(processed_dir)) {
  dir.create(
    processed_dir,
    recursive = TRUE
  )
}

# ============================================================
# 2. Input file
# ============================================================

energy_file <- file.path(
  energy_dir,
  "all_buildings_power.csv"
)

cat("Reading energy dataset...\n")

energy <- read.csv(
  energy_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

cat(
  "Rows loaded:",
  format(nrow(energy), big.mark = ","),
  "\n"
)

# ============================================================
# 3. Convert timestamp
# ============================================================

cat("Converting timestamps...\n")

energy$timestamp <- as.numeric(
  energy$timestamp
)

energy$datetime <- as.POSIXct(
  energy$timestamp,
  origin = "1970-01-01",
  tz = "Asia/Kolkata"
)

# ============================================================
# 4. Convert power columns to numeric
# ============================================================

power_columns <- c(
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

for (column in power_columns) {
  
  energy[[column]] <- as.numeric(
    energy[[column]]
  )
}

# ============================================================
# 5. Create dormitory total power
# ============================================================
# Boys dormitory:
#   mains + backup/UPS
#
# Girls dormitory:
#   mains + backup/UPS
#
# Missing values are preserved.
# We do NOT replace missing meter readings with zero.

energy$Boys_total <- with(
  energy,
  ifelse(
    !is.na(Boys_main) &
      !is.na(Boys_backup),
    Boys_main + Boys_backup,
    NA
  )
)

energy$Girls_total <- with(
  energy,
  ifelse(
    !is.na(Girls_main) &
      !is.na(Girls_backup),
    Girls_main + Girls_backup,
    NA
  )
)

# ============================================================
# 6. Create campus power
# ============================================================
# Campus total is calculated only when all required
# building-level measurements are available.
#
# This avoids treating an NA reading as zero.

campus_columns <- c(
  "Academic",
  "Boys_total",
  "Facilities",
  "Girls_total",
  "Lecture",
  "Library",
  "Mess"
)

energy$Campus_total_power <- rowSums(
  energy[, campus_columns],
  na.rm = FALSE
)

# ============================================================
# 7. Reorder columns
# ============================================================

energy <- energy[
  c(
    "timestamp",
    "datetime",
    
    "Academic",
    
    "Boys_main",
    "Boys_backup",
    "Boys_total",
    
    "Facilities",
    
    "Girls_main",
    "Girls_backup",
    "Girls_total",
    
    "Lecture",
    "Library",
    "Mess",
    
    "Campus_total_power"
  )
]

# ============================================================
# 8. Check chronological order
# ============================================================

cat("\nChecking chronological order...\n")

chronological <- all(
  diff(energy$timestamp) >= 0
)

cat(
  "Chronologically ordered:",
  chronological,
  "\n"
)

# ============================================================
# 9. Check duplicate timestamps
# ============================================================

duplicate_timestamps <- sum(
  duplicated(energy$timestamp)
)

cat(
  "Duplicate timestamps:",
  duplicate_timestamps,
  "\n"
)

# ============================================================
# 10. Missing-value summary
# ============================================================

cat("\nMissing-value summary:\n")

missing_summary <- data.frame(
  column = names(energy),
  missing_values = sapply(
    energy,
    function(x) sum(is.na(x))
  )
)

missing_summary$missing_percent <- round(
  (
    missing_summary$missing_values /
      nrow(energy)
  ) * 100,
  2
)

print(
  missing_summary
)

write.csv(
  missing_summary,
  file.path(
    processed_dir,
    "energy_missing_summary.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 11. Basic power sanity check
# ============================================================

cat("\nPower sanity check:\n")

for (column in c(
  "Academic",
  "Boys_main",
  "Boys_backup",
  "Facilities",
  "Girls_main",
  "Girls_backup",
  "Lecture",
  "Library",
  "Mess"
)) {
  
  negative_count <- sum(
    energy[[column]] < 0,
    na.rm = TRUE
  )
  
  cat(
    column,
    ": negative values =",
    negative_count,
    "\n"
  )
}

# ============================================================
# 12. Dataset time range
# ============================================================

cat("\nDataset time range:\n")

cat(
  "Start:",
  format(
    min(energy$datetime, na.rm = TRUE),
    "%Y-%m-%d %H:%M:%S"
  ),
  "\n"
)

cat(
  "End:",
  format(
    max(energy$datetime, na.rm = TRUE),
    "%Y-%m-%d %H:%M:%S"
  ),
  "\n"
)

# ============================================================
# 13. Save processed minute-level dataset
# ============================================================

output_file <- file.path(
  processed_dir,
  "energy_clean_minute.csv"
)

cat("\nSaving processed dataset...\n")

write.csv(
  energy,
  output_file,
  row.names = FALSE,
  na = ""
)

cat(
  "\nSaved successfully:\n",
  output_file,
  "\n"
)

# ============================================================
# 14. Save RDS copy
# ============================================================
# RDS is faster for later R analysis and preserves
# R data types better than CSV.

rds_file <- file.path(
  processed_dir,
  "energy_clean_minute.rds"
)

saveRDS(
  energy,
  rds_file,
  compress = TRUE
)

cat(
  "RDS file saved:\n",
  rds_file,
  "\n"
)

# ============================================================
# 15. Final structure
# ============================================================

cat("\nFinal dataset structure:\n")

str(
  energy
)

cat("\n============================================\n")
cat("CLEANING COMPLETE\n")
cat("============================================\n")