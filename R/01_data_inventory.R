# ============================================================
# Campus Energy Consumption Analytics
# 01 - Dataset Inventory
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# 1. Project folders
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 2. Check project directory
# ------------------------------------------------------------

cat("PROJECT DIRECTORY:\n")
cat(project_dir, "\n\n")

# ------------------------------------------------------------
# 3. Find all CSV files
# ------------------------------------------------------------

csv_files <- list.files(
  raw_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

cat("NUMBER OF CSV FILES FOUND:", length(csv_files), "\n\n")

# ------------------------------------------------------------
# 4. File inventory
# ------------------------------------------------------------

file_inventory <- data.frame(
  file_name = basename(csv_files),
  full_path = csv_files,
  size_MB = round(
    file.info(csv_files)$size / (1024^2),
    2
  ),
  stringsAsFactors = FALSE
)

file_inventory <- file_inventory[
  order(file_inventory$full_path),
]

print(file_inventory, row.names = FALSE)

write.csv(
  file_inventory,
  file.path(results_dir, "file_inventory.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 5. Function to inspect only first 5 lines
# ------------------------------------------------------------

sample_file <- function(file, n = 5) {
  
  cat("\n")
  cat("============================================================\n")
  cat("FILE:", basename(file), "\n")
  cat(
    "SIZE:",
    round(file.info(file)$size / (1024^2), 2),
    "MB\n"
  )
  cat("PATH:", file, "\n")
  cat("------------------------------------------------------------\n")
  
  lines <- readLines(
    file,
    n = n,
    warn = FALSE
  )
  
  cat(paste(lines, collapse = "\n"))
  cat("\n")
}

# ------------------------------------------------------------
# 6. ENERGY DATA
# ------------------------------------------------------------

cat("\n\n")
cat("############################################################\n")
cat("# ENERGY DATASET - SAMPLE INSPECTION\n")
cat("############################################################\n")

energy_files <- list.files(
  energy_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

for (file in energy_files) {
  sample_file(file, n = 5)
}

# ------------------------------------------------------------
# 7. OCCUPANCY DATA
# ------------------------------------------------------------

cat("\n\n")
cat("############################################################\n")
cat("# OCCUPANCY DATASET - SAMPLE INSPECTION\n")
cat("############################################################\n")

occupancy_files <- list.files(
  occupancy_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

for (file in occupancy_files) {
  sample_file(file, n = 5)
}

# ------------------------------------------------------------
# 8. WEATHER DATA
# ------------------------------------------------------------

cat("\n\n")
cat("############################################################\n")
cat("# WEATHER DATASET - SAMPLE INSPECTION\n")
cat("############################################################\n")

weather_files <- list.files(
  weather_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

for (file in weather_files) {
  sample_file(file, n = 5)
}

# ------------------------------------------------------------
# 9. CALENDAR DATA
# ------------------------------------------------------------

cat("\n\n")
cat("############################################################\n")
cat("# CALENDAR DATASET - SAMPLE INSPECTION\n")
cat("############################################################\n")

calendar_files <- list.files(
  calendar_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

for (file in calendar_files) {
  sample_file(file, n = 5)
}

# ------------------------------------------------------------
# 10. Finished
# ------------------------------------------------------------

cat("\n\n")
cat("============================================================\n")
cat("DATASET INVENTORY COMPLETE\n")
cat("============================================================\n")

cat("Inventory saved to:\n")
cat(file.path(results_dir, "file_inventory.csv"), "\n")