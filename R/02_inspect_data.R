# ============================================================
# Campus Energy Consumption Analytics
# 02 - Inspect Dataset Structure
# ============================================================

rm(list = ls())

project_dir <- getwd()
raw_dir <- file.path(project_dir, "data", "raw")

energy_dir <- file.path(raw_dir, "energy")
occupancy_dir <- file.path(raw_dir, "occupancy")
weather_dir <- file.path(raw_dir, "weather")
calendar_dir <- file.path(raw_dir, "calendar")

# ------------------------------------------------------------
# Function: inspect a CSV without loading the whole file
# ------------------------------------------------------------

inspect_csv <- function(file, n = 5) {
  
  cat("\n")
  cat("============================================================\n")
  cat("FILE:", basename(file), "\n")
  cat("SIZE:", round(file.info(file)$size / 1024^2, 2), "MB\n")
  cat("============================================================\n")
  
  # Read only the header
  header <- read.csv(
    file,
    nrows = 0,
    check.names = FALSE
  )
  
  cat("\nCOLUMNS:\n")
  print(names(header))
  
  cat("\nFIRST", n, "ROWS:\n")
  
  sample <- read.csv(
    file,
    nrows = n,
    check.names = FALSE
  )
  
  print(sample)
  
  cat("\nDATA TYPES:\n")
  print(sapply(sample, class))
}

# ------------------------------------------------------------
# 1. ENERGY
# ------------------------------------------------------------

cat("\n\n")
cat("############################################################\n")
cat("# ENERGY DATA\n")
cat("############################################################\n")

energy_files <- c(
  "acad_build_mains.csv",
  "all_buildings_power.csv"
)

for (filename in energy_files) {
  
  file <- file.path(energy_dir, filename)
  
  if (file.exists(file)) {
    inspect_csv(file)
  } else {
    cat("\nNOT FOUND:", filename, "\n")
  }
}

# ------------------------------------------------------------
# 2. OCCUPANCY
# ------------------------------------------------------------

cat("\n\n")
cat("############################################################\n")
cat("# OCCUPANCY DATA\n")
cat("############################################################\n")

occupancy_file <- file.path(
  occupancy_dir,
  "ACB.csv"
)

if (file.exists(occupancy_file)) {
  inspect_csv(occupancy_file)
} else {
  cat("\nACB.csv not found. Checking available files:\n")
  print(list.files(
    occupancy_dir,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  ))
}

# ------------------------------------------------------------
# 3. WEATHER
# ------------------------------------------------------------

cat("\n\n")
cat("############################################################\n")
cat("# WEATHER DATA\n")
cat("############################################################\n")

weather_files <- list.files(
  weather_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

for (file in weather_files) {
  inspect_csv(file)
}

# ------------------------------------------------------------
# 4. CALENDAR
# ------------------------------------------------------------

cat("\n\n")
cat("############################################################\n")
cat("# CALENDAR DATA\n")
cat("############################################################\n")

calendar_file <- file.path(
  calendar_dir,
  "calender_year_2017_.csv"
)

if (file.exists(calendar_file)) {
  inspect_csv(calendar_file)
} else {
  cat("\n2017 calendar file not found.\n")
}

# ------------------------------------------------------------
# FINISHED
# ------------------------------------------------------------

cat("\n\n")
cat("============================================================\n")
cat("INSPECTION COMPLETE\n")
cat("============================================================\n")