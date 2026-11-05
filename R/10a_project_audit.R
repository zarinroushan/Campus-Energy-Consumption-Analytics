
# ============================================================
# STEP 10A: PROJECT-WIDE STRUCTURE AND OUTPUT AUDIT
# Project: Campus Energy Consumption Analytics
# ============================================================

cat("\n============================================\n")
cat("STEP 10A: PROJECT-WIDE AUDIT\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 1. PROJECT DIRECTORIES
# ------------------------------------------------------------

project_directories <- c(
  "R",
  "data",
  "data/raw",
  "data/processed",
  "results",
  "plots",
  "reports"
)

directory_audit <- data.frame(
  Directory = project_directories,
  Exists = dir.exists(project_directories)
)

cat("PROJECT DIRECTORY CHECK\n")
print(directory_audit)

missing_directories <- project_directories[
  !dir.exists(project_directories)
]

if (length(missing_directories) > 0) {
  cat("\nCreating missing directories...\n")
  dir.create(
    missing_directories,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

# ------------------------------------------------------------
# 2. LIST ALL PROJECT SCRIPTS
# ------------------------------------------------------------

cat("\nR SCRIPTS\n")

r_scripts <- list.files(
  "R",
  pattern = "\\.[Rr]$",
  recursive = TRUE,
  full.names = FALSE
)

if (length(r_scripts) > 0) {
  print(r_scripts)
} else {
  cat("No R scripts found.\n")
}

# ------------------------------------------------------------
# 3. CHECK IMPORTANT PROJECT FILES
# ------------------------------------------------------------

important_files <- c(
  "R/plot_utils.R",
  "R/09_forecasting_preparation.R",
  "R/09c_create_forecasting_dataset.R",
  "R/09d_forecasting_baselines.R",
  "R/09e_advanced_forecasting_models.R",
  "R/09f_forecast_evaluation.R",
  "R/09g_forecasting_robustness.R",
  "R/09h_error_investigation.R",
  "R/09i_final_model_selection.R",
  "R/09j_forecasting_report.R",
  "reports/09_forecasting_report.md",
  "results/forecasting_final_model_comparison.csv",
  "results/forecasting_moving_block_bootstrap.csv",
  "results/forecasting_report_key_findings.csv"
)

important_file_audit <- data.frame(
  File = important_files,
  Exists = file.exists(important_files)
)

cat("\nIMPORTANT FILE CHECK\n")
print(important_file_audit)

# ------------------------------------------------------------
# 4. COUNT FILES IN PROJECT DIRECTORIES
# ------------------------------------------------------------

count_files <- function(path) {
  
  if (!dir.exists(path)) {
    return(0)
  }
  
  files <- list.files(
    path,
    recursive = TRUE,
    all.files = FALSE,
    no.. = TRUE
  )
  
  length(files[file.info(file.path(path, files))$isdir == FALSE])
}

directory_counts <- data.frame(
  Directory = c(
    "R",
    "data/raw",
    "data/processed",
    "results",
    "plots",
    "reports"
  ),
  File_Count = sapply(
    c(
      "R",
      "data/raw",
      "data/processed",
      "results",
      "plots",
      "reports"
    ),
    count_files
  )
)

cat("\nFILE COUNTS BY DIRECTORY\n")
print(directory_counts)

# ------------------------------------------------------------
# 5. CHECK STEP 09J OUTPUTS
# ------------------------------------------------------------

step_09j_outputs <- c(
  "reports/09_forecasting_report.md",
  "results/forecasting_report_key_findings.csv"
)

cat("\nSTEP 09J OUTPUT CHECK\n")

print(
  data.frame(
    File = step_09j_outputs,
    Exists = file.exists(step_09j_outputs),
    Size_KB = ifelse(
      file.exists(step_09j_outputs),
      round(file.info(step_09j_outputs)$size / 1024, 2),
      NA
    )
  )
)

# ------------------------------------------------------------
# 6. CHECK RESULT AND PLOT FILES
# ------------------------------------------------------------

result_files <- list.files(
  "results",
  full.names = FALSE
)

plot_files <- list.files(
  "plots",
  full.names = FALSE
)

cat("\nRESULT FILES:", length(result_files), "\n")
cat("PLOT FILES:", length(plot_files), "\n")

cat("\nRESULT FILE NAMES\n")
if (length(result_files) > 0) {
  print(result_files)
}

cat("\nPLOT FILE NAMES\n")
if (length(plot_files) > 0) {
  print(plot_files)
}

# ------------------------------------------------------------
# 7. CHECK FOR EMPTY FILES
# ------------------------------------------------------------

all_project_files <- list.files(
  c("R", "data", "results", "plots", "reports"),
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE
)

all_project_files <- all_project_files[
  file.exists(all_project_files) &
    !dir.exists(all_project_files)
]

empty_files <- all_project_files[
  file.info(all_project_files)$size == 0
]

cat("\nEMPTY FILE CHECK\n")

if (length(empty_files) == 0) {
  cat("No empty files found.\n")
} else {
  cat("Empty files found:\n")
  print(empty_files)
}

# ------------------------------------------------------------
# 8. SAVE AUDIT TABLES
# ------------------------------------------------------------

write.csv(
  directory_audit,
  "results/project_directory_audit.csv",
  row.names = FALSE
)

write.csv(
  important_file_audit,
  "results/project_important_file_audit.csv",
  row.names = FALSE
)

write.csv(
  directory_counts,
  "results/project_directory_file_counts.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 9. FINAL AUDIT SUMMARY
# ------------------------------------------------------------

cat("\n============================================\n")
cat("PROJECT AUDIT SUMMARY\n")
cat("============================================\n")

cat(
  "Important files present:",
  sum(important_file_audit$Exists),
  "out of",
  nrow(important_file_audit),
  "\n"
)

cat(
  "Project directories present:",
  sum(dir.exists(project_directories)),
  "out of",
  length(project_directories),
  "\n"
)

cat(
  "Total result files:",
  length(result_files),
  "\n"
)

cat(
  "Total plot files:",
  length(plot_files),
  "\n"
)

cat(
  "Empty files:",
  length(empty_files),
  "\n"
)

cat("\nAudit tables saved in results/.\n")

cat("\n============================================\n")
cat("STEP 10A AUDIT COMPLETED\n")
cat("============================================\n")