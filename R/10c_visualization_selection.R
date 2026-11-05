
# ============================================================
# STEP 10C: FINAL VISUALIZATION INVENTORY & SELECTION
# Project: Campus Energy Consumption Analytics
# ============================================================

cat("\n============================================\n")
cat("STEP 10C: VISUALIZATION SELECTION\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 1. CHECK PLOT DIRECTORY
# ------------------------------------------------------------

if (!dir.exists("plots")) {
  stop("The plots/ directory does not exist.")
}

if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
}

if (!dir.exists("reports")) {
  dir.create("reports", recursive = TRUE)
}

# ------------------------------------------------------------
# 2. INVENTORY ALL PLOT FILES
# ------------------------------------------------------------

plot_paths <- list.files(
  "plots",
  pattern = "\\.(png|jpg|jpeg)$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(plot_paths) == 0) {
  stop("No plot images found in plots/.")
}

plot_info <- file.info(plot_paths)

plot_inventory <- data.frame(
  File_Name = basename(plot_paths),
  File_Path = normalizePath(
    plot_paths,
    winslash = "/",
    mustWork = FALSE
  ),
  File_Size_KB = round(plot_info$size / 1024, 2),
  Modified_Time = as.character(plot_info$mtime),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 3. READ IMAGE DIMENSIONS
# ------------------------------------------------------------

read_png_dimensions <- function(path) {
  
  if (!requireNamespace("png", quietly = TRUE)) {
    return(c(NA, NA))
  }
  
  image <- tryCatch(
    png::readPNG(path, native = FALSE, info = TRUE),
    error = function(e) NULL
  )
  
  if (is.null(image)) {
    return(c(NA, NA))
  }
  
  dimensions <- dim(image)
  
  c(dimensions[2], dimensions[1])
}

plot_inventory$Width_Pixels <- NA_real_
plot_inventory$Height_Pixels <- NA_real_

for (i in seq_along(plot_paths)) {
  
  extension <- tolower(
    tools::file_ext(plot_paths[i])
  )
  
  if (extension == "png") {
    
    dimensions <- read_png_dimensions(plot_paths[i])
    
    plot_inventory$Width_Pixels[i] <- dimensions[1]
    plot_inventory$Height_Pixels[i] <- dimensions[2]
  }
}

# ------------------------------------------------------------
# 4. DEFINE PROPOSED FINAL FIGURE SELECTION
# ------------------------------------------------------------

# These are recommendations based on plot filenames and
# the analysis stages completed. They must be visually reviewed.

selection <- data.frame(
  File_Name = c(
    "energy_data_coverage.png",
    "overall_energy_trend_corrected.png",
    "building_comparison.png",
    "building_energy_distributions.png",
    "detailed_hourly_energy_profile.png",
    "detailed_daily_energy_trend.png",
    "detailed_monthly_energy.png",
    "detailed_weekday_energy.png",
    "forecasting_train_test_split.png",
    "forecasting_baseline_comparison.png",
    "advanced_forecasting_comparison.png",
    "actual_vs_predicted.png",
    "forecast_residuals_over_time.png",
    "forecast_error_by_hour.png",
    "forecasting_moving_block_bootstrap.png"
  ),
  
  Section = c(
    "Data Quality",
    "Overall Energy Analysis",
    "Building-Level Analysis",
    "Building-Level Analysis",
    "Temporal Analysis",
    "Temporal Analysis",
    "Temporal Analysis",
    "Temporal Analysis",
    "Forecasting Methodology",
    "Forecasting Model Comparison",
    "Forecasting Model Comparison",
    "Forecasting Evaluation",
    "Forecasting Diagnostics",
    "Forecasting Diagnostics",
    "Forecasting Validation"
  ),
  
  Reason = c(
    "Shows energy data availability and coverage",
    "Shows the corrected overall energy trend",
    "Compares energy consumption across buildings",
    "Shows differences in building-level distributions",
    "Shows hourly consumption patterns",
    "Shows daily consumption patterns",
    "Shows monthly consumption patterns",
    "Shows weekday consumption patterns",
    "Explains the chronological training and testing split",
    "Compares baseline forecasting models",
    "Compares advanced forecasting models",
    "Compares actual and predicted energy values",
    "Shows forecast residual behaviour over time",
    "Shows how forecast errors vary by hour",
    "Shows uncertainty in the bootstrap improvement estimate"
  ),
  
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 5. CHECK WHICH PROPOSED FIGURES EXIST
# ------------------------------------------------------------

selection$Exists <- selection$File_Name %in%
  plot_inventory$File_Name

selection$File_Size_KB <- NA_real_

for (i in seq_len(nrow(selection))) {
  
  matching_row <- match(
    selection$File_Name[i],
    plot_inventory$File_Name
  )
  
  if (!is.na(matching_row)) {
    selection$File_Size_KB[i] <-
      plot_inventory$File_Size_KB[matching_row]
  }
}

cat("PROPOSED FINAL FIGURE SELECTION\n")
print(selection)

# ------------------------------------------------------------
# 6. FLAG MISSING PROPOSED FIGURES
# ------------------------------------------------------------

missing_figures <- selection$File_Name[
  !selection$Exists
]

cat("\nMISSING PROPOSED FIGURES\n")

if (length(missing_figures) == 0) {
  cat("All proposed figures exist.\n")
} else {
  print(missing_figures)
}

# ------------------------------------------------------------
# 7. IDENTIFY OTHER AVAILABLE FIGURES
# ------------------------------------------------------------

other_figures <- plot_inventory[
  !plot_inventory$File_Name %in% selection$File_Name,
]

cat(
  "\nOther available figures:",
  nrow(other_figures),
  "\n"
)

# ------------------------------------------------------------
# 8. SAVE INVENTORY AND SELECTION TABLES
# ------------------------------------------------------------

write.csv(
  plot_inventory,
  "results/complete_plot_inventory.csv",
  row.names = FALSE
)

write.csv(
  selection,
  "results/proposed_final_figure_selection.csv",
  row.names = FALSE
)

write.csv(
  other_figures,
  "results/additional_plot_inventory.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 9. CREATE VISUALIZATION SELECTION REPORT
# ------------------------------------------------------------

report_lines <- c(
  "# Final Visualization Selection",
  "",
  "Project: Campus Energy Consumption Analytics",
  "",
  "## Inventory Summary",
  "",
  paste0(
    "- Total available plot files: ",
    nrow(plot_inventory)
  ),
  paste0(
    "- Proposed final figures: ",
    nrow(selection)
  ),
  paste0(
    "- Proposed figures available: ",
    sum(selection$Exists)
  ),
  paste0(
    "- Proposed figures missing: ",
    sum(!selection$Exists)
  ),
  "",
  "## Proposed Final Figures",
  "",
  "| Figure | Section | Available |",
  "|---|---|---|"
)

for (i in seq_len(nrow(selection))) {
  
  report_lines <- c(
    report_lines,
    paste0(
      "| ",
      selection$File_Name[i],
      " | ",
      selection$Section[i],
      " | ",
      selection$Exists[i],
      " |"
    )
  )
}

report_lines <- c(
  report_lines,
  "",
  "## Review Notes",
  "",
  "This is a proposed selection based on filenames and",
  "analysis coverage. It is not a substitute for visually",
  "reviewing each image.",
  "",
  "Check readability, labels, units, legends, and whether",
  "each figure adds information not already shown elsewhere.",
  "",
  "No original plot files were modified or deleted.",
  "",
  "**End of visualization inventory.**"
)

writeLines(
  report_lines,
  "reports/10c_visualization_selection.md",
  useBytes = TRUE
)

# ------------------------------------------------------------
# 10. FINAL VERIFICATION
# ------------------------------------------------------------

expected_outputs <- c(
  "results/complete_plot_inventory.csv",
  "results/proposed_final_figure_selection.csv",
  "results/additional_plot_inventory.csv",
  "reports/10c_visualization_selection.md"
)

verification <- data.frame(
  File = expected_outputs,
  Exists = file.exists(expected_outputs)
)

cat("\nOUTPUT VERIFICATION\n")
print(verification)

cat(
  "\nFiles generated:",
  sum(verification$Exists),
  "out of",
  length(expected_outputs),
  "\n"
)

if (!all(verification$Exists)) {
  stop("Step 10C output verification failed.")
}

cat("\n============================================\n")
cat("STEP 10C COMPLETED SUCCESSFULLY\n")
cat("============================================\n")

cat(
  "\nAvailable plots:",
  nrow(plot_inventory),
  "\n"
)

cat(
  "Proposed final figures:",
  nrow(selection),
  "\n"
)

cat(
  "Proposed figures found:",
  sum(selection$Exists),
  "\n"
)