
# ============================================================
# STEP 10G: ENERGY-SAVING SCENARIO ANALYSIS
# ENHANCED VERSION WITH ANNUAL COMPARISON
# ============================================================

library(dplyr)
library(ggplot2)
library(readr)
library(tibble)

cat("\n============================================\n")
cat("STEP 10G: ENHANCED ENERGY-SAVING ANALYSIS\n")
cat("============================================\n\n")


# 1. PROJECT DIRECTORY ---------------------------------------

possible_roots <- unique(c(
  getwd(),
  dirname(getwd()),
  file.path(path.expand("~"),
            "Campus Energy Consumption Analytics"),
  file.path(path.expand("~"), "Documents",
            "Campus Energy Consumption Analytics")
))

valid_roots <- possible_roots[
  file.exists(file.path(
    possible_roots,
    "data", "processed",
    "energy_hourly_with_features.rds"
  ))
]

if (length(valid_roots) == 0) {
  stop("Project directory not found.")
}

project_root <- valid_roots[1]

cat("Project directory:", project_root, "\n")


# 2. LOAD DATA -----------------------------------------------

data_path <- file.path(
  project_root,
  "data", "processed",
  "energy_hourly_with_features.rds"
)

energy_data <- readRDS(data_path)

cat("Dataset loaded:", nrow(energy_data), "records\n")


# 3. VALIDATE REQUIRED COLUMNS -------------------------------

energy_column <- "Campus_total_energy_kWh"

if (!all(c("datetime", energy_column) %in%
         names(energy_data))) {
  stop("Required columns datetime or Campus_total_energy_kWh missing.")
}

if (!inherits(energy_data$datetime, "POSIXct")) {
  stop("datetime must be POSIXct. Check the original dataset.")
}

energy_data <- energy_data %>%
  mutate(
    Year = format(datetime, "%Y"),
    Valid_Energy =
      !is.na(.data[[energy_column]]) &
      is.finite(.data[[energy_column]]) &
      .data[[energy_column]] >= 0
  )

energy_data <- energy_data %>%
  filter(!is.na(Year))


# 4. OVERALL BASELINE ----------------------------------------

total_records <- nrow(energy_data)

observed_records <- sum(energy_data$Valid_Energy)

missing_records <- total_records - observed_records

coverage_percentage <-
  observed_records / total_records * 100

baseline_energy <- sum(
  energy_data[[energy_column]][energy_data$Valid_Energy]
)

average_hourly_energy <-
  baseline_energy / observed_records

cat("\nOVERALL BASELINE STATISTICS\n")
cat("--------------------------------------------\n")
cat("Total records:", total_records, "\n")
cat("Observed records:", observed_records, "\n")
cat("Missing/invalid records:", missing_records, "\n")
cat("Coverage:", round(coverage_percentage, 2), "%\n")
cat("Observed energy:",
    round(baseline_energy, 2), "kWh\n")
cat("Average observed hourly energy:",
    round(average_hourly_energy, 2), "kWh\n")


# 5. DEFINE SCENARIOS ----------------------------------------

scenarios <- tribble(
  ~Scenario, ~Reduction_Percentage, ~Description,
  "Baseline", 0, "No hypothetical reduction",
  "Scenario 1", 5, "5% hypothetical reduction",
  "Scenario 2", 10, "10% hypothetical reduction",
  "Scenario 3", 15, "15% hypothetical reduction",
  "Scenario 4", 20, "20% hypothetical reduction"
)


# 6. OVERALL SCENARIO ANALYSIS -------------------------------

overall_results <- scenarios %>%
  mutate(
    Baseline_Energy_kWh = baseline_energy,
    
    Estimated_Savings_kWh =
      baseline_energy * Reduction_Percentage / 100,
    
    Remaining_Energy_kWh =
      baseline_energy - Estimated_Savings_kWh,
    
    Observed_Records = observed_records,
    
    Dataset_Coverage_Percentage = coverage_percentage
  )

cat("\nOVERALL SCENARIO RESULTS\n")
cat("--------------------------------------------\n")

print(
  overall_results %>%
    select(
      Scenario,
      Reduction_Percentage,
      Estimated_Savings_kWh,
      Remaining_Energy_kWh
    )
)


# 7. ANNUAL BASELINE ANALYSIS -------------------------------

annual_baseline <- energy_data %>%
  group_by(Year) %>%
  summarise(
    Total_Records = n(),
    
    Observed_Records = sum(Valid_Energy),
    
    Missing_Records = Total_Records - Observed_Records,
    
    Observed_Energy_kWh = sum(
      .data[[energy_column]][Valid_Energy]
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    Coverage_Percentage =
      Observed_Records / Total_Records * 100
  ) %>%
  arrange(Year)

cat("\nANNUAL BASELINE STATISTICS\n")
cat("--------------------------------------------\n")

print(annual_baseline)


# 8. ANNUAL SCENARIO ANALYSIS -------------------------------

annual_scenario_results <- bind_rows(
  lapply(seq_len(nrow(scenarios)), function(i) {
    
    annual_baseline %>%
      mutate(
        Scenario = scenarios$Scenario[i],
        
        Reduction_Percentage =
          scenarios$Reduction_Percentage[i],
        
        Estimated_Savings_kWh =
          Observed_Energy_kWh *
          Reduction_Percentage / 100,
        
        Remaining_Energy_kWh =
          Observed_Energy_kWh -
          Estimated_Savings_kWh
      )
  })
) %>%
  select(
    Year,
    Scenario,
    Reduction_Percentage,
    Observed_Energy_kWh,
    Estimated_Savings_kWh,
    Remaining_Energy_kWh,
    Total_Records,
    Observed_Records,
    Missing_Records,
    Coverage_Percentage
  ) %>%
  arrange(Year, Reduction_Percentage)

cat("\nANNUAL SCENARIO RESULTS\n")
cat("--------------------------------------------\n")

print(annual_scenario_results, n = Inf)


# 9. CREATE OUTPUT DIRECTORIES ------------------------------

results_dir <- file.path(project_root, "results")
plots_dir <- file.path(project_root, "plots")

dir.create(results_dir, recursive = TRUE,
           showWarnings = FALSE)

dir.create(plots_dir, recursive = TRUE,
           showWarnings = FALSE)


# 10. SAVE CSV RESULTS ---------------------------------------

write_csv(
  overall_results,
  file.path(
    results_dir,
    "10g_energy_saving_scenario_results.csv"
  )
)

write_csv(
  annual_baseline,
  file.path(
    results_dir,
    "10g_annual_energy_baseline.csv"
  )
)

write_csv(
  annual_scenario_results,
  file.path(
    results_dir,
    "10g_annual_energy_saving_scenarios.csv"
  )
)

cat("\nCSV files saved successfully.\n")


# 11. PLOT 1: OVERALL POTENTIAL SAVINGS ----------------------

savings_plot_data <- overall_results %>%
  filter(Reduction_Percentage > 0)

p1 <- ggplot(
  savings_plot_data,
  aes(
    x = factor(
      Scenario,
      levels = c(
        "Scenario 1", "Scenario 2",
        "Scenario 3", "Scenario 4"
      )
    ),
    y = Estimated_Savings_kWh
  )
) +
  geom_col(fill = "#3B82A0", width = 0.65) +
  geom_text(
    aes(label = scales::comma(
      round(Estimated_Savings_kWh)
    )),
    vjust = -0.5,
    size = 3.8
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Potential Energy Savings Under Different Scenarios",
    subtitle = "Hypothetical reductions applied to observed baseline energy",
    x = "Energy-Reduction Scenario",
    y = "Estimated Energy Savings (kWh)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(
    plots_dir,
    "10g_energy_savings_comparison.png"
  ),
  p1, width = 10, height = 6, dpi = 300
)

print(p1)


# 12. PLOT 2: REMAINING ENERGY -------------------------------

p2 <- ggplot(
  overall_results,
  aes(
    x = factor(
      Scenario,
      levels = c(
        "Baseline", "Scenario 1", "Scenario 2",
        "Scenario 3", "Scenario 4"
      )
    ),
    y = Remaining_Energy_kWh
  )
) +
  geom_col(fill = "#D18B55", width = 0.65) +
  geom_text(
    aes(label = scales::comma(
      round(Remaining_Energy_kWh)
    )),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Remaining Energy Under Different Reduction Scenarios",
    subtitle = "Comparison against observed baseline energy",
    x = "Scenario",
    y = "Remaining Energy (kWh)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(
    plots_dir,
    "10g_remaining_energy_comparison.png"
  ),
  p2, width = 10, height = 6, dpi = 300
)

print(p2)


# 13. PLOT 3: ANNUAL POTENTIAL SAVINGS -----------------------

annual_plot_data <- annual_scenario_results %>%
  filter(Reduction_Percentage > 0)

p3 <- ggplot(
  annual_plot_data,
  aes(
    x = Year,
    y = Estimated_Savings_kWh,
    fill = Scenario
  )
) +
  geom_col(position = "dodge", width = 0.75) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.10))
  ) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Annual Potential Energy Savings",
    subtitle = "Hypothetical savings based on each year's observed energy",
    x = "Year",
    y = "Estimated Savings (kWh)",
    fill = "Reduction Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  file.path(
    plots_dir,
    "10g_annual_energy_savings_comparison.png"
  ),
  p3, width = 11, height = 7, dpi = 300
)

print(p3)


# 14. PLOT 4: ANNUAL OBSERVED ENERGY -------------------------

p4 <- ggplot(
  annual_baseline,
  aes(
    x = Year,
    y = Observed_Energy_kWh
  )
) +
  geom_col(fill = "#6A9F78", width = 0.65) +
  geom_text(
    aes(label = scales::comma(
      round(Observed_Energy_kWh)
    )),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Observed Annual Campus Energy",
    subtitle = "Sum of valid hourly observations for each year",
    x = "Year",
    y = "Observed Energy (kWh)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(
    plots_dir,
    "10g_annual_observed_energy.png"
  ),
  p4, width = 10, height = 6, dpi = 300
)

print(p4)


# 15. SAVE SUMMARY REPORT ------------------------------------

summary_path <- file.path(
  results_dir,
  "10g_energy_saving_scenario_summary.txt"
)

summary_lines <- c(
  "ENERGY-SAVING SCENARIO ANALYSIS",
  "============================================",
  "",
  "OVERALL BASELINE",
  paste("Total records:", total_records),
  paste("Observed records:", observed_records),
  paste("Missing/invalid records:", missing_records),
  paste("Observed coverage:",
        round(coverage_percentage, 2), "%"),
  paste("Observed energy:",
        round(baseline_energy, 2), "kWh"),
  "",
  "ANNUAL BASELINE",
  capture.output(print(annual_baseline)),
  "",
  "OVERALL SCENARIO RESULTS",
  capture.output(print(overall_results)),
  "",
  "ANNUAL SCENARIO RESULTS",
  capture.output(print(annual_scenario_results)),
  "",
  "IMPORTANT LIMITATIONS",
  "- Scenario reductions are hypothetical assumptions.",
  "- Calculations use observed energy values only.",
  "- Missing readings have not been imputed.",
  "- Annual data coverage may differ between years.",
  "- Annual sums are not necessarily directly comparable",
  "  when observation coverage differs.",
  "- The results do not represent verified campus-wide savings.",
  "- No monetary savings are estimated without a verified tariff.",
  "- Technical feasibility of the reduction percentages",
  "  has not been established."
)

writeLines(summary_lines, summary_path)

cat("\nSummary report saved:\n", summary_path, "\n")


# 16. FINAL VALIDATION ---------------------------------------

expected_files <- c(
  file.path(results_dir,
            "10g_energy_saving_scenario_results.csv"),
  file.path(results_dir,
            "10g_annual_energy_baseline.csv"),
  file.path(results_dir,
            "10g_annual_energy_saving_scenarios.csv"),
  summary_path,
  file.path(plots_dir,
            "10g_energy_savings_comparison.png"),
  file.path(plots_dir,
            "10g_remaining_energy_comparison.png"),
  file.path(plots_dir,
            "10g_annual_energy_savings_comparison.png"),
  file.path(plots_dir,
            "10g_annual_observed_energy.png")
)

cat("\nOUTPUT VALIDATION\n")
cat("--------------------------------------------\n")

for (file in expected_files) {
  cat(
    ifelse(file.exists(file), "[OK] ", "[MISSING] "),
    basename(file), "\n"
  )
}

if (all(file.exists(expected_files))) {
  cat("\n============================================\n")
  cat("STEP 10G COMPLETED SUCCESSFULLY!\n")
  cat("============================================\n")
} else {
  warning("Some output files are missing. Check the messages above.")
}