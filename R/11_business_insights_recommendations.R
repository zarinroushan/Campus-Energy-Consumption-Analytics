
# ============================================================
# STEP 11: BUSINESS INSIGHTS AND RECOMMENDATIONS
# Campus Energy Consumption Analytics
# ============================================================
# Purpose:
# Translate validated analytical findings into practical,
# evidence-based campus energy-management recommendations.
#
# Important:
# Dataset is historical public I-BLEND data from IIIT-Delhi.
# Recommendations are proposed actions, not measured outcomes.
# ============================================================

library(dplyr)
library(readr)
library(ggplot2)
library(tibble)

cat("\n============================================\n")
cat("STEP 11: BUSINESS INSIGHTS AND RECOMMENDATIONS\n")
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
    "results",
    "10g_annual_energy_baseline.csv"
  ))
]

if (length(valid_roots) == 0) {
  stop("Project directory or Step 10G results not found.")
}

project_root <- valid_roots[1]

results_dir <- file.path(project_root, "results")
plots_dir <- file.path(project_root, "plots")
reports_dir <- file.path(project_root, "reports")

dir.create(reports_dir, recursive = TRUE,
           showWarnings = FALSE)


# 2. LOAD STEP 10G RESULTS -----------------------------------

annual_baseline <- read_csv(
  file.path(results_dir, "10g_annual_energy_baseline.csv"),
  show_col_types = FALSE
)

annual_scenarios <- read_csv(
  file.path(results_dir,
            "10g_annual_energy_saving_scenarios.csv"),
  show_col_types = FALSE
)

overall_scenarios <- read_csv(
  file.path(results_dir,
            "10g_energy_saving_scenario_results.csv"),
  show_col_types = FALSE
)

required_annual_columns <- c(
  "Year",
  "Total_Records",
  "Observed_Records",
  "Missing_Records",
  "Observed_Energy_kWh",
  "Coverage_Percentage"
)

if (!all(required_annual_columns %in%
         names(annual_baseline))) {
  stop("Annual baseline CSV does not contain expected columns.")
}

cat("Step 10G results loaded successfully.\n")


# 3. CALCULATE KEY BUSINESS INDICATORS -----------------------

overall_coverage <- sum(annual_baseline$Observed_Records) /
  sum(annual_baseline$Total_Records) * 100

lowest_coverage_year <- annual_baseline %>%
  slice_min(Coverage_Percentage, n = 1)

highest_coverage_year <- annual_baseline %>%
  slice_max(Coverage_Percentage, n = 1)

total_observed_energy <- sum(
  annual_baseline$Observed_Energy_kWh,
  na.rm = TRUE
)

scenario_20 <- overall_scenarios %>%
  filter(Reduction_Percentage == 20)

scenario_10 <- overall_scenarios %>%
  filter(Reduction_Percentage == 10)

cat("\nKEY INDICATORS\n")
cat("--------------------------------------------\n")
cat("Overall observation coverage:",
    round(overall_coverage, 2), "%\n")
cat("Lowest coverage year:",
    lowest_coverage_year$Year,
    "-", round(lowest_coverage_year$Coverage_Percentage, 2),
    "%\n")
cat("Highest coverage year:",
    highest_coverage_year$Year,
    "-", round(highest_coverage_year$Coverage_Percentage, 2),
    "%\n")
cat("Total observed energy:",
    round(total_observed_energy, 2), "kWh\n")


# 4. FORECASTING FINDINGS ------------------------------------

# Previously validated Step 09 results.
# These are historical test-set evaluation metrics.

forecast_findings <- tibble(
  Model = c(
    "Naive Previous Hour",
    "Seasonal Naive (24h)",
    "Linear Trend",
    "Calendar Regression",
    "Lag-Based Regression"
  ),
  
  MAE = c(
    9.187838,
    16.466897,
    30.227108,
    28.904753,
    8.822119
  ),
  
  RMSE = c(
    13.43845,
    24.47987,
    36.57293,
    34.27884,
    12.09897
  )
)

naive_mae <- forecast_findings$MAE[
  forecast_findings$Model == "Naive Previous Hour"
]

lag_mae <- forecast_findings$MAE[
  forecast_findings$Model == "Lag-Based Regression"
]

mae_improvement <- (naive_mae - lag_mae) /
  naive_mae * 100

cat("\nFORECASTING INSIGHTS\n")
cat("--------------------------------------------\n")
cat("Best model by MAE: Lag-Based Regression\n")
cat("Naive MAE:", round(naive_mae, 4), "\n")
cat("Lag-based MAE:", round(lag_mae, 4), "\n")
cat("MAE improvement:",
    round(mae_improvement, 2), "%\n")



# 5. CREATE BUSINESS INSIGHTS -------------------------------

business_insights <- tibble(
  Insight_ID = c("I01", "I02", "I03", "I04", "I05"),
  
  Area = c(
    "Data Monitoring",
    "Data Quality",
    "Energy Planning",
    "Energy Efficiency",
    "Demand Forecasting"
  ),
  
  Finding = c(
    paste0(
      "Overall observed hourly data coverage is ",
      round(overall_coverage, 2), "%."
    ),
    paste0(
      "Annual coverage ranges from ",
      round(min(annual_baseline$Coverage_Percentage), 2),
      "% to ",
      round(max(annual_baseline$Coverage_Percentage), 2),
      "%."
    ),
    paste0(
      "The observed dataset contains approximately ",
      round(total_observed_energy),
      " kWh across valid hourly records."
    ),
    paste0(
      "A hypothetical 10% reduction corresponds to ",
      round(scenario_10$Estimated_Savings_kWh),
      " kWh of observed baseline energy."
    ),
    paste0(
      "Lag-Based Regression reduced test-set MAE by ",
      round(mae_improvement, 2), "% compared with ",
      "the previous-hour baseline."
    )
  ),
  
  Interpretation = c(
    "Some hourly energy observations are missing.",
    
    paste0(
      "Data completeness differs across years. ",
      "Annual totals should therefore be interpreted cautiously."
    ),
    
    paste0(
      "Observed energy records provide a starting point ",
      "for energy-management planning, but do not represent ",
      "a perfectly complete campus-wide measurement."
    ),
    
    paste0(
      "A 10% reduction is a hypothetical planning scenario, ",
      "not an achieved or technically verified saving."
    ),
    
    paste0(
      "The lag-based model performed better on average, ",
      "but its improvement was modest and not conclusive ",
      "under the moving-block bootstrap analysis."
    )
  )
)

cat("\nBUSINESS INSIGHTS CREATED:", nrow(business_insights), "\n")
print(business_insights, n = Inf)



# 6. CREATE RECOMMENDATIONS ----------------------------------

recommendations <- tribble(
  ~Recommendation_ID, ~Priority, ~Area,
  ~Recommendation, ~Reason, ~Suggested_KPI,
  
  "R01", "High", "Energy Monitoring",
  "Improve hourly meter-data completeness and automate missing-data alerts.",
  "Overall coverage is below 100%, and annual coverage varies.",
  "Percentage of valid hourly meter observations",
  
  "R02", "High", "Data Quality",
  "Investigate periods with missing readings, especially years with lower coverage.",
  "2015 has the lowest annual coverage in the available data.",
  "Missing hourly readings per month",
  
  "R03", "High", "Energy Management",
  "Establish a regular energy-monitoring dashboard with monthly reviews.",
  "Consistent monitoring helps identify unusual consumption patterns.",
  "Monthly energy review completion rate",
  
  "R04", "Medium", "Energy Efficiency",
  "Pilot practical energy-efficiency measures and measure their effects against a documented baseline.",
  "Scenario analysis shows possible savings under hypothetical reductions.",
  "Measured kWh reduction against baseline",
  
  "R05", "Medium", "Demand Forecasting",
  "Use the lag-based forecasting model as an experimental planning aid, with continued monitoring against a simple baseline.",
  "It achieved the lowest MAE in the evaluated test set, but improvement was modest.",
  "MAE and RMSE on future chronological test periods",
  
  "R06", "Medium", "Operational Planning",
  "Investigate high-consumption periods and compare them with operating schedules before changing equipment use.",
  "Time-based analysis can help identify periods for further investigation.",
  "Energy consumption by hour and day"
)

cat("\nBUSINESS RECOMMENDATIONS\n")
cat("--------------------------------------------\n")
print(recommendations, n = Inf)


# 7. SAVE CSV OUTPUTS ----------------------------------------

write_csv(
  business_insights,
  file.path(results_dir,
            "11_business_insights.csv")
)

write_csv(
  recommendations,
  file.path(results_dir,
            "11_business_recommendations.csv")
)

write_csv(
  forecast_findings,
  file.path(results_dir,
            "11_forecasting_business_metrics.csv")
)


# 8. VISUALIZATION: RECOMMENDATION PRIORITIES ----------------

priority_data <- recommendations %>%
  count(Priority) %>%
  mutate(
    Priority = factor(
      Priority,
      levels = c("High", "Medium", "Low")
    )
  )

p1 <- ggplot(
  priority_data,
  aes(x = Priority, y = n, fill = Priority)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = n),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "High" = "#B85C5C",
      "Medium" = "#D9A441",
      "Low" = "#6A9F78"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(0, max(priority_data$n) + 1, by = 1),
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title = "Business Recommendations by Priority",
    subtitle = "Priorities assigned according to current analytical findings",
    x = "Recommendation Priority",
    y = "Number of Recommendations"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(plots_dir,
            "11_recommendation_priority.png"),
  p1,
  width = 9,
  height = 6,
  dpi = 300
)

print(p1)


# 9. VISUALIZATION: ANNUAL DATA COVERAGE ---------------------

p2 <- ggplot(
  annual_baseline,
  aes(
    x = Year,
    y = Coverage_Percentage
  )
) +
  geom_col(fill = "#3B82A0", width = 0.65) +
  geom_hline(
    yintercept = 90,
    linetype = "dashed",
    color = "red"
  ) +
  geom_text(
    aes(label = paste0(
      round(Coverage_Percentage, 1), "%"
    )),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    title = "Annual Energy Data Coverage",
    subtitle = "Dashed line represents a 90% reference level, not an official threshold",
    x = "Year",
    y = "Observed Data Coverage (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(plots_dir,
            "11_annual_data_coverage.png"),
  p2,
  width = 9,
  height = 6,
  dpi = 300
)

print(p2)


# 10. GENERATE BUSINESS INSIGHTS REPORT ----------------------

report_path <- file.path(
  reports_dir,
  "11_business_insights_and_recommendations.md"
)

report_lines <- c(
  "# Step 11: Business Insights and Recommendations",
  "",
  "## 1. Purpose",
  "",
  "This section translates the historical energy analysis,",
  "forecasting evaluation and hypothetical energy-saving",
  "scenarios into practical recommendations.",
  "",
  "The dataset used in this project is the public I-BLEND",
  "dataset from IIIT-Delhi. The findings should not be",
  "presented as measurements from Heritage Institute of Technology.",
  "",
  "## 2. Key Findings",
  "",
  paste0(
    "- Overall observed hourly data coverage: ",
    round(overall_coverage, 2), "%."
  ),
  paste0(
    "- Total observed energy across valid records: ",
    round(total_observed_energy), " kWh."
  ),
  paste0(
    "- Lowest annual coverage: ",
    lowest_coverage_year$Year, " (",
    round(lowest_coverage_year$Coverage_Percentage, 2),
    "%)."
  ),
  paste0(
    "- Highest annual coverage: ",
    highest_coverage_year$Year, " (",
    round(highest_coverage_year$Coverage_Percentage, 2),
    "%)."
  ),
  paste0(
    "- Hypothetical 10% reduction: ",
    round(scenario_10$Estimated_Savings_kWh),
    " kWh based on observed baseline energy."
  ),
  paste0(
    "- Hypothetical 20% reduction: ",
    round(scenario_20$Estimated_Savings_kWh),
    " kWh based on observed baseline energy."
  ),
  "",
  "## 3. Forecasting Insight",
  "",
  paste0(
    "The Lag-Based Regression model achieved a test MAE of ",
    round(lag_mae, 4),
    ", compared with ",
    round(naive_mae, 4),
    " for the previous-hour baseline."
  ),
  "",
  paste0(
    "This represents a ",
    round(mae_improvement, 2),
    "% reduction in MAE."
  ),
  "",
  "The improvement is modest. Earlier moving-block bootstrap",
  "confidence intervals included zero, so the improvement",
  "should not be described as statistically conclusive.",
  "",
  "## 4. Business Recommendations",
  ""
)

for (i in seq_len(nrow(recommendations))) {
  
  report_lines <- c(
    report_lines,
    
    paste0(
      "### ",
      recommendations$Recommendation_ID[i],
      " — ",
      recommendations$Recommendation[i]
    ),
    
    paste0(
      "**Priority:** ",
      recommendations$Priority[i]
    ),
    
    paste0(
      "**Reason:** ",
      recommendations$Reason[i]
    ),
    
    paste0(
      "**Suggested KPI:** ",
      recommendations$Suggested_KPI[i]
    ),
    
    ""
  )
}

report_lines <- c(
  report_lines,
  "## 5. Important Limitations",
  "",
  "- Missing energy observations have not been imputed.",
  "- Annual observed energy totals may not be directly comparable",
  "  because coverage differs between years.",
  "- Scenario reductions are hypothetical, not achieved savings.",
  "- No monetary savings are estimated without a verified tariff.",
  "- Technical feasibility of proposed reductions has not been tested.",
  "- Building-specific interventions require verified building-level",
  "  evidence and should not be inferred from campus-level totals.",
  "- Forecasting performance is based on historical chronological",
  "  validation and may change on future data.",
  "",
  "## 6. Conclusion",
  "",
  "The analysis supports improving energy-data monitoring,",
  "investigating missing readings, establishing regular energy",
  "reviews and testing efficiency measures through measured pilots.",
  "",
  "Forecasting can support operational planning as an experimental",
  "tool, but should not replace actual meter monitoring.",
  "",
  "The recommendations are proposed actions for consideration;",
  "their effectiveness must be evaluated using future measured data."
)

writeLines(report_lines, report_path)


# 11. FINAL VALIDATION ---------------------------------------

expected_files <- c(
  file.path(results_dir, "11_business_insights.csv"),
  file.path(results_dir, "11_business_recommendations.csv"),
  file.path(results_dir, "11_forecasting_business_metrics.csv"),
  file.path(plots_dir, "11_recommendation_priority.png"),
  file.path(plots_dir, "11_annual_data_coverage.png"),
  report_path
)

cat("\nOUTPUT VALIDATION\n")
cat("--------------------------------------------\n")

for (f in expected_files) {
  cat(
    ifelse(file.exists(f), "[OK] ", "[MISSING] "),
    basename(f), "\n"
  )
}

if (all(file.exists(expected_files))) {
  cat("\n============================================\n")
  cat("STEP 11 COMPLETED SUCCESSFULLY!\n")
  cat("============================================\n")
} else {
  warning("Some Step 11 output files are missing.")
}