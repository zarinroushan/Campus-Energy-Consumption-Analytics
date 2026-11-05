
# ============================================================
# STEP 09I: FINAL FORECASTING MODEL SELECTION
# ============================================================

source("R/plot_utils.R")

if (!exists("save_and_show_plot")) {
  stop("save_and_show_plot() could not be loaded.")
}

cat("\nLoading forecasting evaluation results...\n")

# ------------------------------------------------------------
# 1. LOAD PREVIOUS STEP RESULTS
# ------------------------------------------------------------

model_comparison <- read.csv(
  "results/final_forecasting_model_comparison.csv",
  stringsAsFactors = FALSE
)

error_investigation <- read.csv(
  "results/forecasting_error_investigation.csv",
  stringsAsFactors = FALSE
)

daily_results <- read.csv(
  "results/forecasting_daily_model_comparison.csv",
  stringsAsFactors = FALSE
)

robustness_results <- read.csv(
  "results/forecasting_robustness_comparison.csv",
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 2. VALIDATE INPUTS
# ------------------------------------------------------------

required_model_columns <- c(
  "Model", "Observations", "MAE", "RMSE", "Bias"
)

required_error_columns <- c(
  "Model",
  "Median_Absolute_Error",
  "Error_95th_Percentile",
  "Maximum_Absolute_Error"
)

required_daily_columns <- c(
  "Date",
  "Naive_Daily_MAE",
  "Lag_Daily_MAE",
  "MAE_Improvement"
)

if (!all(required_model_columns %in% names(model_comparison))) {
  stop("Required columns missing from final model comparison.")
}

if (!all(required_error_columns %in% names(error_investigation))) {
  stop("Required columns missing from error investigation.")
}

if (!all(required_daily_columns %in% names(daily_results))) {
  stop("Required columns missing from daily comparison.")
}

if (nrow(model_comparison) == 0 ||
    nrow(daily_results) < 2) {
  stop("Insufficient evaluation data.")
}

if (anyDuplicated(model_comparison$Model)) {
  stop("Duplicate model names detected.")
}

if (any(!is.finite(model_comparison$MAE)) ||
    any(!is.finite(model_comparison$RMSE))) {
  stop("Non-finite model evaluation metrics detected.")
}

# ------------------------------------------------------------
# 3. MERGE MODEL PERFORMANCE AND ERROR DIAGNOSTICS
# ------------------------------------------------------------

final_comparison <- merge(
  model_comparison,
  error_investigation,
  by = "Model",
  all.x = TRUE,
  sort = FALSE
)

# Models other than the two investigated in Step 09H
# will have NA for the additional error diagnostics.

final_comparison <- final_comparison[
  order(final_comparison$MAE),
]

rownames(final_comparison) <- NULL

final_comparison$MAE_Rank <- seq_len(
  nrow(final_comparison)
)

final_comparison$RMSE_Rank <- rank(
  final_comparison$RMSE,
  ties.method = "min"
)

cat("\nFINAL MODEL PERFORMANCE COMPARISON\n")
print(final_comparison)

# ------------------------------------------------------------
# 4. IDENTIFY BEST-PERFORMING MODELS
# ------------------------------------------------------------

best_mae_model <- final_comparison$Model[
  which.min(final_comparison$MAE)
]

best_rmse_model <- final_comparison$Model[
  which.min(final_comparison$RMSE)
]

best_median_model <- NA_character_

available_median <- which(
  !is.na(final_comparison$Median_Absolute_Error)
)

if (length(available_median) > 0) {
  best_median_model <- final_comparison$Model[
    available_median[
      which.min(
        final_comparison$Median_Absolute_Error[
          available_median
        ]
      )
    ]
  ]
}

cat("\nBEST MODEL BY MAE:", best_mae_model, "\n")
cat("BEST MODEL BY RMSE:", best_rmse_model, "\n")
cat("BEST MODEL BY MEDIAN ERROR:", best_median_model, "\n")

# ------------------------------------------------------------
# 5. MOVING-BLOCK BOOTSTRAP
# ------------------------------------------------------------

# Positive daily improvement means the lag model
# has lower daily MAE than the naive model.

daily_results$Date <- as.Date(daily_results$Date)

daily_results <- daily_results[
  order(daily_results$Date),
]

daily_improvement <- daily_results$MAE_Improvement

if (anyNA(daily_improvement) ||
    any(!is.finite(daily_improvement))) {
  stop("Invalid daily improvement values.")
}

if (any(diff(daily_results$Date) != 1)) {
  stop("Daily comparison contains missing or irregular dates.")
}

set.seed(2026)

bootstrap_repetitions <- 5000

block_lengths <- c(3, 7, 14)

moving_block_bootstrap <- function(
    values,
    block_length,
    repetitions
) {
  
  n <- length(values)
  
  if (block_length > n) {
    stop("Block length exceeds available observations.")
  }
  
  possible_starts <- seq_len(
    n - block_length + 1
  )
  
  blocks_needed <- ceiling(
    n / block_length
  )
  
  bootstrap_means <- replicate(
    repetitions,
    {
      selected_starts <- sample(
        possible_starts,
        size = blocks_needed,
        replace = TRUE
      )
      
      sampled_values <- unlist(
        lapply(
          selected_starts,
          function(start) {
            values[
              start:(start + block_length - 1)
            ]
          }
        )
      )
      
      sampled_values <- sampled_values[
        seq_len(n)
      ]
      
      mean(sampled_values)
    }
  )
  
  bootstrap_means
}

bootstrap_output <- lapply(
  block_lengths,
  function(block_length) {
    
    bootstrap_means <- moving_block_bootstrap(
      daily_improvement,
      block_length,
      bootstrap_repetitions
    )
    
    data.frame(
      Block_Length_Days = block_length,
      
      Bootstrap_Replications = bootstrap_repetitions,
      
      Observed_Mean_Improvement = mean(
        daily_improvement
      ),
      
      Bootstrap_Mean = mean(bootstrap_means),
      
      CI_95_Lower = as.numeric(
        quantile(bootstrap_means, 0.025)
      ),
      
      CI_95_Upper = as.numeric(
        quantile(bootstrap_means, 0.975)
      ),
      
      Proportion_Positive = mean(
        bootstrap_means > 0
      )
    )
  }
)

bootstrap_results <- do.call(
  rbind,
  bootstrap_output
)

cat("\nMOVING-BLOCK BOOTSTRAP RESULTS\n")
print(bootstrap_results)

cat(
  "\nNote: Proportion_Positive is a bootstrap",
  "distribution summary, not a p-value.\n"
)

# ------------------------------------------------------------
# 6. INTERPRET BOOTSTRAP RESULTS
# ------------------------------------------------------------

primary_bootstrap <- bootstrap_results[
  bootstrap_results$Block_Length_Days == 7,
]

if (primary_bootstrap$CI_95_Lower > 0) {
  
  bootstrap_interpretation <-
    paste(
      "The 7-day moving-block bootstrap interval is",
      "entirely above zero. This provides supportive",
      "evidence of lower daily MAE for Lag-Based Regression."
    )
  
} else if (primary_bootstrap$CI_95_Upper < 0) {
  
  bootstrap_interpretation <-
    paste(
      "The 7-day moving-block bootstrap interval is",
      "entirely below zero. This favours the Naive model."
    )
  
} else {
  
  bootstrap_interpretation <-
    paste(
      "The 7-day moving-block bootstrap interval includes",
      "zero. The daily improvement is not conclusive",
      "under this bootstrap analysis."
    )
}

cat("\nBOOTSTRAP INTERPRETATION:\n")
cat(bootstrap_interpretation, "\n")

# ------------------------------------------------------------
# 7. FINAL MODEL SELECTION
# ------------------------------------------------------------

lag_row <- final_comparison[
  final_comparison$Model == "Lag-Based Regression",
]

naive_row <- final_comparison[
  final_comparison$Model == "Naive Previous Hour",
]

if (nrow(lag_row) != 1 || nrow(naive_row) != 1) {
  stop("Required leading models were not found.")
}

mae_improvement <- (
  naive_row$MAE - lag_row$MAE
)

mae_improvement_percent <- (
  mae_improvement / naive_row$MAE
) * 100

rmse_improvement_percent <- (
  (naive_row$RMSE - lag_row$RMSE) /
    naive_row$RMSE
) * 100

# Recommendation is based primarily on test MAE,
# with RMSE as supporting evidence.
# Bootstrap uncertainty is reported separately.

recommended_model <- best_mae_model

selection_summary <- data.frame(
  Selection_Criterion = c(
    "Lowest Overall MAE",
    "Lowest Overall RMSE",
    "Lowest Median Absolute Error",
    "Recommended Model",
    "Bootstrap Evidence (7-day blocks)"
  ),
  
  Result = c(
    best_mae_model,
    best_rmse_model,
    best_median_model,
    recommended_model,
    bootstrap_interpretation
  ),
  
  stringsAsFactors = FALSE
)

cat("\nFINAL MODEL SELECTION\n")
print(selection_summary)

cat(
  "\nLag model MAE improvement:",
  round(mae_improvement, 4),
  "kWh\n"
)

cat(
  "Lag model MAE improvement percentage:",
  round(mae_improvement_percent, 2),
  "%\n"
)

cat(
  "Lag model RMSE improvement percentage:",
  round(rmse_improvement_percent, 2),
  "%\n"
)

# ------------------------------------------------------------
# 8. SAVE FINAL TABLES
# ------------------------------------------------------------

dir.create(
  "results",
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  "plots",
  showWarnings = FALSE,
  recursive = TRUE
)

write.csv(
  final_comparison,
  "results/forecasting_final_model_comparison.csv",
  row.names = FALSE
)

write.csv(
  bootstrap_results,
  "results/forecasting_moving_block_bootstrap.csv",
  row.names = FALSE
)

write.csv(
  selection_summary,
  "results/forecasting_final_model_selection.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 9. SAVE HUMAN-READABLE SUMMARY
# ------------------------------------------------------------

summary_lines <- c(
  "CAMPUS ENERGY CONSUMPTION ANALYTICS",
  "STEP 09I: FINAL FORECASTING MODEL SELECTION",
  "",
  paste("Recommended model:", recommended_model),
  paste("Best model by MAE:", best_mae_model),
  paste("Best model by RMSE:", best_rmse_model),
  paste("Best model by median error:", best_median_model),
  "",
  paste(
    "Lag model MAE improvement:",
    round(mae_improvement, 4),
    "kWh"
  ),
  paste(
    "Lag model MAE improvement:",
    round(mae_improvement_percent, 2),
    "%"
  ),
  paste(
    "Lag model RMSE improvement:",
    round(rmse_improvement_percent, 2),
    "%"
  ),
  "",
  "MOVING-BLOCK BOOTSTRAP",
  bootstrap_interpretation,
  "",
  "METHODOLOGICAL LIMITATIONS",
  "- Evaluation uses one chronological test period.",
  "- Forecasting predictions use rolling one-step-ahead evaluation.",
  "- The 7-day block length is a sensitivity-analysis choice.",
  "- Bootstrap results do not establish generalization to other periods.",
  "- Historical dataset limitations still apply.",
  "- The recommendation is not a deployment guarantee."
)

writeLines(
  summary_lines,
  "results/forecasting_final_model_selection_summary.txt"
)

# ------------------------------------------------------------
# 10. PLOT 1: FINAL MAE COMPARISON
# ------------------------------------------------------------

mae_plot <- ggplot2::ggplot(
  final_comparison,
  ggplot2::aes(
    x = reorder(Model, MAE),
    y = MAE
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Final Forecasting Model Comparison",
    subtitle = "Lower MAE indicates better average accuracy",
    x = "Forecasting Model",
    y = "Mean Absolute Error (kWh)"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  mae_plot,
  "forecasting_final_model_mae.png",
  width = 12,
  height = 7
)

# ------------------------------------------------------------
# 11. PLOT 2: BOOTSTRAP CONFIDENCE INTERVALS
# ------------------------------------------------------------

bootstrap_plot <- ggplot2::ggplot(
  bootstrap_results,
  ggplot2::aes(
    x = factor(Block_Length_Days),
    y = Bootstrap_Mean
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = CI_95_Lower,
      ymax = CI_95_Upper
    ),
    width = 0.15
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::labs(
    title = "Moving-Block Bootstrap of Daily MAE Improvement",
    subtitle = "Positive values favour Lag-Based Regression",
    x = "Block Length (Days)",
    y = "Mean Daily MAE Improvement (kWh)"
  ) +
  ggplot2::theme_minimal()

save_and_show_plot(
  bootstrap_plot,
  "forecasting_moving_block_bootstrap.png",
  width = 10,
  height = 6
)

# ------------------------------------------------------------
# 12. COMPLETION SUMMARY
# ------------------------------------------------------------

cat("\n============================================\n")
cat("STEP 09I COMPLETED SUCCESSFULLY\n")
cat("============================================\n")

cat(
  "\nRecommended model:",
  recommended_model,
  "\n"
)

cat(
  "MAE improvement:",
  round(mae_improvement_percent, 2),
  "%\n"
)

cat(
  "\nMoving-block bootstrap interpretation:\n",
  bootstrap_interpretation,
  "\n"
)

cat("\nAll Step 09I outputs saved successfully.\n")