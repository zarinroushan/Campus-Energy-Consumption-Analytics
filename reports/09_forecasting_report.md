# Campus Energy Consumption Analytics
## Step 09J: Forecasting Report

**Analysis type:** Historical hourly energy forecasting
**Dataset:** I-BLEND public dataset
**Selected forecasting model:** Lag-Based Regression

---

# 1. Executive Summary

This phase evaluated multiple forecasting approaches for
historical hourly campus energy consumption data.

Five forecasting models were compared using chronological
training and testing data. The evaluation included baseline
models, regression models, residual diagnostics, robustness
analysis, and moving-block bootstrap validation.

The Lag-Based Regression model achieved the lowest overall MAE of 8.822 kWh and the lowest RMSE of 12.099 kWh.

Compared with the Naive Previous Hour baseline, its MAE was lower by 0.366 kWh (3.98%).

However, the Naive Previous Hour model achieved a lower
median absolute error. The moving-block bootstrap interval
also included zero, so the improvement is not conclusive
under that validation approach.

---

# 2. Project Objective

The objective was to investigate whether historical hourly
energy consumption patterns could be used to estimate
subsequent hourly energy consumption.

The analysis compared simple forecasting baselines with
regression-based approaches and examined their prediction
errors across time.

# 3. Dataset and Preparation

The project uses the publicly available I-BLEND dataset.
It is a historical case study and does not represent
measurements collected from Heritage Institute of Technology
or any current campus monitoring system.

The hourly forecasting target was constructed using building
energy readings and the project coverage rules. Building
readings were considered eligible when hourly meter coverage
was at least 75%. The campus estimate required at least five
eligible buildings.

Missing target observations were not replaced with zero.
The forecasting analysis selected the longest continuous
period of observed hourly campus energy values.

**Selected continuous period:** 10 June 2016, 09:00 to
5 July 2017, 18:00.

**Selected observations:** 9,370 hourly records.
**Training observations:** 7,496.
**Testing observations:** 1,874.

The split was chronological: earlier observations were used
for training and later observations for testing.

# 4. Forecasting Methodology

The following five models were evaluated:

1. **Naive Previous Hour:** Uses the previous hour's actual
   energy consumption as the forecast.
2. **Seasonal Naive (24 Hours):** Uses the actual value from
   the same hour on the previous day.
3. **Linear Trend Regression:** Models energy consumption
   using a time index.
4. **Calendar Regression:** Uses hour, day of week, and
   weekend information.
5. **Lag-Based Regression:** Uses previous-hour energy,
   previous-24-hour energy, hour, and day of week.

The evaluation used rolling one-step-ahead predictions.
For test-period predictions, the models could use actual
previous observations from the test period when calculating
subsequent forecasts. This is not the same as producing a
fixed-origin forecast for the entire test horizon.

# 5. Model Performance

The models were evaluated using:

- **MAE:** Mean absolute error, measuring average absolute
  prediction error.
- **RMSE:** Root mean squared error, giving greater weight
  to larger prediction errors.
- **Bias:** Mean predicted value minus actual value. Positive
  bias indicates average overprediction; negative bias
  indicates average underprediction.

| Model | Observations | MAE_kWh | RMSE_kWh | Bias_kWh |
| --- | --- | --- | --- | --- |
| Lag-Based Regression | 1874 | 8.822 | 12.099 | 0.528 |
| Naive Previous Hour | 1874 | 9.188 | 13.438 | 0.052 |
| Seasonal Naive (24 Hours) | 1874 | 16.467 | 24.480 | 0.965 |
| Calendar Regression | 1874 | 28.905 | 34.279 | 5.798 |
| Linear Trend Regression | 1874 | 30.227 | 36.573 | -5.980 |

The best model by MAE was **Lag-Based Regression**.
The best model by RMSE was **Lag-Based Regression**.
The best median absolute error among the two models with detailed error diagnostics was **Naive Previous Hour**.

# 6. Detailed Error Investigation

Detailed error diagnostics were conducted for the Lag-Based
Regression model and the Naive Previous Hour baseline.

| Model | Median_Absolute_Error | Error_95th_Percentile | Maximum_Absolute_Error |
| --- | --- | --- | --- |
| Lag-Based Regression | 6.573 | 25.508 | 66.975 |
| Naive Previous Hour | 5.881 | 31.374 | 82.337 |

Across 79 evaluated days, Lag-Based Regression had lower daily MAE on 51 days, while Naive Previous Hour had lower daily MAE on 28 days. There were 0 days with equal daily MAE.

The mean daily MAE improvement was 0.367 kWh.

The results show that the Lag-Based Regression model
performed better on average, but its advantage was not
consistent on every day or across every error metric.

# 7. Robustness and Bootstrap Validation

A moving-block bootstrap was used to examine daily MAE
improvement while resampling consecutive blocks of days.
This approach retains some local time dependence within
the sampled blocks.

| Block_Length_Days | Replications | CI_95_Lower | CI_95_Upper | Proportion_Positive |
| --- | --- | --- | --- | --- |
|  3 | 5000 | -0.066 | 0.732 | 0.9524 |
|  7 | 5000 | -0.089 | 0.654 | 0.9318 |
| 14 | 5000 | -0.143 | 0.601 | 0.8924 |

**7-day bootstrap interpretation:** The 7-day moving-block bootstrap interval includes zero. The daily improvement is not conclusive under this bootstrap analysis.

The proportion of positive bootstrap estimates is a
description of the bootstrap distribution, not a p-value.
The confidence interval should be considered alongside
the observed MAE improvement.

# 8. Final Model Selection

Lag-Based Regression was selected as the recommended model
because it achieved the lowest overall MAE and RMSE among
the five evaluated models.

- MAE: 8.822 kWh
- RMSE: 12.099 kWh
- MAE improvement over Naive Previous Hour: 3.98%
- RMSE improvement over Naive Previous Hour: 9.97%

This is a practical model-selection decision based on the
observed test-period metrics. It should not be interpreted
as proof that the model will outperform the baseline on
all future datasets or operating conditions.

# 9. Limitations

1. The analysis uses historical public data and does not
   measure current energy consumption.
2. The selected continuous period is limited to June 2016
   through July 2017.
3. The campus target depends on building meter coverage
   and the eligibility rules used during preparation.
4. The longest continuous observed period was selected;
   results may differ for other time periods.
5. Rolling one-step-ahead evaluation uses actual previous
   observations during the test period. Performance may
   differ for longer fixed-origin forecasts.
6. The bootstrap confidence intervals include zero, so
   the observed daily improvement is not conclusive under
   this bootstrap analysis.
7. Forecasting errors remain substantial for some hours.
   The largest observed absolute error for the selected
   model was approximately 66.975 kWh.
8. Historical patterns may not represent future occupancy,
   weather, schedules, equipment, or campus operations.

# 10. Conclusion

The forecasting analysis found that Lag-Based Regression
provided the best overall MAE and RMSE among the five
evaluated models on the selected historical test period.

Its improvement over the Naive Previous Hour baseline was
modest: approximately 3.98% in MAE and 9.97% in RMSE.
The Naive baseline nevertheless achieved a lower median
absolute error, and the moving-block bootstrap analysis
did not establish a conclusive positive daily improvement.

Therefore, Lag-Based Regression is recommended as the
best-performing model in this experiment, while the
Naive Previous Hour model remains a useful and competitive
baseline.

Future work could evaluate additional continuous periods,
weather and occupancy features with correctly aligned
timestamps, and fixed-origin multi-step forecasting.

---

# 11. Supporting Figures

The following plots were generated during the forecasting
analysis and are saved in the project's `plots/` directory:

- `forecasting_final_model_mae.png`
- `forecasting_moving_block_bootstrap.png`
- `actual_vs_predicted.png`
- `forecast_residuals_over_time.png`
- `forecast_residual_distribution.png`
- `residuals_vs_predicted.png`
- `forecast_error_by_hour.png`
- `forecast_error_by_weekday.png`
- `forecasting_weekly_robustness.png`
- `forecasting_hourly_robustness.png`
- `forecasting_weekday_robustness.png`
- `forecasting_cumulative_mae.png`
- `forecasting_paired_error_improvement.png`
- `forecasting_error_timeline.png`
- `forecasting_daily_mae_comparison.png`
- `forecasting_daily_improvement_distribution.png`

**End of Step 09 Forecasting Report.**
