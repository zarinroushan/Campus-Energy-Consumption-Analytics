# CAMPUS ENERGY CONSUMPTION ANALYTICS

## 1. Executive Summary
This project presents an exploratory and forecasting-based analysis of historical campus energy consumption using the public I-BLEND dataset from IIIT-Delhi.
The analysis examines energy consumption patterns, building-level differences, temporal trends, data availability and the performance of several forecasting approaches.
The forecasting experiments identify Lag-Based Regression as the best-performing model by Mean Absolute Error among the five evaluated models. However, its improvement over the previous-hour naive baseline is modest.

## 2. Project Objectives
- Examine historical campus energy consumption patterns.
- Assess the availability and quality of energy meter observations.
- Compare energy consumption across monitored buildings.
- Investigate hourly, daily, monthly and weekday patterns.
- Develop baseline and regression-based forecasting models.
- Evaluate model performance using MAE, RMSE and bias.
- Investigate forecasting errors and assess robustness.

## 3. Dataset Description
The project uses the public I-BLEND dataset associated with IIIT-Delhi.
The dataset contains historical energy meter readings and supplementary information. The processed hourly energy dataset covers 10 August 2013 to 31 December 2017.
Important dataset considerations:
- The dataset does not represent measurements from Heritage Institute of Technology.
- Historical energy observations contain missing and irregular coverage.
- Occupancy readings are Wi-Fi-based estimates rather than exact headcounts.
- Building-level comparisons depend on available meter coverage.
- Supplementary weather observations from 2018 were not treated as contemporaneous with the 2013–2017 energy observations.

## 4. Data Preparation and Cleaning
The raw energy readings were cleaned and converted into hourly observations.
Key preparation activities included:
- Date-time standardization using the Asia/Kolkata time zone.
- Conversion of power readings into hourly energy estimates.
- Aggregation of minute-level readings into hourly observations.
- Calculation of meter coverage percentages.
- Creation of calendar and temporal features.
- Integration of available occupancy information.
- Preservation of missing observations rather than automatically replacing them with zero.
The hourly processed dataset contains 38,520 observations.

## 5. Exploratory Data Analysis
The exploratory analysis investigated overall energy trends, building-level consumption and temporal patterns.
### 5.1 Energy Data Coverage
Meter coverage varied across buildings and time periods. Consequently, campus-level energy estimates were calculated using explicit coverage criteria rather than assuming every building had complete observations.
### 5.2 Building-Level Analysis
Average hourly energy consumption and distributions were compared across monitored buildings.
### 5.3 Temporal Analysis
Hourly, daily, monthly and weekday patterns were examined to understand changes in energy consumption over time.
Daily averages were calculated using a minimum of 18 qualifying hourly observations per day. Gaps were preserved rather than connected as continuous observations.

## 6. Forecasting Methodology
The forecasting dataset was constructed from the longest continuous period of eligible campus energy observations.
Selected continuous period: 9,370 hourly observations.
Training observations: 7,496.
Testing observations: 1,874.
Training and testing were separated chronologically to avoid using future observations to train the models.
The training period ended on 18 April 2017 at 16:00, and the testing period began on 18 April 2017 at 17:00.
All evaluated models used rolling one-step-ahead predictions. Actual previous observations from the test period were available when generating subsequent predictions.

## 7. Forecasting Models
The following five models were evaluated:
1. Naive Previous Hour
2. Seasonal Naive (24 Hours)
3. Linear Trend Regression
4. Calendar Regression
5. Lag-Based Regression

## 8. Forecasting Model Comparison
```
                     Model Observations       MAE     RMSE
      Lag-Based Regression         1874  8.822119 12.09897
       Naive Previous Hour         1874  9.187838 13.43845
 Seasonal Naive (24 Hours)         1874 16.466897 24.47987
       Calendar Regression         1874 28.904753 34.27884
   Linear Trend Regression         1874 30.227108 36.57293
        Bias
  0.52839825
  0.05241307
  0.96474680
  5.79816643
 -5.98021697
```
The best-performing model by MAE was Lag-Based Regression.
Its MAE was 8.822 kWh and its RMSE was 12.099 kWh.

## 9. Comparison with the Naive Baseline
Lag-Based Regression achieved an MAE of 8.822 kWh, compared with 9.188 kWh for Naive Previous Hour.
This represents an MAE improvement of approximately 3.98%.
The RMSE improvement was approximately 9.97%.
The improvement is measurable but modest. The naive baseline remains a strong comparison model.

## 10. Forecasting Error Investigation
The forecasting errors were investigated across time, hours, weekdays and individual observations.
Important findings:
- Lag-Based Regression achieved lower average absolute error.
- Naive Previous Hour achieved a lower median absolute error.
- Lag-Based Regression had lower 95th-percentile absolute error.
- Both models produced occasional substantial forecasting errors.
- Performance varied across hours and weekdays.
The results demonstrate that a model performing better on average does not necessarily perform better for every individual observation.

## 11. Statistical and Robustness Validation
Daily model performance was compared using paired daily MAE differences.
Moving-block bootstrap analysis was conducted using block lengths of 3, 7 and 14 days.
The block bootstrap was used to examine uncertainty while retaining some short-term temporal dependence.
The results should be interpreted as exploratory evidence rather than definitive proof of model superiority. Consecutive daily observations can remain serially correlated, and the analysis does not eliminate every source of dependence.

## 12. Limitations
- The dataset represents a historical public case study rather than the user's own college.
- Missing meter observations limit the completeness of campus-level estimates.
- Occupancy estimates are not exact headcounts.
- The selected forecasting period is only one continuous historical segment.
- The models use rolling one-step-ahead evaluation rather than fixed-origin long-horizon forecasting.
- Weather information was not used as a contemporaneous explanatory variable for the historical energy period.
- The forecasting improvement over the naive baseline is relatively small.
- The findings should not be interpreted as evidence of current campus energy consumption or guaranteed future savings.

## 13. Conclusion
This project demonstrates a structured workflow for cleaning, exploring and forecasting historical campus energy consumption data.
The exploratory analysis highlights variation in consumption across buildings and time periods, while the forecasting experiments demonstrate the importance of comparing more complex models against simple baselines.
Lag-Based Regression achieved the lowest MAE among the evaluated models, but its improvement over the previous-hour baseline was limited. Further work could investigate additional explanatory variables, alternative time-series models and evaluation across multiple continuous historical periods.

## 14. Future Scope
- Investigate additional forecasting algorithms.
- Explore weather variables when temporally aligned data is available.
- Evaluate performance across multiple continuous historical periods.
- Investigate building-specific forecasting models.
- Explore longer-horizon forecasting.
- Develop a dashboard for interactive energy analysis.
- Adapt the methodology to a campus with its own verified energy meter data.

## 15. Selected Project Outputs
### Refined visualizations
- detailed_daily_energy_trend_refined.png
- detailed_monthly_energy_refined.png
- actual_vs_predicted_refined.png
- forecast_residuals_over_time_refined.png
### Forecasting results
- final_forecasting_model_comparison.csv
- forecasting_error_investigation.csv
- forecasting_moving_block_bootstrap.csv
- forecasting_final_model_selection.csv

## 16. Reproducibility
All analysis stages are implemented in R scripts and their outputs are saved in the project directories. The report is generated from the existing processed datasets and results files.

