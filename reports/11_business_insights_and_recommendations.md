# Step 11: Business Insights and Recommendations

## 1. Purpose

This section translates the historical energy analysis,
forecasting evaluation and hypothetical energy-saving
scenarios into practical recommendations.

The dataset used in this project is the public I-BLEND
dataset from IIIT-Delhi. The findings should not be
presented as measurements from Heritage Institute of Technology.

## 2. Key Findings

- Overall observed hourly data coverage: 87.41%.
- Total observed energy across valid records: 3434469 kWh.
- Lowest annual coverage: 2015 (72.67%).
- Highest annual coverage: 2017 (98.72%).
- Hypothetical 10% reduction: 343447 kWh based on observed baseline energy.
- Hypothetical 20% reduction: 686894 kWh based on observed baseline energy.

## 3. Forecasting Insight

The Lag-Based Regression model achieved a test MAE of 8.8221, compared with 9.1878 for the previous-hour baseline.

This represents a 3.98% reduction in MAE.

The improvement is modest. Earlier moving-block bootstrap
confidence intervals included zero, so the improvement
should not be described as statistically conclusive.

## 4. Business Recommendations

### R01 — Improve hourly meter-data completeness and automate missing-data alerts.
**Priority:** High
**Reason:** Overall coverage is below 100%, and annual coverage varies.
**Suggested KPI:** Percentage of valid hourly meter observations

### R02 — Investigate periods with missing readings, especially years with lower coverage.
**Priority:** High
**Reason:** 2015 has the lowest annual coverage in the available data.
**Suggested KPI:** Missing hourly readings per month

### R03 — Establish a regular energy-monitoring dashboard with monthly reviews.
**Priority:** High
**Reason:** Consistent monitoring helps identify unusual consumption patterns.
**Suggested KPI:** Monthly energy review completion rate

### R04 — Pilot practical energy-efficiency measures and measure their effects against a documented baseline.
**Priority:** Medium
**Reason:** Scenario analysis shows possible savings under hypothetical reductions.
**Suggested KPI:** Measured kWh reduction against baseline

### R05 — Use the lag-based forecasting model as an experimental planning aid, with continued monitoring against a simple baseline.
**Priority:** Medium
**Reason:** It achieved the lowest MAE in the evaluated test set, but improvement was modest.
**Suggested KPI:** MAE and RMSE on future chronological test periods

### R06 — Investigate high-consumption periods and compare them with operating schedules before changing equipment use.
**Priority:** Medium
**Reason:** Time-based analysis can help identify periods for further investigation.
**Suggested KPI:** Energy consumption by hour and day

## 5. Important Limitations

- Missing energy observations have not been imputed.
- Annual observed energy totals may not be directly comparable
  because coverage differs between years.
- Scenario reductions are hypothetical, not achieved savings.
- No monetary savings are estimated without a verified tariff.
- Technical feasibility of proposed reductions has not been tested.
- Building-specific interventions require verified building-level
  evidence and should not be inferred from campus-level totals.
- Forecasting performance is based on historical chronological
  validation and may change on future data.

## 6. Conclusion

The analysis supports improving energy-data monitoring,
investigating missing readings, establishing regular energy
reviews and testing efficiency measures through measured pilots.

Forecasting can support operational planning as an experimental
tool, but should not replace actual meter monitoring.

The recommendations are proposed actions for consideration;
their effectiveness must be evaluated using future measured data.
