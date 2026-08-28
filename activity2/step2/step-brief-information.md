# Activity 2 — Step 2: Descriptive statistics

**Script:** [`descriptive_statistics.R`](./descriptive_statistics.R)
**Input:** `../dataset/cleaned_data.rds` (9,000 × 39, from [Step 1](../step1/step-brief-information.md))
**Output:** 5 CSV tables, 8 PDF figures, `descriptive_log.txt`

This step describes the data. It reports centre, spread and shape for every variable, then measures how strongly each one is associated with the diagnosis. The associations are quoted as **effect sizes** (Cohen's *d*, Cramér's *V*), not p-values: with n = 9,000 almost any difference is "significant", so a p-value here would tell us nothing about which variables matter. Formal tests and the models belong to Step 3.

---

## 1. The response

2,727 of 9,000 patients are diagnosed with heart disease — a prevalence of **30.3 %**. This is the baseline every subgroup rate below is compared against.

## 2. The numerical variables

Full table: `numeric_summary_table.csv` (n, mean, sd, CV, min, Q1, median, Q3, max, IQR, skewness, excess kurtosis for all 24 numerical variables).

**Centre — this is a screening population, not a ward.** Median age 54, blood pressure 128/81 mmHg, BMI 25.3, total cholesterol 189 mg/dL, HbA1c 5.8 %. Every one of those sits in or just above the normal range, which is what makes a 30.3 % prevalence meaningful: these are ordinary patients being screened, not patients already known to be ill.

**Spread — the lifestyle variables vary far more than the clinical ones.** Using the coefficient of variation (sd / mean), the body's regulated quantities barely move: `percent_predicted_max_hr` 0.09, resting heart rate and systolic pressure 0.11, HbA1c 0.12. The behavioural ones swing widely: `st_depression` 0.95, `alcohol_units_per_week` 0.89, `exercise_minutes_per_week` 0.46. A model therefore has much more variation to work with on the lifestyle side.

**Shape — 21 of the 24 variables are close to symmetric** (|skewness| < 0.25 and |excess kurtosis| < 0.4). Exactly three are not:

| Variable | Skewness | Excess kurtosis | Why |
| --- | --- | --- | --- |
| `st_depression` | +2.04 | +5.60 | 193 patients pinned at 0, long right tail |
| `alcohol_units_per_week` | +2.03 | +6.28 | most drink little, a few drink heavily |
| `cholesterol_hdl_ratio` | +1.41 | +4.25 | a ratio — a low HDL in the denominator blows it up |

`triglycerides` is *not* among them (skewness 0.13), even though a lipid measurement is normally right-skewed. That is a consequence of the truncation Step 1 found at 35 and 390 mg/dL: the tail was cut off before we got the file.

**Normality.** `shapiro.test` refuses samples above 5,000, so it is run on a random subsample of 5,000. More importantly, at n = 9,000 a normality test rejects deviations far too small to matter, so the tests are reported for completeness and the conclusions come from the skewness figures and the QQ plots. Jarque–Bera rejects 14 of 24 columns and does not reject the other 10 (`cholesterol_total` p = 0.85, `hdl` 0.60, `hba1c` 0.18, `stress_score` 0.85 …) — unusually normal-looking for clinical data. Shapiro's *W* is ≥ 0.994 for 21 variables, and drops only for the three skewed ones (0.811, 0.816, 0.915). Both approaches flag the same three.

**Transformations** (`skewed_variables_transformed.pdf`). A log fixes the two long-tailed variables: alcohol +2.03 → +0.02, cholesterol/HDL ratio +1.41 → +0.32. A square root only halves `st_depression`'s skew (+2.04 → +0.65) and cannot do better, because the problem is a point mass at zero, not a tail — Step 3 should split it into "any ST depression yes/no" plus the amount among those who have some. For contrast, logging `triglycerides` would make it *worse* (+0.13 → −1.09): transformations go to the variables that need them, not by habit to every lipid.

## 3. The categorical variables

Full table: `categorical_summary_table.csv`.

The sample is close to balanced on sex (47.4 % female). The clinical categories are deliberately not: **47.4 % of patients are asymptomatic** and only 11.3 % report typical angina. The derived categories from Step 1 say the same thing from the clinical side — 61.6 % are already in a hypertension stage, 58.1 % are prediabetic or diabetic by HbA1c, 53.1 % are overweight or obese, and 56.6 % miss the 150-minute activity guideline.

The smallest level in the entire table is Underweight with 549 patients, so nothing needs collapsing before Step 3 and every cell of a two-way table will still be well filled.

## 4. What separates diagnosed from undiagnosed patients

### Numerical variables — Cohen's *d*

Full table: `numeric_by_outcome_table.csv`. Read as 0.2 small, 0.5 medium, 0.8 large.

| Size | Variables |
| --- | --- |
| **Very large** | `percent_predicted_max_hr` (−1.58), `max_heart_rate_achieved` (−1.55), `heart_rate_reserve` (−1.54) |
| Large | `st_depression` (+0.83), `cholesterol_hdl_ratio` (+0.82) |
| Medium | `age` (+0.66), `non_hdl_cholesterol` (+0.62), `ldl` (+0.60), `hdl` (−0.57), `resting_bp_systolic` (+0.56), `exercise_minutes_per_week` (−0.56), `hba1c` (+0.50) |
| Small | `bmi`, `fasting_blood_sugar`, `resting_bp_diastolic`, `resting_heart_rate`, `cholesterol_total`, `diet_quality_score` (−), `daily_steps` (−), `pulse_pressure`, `triglycerides`, `stress_score` |
| Negligible | `sleep_hours` (−0.11), `alcohol_units_per_week` (+0.09) |

**Peak heart rate is the dominant signal.** Diagnosed patients average 146 bpm at peak effort against 173 bpm for everyone else — a 27 bpm gap, more than one and a half standard deviations. The top three entries are near-duplicates of each other (mutual correlations 0.76–0.93 per Step 1), so this is *one* finding, not three.

Every sign points the way clinical knowledge says it should, which is a useful check that the data behaves sensibly. Two results cut against expectation: **alcohol and sleep separate the groups least of all 24 variables**, and the three heart-rate variables *derived in Step 1* take the top three places ahead of every raw measurement — the evidence that those derivations earned their place.

`top_predictors_density.pdf` makes the important caveat visible: even for the strongest variable the two groups overlap heavily. No single measurement separates them, which is precisely the argument for a multivariable model in Step 3.

### Categorical variables — Cramér's *V*

Full tables: `categorical_by_outcome_table.csv`, `categorical_association_strength.csv`. Figure: `disease_rate_by_category.pdf`.

| Variable | *V* | Disease rate by level |
| --- | --- | --- |
| `exercise_induced_angina` | **0.45** | No 19.1 % → **Yes 67.8 %** |
| `age_group` | 0.28 | 9.5 % → 18.0 % → 31.9 % → **51.1 %** |
| `bp_category` | 0.21 | 17.0 % → 25.3 % → 30.4 % → 43.9 % |
| `glycemic_status` | 0.20 | 20.7 % → 33.7 % → 45.6 % |
| `bmi_category` | 0.20 | 15.8 % → 22.7 % → 34.4 % → 47.2 % |
| `meets_activity_guideline` | 0.19 | No 38.1 % vs Yes 20.1 % |
| `smoker_status` | 0.17 | Never 25.2 % → Former 30.3 % → Current 46.4 % |
| `chest_pain_type` | 0.16 | Asymptomatic 25.4 % → Typical angina 47.9 % |
| `sex` | 0.12 | Female 24.7 % vs Male 35.4 % |
| `wearable_owner` | 0.09 | Yes 25.4 % vs No 34.2 % — see §6 |
| `family_history` | 0.07 | No 28.3 % vs Yes 35.6 % |

`exercise_induced_angina` dominates everything else, at 0.45 against 0.28 for the next variable. Combined with the heart-rate result above: **the exercise stress test carries most of the information in this dataset.**

Two rankings are worth noting. `chest_pain_type` places only eighth, below all four categories derived in Step 1, despite being the variable a textbook heart-disease study leads with. And `family_history` is last — it does separate the groups, but by less than anything else here. Every ordered variable moves monotonically in the expected direction; former smokers sit neatly between never and current.

## 5. Relationships among the predictors

`correlation_heatmap.pdf`, ordered by clustering so that variables measuring the same thing sit together. The blocks are exactly the ones Step 1 flagged: the lipid panel, the two blood-sugar measures, the two blood pressures, the heart-rate group.

Outside those blocks correlations are weak. The strongest link between any lifestyle variable and any clinical one is only −0.31 (`exercise_minutes_per_week` with `resting_heart_rate`), and `sleep_hours` reaches at most 0.03 with anything clinical. **The lifestyle block and the clinical block carry largely separate information**, so both belong in a model — neither is a proxy for the other.

`age_relationships.pdf` isolates age, because it drives several other predictors and is therefore a confounder for all of them. Peak heart rate falls almost linearly with age (r = −0.73) — that is physiology, not disease. Since both age and peak heart rate are among the strongest correlates of the diagnosis, Step 3 has to fit them together; the marginal effect of either is partly the other.

## 6. Three findings that need care

**The wearable-owner gap is not an age effect.** The obvious explanation for owners' lower disease rate is that they are younger — and it is wrong: mean age 53.8 vs 54.1, and the gap survives inside every age band (widest among the youngest, 5.8 % vs 12.6 %). What does differ is activity: 7,302 daily steps vs 5,259, and 165 exercise minutes per week vs 119. `wearable_owner` stands in for how active a patient is and for the kind of person who buys a tracker. Owning a tracker cannot protect anyone; if Step 3 includes it, the activity variables must be in the model beside it.

**The sex gap is not explained by chest-pain type.** The natural explanation — men report higher-risk pain types more often — fails on both counts. The chest-pain profiles of the two sexes are nearly identical (typical angina 10.9 % of women, 11.7 % of men), and the male excess persists inside *every* pain type: 9.0, 10.0, 13.2 and 14.0 percentage points. Sex acts on its own here, not through the reported symptom.

**The truncated columns distort their own summary statistics.** For the five columns Step 1 found cut off at a recording limit, the reported extreme is the limit and the sd is an underestimate. Between 0.6 % and 1.3 % of patients sit exactly on each limit; the largest is `max_heart_rate_achieved`, with 1.28 % pinned at 210 bpm. Since that variable carries the largest effect size in the dataset, the report states the ceiling explicitly — the truncation is at the healthy end, so the measured separation is if anything an underestimate.

---

## 7. What Step 3 takes from here

1. **The exercise stress test** — peak heart rate, exercise-induced angina, ST depression — separates the groups far better than anything else. Any model that omits it is not competitive.
2. **Age belongs in every model as a control**: it is both a strong correlate of the diagnosis and a driver of several other predictors.
3. **The lifestyle block is nearly uncorrelated with the clinical block**, so it is not redundant — but alcohol and sleep separate the groups barely at all on their own. Whether they matter after adjustment is a Step 3 question, and the descriptives cannot settle it.
4. **Transform three variables**: log for alcohol and the cholesterol/HDL ratio; `st_depression` needs its zero mass handled separately. Nothing else needs transforming.
5. **One variable per correlation block** goes into a model, not all of them — and the derived variables are alternatives to their parents, never companions.
6. **No single variable separates the groups.** Even the strongest shows heavy overlap, which is the case for a multivariable model rather than a screening rule on one measurement.
