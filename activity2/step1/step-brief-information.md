# Activity 2 — Step 1: Describing and cleaning the dataset

**Script:** [`data_processing.R`](./data_processing.R)
**Input:** `../dataset/heart_disease_risk_2026.csv`
**Output:** `../dataset/cleaned_data.rds`, `data_inspection_log.txt`, `numeric_boxplots.pdf`, `correlation_heatmap.pdf`

---

## 1. The dataset

`heart_disease_risk_2026.csv` holds **9,000 patients** described by **27 columns**. Each row is one patient: their demographics, the measurements taken at a cardiology visit (resting vital signs, a blood panel, an exercise stress test), what they report about their own habits, and whether they were diagnosed with heart disease.

The requirement for Activity 2 is at least 2 categorical and 4 numerical variables. This dataset supplies **6 categorical** (excluding the response) and **19 numerical** variables, so it clears the requirement comfortably.

### Response

| Variable | Meaning |
| --- | --- |
| `has_heart_disease` | 0 = no diagnosis, 1 = diagnosed with heart disease |

2,727 of the 9,000 patients are cases, a prevalence of **30.3 %**.

### Categorical variables

| Variable | Levels | Counts |
| --- | --- | --- |
| `sex` | Female, Male | 4,269 / 4,731 |
| `chest_pain_type` | Asymptomatic, Non-Anginal Pain, Atypical Angina, Typical Angina | 4,265 / 2,141 / 1,576 / 1,018 |
| `exercise_induced_angina` | No, Yes | 6,938 / 2,062 |
| `family_history` | No, Yes — heart disease in a first-degree relative | 6,548 / 2,452 |
| `smoker_status` | Never, Former, Current | 4,980 / 2,448 / 1,572 |
| `wearable_owner` | No, Yes — owns a fitness tracker | 5,015 / 3,985 |

### Numerical variables

| Variable | Unit | Observed range |
| --- | --- | --- |
| `age` | years | 18 – 90 |
| `resting_bp_systolic` | mmHg | 85 – 181 |
| `resting_bp_diastolic` | mmHg | 50 – 126 |
| `cholesterol_total` | mg/dL | 90 – 314 |
| `hdl` | mg/dL | 18 – 110 |
| `ldl` | mg/dL | 35 – 207 |
| `triglycerides` | mg/dL | 35 – 390 |
| `fasting_blood_sugar` | mg/dL | 60 – 204 |
| `hba1c` | % | 4.0 – 8.6 |
| `bmi` | kg/m² | 15.0 – 43.3 |
| `resting_heart_rate` | bpm | 48 – 111 |
| `max_heart_rate_achieved` | bpm, during the stress test | 93 – 210 |
| `st_depression` | mm, ST-segment depression induced by exercise | 0.0 – 6.5 |
| `alcohol_units_per_week` | units | 0.0 – 45.9 |
| `exercise_minutes_per_week` | minutes | 0 – 366 |
| `sleep_hours` | hours per night | 3.1 – 11.0 |
| `stress_score` | 0–100 self-reported scale | 0.0 – 100.0 |
| `daily_steps` | steps | 500 – 13,950 |
| `diet_quality_score` | 0–100 scale | 4.8 – 100.0 |

`patient_id` is a record number, not a measurement. It is kept as text so a cleaned row can be traced back to the raw file, and it must be excluded from every model.

---

## 2. What the data-quality checks found

All checks are in Section 2 of `data_processing.R`; the printed results are in `data_inspection_log.txt`.

| Check | Result | Action |
| --- | --- | --- |
| `NA` values | 0 in all 27 columns | none needed |
| Placeholder text (`""`, `"NA"`, `"?"`, `"unknown"`) | 0 | none needed |
| Duplicated `patient_id` | 0 | none needed |
| Fully duplicated patients | 0 | none needed |
| Inconsistent category labels (casing, whitespace, typos) | none — one spelling per level | none needed |
| Values outside their clinical range | 0 of 9,000 × 19 | none needed |
| Sentinel codes standing for "not measured" (e.g. cholesterol = 0) | none — the smallest cholesterol is 90 mg/dL | none needed |
| Systolic ≤ diastolic pressure | 0 | none needed |
| Peak heart rate ≤ resting heart rate | 0 | none needed |
| **HDL + LDL ≥ total cholesterol** | **106 patients (1.2 %)** | kept, flagged |
| **Values piled on a recording limit** | **5 columns** | kept, documented |
| Values beyond the 1.5 IQR fences | 0–485 per column | kept, not winsorized |

**The dataset arrived essentially clean.** No row and no value was deleted, imputed or overwritten. Three findings still needed a decision:

**The 106 impossible lipid panels.** By the Friedewald relationship total cholesterol = HDL + LDL + VLDL, with VLDL estimated as triglycerides / 5, and VLDL is strictly positive, so HDL + LDL can never reach the total — yet for 106 patients it does. The overshoot is at most 12 mg/dL and 2 mg/dL at the median, which is the size of the rounding applied to each of the three results independently, not a mixed-up patient or a unit error. Deleting 1.2 % of the sample over a rounding artefact would cost more than it buys, so the rows stay and carry a `lipid_panel_consistent` flag; Step 3 refits without them once, as a sensitivity check.

**Five truncated columns.** `max_heart_rate_achieved` stops dead at 210 bpm for 115 patients, and `triglycerides` (35 mg/dL, 114 patients), `bmi` (15.0, 82), `hba1c` (4.0, 64) and `ldl` (35, 56) pile up on their floor in the same way. The source truncated these columns, so their extreme tail is compressed and any statement about their shape or their extreme quantiles has to say so. The values are not changed — inventing a spread the file does not contain would be worse than reporting the ceiling. Note that the zeros in `st_depression` (193 patients) and `exercise_minutes_per_week` (146) are *not* truncation: they are genuine "no ST depression" and "no exercise at all" readings.

**Outliers are kept, unlike Activity 1.** A systolic pressure of 181 mmHg or an HbA1c of 8.6 % is precisely the patient this study is about. Winsorizing them at the IQR fences — the approach used for salary in Activity 1, where the extremes were plausibly reporting noise — would here erase the signal we are trying to model. Every flagged value passed the range and contradiction checks, so all 9,000 rows are retained. Two columns account for most of the flags: `st_depression` (485 values, 5.4 %) and `alcohol_units_per_week` (431). Both are right-skewed with a large group of patients at zero, so the 1.5 IQR fence there marks the whole upper tail of a skewed distribution rather than a set of suspect readings; the remaining columns flag at most 86.

---

## 3. What the cleaning script changed

The cleaned file has **39 columns**: the 27 original ones, plus 12 added below.

### Types

The six categorical columns arrived as plain text and are converted to factors. Reference levels are set to the healthy / unexposed group (`Female`, `Asymptomatic`, `No`, `Never`) so the coefficients in Step 3 read as "the effect of being exposed".

Every factor is an **unordered** factor, including those whose levels do have a genuine ordering (`smoker_status`: Never < Former < Current; `bp_category`; `glycemic_status`). The level order is still declared, so tables and plots come out in the right sequence, but R is told not to treat the variable as ordinal. The reason is the coding used inside a model: an unordered factor with *k* levels enters `lm` / `glm` / `aov` as *k* − 1 dummy (indicator) variables against the reference level, which is the coding for categorical predictors used throughout this course. An ordered factor would instead be expanded into orthogonal polynomial contrasts (`.L`, `.Q`, `.C`) — a coding the course does not cover, and one whose coefficients no longer read as "this level versus the reference level". Section 5 of the script asserts that no factor was left ordered.

`chest_pain_type` is nominal for a second reason: its four labels record which of the three classic angina criteria the pain met, and `Asymptomatic` is not one end of a scale whose other end is `Typical Angina`. It is listed with `Asymptomatic` first only so that it becomes the reference level.

The response is stored twice on purpose: `has_heart_disease` as a `No` / `Yes` factor for classification, and `has_heart_disease_num` as the original 0/1 so Step 2 can average it to get a prevalence.

### Derived variables

Four of them exist to break up pairs of near-redundant columns found in the correlation check; five turn a measurement into the standard clinical category so that Step 2's tables are readable.

| New variable | Definition | Why |
| --- | --- | --- |
| `pulse_pressure` | systolic − diastolic | The two pressures correlate at r = 0.77; the difference is the part they do not share, and is a risk marker in its own right |
| `non_hdl_cholesterol` | total − HDL | One number for "bad" cholesterol, instead of three collinear lipid columns |
| `cholesterol_hdl_ratio` | total / HDL | The ratio cardiologists actually read |
| `heart_rate_reserve` | peak − resting heart rate | How far the heart rate can rise under effort |
| `percent_predicted_max_hr` | 100 × peak / (220 − age) | Age fixes the attainable peak (r = −0.73); this rescales it to an age-free measure of effort tolerance (r with age falls to −0.18). 220 − age is the Fox–Naughton–Haskell (1971) formula stress-test reports print |
| `bmi_category` | Underweight / Normal / Overweight / Obese | WHO cut-offs of 18.5, 25 and 30 kg/m² (WHO Technical Report Series 894, 2000) |
| `bp_category` | Normal / Elevated / Hypertension stage 1 / stage 2 | 2017 ACC/AHA stages (Whelton et al., *Hypertension* 71(6), 2018), taking the higher of what systolic and diastolic imply |
| `glycemic_status` | Normal / Prediabetic / Diabetic | HbA1c thresholds of 5.7 % and 6.5 % (ADA *Standards of Care*, Section 2) |
| `age_group` | 18–34 / 35–49 / 50–64 / 65+ | Grouped summaries |
| `meets_activity_guideline` | ≥ 150 exercise minutes per week | The WHO recommendation |
| `lipid_panel_consistent` | HDL + LDL < total | Marks the 106 rounding-inconsistent panels |
| `has_heart_disease_num` | original 0/1 response | Lets Step 2 compute prevalences by group |

No original column is dropped, so any later step can go back to the raw measurement.

---

## 4. Warnings carried forward to Steps 2 and 3

1. **Collinear pairs.** `fasting_blood_sugar` ~ `hba1c` (r = 0.88), `cholesterol_total` ~ `ldl` (0.84), `resting_bp_systolic` ~ `resting_bp_diastolic` (0.77) and `age` ~ `max_heart_rate_achieved` (−0.73). Beyond that, the whole lipid panel satisfies total ≈ HDL + LDL + triglycerides/5, with a gap that is centred on 0 with a standard deviation of 8 mg/dL, so the four lipid columns are close to linearly dependent. Putting each pair, or all four lipids, into one regression would inflate the standard errors — use one of the pair or the derived summary, and check the VIFs.
2. **The derived variables are collinear with their parents by construction** (`heart_rate_reserve` ~ `max_heart_rate_achieved`, r = 0.93; `non_hdl_cholesterol` ~ `cholesterol_total`, r = 0.92). They are alternatives to the raw columns, never companions to them in the same model.
3. **Class imbalance.** 30.3 % of patients are cases. That is workable for a logistic regression as-is, but a default 0.5 cut-off favours the majority class, so accuracy alone will overstate performance — Step 3 should report sensitivity, specificity and ROC-AUC alongside it.
4. **Truncated tails** in `max_heart_rate_achieved`, `triglycerides`, `bmi`, `hba1c` and `ldl` (see Section 2).
5. **`wearable_owner` is not a neutral variable.** Owners average 7,302 daily steps against 5,259 for non-owners, and have a lower disease rate (25.4 % vs 34.2 %). Owning a tracker plausibly stands in for activity level and for socioeconomic status rather than causing anything, and `daily_steps` for non-owners is presumably self-reported rather than measured. Treat any `wearable_owner` effect as a confounded association, not a recommendation.
6. **This is observational data.** Every effect Step 3 estimates is an association at one point in time; none of it establishes that changing a variable would change a patient's risk.

---

## 5. Sources for the clinical cut-offs used

1. World Health Organization. *Obesity: Preventing and Managing the Global Epidemic.* WHO Technical Report Series 894, 2000. — BMI categories.
2. Whelton, P. K. et al. 2017 ACC/AHA/AAPA/ABC/ACPM/AGS/APhA/ASH/ASPC/NMA/PCNA Guideline for the Prevention, Detection, Evaluation, and Management of High Blood Pressure in Adults. *Hypertension* 71(6), 2018. — blood-pressure stages.
3. American Diabetes Association. *Standards of Care in Diabetes*, Section 2: Classification and Diagnosis of Diabetes. — HbA1c thresholds of 5.7 % and 6.5 %.
4. Friedewald, W. T., Levy, R. I. & Fredrickson, D. S. Estimation of the concentration of low-density lipoprotein cholesterol in plasma. *Clinical Chemistry* 18(6), 1972. — the lipid-panel identity used in the consistency check.
5. Fox, S. M., Naughton, J. P. & Haskell, W. L. Physical activity and the prevention of coronary heart disease. *Annals of Clinical Research* 3, 1971. — the 220 − age predicted maximum heart rate.
6. World Health Organization. *Guidelines on Physical Activity and Sedentary Behaviour*, 2020. — the 150 minutes per week activity target.
