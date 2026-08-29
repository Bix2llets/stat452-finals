# Activity 2 — Step 2: Descriptive statistics

**Script:** [`descriptive_statistics.R`](./descriptive_statistics.R)
**Input:** `../dataset/cleaned_data.rds` (9,000 × 39, from [Step 1](../step1/step-brief-information.md))
**Output:** 6 CSV tables, 9 PDF figures, `descriptive_log.txt`

This step describes the data. It reports centre, spread and shape for every variable, then measures how strongly each one is associated with the diagnosis. The associations are quoted as **effect sizes** — the standardised mean difference for the numerical variables, the risk difference and odds ratio for the categorical ones — not p-values: with n = 9,000 almost any difference is "significant", so a p-value here would tell us nothing about which variables matter. Formal tests and the models belong to Step 3.

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

`triglycerides` is *not* among them (skewness 0.13), even though a lipid measurement is normally right-skewed. That is a consequence of the truncation Step 1 found: 114 patients pile up on the 35 mg/dL floor, so the distribution was compressed before we got the file.

**Normality.** Judged with the **chi-square goodness-of-fit test** against a normal fitted to each variable.

*Why not Shapiro–Wilk or Jarque–Bera, which Activity 1 Step 2 used?* The script now runs both against the same columns rather than dismissing them, so the answer is checked rather than argued.

**Shapiro–Wilk cannot be used at all.** R's `shapiro.test()` stops at `sample size must be between 3 and 5000`, and this dataset has 9,000 patients. Activity 1 had n = 600, so the limit never arose there. Splitting the column to get under it would make the answer depend on which half was tested. The script prints the error rather than claiming it.

**Jarque–Bera runs, and changes nothing.** It rejects 14 of 24 variables against the chi-square test's 17 — the same large-sample behaviour — and it puts the *same three* variables an order of magnitude clear of the rest:

| Variable | Jarque–Bera | *X*² | Skewness |
| --- | --- | --- | --- |
| `alcohol_units_per_week` | 20,972 | 3,744 | +2.03 |
| `st_depression` | 18,017 | 3,341 | +2.04 |
| `cholesterol_hdl_ratio` | 9,766 | 768 | +1.41 |
| *everything else* | ≤ 102 | ≤ 285 | \|skew\| ≤ 0.23 |

So the transformations in §2.3 are the same either way.

**Where the two disagree says why the binned test is the one we read.** Jarque–Bera is a function of the skewness and excess kurtosis alone, so it sees nothing in `resting_heart_rate` (JB 0.6, skewness −0.019) while the goodness-of-fit test puts it at *X*² = 285 — a departure in the *shape* of the distribution that the two moments cannot detect. The binned test also gives a ranking on a common 7 df, which is what we actually need, rather than a yes/no verdict.

**And it is the method the course teaches.** The chi-square goodness-of-fit test carries the decision; the other two are printed for continuity with Activity 1, not used to choose the transformations.

*The test itself.* Each column is cut at the deciles of the fitted normal, so all ten bins have an expected count of 900, and

$$X^2 = \sum_{\text{bins}} \frac{(\text{observed} - \text{expected})^2}{\text{expected}}$$

is referred to a chi-square distribution on *k* − 1 − 2 = 7 degrees of freedom — two degrees of freedom are spent estimating the mean and sd from the sample. The critical value is `qchisq(0.05, df = 7, lower.tail = FALSE)` = **14.07**: written with the significance level itself and the upper tail, so the number is visibly the point leaving 5 % of the distribution above it — the 5 % the test rejects in.

The **size** of *X*² is what is read, not its p-value. At n = 9,000 the test detects departures far too small to matter, and it duly rejects **17 of the 24 variables**. The ordering shows why only three of those rejections mean anything:

| *X*² | Variables | Verdict |
| --- | --- | --- |
| 7 – 30 | ten variables | at or near the critical value; indistinguishable from normal |
| 80 – 290 | eleven variables | formally rejected, but every one has \|skewness\| < 0.24 and \|excess kurtosis\| < 0.37 — a shape no plot would call non-normal |
| **768, 3341, 3744** | `cholesterol_hdl_ratio`, `st_depression`, `alcohol_units_per_week` | an order of magnitude clear of everything else |

The three at the bottom are exactly the three the skewness column flags, so the test and the moments agree, and they are the only three Section 2.3 transforms.

**Transformations** (`skewed_variables_transformed.pdf`). Every candidate is scored on the skewness it leaves behind, walking the ladder of powers from mildest to strongest. Each variable wants a different rung:

| Variable | Raw | √ | ∛ | log | log(1+*x*) | Chosen |
| --- | --- | --- | --- | --- | --- | --- |
| `alcohol_units_per_week` | +2.027 | +0.666 | +0.152 | — | **+0.021** | log(1+*x*) |
| `cholesterol_hdl_ratio` | +1.409 | +0.814 | +0.642 | **+0.323** | +0.518 | log |
| `st_depression` | +2.040 | +0.652 | **−0.054** | — | +0.775 | cube root |

A plain log is only defined where the variable is strictly positive. `cholesterol_hdl_ratio` is, so it takes the plain log, which beats the shifted one (+0.32 against +0.52 — the shift is not free, it flattens the low end). The other two contain zeros, so only log(1+*x*) is available to them.

A note on `st_depression`, because the obvious explanation of its skew is the wrong one. It is tempting to blame the 193 patients sitting exactly at zero — a point mass that no monotone transformation can move — and conclude that the variable must be split into "any ST depression yes/no" plus an amount. The data rejects that: **dropping the zeros changes the skewness by 0.01** (+2.040 → +2.051). The spike is only 2.1 % of the sample, far too small to bend the third moment of 9,000 observations. The skew is a genuine long right tail (median 0.7 mm, top 1 % at 4.7 mm and beyond), which is precisely what a power transformation fixes — hence the cube root, which lands on −0.05. **No splitting is needed.**

For contrast, logging `triglycerides` would make it *worse* (+0.13 → −1.09): transformations go to the variables the skewness column flags, not by habit to every lipid.

## 3. The categorical variables

Full table: `categorical_summary_table.csv`.

The sample is close to balanced on sex (47.4 % female). The clinical categories are deliberately not: **47.4 % of patients are asymptomatic** and only 11.3 % report typical angina. The derived categories from Step 1 say the same thing from the clinical side — 61.6 % are already in a hypertension stage, 58.1 % are prediabetic or diabetic by HbA1c, 53.1 % are overweight or obese, and 56.6 % miss the 150-minute activity guideline.

The smallest level in the entire table is Underweight with 549 patients, so nothing needs collapsing before Step 3 and every cell of a two-way table will still be well filled.

## 4. What separates diagnosed from undiagnosed patients

### Numerical variables — standardised mean difference

Full table: `numeric_by_outcome_table.csv`. Each gap between the two group means is divided by the **pooled standard deviation** of the two groups — the same pooled estimate a two-group comparison of means uses, and the square root of the within-group mean square a one-factor ANOVA on the same split would report. Dividing by it makes 24 variables measured in mmHg, mg/dL, bpm and minutes comparable with one another. The number reads as a count of standard deviations; the conventional wording is 0.2 small, 0.5 medium, 0.8 large, which is a reporting convention rather than a test.

**The size bands below are distances, not significance labels — 1.96 does not apply to them.** The standardised mean difference divides the gap by the spread of the *patients*, so it does not grow with the sample: 1.55 means the two group means sit 1.55 patient standard deviations apart, and it would still be 1.55 with twice the data. 1.96 is the 5 % two-tail cut-off for a *z* statistic, which divides the same gap by its standard error instead, and so carries a factor of √n:

$$\text{smd} = \frac{\bar x_1 - \bar x_0}{s_p} \qquad z = \frac{\text{smd}}{\sqrt{1/n_1 + 1/n_0}}$$

At these group sizes (2,727 and 6,273) the multiplier is **43.6**. Even the smallest entry in the table — `alcohol_units_per_week` at 0.09 — reaches *z* = 3.9 and clears 1.96 comfortably. So all 24 variables would be "significant", and none of the 24 reaches 1.96 on the smd scale. Comparing an smd against 1.96 is a category error, and the fact that every variable passes the test is exactly why the test is left to Step 3.

To make "very large" mean something in patients rather than in convention, the script prints the overlap each band implies — the share of diagnosed patients falling past the median of the undiagnosed group (50 % for identical groups, 100 % for complete separation):

| Variable | smd | Diagnosed past the healthy median |
| --- | --- | --- |
| `percent_predicted_max_hr` | −1.58 | 93.2 % |
| `st_depression` | +0.83 | 71.8 % |
| `age` | +0.66 | 72.5 % |
| `sleep_hours` | −0.11 | 51.9 % |

| Size | Variables |
| --- | --- |
| **Very large** | `percent_predicted_max_hr` (−1.58), `max_heart_rate_achieved` (−1.55), `heart_rate_reserve` (−1.54) |
| Large | `st_depression` (+0.83), `cholesterol_hdl_ratio` (+0.82) |
| Medium | `age` (+0.66), `non_hdl_cholesterol` (+0.62), `ldl` (+0.60), `hdl` (−0.57), `resting_bp_systolic` (+0.56), `exercise_minutes_per_week` (−0.56), `hba1c` (+0.50) |
| Small | `bmi`, `fasting_blood_sugar`, `resting_bp_diastolic`, `resting_heart_rate`, `cholesterol_total`, `diet_quality_score` (−), `daily_steps` (−), `pulse_pressure`, `triglycerides`, `stress_score` |
| Negligible | `sleep_hours` (−0.11), `alcohol_units_per_week` (+0.09) |

**Peak heart rate is the dominant signal.** Diagnosed patients average 146 bpm at peak effort against 173 bpm for everyone else — a 27 bpm gap, more than one and a half standard deviations. The top three entries are near-duplicates of each other (mutual correlations 0.76–0.93 per Step 1), so this is *one* finding, not three.

**Which variable is "exertion".** All three come from the same exercise stress test, in which the patient walks a treadmill to peak effort. `max_heart_rate_achieved` is the highest heart rate reached during that test; `heart_rate_reserve` is that peak minus the resting rate; `percent_predicted_max_hr` is that peak as a percentage of the 220 − age prediction. The exertion is the treadmill test itself — there is no separate exertion variable in the dataset.

**What the data supports is the gap, not a reason for it.** An earlier draft said diagnosed patients "cannot raise their heart rate under exertion". That is a mechanism, and this is observational data: the recorded fact is that diagnosed patients *reached* a lower peak on the test. Whether they could not go higher, or the test was stopped early because of symptoms, or they were on rate-limiting medication, is not recorded here and cannot be settled from this file. The claim in the report is the 27 bpm gap.

Every sign points the way clinical knowledge says it should, which is a useful check that the data behaves sensibly. Two results cut against expectation: **alcohol and sleep separate the groups least of all 24 variables**, and the three heart-rate variables *derived in Step 1* take the top three places ahead of every raw measurement — the evidence that those derivations earned their place.

`top_predictors_histograms.pdf` makes the important caveat visible. The two groups are drawn on the **patient-count scale** rather than as two densities, because a density curve rescales each group to unit area and would silently inflate the smaller diagnosed group (2,727) to the same height as the larger undiagnosed one (6,273).

**Why an overlap table at all.** The standardised difference ranks variables by how far apart the two group *means* sit, and that says nothing about whether the groups can be told apart patient by patient — two distributions can have means far apart and still cover the same range of values. The overlap table measures that directly, which the effect size cannot and the histogram can only suggest.

**How to read it.** "Central range" is a group's 5th to 95th percentile, i.e. where its middle 90 % sits. Both columns are proportions from 0 to 1: complete separation would give 0 in both, and a variable carrying no information at all would give about 0.90 in both, because the two ranges would be the same range.

**What heavy overlap rules out:** a single-variable screening rule. If most diagnosed patients have values healthy patients also have, no cut-off on that one variable can be drawn without either missing many cases or flagging many healthy patients. Nothing here supports a "peak heart rate below *X* means disease" rule.

**What it does not rule out:** the variable being a strong predictor. Overlapping distributions still shift the *probability* of diagnosis across the range, which is precisely what a logistic regression models — it estimates a probability per patient rather than sorting patients into two boxes. Nor does it rule out several overlapping variables separating the groups well *together*: two variables that each overlap heavily on their own can still be far apart in the plane they span. And it is not evidence that a variable is unrelated to the diagnosis — only the effect sizes speak to that, and Step 3's tests settle it.

The share of each group falling inside the other group's central 90 % range:

| Variable | Diseased inside the healthy range | Healthy inside the diseased range |
| --- | --- | --- |
| `max_heart_rate_achieved` | 0.556 | 0.549 |
| `st_depression` | 0.754 | 0.886 |
| `cholesterol_hdl_ratio` | 0.789 | 0.780 |
| `age` | 0.854 | 0.817 |

`max_heart_rate_achieved` is genuinely the best separator — the only one where nearly half of each group falls outside the other's central range, which is what a standardised difference of −1.55 looks like. Even so the bulk of both groups still shares the same range of values, and the other three overlap almost completely. So the honest statement is not "everything overlaps" but that **no single measurement splits the two groups on its own** — the argument for a multivariable model in Step 3 rather than a screening rule on one measurement.

### Categorical variables — risk difference and odds ratio

Full tables: `categorical_by_outcome_table.csv`, `categorical_association_strength.csv`. Figure: `disease_rate_by_category.pdf`.

**What the percentages in this section are.** Every rate quoted below is a **disease rate**: of the patients at that level, the percentage diagnosed with heart disease. "`age_group` 18–34 9.5 %" means 9.5 of every 100 patients aged 18–34 in this sample carry the diagnosis. The unit is percent *of the patients within the level* — not percent of the sample, and not percent of the cases — so the levels of one variable do not add to 100. The 30.3 % overall prevalence is the reference each is read against.

**What "each factor in isolation" means.** Each rate is computed from one variable at a time, with nothing else held fixed — a **marginal** association, carrying whatever confounding comes with it. The 46.4 % for current smokers is the rate among current smokers of every age, weight and blood pressure the sample happens to contain; if current smokers here are also older, part of that 46.4 % is age rather than tobacco. These rates cannot be added, multiplied or chained across variables, and they are not the effect of the variable "controlling for" the others.

The adjusted version is what **Step 3** produces: a logistic regression gives each level's odds ratio with the other predictors held fixed. Comparing the two is the point — the difference between the marginal odds ratio here and the adjusted one there *is* the confounding the model removes. §6 gives one case (`wearable_owner`) where the marginal number is known in advance to mislead.

Each variable is summarised by two numbers taken between its safest and riskiest level:

- **Risk difference** — highest disease rate minus lowest. Already unit-free, and read directly: moving from the safest to the riskiest level changes the diagnosis rate by this much.
- **Odds ratio** — the odds of diagnosis at the riskiest level divided by the odds at the safest, where odds = *p*/(1 − *p*). This is exactly what a logistic regression estimates: the exponentiated coefficient of a dummy variable *is* the odds ratio against the reference level. Quoting it here means **Step 3's coefficients can be checked straight against this table** — a marginal odds ratio and an adjusted one differ by precisely the confounding the model removes.

The two are reported together on purpose. The odds ratio is insensitive to how common the outcome is and so ranks variables the way a model will; the risk difference says whether that ranking matters in patients, since a large odds ratio between two rare levels still moves very few people.

Ranked by risk difference:

| Variable | Safest level | Riskiest level | Risk diff. | Odds ratio |
| --- | --- | --- | --- | --- |
| `exercise_induced_angina` | No 19.1 % | **Yes 67.8 %** | **0.487** | 8.91 |
| `age_group` | 18–34 9.5 % | **65+ 51.1 %** | 0.416 | **10.00** |
| `bmi_category` | Underweight 15.8 % | Obese 47.2 % | 0.313 | 4.75 |
| `bp_category` | Normal 17.0 % | Hypertension stage 2 43.9 % | 0.269 | 3.82 |
| `glycemic_status` | Normal 20.7 % | Diabetic 45.6 % | 0.249 | 3.21 |
| `chest_pain_type` | Asymptomatic 25.4 % | Typical Angina 47.9 % | 0.225 | 2.71 |
| `smoker_status` | Never 25.2 % | Current 46.4 % | 0.212 | 2.57 |
| `meets_activity_guideline` | Yes 20.1 % | No 38.1 % | 0.180 | 2.45 |
| `sex` | Female 24.7 % | Male 35.4 % | 0.107 | 1.67 |
| `wearable_owner` | Yes 25.4 % | No 34.2 % | 0.088 | 1.52 — see §6 |
| `family_history` | No 28.3 % | Yes 35.6 % | 0.073 | 1.40 |

`exercise_induced_angina` dominates on risk difference, at 0.487 against 0.416 for the next variable. Combined with the heart-rate result above: **the exercise stress test carries most of the information in this dataset.**

The two measures disagree at the top, and the disagreement is informative. `age_group` has the **larger odds ratio** (10.0 vs 8.9) but the **smaller risk difference**, because its safest level starts from a much lower base rate — 9.5 % at 18–34 against 19.1 % for patients without exercise angina. Multiplying a small probability by 10 still leaves a small probability. So the stress-test result is the more useful finding clinically, while age will look like the stronger term in a logistic regression's coefficients.

`disease_rate_by_category.pdf` now prints the rate and the level size on every bar, so the figure carries the numbers itself and the report does not need a supplementary table beside it. *n* is on the bar because both summary measures ignore level size, and a rate computed on 549 patients should be read differently from one computed on 4,980.

Two rankings are worth noting. `chest_pain_type` places only sixth, below three of the four categories derived in Step 1, despite being the variable a textbook heart-disease study leads with. And `family_history` is last — it does separate the groups, but by less than anything else here. Every ordered variable moves monotonically in the expected direction: `age_group` 9.5 → 18.0 → 31.9 → 51.1 %, `bp_category` 17.0 → 25.3 → 30.4 → 43.9 %, `glycemic_status` 20.7 → 33.7 → 45.6 %, and `smoker_status` 25.2 → 30.3 → 46.4 %, with former smokers sitting neatly between never and current.

## 5. Relationships among the predictors

`correlation_heatmap.pdf`, grouped so that variables measuring the same physiological quantity sit together. The grouping is written out by hand from the blocks Step 1 identified rather than discovered automatically, so the figure is reproducible and the reader can see exactly which variables the report claims belong together. The blocks are exactly the ones Step 1 flagged: the lipid panel, the two blood-sugar measures, the two blood pressures, the heart-rate group.

Outside those blocks correlations are weak. The strongest link between any lifestyle variable and any clinical one is only −0.31 (`exercise_minutes_per_week` with `resting_heart_rate`), and `sleep_hours` reaches at most 0.03 with anything clinical. **The lifestyle block and the clinical block carry largely separate information**, so both belong in a model — neither is a proxy for the other.

`age_relationships.pdf` isolates age, because it drives several other predictors and is therefore a confounder for all of them. Each panel carries both a straight-line fit and a quadratic one: where the two curves coincide, a linear term in age is enough, and a visible gap would be the signal to carry a squared term into Step 3. The two are indistinguishable in all four panels, so **age enters the models linearly**. The fitted slopes are −1.196 bpm per year for peak heart rate (*R*² = 0.53), +0.442 mmHg per year for systolic pressure (0.18), +0.341 mg/dL for LDL (0.03) and +0.008 % for HbA1c (0.02). Peak heart rate falls almost linearly with age (r = −0.73) — that is physiology, not disease. Since both age and peak heart rate are among the strongest correlates of the diagnosis, Step 3 has to fit them together; the marginal effect of either is partly the other.

## 6. Three findings that need care

**The wearable-owner gap is not an age effect.** The obvious explanation for owners' lower disease rate is that they are younger — and it is wrong: mean age 53.8 vs 54.1, and the gap survives inside every age band (widest among the youngest, 5.8 % vs 12.6 %). What does differ is activity: 7,302 daily steps vs 5,259, and 165 exercise minutes per week vs 119. `wearable_owner` stands in for how active a patient is and for the kind of person who buys a tracker. Owning a tracker cannot protect anyone; if Step 3 includes it, the activity variables must be in the model beside it.

**The sex gap is not explained by chest-pain type.** The natural explanation — men report higher-risk pain types more often — fails on both counts. The chest-pain profiles of the two sexes are nearly identical (typical angina 10.9 % of women, 11.7 % of men), and the male excess persists inside *every* pain type: 9.0, 10.0, 13.2 and 14.0 percentage points. Sex acts on its own here, not through the reported symptom.

**The truncated columns distort their own summary statistics.** For the five columns Step 1 found cut off at a recording limit, the reported extreme is the limit and the sd is an underestimate. Each is truncated at **one end only**, and the QQ plots show which: `max_heart_rate_achieved` runs flat at the *top* (115 patients at the 210 bpm ceiling), while `triglycerides`, `ldl`, `bmi` and `hba1c` run flat at the *bottom* (114, 56, 82 and 64 patients on their floors). `st_depression`'s flat step is a different thing again — a point mass at zero, not a boundary of the measuring scale. Between 0.6 % and 1.3 % of patients sit exactly on each limit; the largest is `max_heart_rate_achieved`, with 1.28 % pinned at 210 bpm. Since that variable carries the largest effect size in the dataset, the report states the ceiling explicitly — the truncation is at the healthy end, so the measured separation is if anything an underestimate.

---

## 7. What Step 3 takes from here

1. **The exercise stress test** — peak heart rate, exercise-induced angina, ST depression — separates the groups far better than anything else. Any model that omits it is not competitive.
2. **Age belongs in every model as a control**: it is both a strong correlate of the diagnosis and a driver of several other predictors.
3. **The lifestyle block is nearly uncorrelated with the clinical block**, so it is not redundant — but alcohol and sleep separate the groups barely at all on their own. Whether they matter after adjustment is a Step 3 question, and the descriptives cannot settle it.
4. **Transform three variables**, each on its own rung of the ladder of powers: log for `alcohol_units_per_week` and `cholesterol_hdl_ratio`, cube root for `st_depression`. All three end up with |skewness| < 0.35, and none needs splitting. Nothing else needs transforming.
5. **One variable per correlation block** goes into a model, not all of them — and the derived variables are alternatives to their parents, never companions.
6. **No single variable separates the groups.** Even the strongest shows heavy overlap, which is the case for a multivariable model rather than a screening rule on one measurement.
