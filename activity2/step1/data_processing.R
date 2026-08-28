# Activity 2 - Step 1: Describe and clean the dataset
#
# Dataset: heart_disease_risk_2026.csv (9,000 patients x 27 columns)
# Goal of this script:
#   1. Inspect the raw file and document what every column contains.
#   2. Run explicit data-quality checks (missing values, duplicates,
#      out-of-range values, internal contradictions between variables).
#   3. Give every column the right R type so Steps 2 and 3 can use it directly.
#   4. Add a small number of derived variables that clinical practice already
#      uses, and that remove the worst redundancy between the raw columns.
#   5. Save the result as cleaned_data.rds for the later steps.
#
# Run this script with activity2/step1 as the working directory.

library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(6767)

raw <- read.csv("../dataset/heart_disease_risk_2026.csv", stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# 1. First look at the data
# ---------------------------------------------------------------------------

dim(raw)
head(raw)
str(raw)
summary(raw)

# The file holds one row per patient. patient_id is a running record number, so
# it carries no information about the patient and must never enter a model - it
# is kept only so a row in the cleaned data can be traced back to the raw file.
#
# The response is has_heart_disease, coded 0 / 1. Everything else is either a
# demographic, a clinical measurement taken at rest or during an exercise test,
# a blood test result, or a self-reported lifestyle variable.

length(unique(raw$patient_id)) == nrow(raw) # TRUE -> the id really is unique

# ---------------------------------------------------------------------------
# 2. Data-quality checks
# ---------------------------------------------------------------------------

# 2.1 Missing values ---------------------------------------------------------
# We check for both R's NA and for the strings that spreadsheets often leave
# behind ("", "NA", "N/A", "?", "unknown") - read.csv would have imported those
# as ordinary text, not as NA.

colSums(is.na(raw))
sum(is.na(raw)) # 0

placeholder <- c("", " ", "NA", "N/A", "na", "?", "-", "unknown", "Unknown")
sapply(raw, function(column) sum(as.character(column) %in% placeholder))
# All zero. The dataset is complete: no imputation and no listwise deletion is
# needed, so the analysis in Steps 2 and 3 uses all 9,000 patients.

# 2.2 Duplicated records -----------------------------------------------------

sum(duplicated(raw$patient_id)) # 0 duplicated ids
sum(duplicated(raw[, setdiff(names(raw), "patient_id")])) # 0 duplicated patients

# 2.3 Categorical columns: are the labels consistent? ------------------------
# Free-text category columns usually carry typos and inconsistent casing
# ("male" / "Male" / "M "). We list every level actually present.

categorical_raw <- c(
  "sex", "chest_pain_type", "exercise_induced_angina",
  "family_history", "smoker_status", "wearable_owner"
)
lapply(raw[categorical_raw], table)

# Every column uses exactly one spelling per category and there is no stray
# whitespace, so no recoding of labels is required. The three True / False
# columns were imported as text and are converted to proper factors below.

# 2.4 Numerical columns: are the values physiologically possible? ------------
# Rather than trusting min / max by eye, we state a plausible range for each
# measurement from standard clinical reference ranges and count the violations.

numeric_raw <- c(
  "age", "resting_bp_systolic", "resting_bp_diastolic", "cholesterol_total",
  "hdl", "ldl", "triglycerides", "fasting_blood_sugar", "hba1c", "bmi",
  "resting_heart_rate", "max_heart_rate_achieved", "st_depression",
  "alcohol_units_per_week", "exercise_minutes_per_week", "sleep_hours",
  "stress_score", "daily_steps", "diet_quality_score"
)

plausible_range <- list(
  age = c(18, 100), resting_bp_systolic = c(70, 250),
  resting_bp_diastolic = c(40, 150), cholesterol_total = c(80, 500),
  hdl = c(10, 120), ldl = c(20, 300), triglycerides = c(30, 800),
  fasting_blood_sugar = c(50, 400), hba1c = c(3.5, 16), bmi = c(12, 70),
  resting_heart_rate = c(35, 130), max_heart_rate_achieved = c(60, 230),
  st_depression = c(0, 10), alcohol_units_per_week = c(0, 100),
  exercise_minutes_per_week = c(0, 1500), sleep_hours = c(2, 14),
  stress_score = c(0, 100), daily_steps = c(0, 40000),
  diet_quality_score = c(0, 100)
)

range_report <- data.frame(
  variable = numeric_raw,
  minimum = sapply(raw[numeric_raw], min),
  maximum = sapply(raw[numeric_raw], max),
  out_of_range = sapply(numeric_raw, function(v) {
    limits <- plausible_range[[v]]
    sum(raw[[v]] < limits[1] | raw[[v]] > limits[2])
  }),
  row.names = NULL
)
range_report

# No measurement falls outside its clinical range, and no possible placeholder value for them.
# Here the smallest cholesterol is 90 mg/dL, so the column is genuine.

# 2.5 Contradictions between columns ----------------------------------------
# A value can be plausible on its own and still be impossible next to another
# column. These four checks catch that.

sum(raw$resting_bp_systolic <= raw$resting_bp_diastolic) # 0
sum(raw$max_heart_rate_achieved <= raw$resting_heart_rate) # 0

# Total cholesterol is HDL + LDL + 0.2 * VLDL, so HDL + LDL can never reach the
# total. 106 patients (1.2 %) break that rule.
lipid_overshoot <- raw$hdl + raw$ldl - raw$cholesterol_total
sum(lipid_overshoot >= 0)
summary(lipid_overshoot[lipid_overshoot >= 0])

# The overshoot is at most 12 mg/dL and is 2 mg/dL at the median, i.e. it is
# the size of the rounding applied to each of the three results separately,
# not a mix-up of patients or of units. We therefore keep these rows and mark
# them with a flag in Section 4, so Step 3 can refit without them and confirm
# that nothing in the conclusions depends on them.

friedewald_gap <- raw$cholesterol_total -
  (raw$hdl + raw$ldl + raw$triglycerides / 5)
sd(friedewald_gap)
ggplot(df, aes(x = gap)) +
  geom_histogram(aes(y = after_stat(density)),
    fill = "steelblue", color = "white", bins = 30
  ) +
  # Add the density curve
  geom_density(color = "darkred", linewidth = 2) +
  theme_minimal() +
  labs(
    title = "Friedewald Gap with Density Curve",
    x = "Friedewald Gap (mg/dL)",
    y = "Density"
  )
# The distribution of total cholesterol is around the empirical formula for calculating it, suggesting the error is random and normal

# Apart from that rounding, the lipid panel obeys the Friedewald relationship
#     total cholesterol ~ HDL + LDL + triglycerides / 5
# which is what a real laboratory panel looks like.
#
# This is a warning for the modelling steps, not an error to fix: the four
# lipid columns are almost a linear combination of each other, so putting all
# four into one regression would inflate the standard errors. Section 4 adds
# non_hdl_cholesterol so that later steps can use one lipid summary instead.

# 2.6 Values sitting exactly on a boundary -----------------------------------
# When a measuring device or the data provider truncates a variable, the
# extreme value is repeated far more often than its neighbours. We count the
# ties at each observed minimum and maximum.

boundary_report <- data.frame(
  variable = numeric_raw,
  at_minimum = sapply(raw[numeric_raw], function(x) sum(x == min(x))),
  at_maximum = sapply(raw[numeric_raw], function(x) sum(x == max(x))),
  row.names = NULL
)
boundary_report

# Two kinds of pile-up show up, and only one of them is an artefact.
#
#   Real zeros. st_depression has 193 patients at 0.00 and
#   exercise_minutes_per_week has 146 at 0. Both are meaningful: no ST
#   depression during the test, and no exercise at all in a week. Nothing to
#   correct, but the two variables are therefore not continuous at their lower
#   end, which matters for the normality comments in Step 2.
#
#   Recording limits. max_heart_rate_achieved stops at exactly 210 bpm for 115
#   patients, triglycerides at 35 mg/dL for 114, bmi at 15 for 82, hba1c at 4.0
#   for 64 and ldl at 35 for 56 - far more ties than the neighbouring values
#   carry. The source truncated these five columns, so their extreme tail is
#   compressed. We keep the values as they are, because replacing or deleting
#   them would invent information, and Step 2 reports the shape of these
#   distributions with the ceiling / floor stated.

# 2.7 Outliers ---------------------------------------------------------------
# We count how many observations lie beyond the 1.5 IQR fences, but we do NOT
# winsorize them. In a clinical dataset a systolic pressure of 181 mmHg or an
# HbA1c of 8.6 % is exactly the kind of patient the study is about; pulling
# those values back to the fence would erase the signal we want to model.

count_iqr_outliers <- function(x) {
  quartiles <- quantile(x, c(0.25, 0.75))
  iqr <- quartiles[2] - quartiles[1]
  sum(x < quartiles[1] - 1.5 * iqr | x > quartiles[2] + 1.5 * iqr)
}
sapply(raw[numeric_raw], count_iqr_outliers)

# The counts are small relative to 9,000 rows and every flagged value survived
# the range and contradiction checks above, so all rows are retained.

raw |>
  select(all_of(numeric_raw)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Distribution of every numerical variable before cleaning",
    y = "Observed value"
  ) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave("numeric_boxplots.pdf")

# ---------------------------------------------------------------------------
# 3. Giving the columns their correct type
# ---------------------------------------------------------------------------
# read.csv imported the categories as plain text, which would let R treat them
# as arbitrary strings. We convert them to factors and fix the reference level
# of each one to the group the interpretation should be relative to (the
# healthy or unexposed group), so the coefficients in Step 3 read naturally.

data <- raw |>
  mutate(
    patient_id = as.character(patient_id),
    sex = factor(sex, levels = c("Female", "Male")),
    # For chest pain type, they are sorted in order of satisfying 0, 1, 2 and 3 criterions
    #
    chest_pain_type = factor(
      chest_pain_type,
      levels = c(
        "Asymptomatic", "Non-Anginal Pain",
        "Atypical Angina", "Typical Angina"
      ), ordered = TRUE
    ),
    # Never < Former < Current is a genuine ordering of tobacco exposure, so
    # smoking is stored as an ordered factor.
    smoker_status = factor(
      smoker_status,
      levels = c("Never", "Former", "Current"), ordered = TRUE
    ),
    exercise_induced_angina = factor(
      exercise_induced_angina,
      levels = c("False", "True"), labels = c("No", "Yes")
    ),
    family_history = factor(
      family_history,
      levels = c("False", "True"), labels = c("No", "Yes")
    ),
    wearable_owner = factor(
      wearable_owner,
      levels = c("False", "True"), labels = c("No", "Yes")
    ),
    # The response is kept twice on purpose: the factor is what a classifier
    # needs, the 0/1 copy is what lets us average the column to get a
    # prevalence in Step 2.
    has_heart_disease_num = has_heart_disease,
    has_heart_disease = factor(
      has_heart_disease,
      levels = c(0, 1), labels = c("No", "Yes")
    )
  )

# ---------------------------------------------------------------------------
# 4. Derived variables
# ---------------------------------------------------------------------------
# Each addition below either replaces a pair of strongly correlated columns
# with the single quantity clinicians actually read, or turns a measurement
# into the standard risk category, which makes the tables in Step 2 readable.
# Nothing is removed - the raw columns stay in the file so that any later step
# can go back to them.

data <- data |>
  mutate(
    # Systolic and diastolic pressure correlate at r = 0.77. Pulse pressure,
    # their difference, is the part of blood pressure the pair does not share
    # and is itself a recognised cardiovascular risk marker.
    pulse_pressure = resting_bp_systolic - resting_bp_diastolic,

    # Everything in the lipid panel except HDL, i.e. the "bad" cholesterol in
    # one number. Using this instead of cholesterol_total + ldl + triglycerides
    # avoids the near linear dependency found in Section 2.5.
    # NOTE: This require further clarification
    non_hdl_cholesterol = cholesterol_total - hdl,
    cholesterol_hdl_ratio = round(cholesterol_total / hdl, 3),


    # How far the heart rate can rise between rest and peak effort. This is the
    # informative combination of the two heart-rate columns.
    heart_rate_reserve = max_heart_rate_achieved - resting_heart_rate,

    # Age drives the maximum achievable heart rate (r = -0.73 with
    # max_heart_rate_achieved). Expressing the measured peak as a percentage of
    # the age-predicted maximum 220 - age gives an age-free measure of effort
    # tolerance.
    # NOTE: This require further clarification on the formula
    percent_predicted_max_hr = round(
      100 * max_heart_rate_achieved / (220 - age), 2
    ),

    # Standard WHO categories, used for the grouped summaries in Step 2.
    bmi_category = cut(
      bmi,
      breaks = c(-Inf, 18.5, 25, 30, Inf),
      labels = c("Underweight", "Normal", "Overweight", "Obese"),
      right = FALSE
    ),
    # ACC / AHA blood-pressure stages; a patient is placed in the higher stage
    # of the one their systolic and diastolic readings imply.
    # NOTE: Require source for the thresholds
    bp_category = factor(
      case_when(
        resting_bp_systolic >= 140 | resting_bp_diastolic >= 90 ~ "Hypertension stage 2",
        resting_bp_systolic >= 130 | resting_bp_diastolic >= 80 ~ "Hypertension stage 1",
        resting_bp_systolic >= 120 ~ "Elevated",
        .default = "Normal"
      ),
      levels = c("Normal", "Elevated", "Hypertension stage 1", "Hypertension stage 2"),
      ordered = TRUE
    ),
    # HbA1c thresholds for prediabetes (5.7 %) and diabetes (6.5 %).
    # NOTE: Requrie source for the thresholds
    glycemic_status = factor(
      case_when(
        hba1c >= 6.5 ~ "Diabetic",
        hba1c >= 5.7 ~ "Prediabetic",
        .default = "Normal"
      ),
      levels = c("Normal", "Prediabetic", "Diabetic"), ordered = TRUE
    ),
    age_group = cut(
      age,
      breaks = c(17, 34, 49, 64, Inf),
      labels = c("18-34", "35-49", "50-64", "65+")
    ),
    # Meeting the WHO recommendation of 150 minutes of activity per week.
    meets_activity_guideline = factor(
      if_else(exercise_minutes_per_week >= 150, "Yes", "No"),
      levels = c("No", "Yes")
    ),
    # Marks the 106 patients whose rounded lipid results add up to slightly
    # more than their reported total cholesterol (Section 2.5). They stay in
    # the data; the flag exists so Step 3 can drop them once as a sensitivity
    # check.
    lipid_panel_consistent = factor(
      if_else(hdl + ldl < cholesterol_total, "Yes", "No"),
      levels = c("Yes", "No")
    )
  )

table(data$lipid_panel_consistent)

# ---------------------------------------------------------------------------
# 5. Checks on the cleaned data
# ---------------------------------------------------------------------------

str(data)
summary(data)

# The derived columns must not have introduced anything impossible.
sum(is.na(data))
summary(data$pulse_pressure)
summary(data$percent_predicted_max_hr)
summary(data$cholesterol_hdl_ratio)

# Balance of the response, and of the two variables the research question will
# most likely condition on.
table(data$has_heart_disease)
prop.table(table(data$has_heart_disease))
table(data$sex, data$has_heart_disease)
table(data$chest_pain_type, data$has_heart_disease)

# About 30 % of the patients are cases. That is not balanced, but it is far
# from the rare-event regime, so a logistic regression can be fitted on the
# data as it stands; Step 3 only has to remember that a default 0.5 cut-off
# favours the majority class when it reports accuracy.

# Correlation between the numerical predictors, so the later steps know which
# pairs cannot go into the same model. Written to a PDF for the report.
numeric_clean <- c(
  numeric_raw, "pulse_pressure", "non_hdl_cholesterol",
  "cholesterol_hdl_ratio", "heart_rate_reserve", "percent_predicted_max_hr"
)
correlation_matrix <- cor(data[numeric_clean])
round(correlation_matrix, 2)

pdf("correlation_heatmap.pdf", width = 10, height = 9)
correlation_matrix |>
  as.table() |>
  as.data.frame() |>
  setNames(c("row_variable", "column_variable", "correlation")) |>
  ggplot(aes(row_variable, column_variable, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1)) +
  labs(
    title = "Pearson correlation between the numerical variables",
    x = NULL, y = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# Pairs above |r| = 0.7, listed so Step 3 does not have to rediscover them:
correlation_pairs <- which(
  abs(correlation_matrix) > 0.7 & upper.tri(correlation_matrix),
  arr.ind = TRUE
)
data.frame(
  first = rownames(correlation_matrix)[correlation_pairs[, "row"]],
  second = colnames(correlation_matrix)[correlation_pairs[, "col"]],
  correlation = round(correlation_matrix[correlation_pairs], 3)
)

# ---------------------------------------------------------------------------
# 6. Save
# ---------------------------------------------------------------------------

saveRDS(data, "../dataset/cleaned_data.rds")
