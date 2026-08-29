# Activity 2 - Step 1: Describe and clean the dataset
#
# Input : ../dataset/heart_disease_risk_2026.csv  (9,000 patients x 27 columns)
# Output: ../dataset/cleaned_data.rds, numeric_boxplots.pdf,
#         correlation_heatmap.pdf
#
# Run with activity2/step1 as the working directory.

library(dplyr)
library(tidyr)
library(ggplot2)

raw <- read.csv("../dataset/heart_disease_risk_2026.csv", stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# 1. First look
# ---------------------------------------------------------------------------
# One row per patient. The response is has_heart_disease (0/1). patient_id is a
# record number and must never enter a model.

dim(raw)
str(raw)
summary(raw)

length(unique(raw$patient_id)) == nrow(raw) # TRUE -> the id is unique

# ---------------------------------------------------------------------------
# 2. Data-quality checks
# ---------------------------------------------------------------------------

numeric_raw <- c(
  "age", "resting_bp_systolic", "resting_bp_diastolic", "cholesterol_total",
  "hdl", "ldl", "triglycerides", "fasting_blood_sugar", "hba1c", "bmi",
  "resting_heart_rate", "max_heart_rate_achieved", "st_depression",
  "alcohol_units_per_week", "exercise_minutes_per_week", "sleep_hours",
  "stress_score", "daily_steps", "diet_quality_score"
)
categorical_raw <- c(
  "sex", "chest_pain_type", "exercise_induced_angina",
  "family_history", "smoker_status", "wearable_owner"
)

# 2.1 Missing values and duplicates ------------------------------------------
# read.csv imports "NA", "?" and the like as text, so check those too.

sum(is.na(raw)) # 0
placeholder <- c("", " ", "NA", "N/A", "na", "?", "-", "unknown", "Unknown")
sum(sapply(raw, function(column) sum(as.character(column) %in% placeholder)))

sum(duplicated(raw$patient_id))
sum(duplicated(raw[, setdiff(names(raw), "patient_id")]))

# All zero, so all 9,000 patients are used with no imputation or deletion.

# 2.2 Are the category labels consistent? ------------------------------------

lapply(raw[categorical_raw], table)

# One spelling per category and no stray whitespace, so no recoding is needed.

# 2.3 Are the numbers physiologically possible? ------------------------------
# Ranges are the standard clinical reference ranges.

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

# Nothing out of range.

# 2.4 Contradictions between columns -----------------------------------------
# Systolic must exceed diastolic and the exercise peak must exceed the resting
# rate. The Friedewald relation says total = HDL + LDL + triglycerides / 5
# (mg/dL); the last term is positive, so HDL + LDL cannot reach the total.

sum(raw$resting_bp_systolic <= raw$resting_bp_diastolic) # 0
sum(raw$max_heart_rate_achieved <= raw$resting_heart_rate) # 0

lipid_overshoot <- raw$hdl + raw$ldl - raw$cholesterol_total
sum(lipid_overshoot >= 0)
summary(lipid_overshoot[lipid_overshoot >= 0])

friedewald_gap <- raw$cholesterol_total -
  (raw$hdl + raw$ldl + raw$triglycerides / 5)
summary(friedewald_gap)
sd(friedewald_gap)

# 106 patients (1.2%) break the rule, by at most 12 mg/dL and 2 at the median.
# The gap is centred on zero with sd 8 mg/dL, which is the rounding on each
# result, not a mix-up. The rows stay; Section 4 flags them so Step 3 can refit
# without them.
#
# But the panel does obey Friedewald, so the four lipid columns are nearly a
# linear combination of each other. Section 4 adds one lipid summary to use in
# their place.

# 2.5 Truncated columns ------------------------------------------------------
# A truncated variable repeats its extreme value far more often than its
# neighbours, so count the ties at each observed minimum and maximum.

boundary_report <- data.frame(
  variable = numeric_raw,
  at_minimum = sapply(raw[numeric_raw], function(x) sum(x == min(x))),
  at_maximum = sapply(raw[numeric_raw], function(x) sum(x == max(x))),
  row.names = NULL
)
boundary_report

# Real zeros: st_depression (193) and exercise_minutes_per_week (146), so
# neither is continuous at its lower end. Recording limits:
# max_heart_rate_achieved 115 ties at 210 bpm, triglycerides 114 at 35 mg/dL,
# bmi 82 at 15, hba1c 64 at 4.0, ldl 56 at 35. Those five have a compressed
# tail; the values are kept and Step 2 states the ceiling or floor.

# 2.6 Outliers ---------------------------------------------------------------
# Counted but NOT winsorized. A systolic of 181 mmHg is the patient the study
# is about; clamping it to the fence would erase the signal.

count_iqr_outliers <- function(x) {
  quartiles <- quantile(x, c(0.25, 0.75))
  iqr <- quartiles[2] - quartiles[1]
  sum(x < quartiles[1] - 1.5 * iqr | x > quartiles[2] + 1.5 * iqr)
}
sapply(raw[numeric_raw], count_iqr_outliers)

raw |>
  select(all_of(numeric_raw)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Every numerical variable before cleaning", y = "Observed value") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave("numeric_boxplots.pdf", width = 11, height = 7)

# 0 to 485 per column, worst for st_depression (5.4%) and alcohol (431). Both
# are right skewed with a large group at zero, so the fence flags the upper
# tail rather than errors. All rows are retained.

# ---------------------------------------------------------------------------
# 3. Column types
# ---------------------------------------------------------------------------
# Every factor is UNORDERED, with the level order declared and the reference
# level set to the healthy or unexposed group. An unordered factor enters
# lm / glm / aov as dummy variables against that reference; an ordered one
# would give polynomial contrasts (.L, .Q, .C), whose coefficients no longer
# read as "this level versus the reference".

data <- raw |>
  mutate(
    patient_id = as.character(patient_id),
    sex = factor(sex, levels = c("Female", "Male")),
    # Nominal, not a scale. Asymptomatic is listed first only to make it the
    # reference level.
    chest_pain_type = factor(
      chest_pain_type,
      levels = c(
        "Asymptomatic", "Non-Anginal Pain",
        "Atypical Angina", "Typical Angina"
      )
    ),
    smoker_status = factor(
      smoker_status,
      levels = c("Never", "Former", "Current")
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
    # Kept twice: the factor for a classifier, the 0/1 copy so Step 2 can
    # average the column to get a rate.
    has_heart_disease_num = has_heart_disease,
    has_heart_disease = factor(
      has_heart_disease,
      levels = c(0, 1), labels = c("No", "Yes")
    )
  )

# ---------------------------------------------------------------------------
# 4. Derived variables
# ---------------------------------------------------------------------------
# Each one either replaces a pair of strongly correlated columns with the
# quantity clinicians read, or turns a measurement into its risk category. The
# raw columns all stay.

data <- data |>
  mutate(
    # Systolic and diastolic correlate at r = 0.77; pulse pressure is the part
    # they do not share.
    pulse_pressure = resting_bp_systolic - resting_bp_diastolic,

    # By the Friedewald identity this carries what cholesterol_total, ldl and
    # triglycerides carry between them, without the linear dependency.
    non_hdl_cholesterol = cholesterol_total - hdl,
    cholesterol_hdl_ratio = round(cholesterol_total / hdl, 3),

    heart_rate_reserve = max_heart_rate_achieved - resting_heart_rate,

    # Age drives the maximum achievable rate (r = -0.73), so the peak is
    # rescaled against the age-predicted maximum 220 - age; the correlation
    # with age then falls to -0.18.
    percent_predicted_max_hr = round(
      100 * max_heart_rate_achieved / (220 - age), 2
    ),

    # WHO adult BMI categories.
    bmi_category = cut(
      bmi,
      breaks = c(-Inf, 18.5, 25, 30, Inf),
      labels = c("Underweight", "Normal", "Overweight", "Obese"),
      right = FALSE
    ),
    # 2017 ACC/AHA stages. The guideline takes the HIGHER of the two stages the
    # systolic and diastolic readings imply, which is what testing from the top
    # down does here.
    bp_category = factor(
      case_when(
        resting_bp_systolic >= 140 | resting_bp_diastolic >= 90 ~ "Hypertension stage 2",
        resting_bp_systolic >= 130 | resting_bp_diastolic >= 80 ~ "Hypertension stage 1",
        resting_bp_systolic >= 120 ~ "Elevated",
        .default = "Normal"
      ),
      levels = c("Normal", "Elevated", "Hypertension stage 1", "Hypertension stage 2")
    ),
    # ADA thresholds: prediabetes 5.7-6.4%, diabetes >= 6.5%.
    glycemic_status = factor(
      case_when(
        hba1c >= 6.5 ~ "Diabetic",
        hba1c >= 5.7 ~ "Prediabetic",
        .default = "Normal"
      ),
      levels = c("Normal", "Prediabetic", "Diabetic")
    ),
    age_group = cut(
      age,
      breaks = c(17, 34, 49, 64, Inf),
      labels = c("18-34", "35-49", "50-64", "65+")
    ),
    # WHO recommendation of 150 minutes of activity per week.
    meets_activity_guideline = factor(
      if_else(exercise_minutes_per_week >= 150, "Yes", "No"),
      levels = c("No", "Yes")
    ),
    # The 106 rounding-inconsistent lipid panels of Section 2.4.
    lipid_panel_consistent = factor(
      if_else(hdl + ldl < cholesterol_total, "Yes", "No"),
      levels = c("Yes", "No")
    )
  )

# ---------------------------------------------------------------------------
# 5. Checks on the cleaned data
# ---------------------------------------------------------------------------

str(data)
summary(data)

# Must all be FALSE, or Step 3 would fit polynomial contrasts instead of dummy
# variables.
sapply(data[sapply(data, is.factor)], is.ordered)

sum(is.na(data))
table(data$has_heart_disease)
prop.table(table(data$has_heart_disease))

# About 30% are cases: unbalanced, but far from rare-event, so logistic
# regression fits as it stands. Step 3 only has to remember that a 0.5 cut-off
# favours the majority class when reporting accuracy.

numeric_clean <- c(
  numeric_raw, "pulse_pressure", "non_hdl_cholesterol",
  "cholesterol_hdl_ratio", "heart_rate_reserve", "percent_predicted_max_hr"
)
correlation_matrix <- cor(data[numeric_clean])

pdf("correlation_heatmap.pdf", width = 10, height = 9)
correlation_matrix |>
  as.table() |>
  as.data.frame() |>
  setNames(c("row_variable", "column_variable", "correlation")) |>
  ggplot(aes(row_variable, column_variable, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1)) +
  labs(title = "Correlation between the numerical variables", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

correlation_pairs <- which(
  abs(correlation_matrix) > 0.7 & upper.tri(correlation_matrix),
  arr.ind = TRUE
)
data.frame(
  first = rownames(correlation_matrix)[correlation_pairs[, "row"]],
  second = colnames(correlation_matrix)[correlation_pairs[, "col"]],
  correlation = round(correlation_matrix[correlation_pairs], 3)
)

# Ten pairs exceed |r| = 0.7, in four blocks: blood pressure, the lipid panel,
# blood sugar, and heart rate. Step 3 takes one variable per block.

# ---------------------------------------------------------------------------
# 6. Save
# ---------------------------------------------------------------------------

saveRDS(data, "../dataset/cleaned_data.rds")
