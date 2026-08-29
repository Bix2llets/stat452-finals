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

raw <- read.csv("activity2/dataset/heart_disease_risk_2026.csv", stringsAsFactors = FALSE) # Adjust this to fit the csv file location

head(raw)
dim(raw)
str(raw)
summary(raw)

# Mostly continuous variables. Categorical include the sex, chest pain type, exercise induced angina, family history of having heart disease, smoker status and if the patient has wearable

length(unique(raw$patient_id)) == nrow(raw) # TRUE -> the id is unique

# Setup list of numerical and categorical column

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

# Data handling
#
# 2.1 Missing values and duplicates ------------------------------------------
# read.csv imports "NA", "?" and the like as text, so check those too.

sum(is.na(raw)) # 0, no NA value
unique(raw$sex)
unique(raw$chest_pain_type)
unique(raw$exercise_induced_angina)
unique(raw$family_history)
unique(raw$smoker_status)
unique(raw$wearable_owner)

# No NA value in numerical columns, and no strange value in catgorical one (as string)

raw[duplicated(raw$patient_id) == TRUE, ]
# No duplicated patitents ID


# Check for accidental duplication of data.
sum(duplicated(raw[, setdiff(names(raw), "patient_id")]))
# No duplication
# All zero, so all 9,000 patients are used with no imputation or deletion.

# 2.3 Are the numbers physiologically possible? ------------------------------
# Ranges are the standard clinical reference ranges.

plausible_range <- list(
  age = c(18, 100),
  resting_bp_systolic = c(70, 250),
  resting_bp_diastolic = c(40, 150),
  cholesterol_total = c(80, 500),
  hdl = c(10, 120),
  ldl = c(20, 300),
  triglycerides = c(30, 800),
  fasting_blood_sugar = c(50, 400),
  hba1c = c(3.5, 16), bmi = c(12, 70),
  resting_heart_rate = c(35, 130),
  max_heart_rate_achieved = c(60, 230),
  st_depression = c(0, 10),
  alcohol_units_per_week = c(0, 100),
  exercise_minutes_per_week = c(0, 1500),
  sleep_hours = c(2, 14),
  stress_score = c(0, 100),
  daily_steps = c(0, 40000),
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

# No suspicious that falls outside of plausible range

# Check for semantically impossible values
# Systolic (Pressure when the heart beat) must exceed diastolic (Pressure when the hear rest) and the exercise peak must exceed the resting
# rate.

sum(raw$resting_bp_systolic <= raw$resting_bp_diastolic) # 0
sum(raw$max_heart_rate_achieved <= raw$resting_heart_rate) # 0

# # The Friedewald relation says total = HDL + LDL + triglycerides / 5
# (mg/dL). Since the last term is positive , HDL + LDL <  total.

lipid_overshoot <- raw$hdl + raw$ldl - raw$cholesterol_total
sum(lipid_overshoot >= 0)
summary(lipid_overshoot[lipid_overshoot >= 0])

friedewald_gap <- raw$cholesterol_total -
  (raw$hdl + raw$ldl + raw$triglycerides / 5)
hist(friedewald_gap)
mean(friedewald_gap)
sd(friedewald_gap)
# Normally distributed around 0, so it is more like a random error. This is due to the approximation of the total cholesterol with Friedwald equation

# This suggests that the cholesterol_total can be estimated with hdl, ldl and triglycerides despite the discreparency between teh Friedwald formula and the actual measurement, as the linear sum of the former one normally dsitributes around the later variable


# 2.6 Outliers ---------------------------------------------------------------
# Will not clamp them. An outlier here mean a significant observation of health condition

count_iqr_outliers <- function(x) {
  quartiles <- quantile(x, c(0.25, 0.75))
  iqr <- quartiles[2] - quartiles[1]
  sum(x < quartiles[1] - 1.5 * iqr | x > quartiles[2] + 1.5 * iqr)
}
sapply(raw[numeric_raw], count_iqr_outliers)

ggplot(
  raw |>
    select(all_of(numeric_raw)) |>
    pivot_longer(everything(), names_to = "variable", values_to = "value"),
  aes(y = value)
) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Every numerical variable before cleaning", y = "Observed value") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave("numeric_boxplots.pdf", width = 11, height = 7)

# st_despression and alchold_units per week is right skwed, thus natually greater values tends to be outlier

# Conversion of columns into factors

data <- raw |>
  mutate(
    patient_id = as.character(patient_id),
    sex = factor(sex, levels = c("Female", "Male")),
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
    has_heart_disease_num = has_heart_disease,
    has_heart_disease = factor(
      has_heart_disease,
      levels = c(0, 1), labels = c("No", "Yes")
    )
  )


data <- data |>
  mutate(
    # We calculate the pulse pressure: the pressure that the heart exert to circulate blood aroudn the body
    pulse_pressure = resting_bp_systolic - resting_bp_diastolic,
    non_hdl_cholesterol = cholesterol_total - hdl, # The hdl cholesterol is considred good, as high hdl reduces cardiac-related problems. The other are combination of the ldl (low density cholesterol) and triglicerides, whose effect badly affect the cardiovascular system.
    # We don't use the sum here since the sum would be ilkely to underestimate or overestimate the total choresterol
    cholesterol_hdl_ratio = round(cholesterol_total / hdl, 3),
    heart_rate_difference = max_heart_rate_achieved - resting_heart_rate,

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


# Final inspection
str(data)
summary(data)

table(data$has_heart_disease)
prop.table(table(data$has_heart_disease))

# About 30% have heart disease.

numeric_clean <- c(
  numeric_raw, "pulse_pressure", "non_hdl_cholesterol",
  "cholesterol_hdl_ratio", "heart_rate_difference"
)
correlation_matrix <- cor(data[numeric_clean])

correlation_matrix |>
  as.table() |>
  as.data.frame() |>
  setNames(c("row_variable", "column_variable", "correlation")) |>
  ggplot(aes(row_variable, column_variable, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1)) +
  labs(title = "Correlation between the numerical variables", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("correlation_heatmap.pdf")

correlation_pairs <- which(
  abs(correlation_matrix) > 0.7 & upper.tri(correlation_matrix),
  arr.ind = TRUE
)
data.frame(
  first = rownames(correlation_matrix)[correlation_pairs[, "row"]],
  second = colnames(correlation_matrix)[correlation_pairs[, "col"]],
  correlation = round(correlation_matrix[correlation_pairs], 3)
)

# Except the derived values, there are correslation between age and max heart rate, fasting blood sugar and hba1c, resting bp systolic and resting bp diastolic, and between cholesterol total and ldl

saveRDS(data, "activity2/dataset/cleaned_data.rds")
