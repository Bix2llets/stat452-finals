# Activity 2 - Step 4a: The predictor set and the train / test split the
# regression and the logistic model both work from.
#
# Sourced by heart_rate_linear_model.R and heart_disease_logistic_model.R.
# Running it on its own prints the screening decisions and nothing else.
#
# Input : ../dataset/cleaned_data.rds
# Output: none on disk; defines data, predictors, training_data, testing_data
#
# Run with activity2/step4 as the working directory.

data <- readRDS("activity2/dataset/cleaned_data.rds") # Adjust this to fit the rds file location

# 4a.1 Screening the predictors for redundancy --------------------------------
# Two near-duplicate predictors split one effect between them: the coefficients
# get large standard errors and unstable signs, which is what a VIF above 5
# reports after the fact. The correlation matrix shows it before fitting.
# Take the pairs above 0.7 that Step 2's heatmap makes visible, and keep one
# member of each - a screen on the predictors alone, so it does not peek at
# either response.
numeric_candidates <- c(
  "age", "resting_bp_systolic", "resting_bp_diastolic", "cholesterol_total",
  "hdl", "ldl", "triglycerides", "fasting_blood_sugar", "hba1c", "bmi",
  "resting_heart_rate", "max_heart_rate_achieved", "st_depression",
  "alcohol_units_per_week", "exercise_minutes_per_week", "sleep_hours",
  "stress_score", "daily_steps", "diet_quality_score"
)
correlation_matrix <- cor(data[numeric_candidates])
correlation_threshold <- 0.7
strong_pairs <- which(
  abs(correlation_matrix) > correlation_threshold & upper.tri(correlation_matrix),
  arr.ind = TRUE
)
print(data.frame(
  first = rownames(correlation_matrix)[strong_pairs[, "row"]],
  second = colnames(correlation_matrix)[strong_pairs[, "col"]],
  correlation = round(correlation_matrix[strong_pairs], 3),
  row.names = NULL
))

# cholesterol_total is roughly hdl + ldl + triglycerides / 5, so it is close to
# a linear combination of three predictors already in the model; hba1c is the
# three-month average that fasting_blood_sugar samples once; and the two blood
# pressure readings move together. Keep the more informative member of each.
redundant_predictors <- c(
  "cholesterol_total", "fasting_blood_sugar", "resting_bp_diastolic"
)
numeric_predictors <- setdiff(numeric_candidates, redundant_predictors)
cat(
  "dropped as redundant:", paste(redundant_predictors, collapse = ", "),
  "\n numeric predictors kept:", length(numeric_predictors), "of",
  length(numeric_candidates), "\n"
)
# age and max_heart_rate_achieved stay together at -0.731: that leaves a VIF
# near 2.1, low enough to read both coefficients, and the logistic model needs
# them separately.

# 4a.2 A predictor that cannot be one ------------------------------------------
# wearable_owner records whether the patient owns a fitness tracker, which is a
# habit rather than a measurement of the heart. Whatever it marks reaches the
# diagnosis through activity, and those variables are already in the model, so
# it would enter as a proxy for predictors that are present.
# Print the activity gap that makes it a proxy, then leave it out; Step 2 still
# describes it.
cat(
  "wearable owners average", round(mean(data$daily_steps[data$wearable_owner == "Yes"])),
  "steps and", round(mean(data$exercise_minutes_per_week[data$wearable_owner == "Yes"])),
  "exercise minutes against",
  round(mean(data$daily_steps[data$wearable_owner == "No"])), "and",
  round(mean(data$exercise_minutes_per_week[data$wearable_owner == "No"])),
  "for non-owners\n"
)
cat(
  " marginal disease rate:",
  paste(names(tapply(data$has_heart_disease_num, data$wearable_owner, mean)),
    round(tapply(data$has_heart_disease_num, data$wearable_owner, mean), 4),
    collapse = ", "
  ), "- a gap that runs through those two, not through the heart\n"
)

categorical_predictors <- c(
  "sex", "chest_pain_type", "exercise_induced_angina",
  "family_history", "smoker_status"
)
predictors <- c(numeric_predictors, categorical_predictors)
stopifnot(!any(sapply(data[categorical_predictors], is.ordered)))

# The regression's response is peak minus resting heart rate, so its two
# components are dropped there - keeping them would hand the model the answer.
# The logistic model keeps them: the exercise test is a measurement, not a
# restatement, of the diagnosis.
regression_predictors <- setdiff(
  predictors, c("resting_heart_rate", "max_heart_rate_achieved")
)

set.seed(6767)
training_rows <- sample(nrow(data), round(0.8 * nrow(data)))
training_data <- data[training_rows, ]
testing_data <- data[-training_rows, ]
cat(
  "training rows:", nrow(training_data), " testing rows:", nrow(testing_data),
  " disease rate:", round(mean(data$has_heart_disease_num), 4), "\n"
)

# One fold assignment reused by every penalised fit, so the penalties are
# compared on identical splits rather than on their own lucky folds.
fold_count <- 10
fold_assignment <- sample(rep(1:fold_count, length.out = nrow(training_data)))

# The level every test in both scripts is read at, and the matching penalty for
# backward elimination: `step` charges k per coefficient kept, and the 5%
# chi-square critical value on 1 df makes "worth keeping" mean "significant
# at 5%".
significance_level <- 0.05
selection_penalty <- qchisq(significance_level, df = 1, lower.tail = FALSE)
