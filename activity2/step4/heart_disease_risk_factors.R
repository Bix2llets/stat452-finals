# Activity 2 - Step 4: Which clinical and lifestyle variables predict the
# odds of heart disease, and which survive once the others are accounted for?
#
# Input : ../dataset/cleaned_data.rds
# Output: heart_disease_lasso_coefficients.csv, heart_disease_cv_curve.pdf,
#         heart_disease_confusion_matrix.csv
#
# Run with activity2/step4 as the working directory.

library(glmnet)

data <- readRDS("../dataset/cleaned_data.rds") # Adjust this to fit the rds file location

numeric_predictors <- c(
  "age", "resting_bp_systolic", "resting_bp_diastolic", "cholesterol_total",
  "hdl", "ldl", "triglycerides", "fasting_blood_sugar", "hba1c", "bmi",
  "resting_heart_rate", "max_heart_rate_achieved", "st_depression",
  "alcohol_units_per_week", "exercise_minutes_per_week", "sleep_hours",
  "stress_score", "daily_steps", "diet_quality_score"
)
categorical_predictors <- c(
  "sex", "chest_pain_type", "exercise_induced_angina",
  "family_history", "smoker_status", "wearable_owner"
)

response <- data$has_heart_disease_num
cat("cases:", sum(response), "of", length(response),
  sprintf("(%.1f%%)", 100 * mean(response)), "\n"
)

# 4.1 LASSO-selected logistic regression --------------------------------------
# Shrinks weak/redundant predictors to exactly zero, so the nonzero survivors
# are the variables worth reading odds ratios from; cv.glmnet picks the
# penalty by 10-fold cross-validation.
predictor_matrix <- model.matrix(
  ~.,
  data = data[c(numeric_predictors, categorical_predictors)]
)[, -1] # drop the intercept column; glmnet fits its own unpenalized intercept

set.seed(452)
cv_fit <- cv.glmnet(predictor_matrix, response, family = "binomial", alpha = 1)
cat("lambda.min:", round(cv_fit$lambda.min, 5), "\n")

lasso_coefficients <- coef(cv_fit, s = "lambda.min")
selected <- lasso_coefficients[lasso_coefficients[, 1] != 0, , drop = FALSE]
odds_ratio_table <- data.frame(
  variable = rownames(selected),
  coefficient = round(selected[, 1], 4),
  odds_ratio = round(exp(selected[, 1]), 4)
)
print(odds_ratio_table, row.names = FALSE)
write.csv(odds_ratio_table, "heart_disease_lasso_coefficients.csv", row.names = FALSE)

cat(
  "kept", nrow(odds_ratio_table) - 1, "of", ncol(predictor_matrix), "predictors\n"
)
# Kept 27 of 28 - only resting_bp_diastolic dropped to zero, redundant with
# resting_bp_systolic. exercise_induced_anginaYes has the largest odds ratio
# (8.89).

# 4.2 Cross-validation curve ---------------------------------------------------
# Shows whether lambda.min sits in a flat region (many penalties equally
# good) or a sharp minimum.
pdf("heart_disease_cv_curve.pdf", width = 7, height = 5)
plot(cv_fit)
dev.off()

# 4.3 Classification performance (in-sample) -----------------------------------
# Confusion matrix at the standard 0.5 cutoff checks whether the selected
# model actually separates the two groups, not just fits the deviance.
predicted_probability <- predict(cv_fit, predictor_matrix, s = "lambda.min", type = "response")
predicted_class <- ifelse(predicted_probability >= 0.5, 1, 0)
confusion_matrix <- table(actual = response, predicted = predicted_class)
print(confusion_matrix)

accuracy <- mean(predicted_class == response)
sensitivity <- confusion_matrix["1", "1"] / sum(confusion_matrix["1", ])
specificity <- confusion_matrix["0", "0"] / sum(confusion_matrix["0", ])
cat(
  "accuracy:", round(accuracy, 4), " sensitivity:", round(sensitivity, 4),
  " specificity:", round(specificity, 4), "\n"
)
write.csv(as.data.frame.matrix(confusion_matrix), "heart_disease_confusion_matrix.csv")
# 90.2% accuracy, but sensitivity (80.6%) trails specificity (94.3%) - the
# model misses more true cases than it misflags healthy ones.
