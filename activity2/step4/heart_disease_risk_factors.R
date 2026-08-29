# Activity 2 - Step 4: Which clinical and lifestyle variables predict the
# odds of heart disease, and which survive once the others are accounted for?
#
# Input : ../dataset/cleaned_data.rds
# Output: heart_disease_lasso_coefficients.csv, heart_disease_cv_curve.pdf,
#         heart_disease_confusion_matrix.csv, heart_disease_model_comparison.csv
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
# LASSO shrinks weak and redundant predictors to exactly zero, so the survivors
# are the ones worth reading odds ratios from.
predictor_matrix <- model.matrix(
  ~.,
  data = data[c(numeric_predictors, categorical_predictors)]
)[, -1] # drop the intercept column; glmnet fits its own unpenalized intercept

# One fold assignment, reused by every penalty below, so they are compared on
# identical splits.
set.seed(452)
fold_assignment <- sample(rep(1:10, length.out = length(response)))

cv_fit <- cv.glmnet(
  predictor_matrix, response,
  family = "binomial", alpha = 1, foldid = fold_assignment
)
lasso_coefficients <- coef(cv_fit, s = "lambda.min")
selected <- lasso_coefficients[lasso_coefficients[, 1] != 0, , drop = FALSE]

# exp(beta) is per one unit, which hides predictors measured in small steps:
# one extra daily step moves nothing. exp(beta * sd) puts each continuous
# predictor on a one-sd move, comparable with a dummy's full 0 -> 1 step.
predictor_sd <- apply(predictor_matrix, 2, sd)
selected_names <- rownames(selected)
is_continuous <- selected_names %in% numeric_predictors
step_size <- ifelse(is_continuous, predictor_sd[selected_names], 1)

odds_ratio_table <- data.frame(
  variable = selected_names,
  coefficient = round(selected[, 1], 4),
  odds_ratio_per_unit = round(exp(selected[, 1]), 4),
  step = round(step_size, 3),
  odds_ratio_per_step = round(exp(selected[, 1] * step_size), 4)
)
print(odds_ratio_table, row.names = FALSE)
write.csv(odds_ratio_table, "heart_disease_lasso_coefficients.csv", row.names = FALSE)

cat(
  "kept", nrow(odds_ratio_table) - 1, "of", ncol(predictor_matrix), "predictors\n"
)
# Kept 27 of 28; only resting_bp_diastolic dropped, redundant with the systolic
# reading. exercise_induced_anginaYes leads per unit (8.89), but per sd
# max_heart_rate_achieved is strongest (0.073 over 21.3 bpm) and daily_steps
# moves from 1.0000 to 0.916.

# 4.2 Cross-validation curve ---------------------------------------------------
# Shows whether lambda.min sits in a flat region (many penalties equally
# good) or a sharp minimum.
pdf("heart_disease_cv_curve.pdf", width = 7, height = 5)
plot(cv_fit)
dev.off()

# lambda.1se is the largest penalty still within one standard error of the best
# deviance - nearly free if the curve is flat, costly if it is sharp. Read the
# two side by side instead of eyeballing the figure.
minimum_index <- cv_fit$index["min", ]
one_se_index <- cv_fit$index["1se", ]
cat("lambda.min:", round(cv_fit$lambda.min, 5),
  " deviance:", round(cv_fit$cvm[minimum_index], 4),
  " predictors:", cv_fit$nzero[minimum_index], "\n"
)
cat("lambda.1se:", round(cv_fit$lambda.1se, 5),
  " deviance:", round(cv_fit$cvm[one_se_index], 4),
  " predictors:", cv_fit$nzero[one_se_index], "\n"
)
# Flat: an 11-fold larger penalty costs 0.011 of deviance (0.5021 against
# 0.4912) but drops only 2 predictors, so lambda.min is kept.

# 4.3 Classification performance (in-sample) -----------------------------------
# Confusion matrix at the 0.5 cutoff: does the model separate the two groups,
# not just fit the deviance.
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

# 4.4 Unpenalized logistic regression ------------------------------------------
# Maximum likelihood with no penalty - the baseline the shrunk fits are read
# against. `lm` is not an option: a binary response would get probabilities
# outside [0, 1]. Scored at the same 0.5 cutoff, so all four share one scale.
unpenalized_data <- data[c(numeric_predictors, categorical_predictors)]
unpenalized_data$response <- response
unpenalized_fit <- glm(response ~ ., data = unpenalized_data, family = binomial)
cat("unpenalized residual deviance:", round(deviance(unpenalized_fit), 1),
  "on", df.residual(unpenalized_fit), "df\n"
)

# LASSO gives no p-values: shrinkage biases the coefficients, so the usual
# standard errors do not apply. The unpenalized fit has z tests, so it says
# which survivors are distinguishable from zero rather than merely kept.
significance_level <- 0.05
coefficient_tests <- coef(summary(unpenalized_fit))
indistinguishable <- rownames(coefficient_tests)[
  coefficient_tests[, "Pr(>|z|)"] >= significance_level
]
cat("significant at", significance_level, ":",
  sum(coefficient_tests[, "Pr(>|z|)"] < significance_level) - 1, "of",
  nrow(coefficient_tests) - 1, "coefficients\n"
)
cat("kept by LASSO but not distinguishable from zero:",
  paste(intersect(selected_names, indistinguishable), collapse = ", "), "\n"
)

# Reused by every model below: the 0.5-cutoff confusion matrix and its three rates.
score_model <- function(model_name, probability) {
  predicted <- ifelse(probability >= 0.5, 1, 0)
  matrix_counts <- table(actual = response, predicted = predicted)
  data.frame(
    model = model_name,
    accuracy = round(mean(predicted == response), 4),
    sensitivity = round(matrix_counts["1", "1"] / sum(matrix_counts["1", ]), 4),
    specificity = round(matrix_counts["0", "0"] / sum(matrix_counts["0", ]), 4)
  )
}
unpenalized_score <- score_model(
  "unpenalized logistic", predict(unpenalized_fit, type = "response")
)
print(unpenalized_score, row.names = FALSE)
# Deviance 4356.7 on 8971 df and 90.17% accuracy, matching LASSO. But only 21
# of 28 coefficients are significant, and six LASSO kept are not - surviving
# the penalty is a weaker claim than being significant.

# 4.5 Ridge-penalized logistic regression --------------------------------------
# Ridge penalizes the squared coefficients: correlated predictors shrink
# together and nothing reaches zero, so no variable is dropped.
ridge_fit <- cv.glmnet(
  predictor_matrix, response,
  family = "binomial", alpha = 0, foldid = fold_assignment
)
cat("ridge lambda.min:", round(ridge_fit$lambda.min, 5),
  " nonzero coefficients:", ridge_fit$nzero[ridge_fit$index["min", ]], "\n"
)
ridge_score <- score_model(
  "ridge (alpha = 0)",
  predict(ridge_fit, predictor_matrix, s = "lambda.min", type = "response")
)
print(ridge_score, row.names = FALSE)
# All 28 kept, as expected, and accuracy drops to 89.56%: ridge trades 5 points
# of sensitivity (75.76%) for 1 of specificity (95.55%).

# 4.6 Elastic-net logistic regression ------------------------------------------
# Elastic net mixes both penalties, keeping the ability to zero a coefficient
# while sharing weight between correlated predictors the way ridge does.
elastic_fit <- cv.glmnet(
  predictor_matrix, response,
  family = "binomial", alpha = 0.5, foldid = fold_assignment
)
cat("elastic net lambda.min:", round(elastic_fit$lambda.min, 5),
  " nonzero coefficients:", elastic_fit$nzero[elastic_fit$index["min", ]], "\n"
)
elastic_score <- score_model(
  "elastic net (alpha = 0.5)",
  predict(elastic_fit, predictor_matrix, s = "lambda.min", type = "response")
)
print(elastic_score, row.names = FALSE)
# lambda.min = 0.00045 keeps all 28 - small enough to leave the unpenalized fit
# essentially intact (90.14% accuracy).

# 4.7 Comparing the four fits --------------------------------------------------
# One model under four penalties, so the gaps measure what the penalty costs.
# The rates are in-sample at the 0.5 cutoff and so an upper bound; the deviance
# is the held-out value cv.glmnet computed for lambda, read across alpha too.
# The unpenalized fit has no held-out version, hence the flag column.
cross_validated_deviance <- function(fit) round(fit$cvm[fit$index["min", ]], 4)

lasso_score <- score_model("LASSO (alpha = 1)", predicted_probability)
model_comparison <- rbind(
  unpenalized_score, ridge_score, elastic_score, lasso_score
)
model_comparison$deviance <- c(
  round(deviance(unpenalized_fit) / length(response), 4),
  cross_validated_deviance(ridge_fit),
  cross_validated_deviance(elastic_fit),
  cross_validated_deviance(cv_fit)
)
model_comparison$deviance_held_out <- c(FALSE, TRUE, TRUE, TRUE)
print(model_comparison, row.names = FALSE)
write.csv(model_comparison, "heart_disease_model_comparison.csv", row.names = FALSE)
# Accuracy cannot separate them (89.56-90.17%); the held-out deviance can:
# ridge 0.5432 against 0.4912 for LASSO and elastic net, so refusing to drop
# anything costs accuracy. LASSO and elastic net tie, and LASSO wins for being
# the one that drops a variable.
