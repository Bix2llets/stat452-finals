# Activity 2 - Step 4: Modelling
#   4A  Which clinical and lifestyle variables predict the heart's working
#       range (heart_rate_difference = peak minus resting), once every other
#       variable is held fixed? Multiple linear regression against ridge,
#       LASSO and elastic net, all tuned by cross-validation.
#   4B  Which variables raise the odds of heart disease? Ordinary binary
#       logistic regression, read as odds ratios and scored on held-out data.
#
# Input : ../dataset/cleaned_data.rds
# Output: heart_rate_coefficients.csv, heart_rate_penalty_selection.csv,
#         heart_rate_cv_curve.pdf, heart_rate_diagnostics.pdf,
#         heart_rate_model_comparison.csv, heart_disease_odds_ratios.csv,
#         heart_disease_confusion_matrix.csv, heart_disease_threshold_sweep.csv
#
# Run with activity2/step4 as the working directory.

library(glmnet)
library(car)

data <- readRDS("../dataset/cleaned_data.rds") # Adjust this to fit the rds file location

# 4.0 Screening the predictors for redundancy ---------------------------------
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
# near 2.1, low enough to read both coefficients, and 4B needs them separately.

categorical_predictors <- c(
  "sex", "chest_pain_type", "exercise_induced_angina",
  "family_history", "smoker_status", "wearable_owner"
)
predictors <- c(numeric_predictors, categorical_predictors)
stopifnot(!any(sapply(data[categorical_predictors], is.ordered)))

# 4.0b A shared train / test split --------------------------------------------
# Every model below is fitted on the same training rows and scored on the same
# held-out rows, so the comparison measures the model and not the split.
# 80 / 20: with 9,000 patients that still leaves 1,800 held-out rows, enough to
# score a classifier on, while giving every fit more to learn from.
set.seed(452)
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

# 4A's response is peak minus resting heart rate, so its two components are
# dropped there - keeping them would hand the model the answer. 4B keeps them:
# the exercise test is a measurement, not a restatement, of the diagnosis.
regression_predictors <- setdiff(
  predictors, c("resting_heart_rate", "max_heart_rate_achieved")
)

# ============================================================================
# 4A  Continuous response: the heart's working range
# ============================================================================

y_training <- training_data$heart_rate_difference
y_testing <- testing_data$heart_rate_difference

# 4A.1 Multiple linear regression ---------------------------------------------
# Least squares with dummy-coded factors: each coefficient is the change in the
# working range per unit of its predictor, with the others held fixed.
# Fit the full model first - it is the unpenalised baseline the shrunk fits are
# read against, and the only one with t tests on its coefficients.
linear_model <- lm(
  reformulate(regression_predictors, response = "heart_rate_difference"),
  data = training_data
)
linear_summary <- summary(linear_model)
cat(
  "MLR R-squared:", round(linear_summary$r.squared, 4),
  " adjusted:", round(linear_summary$adj.r.squared, 4),
  " residual se:", round(linear_summary$sigma, 3), "bpm\n"
)

significance_level <- 0.05
coefficient_table <- coef(linear_summary)
significant_terms <- rownames(coefficient_table)[
  coefficient_table[, "Pr(>|t|)"] < significance_level
]
cat(
  "significant at", significance_level, ":",
  length(setdiff(significant_terms, "(Intercept)")), "of",
  nrow(coefficient_table) - 1, "coefficients\n"
)
print(round(coefficient_table[significant_terms, ], 4))
write.csv(
  data.frame(term = rownames(coefficient_table), round(coefficient_table, 5),
    row.names = NULL
  ),
  "heart_rate_coefficients.csv",
  row.names = FALSE
)
# 18 of 23 coefficients are significant and the fit explains 57.5% of the
# training variance; age alone costs 1.08 bpm of working range per year.

# 4A.2 Regression assumptions --------------------------------------------------
# Least squares needs constant residual variance, roughly normal residuals and
# predictors that are not near-duplicates of each other.
# Chi-square goodness of fit on the residuals, binned at the deciles of a fitted
# normal so every expected count is n/10; df = 10 - 1 - 2 for the two estimated
# parameters. Shapiro-Wilk cannot be used at all: it refuses n > 5000.
goodness_of_fit_normal <- function(x, bin_count = 10) {
  cut_points <- qnorm(
    seq(0, 1, length.out = bin_count + 1),
    mean = mean(x), sd = sd(x)
  )
  cut_points[1] <- -Inf
  cut_points[bin_count + 1] <- Inf
  observed <- as.numeric(table(cut(x, breaks = cut_points)))
  expected <- rep(length(x) / bin_count, bin_count)
  c(chisq = sum((observed - expected)^2 / expected), df = bin_count - 1 - 2)
}
residual_fit <- goodness_of_fit_normal(residuals(linear_model))
cat(
  "residual normality X2:", round(residual_fit["chisq"], 2),
  " on", residual_fit["df"], "df, 5% critical value:",
  round(qchisq(significance_level, residual_fit["df"], lower.tail = FALSE), 2), "\n"
)
# X2 = 8.40 against a 14.07 critical value, so the residuals are close enough to
# normal for the t and F tests to be read as they stand.

# VIF above 5 means a predictor is largely reproducible from the others, which
# inflates its standard error; the generalised version is squared-and-scaled so
# multi-level factors stay comparable with single columns.
variance_inflation <- vif(linear_model)
inflation_scores <- variance_inflation[, "GVIF^(1/(2*Df))"]^2
print(round(sort(inflation_scores, decreasing = TRUE)[1:5], 3))
cat("predictors with VIF > 5:", sum(inflation_scores > 5), "\n")
# Nothing reaches 5 now - the largest is exercise_minutes_per_week at 1.47.
# Screening the correlated pairs first is what keeps the coefficients readable.

pdf("heart_rate_diagnostics.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
plot(linear_model)
dev.off()

# 4A.3 Backward variable selection ---------------------------------------------
# Backward elimination removes one term at a time - each time the one whose
# removal costs the least - and stops when no further removal pays. `step`
# charges k per coefficient kept; setting k to the 5% chi-square critical value
# on 1 df makes "worth keeping" mean exactly "significant at 5%", so the search
# runs on the same rule the coefficient tests use.
# The full fit leaves five insignificant coefficients, so ask how much of the
# model is actually carrying the response, then check the whole removed block
# with one F test rather than trusting a chain of separate decisions.
selection_penalty <- qchisq(significance_level, df = 1, lower.tail = FALSE)
reduced_linear_model <- step(
  linear_model, direction = "backward", k = selection_penalty, trace = 0
)
linear_dropped <- setdiff(
  regression_predictors, attr(terms(reduced_linear_model), "term.labels")
)
linear_block_test <- anova(reduced_linear_model, linear_model)
cat(
  "backward elimination dropped", length(linear_dropped), "of",
  length(regression_predictors), "predictors:",
  paste(linear_dropped, collapse = ", "), "\n"
)
cat(
  "block test: F =", round(linear_block_test[2, "F"], 3),
  " on", linear_block_test[2, "Df"], "and", linear_block_test[2, "Res.Df"],
  "df, p =", round(linear_block_test[2, "Pr(>F)"], 4), "\n"
)
# triglycerides, alcohol, daily steps and wearable ownership leave together for
# F = 0.34 on 4 and 7176 df (p = 0.851), and the held-out error below moves by
# 0.003 bpm - four fewer variables to report at no cost.

# The penalised fits below are given the selected predictors, not all of them:
# a penalty is there to control how hard the surviving predictors are shrunk,
# not to repeat a selection the F tests have already made.
selected_regression_predictors <- attr(terms(reduced_linear_model), "term.labels")
design_formula <- reformulate(selected_regression_predictors)
x_training <- model.matrix(design_formula, training_data)[, -1]
x_testing <- model.matrix(design_formula, testing_data)[, -1]
stopifnot(identical(colnames(x_training), colnames(x_testing)))

# 4A.4 Choosing the penalty mixture and its strength --------------------------
# Ridge shrinks correlated coefficients towards each other and keeps every
# predictor; LASSO can drive one to exactly zero; elastic net is the mixture
# alpha between them, so alpha is a tuning parameter like lambda, not a setting.
# Cross-validate alpha over a grid on the same folds and take the alpha whose
# best lambda gives the smallest held-out error - fixing alpha = 0.5 by hand
# would be an arbitrary choice with no evidence behind it.
alpha_grid <- seq(0, 1, by = 0.1)
penalised_fits <- lapply(alpha_grid, function(alpha) {
  cv.glmnet(x_training, y_training, alpha = alpha, foldid = fold_assignment)
})
names(penalised_fits) <- paste0("alpha_", alpha_grid)

penalty_selection <- data.frame(
  alpha = alpha_grid,
  lambda_min = sapply(penalised_fits, function(fit) round(fit$lambda.min, 5)),
  cv_mean_squared_error = sapply(
    penalised_fits, function(fit) round(fit$cvm[fit$index["min", ]], 4)
  ),
  nonzero_coefficients = sapply(
    penalised_fits, function(fit) fit$nzero[fit$index["min", ]]
  ),
  row.names = NULL
)
print(penalty_selection, row.names = FALSE)
write.csv(penalty_selection, "heart_rate_penalty_selection.csv", row.names = FALSE)
# Pure ridge is the worst rung (246.46); everything from alpha = 0.1 up sits
# within 0.005 of the winner at 0.4, so what helps is having some L1 at all,
# not the exact mixture - which is also why fixing 0.5 by hand cannot be
# defended: on this data the evidence points at 0.4.

best_position <- which.min(penalty_selection$cv_mean_squared_error)
best_alpha <- penalty_selection$alpha[best_position]
elastic_fit <- penalised_fits[[best_position]]
cat(
  "cross-validated alpha:", best_alpha,
  " lambda.min:", round(elastic_fit$lambda.min, 5),
  " lambda.1se:", round(elastic_fit$lambda.1se, 5), "\n"
)

ridge_fit <- penalised_fits[[which(alpha_grid == 0)]]
lasso_fit <- penalised_fits[[which(alpha_grid == 1)]]
lasso_coefficients <- coef(lasso_fit, s = "lambda.min")
cat(
  "LASSO keeps", sum(lasso_coefficients[, 1] != 0) - 1, "of",
  ncol(x_training), "predictors; dropped:",
  paste(setdiff(
    rownames(lasso_coefficients)[lasso_coefficients[, 1] == 0], "(Intercept)"
  ), collapse = ", "), "\n"
)
# The cross-validated alpha is 0.4, a genuine mixture. Pure LASSO removes only
# one more column, non-anginal chest pain: the correlation screen and the
# backward elimination have already taken out everything that was carrying
# nothing, which is what feeding the penalties the selected set is for.

# The curve shows whether lambda.min sits in a flat region, where a larger
# penalty costs almost nothing, or at a sharp minimum.
pdf("heart_rate_cv_curve.pdf", width = 7, height = 5)
plot(elastic_fit)
title(paste("Elastic net, cross-validated alpha =", best_alpha), line = 2.5)
dev.off()

# 4A.5 Held-out comparison ------------------------------------------------------
# Root mean squared error is in bpm, mean absolute error resists large misses,
# and R-squared on the test rows is 1 - SSE/SST against the test mean.
score_regression <- function(model_name, predicted) {
  residual <- y_testing - predicted
  data.frame(
    model = model_name,
    test_rmse = round(sqrt(mean(residual^2)), 3),
    test_mae = round(mean(abs(residual)), 3),
    test_r_squared = round(1 - sum(residual^2) / sum((y_testing - mean(y_testing))^2), 4)
  )
}
predict_penalised <- function(fit) {
  as.numeric(predict(fit, newx = x_testing, s = "lambda.min"))
}

regression_comparison <- rbind(
  score_regression("multiple linear regression", predict(linear_model, testing_data)),
  score_regression(
    paste0("backward elimination (", length(linear_dropped), " dropped)"),
    predict(reduced_linear_model, testing_data)
  ),
  score_regression("ridge (alpha = 0)", predict_penalised(ridge_fit)),
  score_regression("LASSO (alpha = 1)", predict_penalised(lasso_fit)),
  score_regression(
    paste0("elastic net (cross-validated alpha = ", best_alpha, ")"),
    predict_penalised(elastic_fit)
  )
)
print(regression_comparison, row.names = FALSE)
write.csv(regression_comparison, "heart_rate_model_comparison.csv", row.names = FALSE)
# All five land within 0.03 bpm of each other (RMSE 15.961-15.993, R-squared
# 0.548-0.550). With 7,200 training rows against 19 columns least squares is not
# overfitting, so there is no variance for a penalty to buy back; what selection
# buys here is a smaller model to report, not a more accurate one.

# ============================================================================
# 4B  Binary response: the odds of heart disease
# ============================================================================

# 4B.1 Ordinary binary logistic regression -------------------------------------
# Logistic regression models the log-odds of the diagnosis as a linear function
# of the predictors, so exp(coefficient) is the odds ratio per unit. Linear
# regression is not an option: it would return probabilities outside [0, 1].
# Fit the full model on the training rows, then read which terms are
# distinguishable from zero by their Wald z tests.
logistic_data <- training_data[c(predictors, "has_heart_disease_num")]
logistic_model <- glm(
  has_heart_disease_num ~ .,
  data = logistic_data, family = binomial
)
logistic_summary <- summary(logistic_model)
cat(
  "null deviance:", round(logistic_model$null.deviance, 1),
  " residual deviance:", round(deviance(logistic_model), 1),
  "on", df.residual(logistic_model), "df\n"
)
print(round(coef(logistic_summary), 4))
# The deviance falls from 8803.2 to 3442.4, so the predictors together carry a
# large amount of information about the diagnosis.

# 4B.2 Backward variable selection ----------------------------------------------
# The same search on the logistic model, where the cost of a deletion is the
# rise in deviance - chi-square on the coefficients removed - so the same k
# keeps the rule at the 5% level. `step`'s default k = 2 is far weaker: at 7,200
# patients it keeps everything with a p-value under roughly 0.16, which here
# means dropping a single predictor.
# Print what the default would have kept, so the stricter setting is a shown
# choice rather than a silent one.
permissive_model <- step(logistic_model, direction = "backward", trace = 0)
reduced_model <- step(
  logistic_model, direction = "backward", k = selection_penalty, trace = 0
)
cat(
  "step at k = 2 keeps", length(attr(terms(permissive_model), "term.labels")),
  "predictors; at the 5% level it keeps",
  length(attr(terms(reduced_model), "term.labels")), "\n"
)
droppable <- setdiff(predictors, attr(terms(reduced_model), "term.labels"))
cat(
  "backward elimination dropped", length(droppable), "of", length(predictors),
  "predictors:", paste(droppable, collapse = ", "), "\n"
)

deviance_test <- anova(reduced_model, logistic_model, test = "Chisq")
cat(
  "block test: X2 =", round(deviance_test[2, "Deviance"], 3),
  " on", deviance_test[2, "Df"], "df, p =",
  round(deviance_test[2, "Pr(>Chi)"], 4), "\n"
)
# The same four the linear model lost, bar exercise minutes for daily steps.
# Removing them together costs X2 = 8.80 on 4 df (p = 0.066), short of the 5%
# cut, so the smaller model is the one to interpret.

# 4B.3 Odds ratios ---------------------------------------------------------------
# exp(coefficient) is the odds ratio, and exp of the Wald interval gives its
# confidence interval; an interval covering 1 means no detected effect.
# Per one unit hides predictors measured in small steps - one extra daily step
# moves nothing - so also report a one-standard-deviation move, which is
# comparable with a dummy's full 0 -> 1 step.
model_terms <- names(coef(reduced_model))[-1]
predictor_sd <- apply(model.matrix(reduced_model)[, -1, drop = FALSE], 2, sd)
step_size <- ifelse(model_terms %in% numeric_predictors, predictor_sd[model_terms], 1)
wald_interval <- confint.default(reduced_model, level = 1 - significance_level)

odds_ratio_table <- data.frame(
  variable = model_terms,
  coefficient = round(coef(reduced_model)[model_terms], 4),
  odds_ratio_per_unit = round(exp(coef(reduced_model)[model_terms]), 4),
  lower = round(exp(wald_interval[model_terms, 1]), 4),
  upper = round(exp(wald_interval[model_terms, 2]), 4),
  step = round(step_size, 3),
  odds_ratio_per_step = round(exp(coef(reduced_model)[model_terms] * step_size), 4),
  row.names = NULL
)
odds_ratio_table <- odds_ratio_table[
  order(abs(log(odds_ratio_table$odds_ratio_per_step)), decreasing = TRUE),
]
print(odds_ratio_table, row.names = FALSE)
write.csv(odds_ratio_table, "heart_disease_odds_ratios.csv", row.names = FALSE)
# Exercise-induced angina multiplies the odds by 9.22 with everything else held
# fixed, but per standard deviation the exercise test dominates: 21.3 bpm more
# peak heart rate multiplies the odds by 0.070.

# 4B.4 Classification on the held-out rows ----------------------------------------
# The fitted probability becomes a prediction only after a cutoff is chosen; at
# 0.5 the confusion matrix gives accuracy, sensitivity (share of true cases
# found) and specificity (share of healthy patients cleared).
# Score the test rows, which the model never saw, so the rates are not inflated
# by the fit.
predicted_probability <- predict(reduced_model, testing_data, type = "response")
actual <- testing_data$has_heart_disease_num

score_classification <- function(cutoff) {
  predicted <- as.integer(predicted_probability >= cutoff)
  true_positive <- sum(predicted == 1 & actual == 1)
  true_negative <- sum(predicted == 0 & actual == 0)
  false_positive <- sum(predicted == 1 & actual == 0)
  false_negative <- sum(predicted == 0 & actual == 1)
  precision <- true_positive / (true_positive + false_positive)
  recall <- true_positive / (true_positive + false_negative)
  data.frame(
    cutoff = cutoff,
    accuracy = round((true_positive + true_negative) / length(actual), 4),
    sensitivity = round(recall, 4),
    specificity = round(true_negative / (true_negative + false_positive), 4),
    precision = round(precision, 4),
    f1 = round(2 * precision * recall / (precision + recall), 4)
  )
}

confusion_matrix <- table(
  actual = actual,
  predicted = as.integer(predicted_probability >= 0.5)
)
print(confusion_matrix)
write.csv(as.data.frame.matrix(confusion_matrix), "heart_disease_confusion_matrix.csv")
print(score_classification(0.5), row.names = FALSE)
# 90.1% accuracy on rows the model never saw, but sensitivity (80.3%) trails
# specificity (94.5%): it misses more true cases than it misflags healthy ones.

# A cutoff is a choice, not a property of the model: lowering it trades
# specificity for sensitivity. Sweep it so the trade is visible in numbers.
threshold_sweep <- do.call(rbind, lapply(seq(0.1, 0.9, by = 0.1), score_classification))
print(threshold_sweep, row.names = FALSE)
write.csv(threshold_sweep, "heart_disease_threshold_sweep.csv", row.names = FALSE)
# F1 peaks at 0.5 (0.8347) with 0.4 a hair behind (0.8335) at 84.0% sensitivity;
# screening, where a missed case costs more than a false alarm, would go lower
# still - 0.2 finds 91.1% of the cases for 10.0 points of specificity.
