# Activity 2 - Step 4b: Which clinical and lifestyle variables predict the
# heart's working range (heart_rate_difference = peak minus resting), once
# every other variable is held fixed? Multiple linear regression, backward
# elimination, then ridge, LASSO and elastic net on what survives.
#
# Input : ../dataset/cleaned_data.rds via data_preparation.R
# Output: heart_rate_coefficients.csv, heart_rate_penalty_selection.csv,
#         heart_rate_diagnostics.pdf, heart_rate_cv_curve.pdf,
#         heart_rate_predictions.rds (read by model_evaluation.R)
#
# Run with activity2/step4 as the working directory.

library(glmnet)
library(car)

source("data_preparation.R")

y_training <- training_data$heart_rate_difference
y_testing <- testing_data$heart_rate_difference

# 4b.1 Multiple linear regression ----------------------------------------------
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
# 18 of 22 coefficients are significant and the fit explains 57.5% of the
# training variance; age alone costs 1.08 bpm of working range per year.

# 4b.2 Regression assumptions --------------------------------------------------
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
# X2 = 8.13 against a 14.07 critical value, so the residuals are close enough to
# normal for the t and F tests to be read as they stand.

# VIF above 5 means a predictor is largely reproducible from the others, which
# inflates its standard error; the generalised version is squared-and-scaled so
# multi-level factors stay comparable with single columns.
variance_inflation <- vif(linear_model)
inflation_scores <- variance_inflation[, "GVIF^(1/(2*Df))"]^2
print(round(sort(inflation_scores, decreasing = TRUE)[1:5], 3))
cat("predictors with VIF > 5:", sum(inflation_scores > 5), "\n")
# Nothing reaches 5 - the largest is exercise_minutes_per_week at 1.41.
# Screening the correlated pairs first is what keeps the coefficients readable.

pdf("heart_rate_diagnostics.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
plot(linear_model)
dev.off()

# 4b.3 Backward variable selection ---------------------------------------------
# Backward elimination removes one term at a time - each time the one whose
# removal costs the least - and stops when no further removal pays. With k set
# to the 5% critical value, `step` keeps exactly the terms the F tests call
# significant.
# The full fit leaves four insignificant coefficients, so ask how much of the
# model is actually carrying the response, then check the whole removed block
# with one F test rather than trusting a chain of separate decisions.
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
# triglycerides, alcohol and daily steps leave together for F = 0.44 on 3 and
# 7177 df (p = 0.722) - three fewer variables to report, and the evaluation
# shows the held-out error moving by 0.002 bpm.

# The penalised fits below are given the selected predictors, not all of them:
# a penalty is there to control how hard the surviving predictors are shrunk,
# not to repeat a selection the F tests have already made.
selected_regression_predictors <- attr(terms(reduced_linear_model), "term.labels")
design_formula <- reformulate(selected_regression_predictors)
x_training <- model.matrix(design_formula, training_data)[, -1]
x_testing <- model.matrix(design_formula, testing_data)[, -1]
stopifnot(identical(colnames(x_training), colnames(x_testing)))

# 4b.4 Choosing the penalty mixture and its strength ---------------------------
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

# 4b.5 Held-out predictions for the evaluation ---------------------------------
# Every fit predicts the same held-out patients; scoring them is
# model_evaluation.R's job, so hand it the predictions beside the truth.
predict_penalised <- function(fit) {
  as.numeric(predict(fit, newx = x_testing, s = "lambda.min"))
}
saveRDS(
  list(
    response = "heart_rate_difference",
    actual = y_testing,
    predictions = data.frame(
      `multiple linear regression` = predict(linear_model, testing_data),
      `backward elimination` = predict(reduced_linear_model, testing_data),
      `ridge (alpha = 0)` = predict_penalised(ridge_fit),
      `LASSO (alpha = 1)` = predict_penalised(lasso_fit),
      `elastic net (cross-validated)` = predict_penalised(elastic_fit),
      check.names = FALSE
    ),
    best_alpha = best_alpha,
    dropped = linear_dropped,
    selected = selected_regression_predictors
  ),
  "heart_rate_predictions.rds"
)
cat("saved heart_rate_predictions.rds for the evaluation\n")
