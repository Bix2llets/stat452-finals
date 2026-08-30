# Activity 2 - Step 4c: Which clinical and lifestyle variables raise the odds
# of heart disease? Ordinary binary logistic regression, backward elimination,
# and the surviving effects read as odds ratios.
#
# Input : ../dataset/cleaned_data.rds via data_preparation.R
# Output: heart_disease_odds_ratios.csv,
#         heart_disease_predictions.rds (read by model_evaluation.R)
#
# Run with activity2/step4 as the working directory.

source("activity2/step4/data_preparation.R")

# 4c.1 Ordinary binary logistic regression -------------------------------------
# Logistic regression models the log-odds of the diagnosis as a linear function
# of the predictors, so exp(coefficient) is the odds ratio per unit. Linear
# regression is not an option: it would return probabilities outside [0, 1].
# Fit the full model on the training rows, then read which terms are
# distinguishable from zero by their Wald z tests. No penalty is used: shrinkage
# would remove the standard errors the odds ratios are read from.
logistic_data <- training_data[c(predictors, "has_heart_disease_num")]
logistic_model <- glm(
  has_heart_disease_num ~ .,
  data = logistic_data, family = "binomial"
)
logistic_summary <- summary(logistic_model)
logistic_summary
# The deviance falls from 8803.2 to 3444.5, so the predictors together carry a
# large amount of information about the diagnosis.

# 4c.2 Backward variable selection ---------------------------------------------
# The same search the regression uses, except the cost of a deletion here is the
# rise in deviance - chi-square on the coefficients removed - so the same k
# keeps the rule at the 5% level. `step`'s default k = 2 is far weaker: at 7,200
# patients it keeps everything with a p-value under roughly 0.16.
# Print what the default would have kept, so the stricter setting is a shown
# choice rather than a silent one.
permissive_model <- step(logistic_model, direction = "backward", trace = 0)
reduced_model <- step(
  logistic_model,
  direction = "backward", k = selection_penalty, trace = 0
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
# The same three the regression lost, bar exercise minutes for daily steps.
# Removing them together costs X2 = 6.76 on 3 df (p = 0.080), short of the 5%
# cut, so the smaller model is the one to interpret.

# 4c.3 Odds ratios --------------------------------------------------------------
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
# peak heart rate multiplies the odds by 0.070. These are associations in
# observational data, not effects of changing the variable.

# 4c.4 Held-out probabilities for the evaluation -------------------------------
# The model returns a probability per patient; turning it into a prediction
# needs a cutoff, which is model_evaluation.R's business. Hand it the
# probabilities for rows the model never saw, beside the truth.
saveRDS(
  list(
    response = "has_heart_disease_num",
    actual = testing_data$has_heart_disease_num,
    probability = as.numeric(predict(reduced_model, testing_data, type = "response")),
    dropped = droppable,
    selected = attr(terms(reduced_model), "term.labels")
  ),
  "heart_disease_predictions.rds"
)
cat("saved heart_disease_predictions.rds for the evaluation\n")
