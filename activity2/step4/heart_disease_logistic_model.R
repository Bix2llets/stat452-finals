source("activity2/step4/data_preparation.R")

# 4c.1 Ordinary binary logistic regression -------------------------------------
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
summary(reduced_model)
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
# 4c.3 Odds ratios --------------------------------------------------------------
# exp(coefficient) is the odds ratio, and exp of the Wald interval gives its
# confidence interval; an interval covering 1 means no detected effect.
# Per one unit hides predictors measured in small steps - one extra daily step
# moves nothing - so also report a one-standard-deviation move, which is
# comparable with a dummy's full 0 -> 1 step.
(model_terms <- names(coef(reduced_model))[-1])
(predictor_sd <- apply(model.matrix(reduced_model)[, -1, drop = FALSE], 2, sd))
(step_size <- ifelse(model_terms %in% numeric_predictors, predictor_sd[model_terms], 1))
(wald_interval <- confint.default(reduced_model, level = 1 - significance_level))

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
# The exercise indudced angina is the strongest sign of having heart disease: having it would cause the odds ratio to increase by 9.68 times.
# Smoking increases the odds of having heart disease by 2.71 times compared to never smoked people . It is alleviated by quitting, which reduced the increment in odds to just 1.46 times never smoked people
# Each 1.1 hours of sleep leads to a 10% drops in odds
# Having chest pain is a sign of having heart disease, where the odds of having disease is increased by 4.788, 2.091 and 1.221 times they have typical angina, atypical angina and non anginal pain, compared to asymptomatic pains
# for each 2170 steps of walking every day the odds of having disease is reduced by 10%
# Male has their odd of having disaese 1.65 times more than female.
# Strangely, the increasing age reduces the odds of having heart disease, in contrary to the anova result on age group, when the other conditions are kept as-is.
# This can be explained as with the same health condition, the older people are considered "healthier" than the young one with the sahme condition, thus their odds of having disease is reduced.
# For each 21 bpm more in max heart rate, the odds of having heart disease is mutiplied by 0.07. This is the sign of a healthy heart that can works more, thus less prone to diseases

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
