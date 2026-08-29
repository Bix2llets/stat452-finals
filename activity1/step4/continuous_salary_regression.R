# Step 4 - Continuous salary regression
# Run: Rscript activity1/step4/continuous_salary_regression.R

# 1. Data and predictor definitions ------------------------------------------

input_path <- file.path("activity1", "dataset", "cleaned_data.rds")
output_path <- file.path("activity1", "step4", "continuous_salary_models.rds")
diagnostic_path <- file.path("activity1", "step4", "mlr_diagnostics.pdf")
salary_data <- readRDS(input_path)
set.seed(6767)

# observed_salary_in_usd is the original response. salary_in_usd is Step 1's
# winsorised copy and is excluded as target leakage.
response <- "observed_salary_in_usd"
factor_predictors <- c(
  "experience_level", "employment_type", "company_location",
  "company_size", "role", "leadership"
)
predictors <- c(factor_predictors)
required_columns <- c("source_row_id", "salary_in_usd", response, predictors)
missing_columns <- setdiff(required_columns, names(salary_data))
if (length(missing_columns) > 0L) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}
stopifnot(
  !anyNA(salary_data[required_columns]),
  all(is.finite(salary_data[[response]])),
  all(salary_data[[response]] > 0),
  all(vapply(salary_data[factor_predictors], is.factor, logical(1)))
)

# 2. Strict future-year split -------------------------------------------------

salary_data$original_row_index <- seq_len(nrow(salary_data))
salary_data <- salary_data[
  order(salary_data$work_year, salary_data$original_row_index, method = "radix"), ,
  drop = FALSE
]
shuffle_row <- sample(nrow(data), size = 0.8 * nrow(data))
test_year <- max(salary_data$work_year)
training_data <- salary_data[shuffle_row, ]
testing_data <- salary_data[-shuffle_row, ]
# stopifnot(max(training_data$work_year) < min(testing_data$work_year))

# unique(training_data$work_year)
# unique(testing_data$work_year)
# 3. Factor levels and treatment coding --------------------------------------

for (variable in factor_predictors) {
  training_data[[variable]] <- droplevels(training_data[[variable]])
  unseen <- setdiff(
    levels(droplevels(testing_data[[variable]])),
    levels(training_data[[variable]])
  )
  if (length(unseen) > 0L) {
    stop("Unseen test level(s) in ", variable, ": ", paste(unseen, collapse = ", "))
  }
  testing_data[[variable]] <- factor(
    testing_data[[variable]],
    levels = levels(training_data[[variable]]),
    ordered = is.ordered(training_data[[variable]])
  )
}
stopifnot(
  !anyNA(testing_data[factor_predictors]),
  all(vapply(testing_data[factor_predictors], is.factor, logical(1)))
)

factor_levels <- lapply(training_data[factor_predictors], levels)
factor_ordered <- vapply(training_data[factor_predictors], is.ordered, logical(1))
factor_references <- vapply(factor_levels, `[`, character(1), 1L)
treatment_contrasts <- lapply(
  factor_levels, function(levels) contr.treatment(levels, base = 1L)
)

# 4. Multiple linear regression ----------------------------------------------

model_formula <- salary_in_usd ~
  experience_level + employment_type + remote_ratio +
  (company_location * company_size) + leadership + role
design_formula <- ~
  experience_level + employment_type + remote_ratio +
    (company_location * company_size) + leadership + role

multiple_linear_model <- lm(
  model_formula,
  data = training_data, contrasts = treatment_contrasts
)
boxcox(multiple_linear_model)
model_formula <- sqrt(salary_in_usd) ~
  experience_level + employment_type + remote_ratio +
  (company_location * company_size) + leadership + role
design_formula <- ~
  experience_level + employment_type + remote_ratio +
    (company_location * company_size) + leadership + role
multiple_linear_model <- lm(
  (model_formula),
  data = training_data, contrasts = treatment_contrasts
)
boxcox(multiple_linear_model)
summary(multiple_linear_model)
stopifnot(
  multiple_linear_model$rank == length(coef(multiple_linear_model)),
  !anyNA(coef(multiple_linear_model))
)
multiple_linear_predictions <- as.numeric(
  predict(multiple_linear_model, newdata = testing_data)
)

# 5. MLR diagnostics: report only; do not remove or refit ---------------------

(mlr_residuals <- residuals(multiple_linear_model))
(residual_mean_test <- t.test(mlr_residuals, mu = 0))
(shapiro_test <- shapiro.test(mlr_residuals))
(breusch_pagan_test <- lmtest::bptest(multiple_linear_model))

par(mfrow = c(2, 2))
plot(multiple_linear_model)
par(mfrow = c(1, 1))
(gvif <- car::vif(multiple_linear_model))
(
  gvif_table <- data.frame(
    term = rownames(gvif), GVIF = gvif[, 1],
    degrees_of_freedom = gvif[, 2], adjusted_GVIF = gvif[, 3], row.names = NULL
  )
)

# cooks_values <- cooks.distance(multiple_linear_model)
# leverage_values <- hatvalues(multiple_linear_model)
# studentized_residuals <- rstudent(multiple_linear_model)
# influence_cutoffs <- c(
#   cooks_distance = 4 / nrow(training_data),
#   leverage = 2 * length(coef(multiple_linear_model)) / nrow(training_data),
#   absolute_studentized_residual = 3
# )
# influence_counts <- c(
#   cooks_distance = sum(cooks_values > influence_cutoffs["cooks_distance"]),
#   leverage = sum(leverage_values > influence_cutoffs["leverage"]),
#   absolute_studentized_residual = sum(
#     abs(studentized_residuals) > influence_cutoffs["absolute_studentized_residual"]
#   )
# )
# most_influential_position <- which.max(cooks_values)
# most_influential_observation <- data.frame(
#   original_row_index = training_data$original_row_index[most_influential_position],
#   source_row_id = training_data$source_row_id[most_influential_position],
#   cooks_distance = cooks_values[most_influential_position],
#   leverage = leverage_values[most_influential_position],
#   studentized_residual = studentized_residuals[most_influential_position]
# )


# 6. LASSO with year-balanced training-only CV -------------------------------

x_training_full <- model.matrix(
  design_formula, training_data,
  contrasts.arg = treatment_contrasts
)
x_testing_full <- model.matrix(
  design_formula, testing_data,
  contrasts.arg = treatment_contrasts
)
x_training <- x_training_full[, -1, drop = FALSE]
x_testing <- x_testing_full[, -1, drop = FALSE]
y_training <- training_data[[response]]

n_folds <- 5L
fold_id <- integer(nrow(training_data))
for (year in sort(unique(training_data$work_year))) {
  positions <- which(training_data$work_year == year)
  fold_id[positions] <- rep(seq_len(n_folds), length.out = length(positions))
}
fold_year_distribution <- table(work_year = training_data$work_year, fold = fold_id)
stopifnot(all(fold_year_distribution > 0L))

lasso_cv <- glmnet::cv.glmnet(
  x_training, y_training,
  alpha = 1, foldid = fold_id,
  standardize = TRUE, type.measure = "mse"
)
selected_lambda <- lasso_cv$lambda.1se
lasso_model <- glmnet::glmnet(
  x_training, y_training,
  alpha = 1,
  lambda = selected_lambda, standardize = TRUE
)
lasso_predictions <- as.numeric(
  predict(lasso_model, newx = x_testing, s = selected_lambda)
)

# 7. Save the Step 6 interface ------------------------------------------------

stopifnot(
  length(multiple_linear_predictions) == nrow(testing_data),
  length(lasso_predictions) == nrow(testing_data),
  all(is.finite(multiple_linear_predictions)),
  all(is.finite(lasso_predictions))
)
test_predictions <- data.frame(
  original_row_index = testing_data$original_row_index,
  source_row_id = testing_data$source_row_id,
  work_year = testing_data$work_year,
  actual_salary_in_usd = testing_data[[response]],
  multiple_linear_regression = multiple_linear_predictions,
  lasso_regression = lasso_predictions
)

bundle <- list(
  response = response,
  predictors = predictors,
  formulas = list(mlr = model_formula, lasso = design_formula),
  split = list(
    method = "Train on 2020-2021; test on 2022",
    training_original_row_index = training_data$original_row_index,
    testing_original_row_index = testing_data$original_row_index,
    training_years = sort(unique(training_data$work_year)),
    testing_years = sort(unique(testing_data$work_year)),
    training_year_distribution = table(training_data$work_year),
    testing_year_distribution = table(testing_data$work_year)
  ),
  preprocessing = list(
    factor_levels = factor_levels,
    factor_ordered = factor_ordered,
    factor_coding = "Treatment contrasts; first level is reference",
    factor_reference_levels = factor_references,
    design_matrix_columns = colnames(x_training)
  ),
  tuning = list(
    method = "Deterministic 5-fold CV balanced by training year",
    fold_id = fold_id,
    fold_year_distribution = fold_year_distribution,
    selection_rule = "lambda.1se",
    lambda_min = lasso_cv$lambda.min,
    lambda_1se = lasso_cv$lambda.1se,
    selected_lambda = selected_lambda,
    cv_summary = data.frame(
      lambda = lasso_cv$lambda, mean_cv_mse = lasso_cv$cvm,
      cv_standard_error = lasso_cv$cvsd, nonzero_coefficients = lasso_cv$nzero
    )
  ),
  models = list(
    multiple_linear_regression = multiple_linear_model,
    lasso_regression = lasso_model
  ),
  mlr_diagnostics = list(
    plot_file = diagnostic_path,
    residual_mean = mean(mlr_residuals),
    residual_mean_test = residual_mean_test,
    shapiro_wilk = shapiro_test,
    breusch_pagan = breusch_pagan_test,
    gvif = gvif_table,
    influence_cutoffs = influence_cutoffs,
    influence_counts = influence_counts,
    most_influential_observation = most_influential_observation
  ),
  test_predictions = test_predictions
)
saveRDS(bundle, output_path)

cat("\nStep 4 completed\n")
cat("Train/test rows:", nrow(training_data), "/", nrow(testing_data), "\n")
cat(
  "Train/test years:", paste(names(table(training_data$work_year)), collapse = ","),
  "/", test_year, "\n"
)
cat(
  "MLR: Shapiro p =", signif(shapiro_test$p.value, 4),
  "; BP p =", signif(breusch_pagan_test$p.value, 4),
  "; max adjusted GVIF =", signif(max(gvif_table$adjusted_GVIF), 4), "\n"
)
cat("Influence flags (Cook/leverage/studentized):", paste(influence_counts, collapse = "/"), "\n")
cat(
  "LASSO lambda.min / lambda.1se:", signif(lasso_cv$lambda.min, 7), "/",
  signif(selected_lambda, 7), "\n"
)
cat(
  "Finite predictions:", all(is.finite(multiple_linear_predictions)), "/",
  all(is.finite(lasso_predictions)), "\n"
)
cat("Saved:", output_path, "and", diagnostic_path, "\n")
