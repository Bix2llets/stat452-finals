# Activity 1 - Step 4: continuous salary regression modeling

required_packages <- c("tidyverse", "rsample", "recipes", "glmnet")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the required package(s) before running Step 4: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(tidyverse)
library(rsample)
library(recipes)
library(glmnet)

# Pin the factor coding explicitly instead of inheriting whatever the session
# happens to have. Step 3 sets contr.sum globally for its Type III ANOVA; if
# both files are sourced in one session and that setting leaked in here, the
# model.matrix handed to cv.glmnet would be coded differently, which changes the
# ridge / LASSO penalty and therefore the fitted models. Step 3 now restores the
# option when it finishes, and this line is the second line of defence.
options(contrasts = c("contr.treatment", "contr.poly"))

split_seed <- 6767L
cv_seed <- 6768L
training_proportion <- 0.80
rare_level_threshold <- 0.02

# Resolve paths from the script location so the file works from Rscript and
# when sourced from another working directory.
script_directory <- function() {
  command_line <- commandArgs(trailingOnly = FALSE)
  file_argument <- command_line[str_detect(command_line, "^--file=")]

  if (length(file_argument) > 0) {
    script_path <- str_remove(file_argument[[1]], "^--file=")
    return(dirname(normalizePath(script_path, mustWork = TRUE)))
  }

  source_path <- tryCatch(sys.frame(1)$ofile, error = function(error) NULL)
  if (!is.null(source_path)) {
    return(dirname(normalizePath(source_path, mustWork = TRUE)))
  }

  normalizePath(getwd(), mustWork = TRUE)
}

step4_directory <- script_directory()
cleaned_data_path <- file.path(step4_directory, "..", "dataset", "cleaned_data.rds")
artifact_path <- file.path(step4_directory, "continuous_salary_models.rds")
summary_path <- file.path(step4_directory, "model_training_summary.txt")

data <- readRDS(cleaned_data_path)

required_columns <- c(
  "source_row_id",
  "observed_salary_in_usd",
  "work_year",
  "experience_level",
  "employment_type_code",
  "remote_ratio",
  "company_location_code",
  "company_size",
  "role",
  "leadership"
)

missing_columns <- setdiff(required_columns, names(data))
if (length(missing_columns) > 0) {
  stop(
    "The cleaned data is missing Step 4 fields. Run Step 1 first. Missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (anyNA(data[required_columns])) {
  stop("Step 4 required fields contain missing values before modeling.")
}

if (any(data$observed_salary_in_usd <= 0)) {
  stop("Observed salary must be positive for the square-root response transform.")
}

# Use the original category codes retained by Step 1. Employee residence is
# omitted because it is strongly associated with company location; using both
# would make ordinary least-squares coefficients unstable. Remote ratio is
# categorical because the data contain only 0, 50, and 100 and do not support a
# linear percentage effect assumption.
# We will predict the square root of salary_in_usd by work_year, remote ratio, company location, company size, role and leadership
# Need confirmation from @NguyenVanLeBao
model_data <- data |>
  transmute(
    source_row_id,
    salary_in_usd = observed_salary_in_usd,
    salary_sqrt = sqrt(observed_salary_in_usd),
    work_year = as.numeric(work_year),
    remote_work = factor(case_when(
      remote_ratio == 0 ~ "On-site",
      remote_ratio == 50 ~ "Hybrid",
      remote_ratio == 100 ~ "Fully remote",
      .default = "Other"
    ), ordered = TRUE, labels = c("On-site", "Hybrid", "Fully remote")),
    company_location = company_location,
    company_size = company_size,
    role = role,
    leadership = leadership
  )

predictor_names <- c(
  "work_year",
  "experience_level",
  "employment_type",
  "remote_work",
  "company_location",
  "company_size",
  "role",
  "leadership"
)

# Identical modeling records are kept together in every split. This preserves
# potentially repeated observations while preventing an exact test-set twin
# from appearing in training.
signature_columns <- c("salary_in_usd", predictor_names)
record_signature <- do.call(
  paste,
  c(lapply(model_data[signature_columns], as.character), sep = "\u001f")
)
model_data$duplicate_group <- match(record_signature, unique(record_signature))

# rsample requires a categorical stratum for grouped resampling. Define salary
# quartiles explicitly because grouped resampling otherwise treats each numeric
# salary value as a separate category.
salary_stratum_breaks <- unique(
  quantile(
    model_data$salary_in_usd,
    probs = seq(0, 1, length.out = 5),
    na.rm = TRUE,
    type = 8
  )
)
if (length(salary_stratum_breaks) < 3) {
  stop("There are too few distinct salary values for stratified splitting.")
}
model_data$salary_stratum <- cut(
  model_data$salary_in_usd,
  breaks = salary_stratum_breaks,
  include.lowest = TRUE,
  ordered_result = TRUE
)

set.seed(split_seed)
salary_split <- group_initial_split(
  model_data,
  group = duplicate_group,
  prop = training_proportion,
  strata = salary_stratum
)

training_data <- training(salary_split)
testing_data <- testing(salary_split)

if (length(intersect(training_data$source_row_id, testing_data$source_row_id)) > 0) {
  stop("Training and testing source row IDs overlap.")
}

if (length(intersect(training_data$duplicate_group, testing_data$duplicate_group)) > 0) {
  stop("An identical modeling record occurs in both training and testing.")
}

# All transformations below are prepared from training data only. Rare raw
# employment/location categories are pooled; missing and future unseen levels
# are handled before treatment-dummy encoding. Numeric scaling is learned only
# for work year. step_zv removes unlearnable empty unknown/novel indicators.
training_recipe_data <- training_data |>
  select(salary_sqrt, all_of(predictor_names))

salary_recipe <- recipe(salary_sqrt ~ ., data = training_recipe_data) |>
  step_other(
    employment_type,
    company_location,
    threshold = rare_level_threshold,
    other = ".other"
  ) |>
  step_novel(all_nominal_predictors(), new_level = ".novel") |>
  step_unknown(all_nominal_predictors(), new_level = ".unknown") |>
  step_normalize(work_year) |>
  step_dummy(all_nominal_predictors(), one_hot = FALSE) |>
  step_zv(all_predictors())

prepared_recipe <- prep(
  salary_recipe,
  training = training_recipe_data,
  retain = TRUE,
  verbose = FALSE
)

baked_training <- bake(prepared_recipe, new_data = training_recipe_data)
baked_testing <- bake(
  prepared_recipe,
  new_data = testing_data |> select(all_of(predictor_names))
)

training_predictors <- setdiff(names(baked_training), "salary_sqrt")
if (!identical(training_predictors, names(baked_testing))) {
  stop("Baked training and testing predictor columns are not identical.")
}

if (
  anyNA(baked_training) ||
    anyNA(baked_testing) ||
    any(!is.finite(as.matrix(baked_training))) ||
    any(!is.finite(as.matrix(baked_testing)))
) {
  stop("Baked model matrices contain missing or non-finite values.")
}

# Explicitly probe missing and unseen levels. These values are not used to fit
# or tune a model; the check only verifies the saved recipe's prediction-time
# behavior.
novel_level_probe <- testing_data[1, predictor_names, drop = FALSE]
novel_level_probe$role <- "__unseen_role__"
novel_level_probe$employment_type <- NA_character_
novel_level_probe_baked <- bake(prepared_recipe, new_data = novel_level_probe)

if (
  !identical(names(novel_level_probe_baked), training_predictors) ||
    anyNA(novel_level_probe_baked) ||
    any(!is.finite(as.matrix(novel_level_probe_baked)))
) {
  stop("The recipe failed its missing/unseen categorical-level check.")
}

x_training <- as.matrix(baked_training[training_predictors])
y_training <- baked_training$salary_sqrt
x_testing <- as.matrix(baked_testing[training_predictors])

# Model 1: multiple linear regression on the square-root salary scale.
linear_model <- lm(salary_sqrt ~ ., data = as.data.frame(baked_training))
if (linear_model$rank != length(coef(linear_model))) {
  stop("The multiple linear regression design is not full rank.")
}

# Create duplicate-group-aware tuning folds from training data only. The same
# folds are used for ridge and LASSO so their lambda searches are comparable.
set.seed(cv_seed)
training_folds <- group_vfold_cv(
  training_data,
  group = duplicate_group,
  v = 10,
  balance = "observations",
  strata = salary_stratum
)

fold_id <- integer(nrow(training_data))
for (fold_number in seq_along(training_folds$splits)) {
  assessment_ids <- assessment(training_folds$splits[[fold_number]])$source_row_id
  assessment_rows <- match(assessment_ids, training_data$source_row_id)

  if (anyNA(assessment_rows) || any(fold_id[assessment_rows] != 0L)) {
    stop("Grouped cross-validation folds do not form a valid partition.")
  }

  fold_id[assessment_rows] <- fold_number
}

if (any(fold_id == 0L)) {
  stop("At least one training row was not assigned to a tuning fold.")
}

groups_per_fold <- training_data |>
  mutate(fold_id = fold_id) |>
  group_by(duplicate_group) |>
  summarise(number_of_folds = n_distinct(fold_id), .groups = "drop")

if (any(groups_per_fold$number_of_folds != 1L)) {
  stop("An identical training record was split across tuning folds.")
}

# Models 2 and 3: ridge and LASSO. glmnet standardization is estimated from the
# training matrix inside each fit; the held-out testing matrix is never used to
# choose a penalty.
set.seed(cv_seed)
ridge_model <- cv.glmnet(
  x = x_training,
  y = y_training,
  alpha = 0,
  foldid = fold_id,
  type.measure = "mse",
  standardize = TRUE,
  keep = TRUE
)

set.seed(cv_seed)
lasso_model <- cv.glmnet(
  x = x_training,
  y = y_training,
  alpha = 1,
  foldid = fold_id,
  type.measure = "mse",
  standardize = TRUE,
  keep = TRUE
)

selected_ridge_lambda <- ridge_model$lambda.1se
selected_lasso_lambda <- lasso_model$lambda.1se

linear_training_prediction <- as.numeric(predict(linear_model, newdata = baked_training))
linear_testing_prediction <- as.numeric(predict(linear_model, newdata = baked_testing))
ridge_testing_prediction <- as.numeric(
  predict(ridge_model, newx = x_testing, s = "lambda.1se")
)
lasso_testing_prediction <- as.numeric(
  predict(lasso_model, newx = x_testing, s = "lambda.1se")
)

# For y = z^2 with an additive error on z = sqrt(y), empirical smearing gives
# fitted_z^2 + 2 * fitted_z * mean(error) + mean(error^2). Estimate the terms
# from training residuals only. Penalized-model corrections use grouped
# out-of-fold residuals, not the held-out testing outcomes; they are therefore
# conservative approximations that also include fold-level estimation error.
linear_training_residual <- y_training - linear_training_prediction

ridge_lambda_index <- which.min(abs(ridge_model$lambda - selected_ridge_lambda))
lasso_lambda_index <- which.min(abs(lasso_model$lambda - selected_lasso_lambda))
ridge_oof_prediction <- ridge_model$fit.preval[, ridge_lambda_index]
lasso_oof_prediction <- lasso_model$fit.preval[, lasso_lambda_index]
ridge_oof_residual <- y_training - ridge_oof_prediction
lasso_oof_residual <- y_training - lasso_oof_prediction

smearing_terms <- function(residual) {
  list(
    mean_error = mean(residual),
    mean_squared_error = mean(residual^2)
  )
}

linear_smearing <- smearing_terms(linear_training_residual)
ridge_smearing <- smearing_terms(ridge_oof_residual)
lasso_smearing <- smearing_terms(lasso_oof_residual)

back_transform_salary <- function(
  prediction,
  smearing = list(mean_error = 0, mean_squared_error = 0)
) {
  nonnegative_prediction <- pmax(prediction, 0)
  nonnegative_prediction^2 +
    2 * nonnegative_prediction * smearing$mean_error +
    smearing$mean_squared_error
}

heldout_predictions <- tibble(
  source_row_id = testing_data$source_row_id,
  duplicate_group = testing_data$duplicate_group,
  salary_in_usd = testing_data$salary_in_usd,
  linear_prediction_sqrt = linear_testing_prediction,
  linear_prediction_usd_naive = back_transform_salary(linear_testing_prediction),
  linear_prediction_usd = back_transform_salary(
    linear_testing_prediction,
    linear_smearing
  ),
  ridge_prediction_sqrt = ridge_testing_prediction,
  ridge_prediction_usd_naive = back_transform_salary(ridge_testing_prediction),
  ridge_prediction_usd = back_transform_salary(
    ridge_testing_prediction,
    ridge_smearing
  ),
  lasso_prediction_sqrt = lasso_testing_prediction,
  lasso_prediction_usd_naive = back_transform_salary(lasso_testing_prediction),
  lasso_prediction_usd = back_transform_salary(
    lasso_testing_prediction,
    lasso_smearing
  )
)

prediction_columns <- str_subset(names(heldout_predictions), "_prediction_usd$")
if (
  anyNA(heldout_predictions[prediction_columns]) ||
    any(!is.finite(as.matrix(heldout_predictions[prediction_columns]))) ||
    any(as.matrix(heldout_predictions[prediction_columns]) < 0)
) {
  stop("At least one held-out dollar prediction is invalid.")
}

lasso_coefficients <- as.matrix(coef(lasso_model, s = "lambda.1se"))
lasso_nonzero_predictors <- sum(lasso_coefficients[-1, 1] != 0)

step4_artifact <- list(
  metadata = list(
    activity = 1L,
    step = 4L,
    response = "observed salary_in_usd",
    response_transform = "square root",
    primary_dollar_prediction = "nonnegative square plus empirical training-only smearing correction",
    predictor_names = predictor_names,
    excluded_fields = c(
      "salary_in_usd (legacy full-data-winsorized response)",
      "standardized_year",
      "standardized_remote_ratio",
      "employee_residence_code"
    ),
    package_versions = vapply(
      required_packages,
      function(package) as.character(packageVersion(package)),
      character(1)
    ),
    r_version = R.version.string
  ),
  split = list(
    method = "80/20 grouped stratified initial split",
    split_seed = split_seed,
    training_proportion = training_proportion,
    salary_stratum_breaks = salary_stratum_breaks,
    split_object = salary_split,
    training_source_row_ids = training_data$source_row_id,
    testing_source_row_ids = testing_data$source_row_id,
    training_duplicate_groups = unique(training_data$duplicate_group),
    testing_duplicate_groups = unique(testing_data$duplicate_group)
  ),
  preprocessing = list(
    prepared_recipe = prepared_recipe,
    rare_level_threshold = rare_level_threshold,
    baked_predictor_names = training_predictors,
    unseen_level_probe_passed = TRUE
  ),
  tuning = list(
    method = "10-fold grouped cross-validation on training data",
    cv_seed = cv_seed,
    fold_id = fold_id,
    folds = training_folds
  ),
  models = list(
    multiple_linear = list(
      fit = linear_model,
      smearing = linear_smearing
    ),
    ridge = list(
      fit = ridge_model,
      selected_lambda = selected_ridge_lambda,
      smearing = ridge_smearing
    ),
    lasso = list(
      fit = lasso_model,
      selected_lambda = selected_lasso_lambda,
      smearing = lasso_smearing,
      nonzero_predictors = lasso_nonzero_predictors
    )
  ),
  data = list(
    training = training_data,
    testing = testing_data
  ),
  heldout_predictions = heldout_predictions
)

saveRDS(step4_artifact, artifact_path)

summary_lines <- c(
  "Activity 1 Step 4 model training summary",
  "",
  sprintf("Input modeling rows: %d", nrow(model_data)),
  sprintf(
    "Split: %d training rows (%.1f%%), %d testing rows (%.1f%%)",
    nrow(training_data),
    100 * nrow(training_data) / nrow(model_data),
    nrow(testing_data),
    100 * nrow(testing_data) / nrow(model_data)
  ),
  sprintf("Split seed: %d; grouped CV seed: %d", split_seed, cv_seed),
  sprintf(
    "Identical-record groups: %d total; none cross the train/test split or CV folds",
    n_distinct(model_data$duplicate_group)
  ),
  "Response: uncapped observed salary_in_usd; modeled on its square-root scale",
  paste("Predictors:", paste(predictor_names, collapse = ", ")),
  paste(
    "Training-only preprocessing:",
    "2% rare-level pooling for original employment/location codes;",
    "unknown/novel handling; treatment dummies; zero-variance removal;",
    "work-year normalization"
  ),
  sprintf("Baked predictor columns: %d", length(training_predictors)),
  "Models fitted: multiple linear regression, ridge regression, LASSO regression",
  sprintf("Ridge selected lambda (1-SE rule): %.10g", selected_ridge_lambda),
  sprintf("LASSO selected lambda (1-SE rule): %.10g", selected_lasso_lambda),
  sprintf("LASSO nonzero predictors at selected lambda: %d", lasso_nonzero_predictors),
  paste(
    "Dollar inverse:",
    "nonnegative square plus empirical training/OOF smearing terms;",
    "naive squared predictions are also retained"
  ),
  "Held-out transformed and dollar predictions are stored in continuous_salary_models.rds.",
  "RMSE, MAE, R-squared, model ranking, and best-model selection are intentionally deferred to Step 6."
)

writeLines(summary_lines, summary_path, useBytes = TRUE)
print(summary_lines)
