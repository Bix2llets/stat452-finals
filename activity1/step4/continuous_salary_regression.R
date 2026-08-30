# ==============================================================================
# Step 4 - Continuous Salary Regression Modeling (STAT452 Core Suite)
# Multiple Linear Regression, Mallow's Cp Selection, Ridge, LASSO, Elastic Net
# Applied Statistics for Engineers and Scientists II (STAT452)
# ==============================================================================

library(MASS)
library(dplyr)
library(glmnet)
library(car)
library(lmtest)
library(leaps)

# Pin factor coding explicitly instead of inheriting whatever the session
# happens to have. Step 3 sets contr.sum globally for its Type III ANOVA; if
# that setting leaked in here, the model.matrix handed to lm/glmnet would be
# coded differently (changing the sign/scale of every categorical coefficient)
# and the saved models would no longer match the report interpretation.
options(contrasts = c("contr.treatment", "contr.poly"))

# 1. Load Data -----------------------------------------------------------------
input_path <- file.path("activity1", "dataset", "cleaned_data.rds")
output_path <- file.path("activity1", "step4", "continuous_salary_models.rds")
diagnostic_path <- file.path("activity1", "step4", "mlr_diagnostics.pdf")

salary_data <- readRDS(input_path)
set.seed(6767)

# Convert ordered factors to standard unordered factors for clean dummy coding
salary_data$experience_level <- factor(salary_data$experience_level, ordered = FALSE)
salary_data$company_size <- factor(salary_data$company_size, ordered = FALSE)
salary_data$leadership <- factor(salary_data$leadership, ordered = FALSE)

# 2. Train/Test Split (80/20) --------------------------------------------------
train_idx <- sample(seq_len(nrow(salary_data)), size = 0.8 * nrow(salary_data))
training_data <- salary_data[train_idx, ]
testing_data <- salary_data[-train_idx, ]

cat("==================== STEP 4: REGRESSION MODELING ====================\n")
cat("Training set size:", nrow(training_data), "observations\n")
cat("Testing set size :", nrow(testing_data), "observations\n\n")

# 3. Model 1: Multiple Linear Regression (Full Model) --------------------------
cat("--- 3.1 Fitting Full Multiple Linear Regression Model ---\n")
model_mlr_full <- lm(
  sqrt(salary_in_usd) ~ experience_level + employment_type + company_location +
    company_size + role + leadership + remote_ratio,
  data = training_data
)
print(summary(model_mlr_full))
cat("\n")

# ANOVA Table
cat("--- ANOVA Table for Full MLR ---\n")
print(anova(model_mlr_full))
cat("\n")

# Multicollinearity Check (Generalized VIF)
cat("--- Multicollinearity Diagnostics (Generalized VIF) ---\n")
gvif_res <- car::vif(model_mlr_full)
print(gvif_res)
cat("\n")

# 4. Model Selection: Mallow's Cp & Backward Stepwise AIC ---------------------
cat("--- 4.1 Best Subset Selection using Mallow's Cp Criterion ---\n")
subset_fit <- regsubsets(
  salary_in_usd ~ experience_level + employment_type + company_location +
    company_size + role + leadership + remote_ratio,
  data = training_data,
  nvmax = 14
)
subset_summary <- summary(subset_fit)

cp_table <- data.frame(
  Variables = 1:length(subset_summary$cp),
  Mallows_Cp = round(subset_summary$cp, 3),
  Adj_R2 = round(subset_summary$adjr2, 4),
  BIC = round(subset_summary$bic, 2),
  RSS = round(subset_summary$rss, 0)
)
print(cp_table)

best_cp_idx <- which.min(subset_summary$cp)
cat(
  "\nOptimal Model by Mallow's Cp has", best_cp_idx, "variables (Cp =",
  round(subset_summary$cp[best_cp_idx], 3), "approx p =", best_cp_idx + 1, ")\n"
)
cat("Selected Variables by Mallow's Cp:\n")
print(names(which(subset_summary$which[best_cp_idx, ])))
cat("\n")

# Stepwise AIC
cat("--- 4.2 Stepwise Model Selection (Backward AIC) ---\n")
model_mlr_step <- step(model_mlr_full, direction = "backward", trace = 0)
cat("Stepwise Optimal MLR Summary:\n")
print(summary(model_mlr_step))
cat("\n")

# 5. MLR Assumption Diagnostics ------------------------------------------------
cat("--- 5. MLR Assumption Diagnostics ---\n")
mlr_residuals <- residuals(model_mlr_step)

# 5.1 Normality (Shapiro-Wilk)
shapiro_test <- shapiro.test(mlr_residuals)
cat(
  "1. Normality (Shapiro-Wilk test): W =", round(shapiro_test$statistic, 5),
  ", p-value =", format.pval(shapiro_test$p.value), "\n"
)

# 5.2 Homoscedasticity (Breusch-Pagan)
bp_test <- lmtest::bptest(model_mlr_step)
cat(
  "2. Homoscedasticity (Breusch-Pagan test): BP =", round(bp_test$statistic, 4),
  ", p-value =", format.pval(bp_test$p.value), "\n"
)

# 5.3 Independence (Durbin-Watson)
dw_test <- car::durbinWatsonTest(model_mlr_step)
cat(
  "3. Independence (Durbin-Watson test): DW =", round(dw_test$dw, 4),
  ", p-value =", format.pval(dw_test$p), "\n\n"
)

# Save diagnostic plots
pdf(diagnostic_path, width = 9, height = 7)
par(mfrow = c(2, 2))
plot(model_mlr_step, main = "MLR Diagnostic Plots")
par(mfrow = c(1, 1))
dev.off()
cat("Diagnostic plots saved to:", diagnostic_path, "\n\n")

# 6. Regularized Regression Models (Ridge, LASSO, Elastic Net) -----------------
design_formula <- ~ experience_level + employment_type + company_location +
  company_size + role + leadership + remote_ratio + work_year

x_train <- model.matrix(design_formula, data = training_data)[, -1]
y_train <- training_data$salary_in_usd
x_test <- model.matrix(design_formula, data = testing_data)[, -1]
y_test <- testing_data$salary_in_usd

# 6.1 Ridge Regression (alpha = 0)
cat("--- 6.1 Ridge Regression (alpha = 0) ---\n")
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 10)
cat("Ridge optimal lambda (min):", round(cv_ridge$lambda.min, 4), "\n")
cat("Ridge optimal lambda (1se):", round(cv_ridge$lambda.1se, 4), "\n\n")

# 6.2 LASSO Regression (alpha = 1)
cat("--- 6.2 LASSO Regression (alpha = 1) ---\n")
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)
cat("LASSO optimal lambda (min):", round(cv_lasso$lambda.min, 4), "\n")
cat("LASSO optimal lambda (1se):", round(cv_lasso$lambda.1se, 4), "\n")
cat("LASSO coefficients at lambda.min (Feature selection):\n")
lasso_coef <- coef(cv_lasso, s = "lambda.min")
print(lasso_coef[lasso_coef[, 1] != 0, , drop = FALSE])
cat("\n")

# 6.3 Elastic Net Regression (alpha = 0.5)
cat("--- 6.3 Elastic Net Regression (alpha = 0.5) ---\n")
cv_elastic <- cv.glmnet(x_train, y_train, alpha = 0.5, nfolds = 10)
cat("Elastic Net optimal lambda (min):", round(cv_elastic$lambda.min, 4), "\n")
cat("Elastic Net optimal lambda (1se):", round(cv_elastic$lambda.1se, 4), "\n\n")

# 7. Generate Predictions on Test Set -----------------------------------------
pred_mlr_step <- predict(model_mlr_step, newdata = testing_data)
pred_ridge <- as.numeric(predict(cv_ridge, newx = x_test, s = "lambda.min"))
pred_lasso <- as.numeric(predict(cv_lasso, newx = x_test, s = "lambda.min"))
pred_elastic <- as.numeric(predict(cv_elastic, newx = x_test, s = "lambda.min"))

test_predictions <- data.frame(
  actual_salary_in_usd = y_test,
  mlr_stepwise = pred_mlr_step,
  ridge = pred_ridge,
  lasso = pred_lasso,
  elastic_net = pred_elastic
)

# 8. Save Artifact Bundle for Step 6 & Step 7 ----------------------------------
bundle <- list(
  training_data = training_data,
  testing_data = testing_data,
  design_formula = design_formula,
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  y_test = y_test,
  mallows_cp = list(
    table = cp_table,
    best_index = best_cp_idx,
    best_variables = names(which(subset_summary$which[best_cp_idx, ]))
  ),
  models = list(
    mlr_full = model_mlr_full,
    mlr_stepwise = model_mlr_step,
    ridge = cv_ridge,
    lasso = cv_lasso,
    elastic_net = cv_elastic
  ),
  diagnostics = list(
    shapiro_wilk = shapiro_test,
    breusch_pagan = bp_test,
    durbin_watson = dw_test,
    gvif = gvif_res
  ),
  test_predictions = test_predictions
)

saveRDS(bundle, output_path)
cat("All models, Mallow's Cp results, and predictions saved to:", output_path, "\n")
cat("==================== STEP 4 COMPLETE ====================\n")
