library(tidyverse)
library(pROC)
top_tier_classification <- readRDS("activity1/step5/logistic_top_tier_model.rds")
top_tier_classification
# continuous_salary_model <- readRDS("activity1/step4/continuous_salary_models.rds")
# continuous_salary_model

data <- readRDS("activity1/dataset/cleaned_data.rds")
head(data)
str(data)
# Evaluate the prediction on the given mode
prediction_result <- predict(top_tier_classification, newdata = data, type = "response")
classification_result <- prediction_result >= 0.5
# Extract the ground truth
ground_truth <- model.frame(top_tier_classification)$is_top_tier

classification_table <- table(ground_truth, classification_result)
str(classification_table)
true_positive <- classification_table["TRUE", "TRUE"]
true_negative <- classification_table["FALSE", "FALSE"]
false_positive <- classification_table["FALSE", "TRUE"]
false_negative <- classification_table["TRUE", "FALSE"]


# Calclulating the metrics
(accuracy <- (true_positive + true_negative) / sum(classification_table) * 100)
# 81%
(precision <- true_positive / (false_positive + true_positive) * 100)
# 62.06%
(recall <- true_positive / (true_positive + false_negative) * 100)
# 69.23%
(f1 <- 2 * (precision * recall) / (precision + recall))
# 65.45%

roc_object <- roc(ground_truth, prediction_result)
roc_data <- data.frame(
  Sensitivity = roc_object$sensitivities,
  # Convert Specificity to False Positive Rate
  FalsePositiveRate = 1 - roc_object$specificities
)

# 2. Plot with standard ggplot2 syntax
ggplot(roc_data, aes(x = FalsePositiveRate, y = Sensitivity)) +
  geom_line(color = "steelblue", linewidth = 1) +
  # Add diagonal random-guess line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgrey") +
  theme_minimal() +
  labs(
    title = "ROC Curve",
    subtitle = paste("AUC =", round(roc_object$auc, 3)),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  )

# The area under the curve, as extracted, is 0.87. In 87% of the cases it correctly rank those with that has top tier salary higher than those without. Since the predition of top tier salary is not critical i.e. they only serve as statistical tools and not contributing
# in making critical decision, this value is accepted as excellent, which mean the model's predictability is good for the data.
# Source for it:  https://www.statology.org/what-is-a-good-auc-score/


# ==============================================================================
# CONTINUOUS SALARY REGRESSION MODEL EVALUATION (STEP 6)
# ==============================================================================

cat("\n==================== 6.2 CONTINUOUS REGRESSION MODEL EVALUATION ====================\n\n")

# 1. Load trained continuous regression models and test predictions from Step 4
continuous_bundle <- readRDS("activity1/step4/continuous_salary_models.rds")
test_preds <- continuous_bundle$test_predictions
actual_y <- test_preds$actual_salary_in_usd

cat("Number of test set observations:", nrow(test_preds), "\n\n")

# 2. Evaluation Metric Function (RMSE, MAE, R-squared)
eval_regression <- function(actual, predicted, model_name) {
  rmse_val <- sqrt(mean((predicted - actual)^2))
  mae_val <- mean(abs(predicted - actual))
  ss_tot <- sum((actual - mean(actual))^2)
  ss_res <- sum((predicted - actual)^2)
  r2_val <- 1 - (ss_res / ss_tot)
  
  data.frame(
    Model = model_name,
    RMSE = round(rmse_val, 2),
    MAE = round(mae_val, 2),
    R_squared = round(r2_val, 4)
  )
}

# 3. Compute metrics for all regression models (MLR, Polynomial, Ridge, LASSO, Elastic Net)
eval_mlr <- eval_regression(actual_y, test_preds$mlr_stepwise, "1. MLR (Stepwise AIC / Mallow's Cp)")
eval_poly <- eval_regression(actual_y, test_preds$polynomial, "2. Polynomial MLR (Degree 2)")
eval_ridge <- eval_regression(actual_y, test_preds$ridge, "3. Ridge Regression (alpha = 0)")
eval_lasso <- eval_regression(actual_y, test_preds$lasso, "4. LASSO Regression (alpha = 1)")
eval_elastic_05 <- eval_regression(actual_y, test_preds$elastic_net_05, "5. Elastic Net (alpha = 0.5)")
eval_elastic_opt <- eval_regression(
  actual_y,
  test_preds$elastic_net_optimal,
  paste0("6. Optimal Elastic Net (alpha = ", continuous_bundle$best_alpha, ")")
)

regression_comparison_table <- rbind(
  eval_mlr, eval_poly, eval_ridge, eval_lasso, eval_elastic_05, eval_elastic_opt
)

cat("--- Continuous Regression Model Comparison on Test Set ---\n")
print(regression_comparison_table)
cat("\n")

# 4. Best Model Selection for Continuous Task
best_model_row <- regression_comparison_table[which.min(regression_comparison_table$RMSE), ]
cat("Best Continuous Regression Model (by lowest Test RMSE):\n")
cat("  Model    :", best_model_row$Model, "\n")
cat("  RMSE ($) :", best_model_row$RMSE, "\n")
cat("  MAE ($)  :", best_model_row$MAE, "\n")
cat("  R-squared:", best_model_row$R_squared, "\n\n")

# 5. Scatter Plot: Predicted vs Actual for primary models
p_mlr <- ggplot(data.frame(Actual = actual_y, Predicted = test_preds$mlr_stepwise), aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "firebrick") +
  labs(title = "MLR (Stepwise / Cp)", x = "Actual Salary (USD)", y = "Predicted Salary (USD)") +
  theme_minimal()

p_poly <- ggplot(data.frame(Actual = actual_y, Predicted = test_preds$polynomial), aes(x = Actual, y = Predicted)) +
  geom_point(color = "brown", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "firebrick") +
  labs(title = "Polynomial Regression", x = "Actual Salary (USD)", y = "Predicted Salary (USD)") +
  theme_minimal()

p_ridge <- ggplot(data.frame(Actual = actual_y, Predicted = test_preds$ridge), aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "firebrick") +
  labs(title = "Ridge (Best Model)", x = "Actual Salary (USD)", y = "Predicted Salary (USD)") +
  theme_minimal()

p_lasso <- ggplot(data.frame(Actual = actual_y, Predicted = test_preds$lasso), aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "firebrick") +
  labs(title = "LASSO Regression", x = "Actual Salary (USD)", y = "Predicted Salary (USD)") +
  theme_minimal()

# Save regression evaluation plots to PDF
pdf("activity1/step6/regression_evaluation_plots.pdf", width = 10, height = 8)
gridExtra::grid.arrange(p_mlr, p_poly, p_ridge, p_lasso, ncol = 2)
dev.off()

cat("Regression comparison plots saved to: activity1/step6/regression_evaluation_plots.pdf\n")
cat("==================== STEP 6 EVALUATION COMPLETE ====================\n")
