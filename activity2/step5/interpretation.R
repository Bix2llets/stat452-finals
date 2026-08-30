# Activity 2 - Step 5: Interpretation of the models fitted in step 4.
#
# Reads the step 4 outputs and prints the numbers the interpretation report
# quotes, so every figure in the report comes from the data and not from
# typing. No models are refitted and nothing new is written to disk.
#
# Input : heart_rate_reduced_coefficients.csv, heart_rate_model_comparison.csv,
#         heart_disease_odds_ratios.csv, heart_disease_confusion_matrix.csv,
#         heart_disease_threshold_sweep.csv,
#         heart_disease_predictions.rds
# Output: printed tables only
#
# Run from the project root, after the three step 4 scripts.

library(pROC)

cat("--- 5.1 REDUCED REGRESSION (heart's working range) ---\n\n")
reduced_coefficients <- read.csv("heart_rate_reduced_coefficients.csv")
print(reduced_coefficients, row.names = FALSE)
cat("\nHeld-out performance of the five regression fits:\n")
regression_comparison <- read.csv("heart_rate_model_comparison.csv")
print(regression_comparison, row.names = FALSE)

cat("\n--- 5.2 LOGISTIC (heart disease odds) ---\n\n")
odds_ratio_table <- read.csv("heart_disease_odds_ratios.csv")
print(odds_ratio_table, row.names = FALSE)

heart_disease <- readRDS("heart_disease_predictions.rds")
cat("Confusion matrix at cutoff 0.5:\n")
confusion_matrix <- read.csv("heart_disease_confusion_matrix.csv", row.names = 1)
print(confusion_matrix)
actual <- heart_disease$actual
predicted <- as.integer(heart_disease$probability >= 0.5)
cat(
  "accuracy:", round(mean(predicted == actual), 4),
  " sensitivity:", round(sum(predicted == 1 & actual == 1) / sum(actual == 1), 4),
  " specificity:", round(sum(predicted == 0 & actual == 0) / sum(actual == 0), 4),
  " AUC:", round(auc(roc(actual, heart_disease$probability)), 4), "\n\n"
)
cat("Cutoff sweep:\n")
threshold_sweep <- read.csv("heart_disease_threshold_sweep.csv")
print(threshold_sweep, row.names = FALSE)
