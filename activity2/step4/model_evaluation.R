# Activity 2 - Step 4d: How well do the two models do on patients they were
# never fitted on? The regression is scored by its error in bpm, the logistic
# model by what its probabilities get right once a cutoff is chosen.
#
# Input : heart_rate_predictions.rds, heart_disease_predictions.rds
# Output: heart_rate_model_comparison.csv, heart_disease_confusion_matrix.csv,
#         heart_disease_threshold_sweep.csv
#
# Run with activity2/step4 as the working directory, after
# heart_rate_linear_model.R and heart_disease_logistic_model.R.

heart_rate <- readRDS("heart_rate_predictions.rds")
heart_disease <- readRDS("heart_disease_predictions.rds")
cat(
  "scoring", nrow(heart_rate$predictions), "held-out patients;",
  "disease cases among them:", sum(heart_disease$actual), "\n"
)

# 4d.1 The regression fits -----------------------------------------------------
# Root mean squared error is in bpm and punishes large misses, mean absolute
# error does not, and R-squared on the test rows is 1 - SSE/SST measured
# against the test mean, so it says how much of the held-out variation is
# explained.
# One fit per penalty, all predicting the same patients, so the gaps are what
# the penalty costs or buys.
actual_range <- heart_rate$actual
score_regression <- function(model_name, predicted) {
  residual <- actual_range - predicted
  data.frame(
    model = model_name,
    test_rmse = round(sqrt(mean(residual^2)), 3),
    test_mae = round(mean(abs(residual)), 3),
    test_r_squared = round(
      1 - sum(residual^2) / sum((actual_range - mean(actual_range))^2), 4
    )
  )
}
regression_comparison <- do.call(rbind, lapply(
  names(heart_rate$predictions),
  function(model_name) score_regression(model_name, heart_rate$predictions[[model_name]])
))
regression_comparison$model[regression_comparison$model == "backward elimination"] <-
  paste0("backward elimination (", length(heart_rate$dropped), " dropped)")
regression_comparison$model[
  regression_comparison$model == "elastic net (cross-validated)"
] <- paste0("elastic net (cross-validated alpha = ", heart_rate$best_alpha, ")")
print(regression_comparison, row.names = FALSE)
write.csv(regression_comparison, "heart_rate_model_comparison.csv", row.names = FALSE)
# All five land within 0.03 bpm of each other (RMSE 15.962-15.993, R-squared
# 0.548-0.550). With 7,200 training rows against 19 columns least squares is not
# overfitting, so there is no variance for a penalty to buy back; what selection
# buys here is a smaller model to report, not a more accurate one.

# 4d.2 The logistic fit at the usual cutoff ------------------------------------
# A predicted probability becomes a prediction only after a cutoff. At 0.5 the
# confusion matrix gives accuracy, sensitivity (share of true cases found),
# specificity (share of healthy patients cleared), precision (share of flagged
# patients who have the disease) and F1, their harmonic mean.
# Score the held-out rows, so the rates are not inflated by the fit.
predicted_probability <- heart_disease$probability
actual <- heart_disease$actual

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

# 4d.3 The cutoff is a choice --------------------------------------------------
# Lowering the cutoff flags more patients: sensitivity rises and specificity
# falls, and no single number settles the trade - the cost of a missed case
# against a false alarm does.
# Sweep it so the trade is visible as numbers rather than as a curve to eyeball.
threshold_sweep <- do.call(rbind, lapply(seq(0.1, 0.9, by = 0.1), score_classification))
print(threshold_sweep, row.names = FALSE)
write.csv(threshold_sweep, "heart_disease_threshold_sweep.csv", row.names = FALSE)
# F1 peaks at 0.5 (0.8347) with 0.4 a hair behind (0.8335) at 84.0% sensitivity;
# screening, where a missed case costs more than a false alarm, would go lower
# still - 0.2 finds 91.1% of the cases for 10.0 points of specificity.
