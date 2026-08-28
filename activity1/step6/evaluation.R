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
false_positive <- classification_table["TRUE", "FALSE"]
false_negative <- classification_table["FALSE", "TRUE"]


# Calclulating the metrics
(accuracy <- (true_positive + true_negative) / sum(classification_table)) * 100
# 81%
(precision <- true_positive / (false_positive + true_positive)) * 100
# 69.23%
(recall <- true_positive / (true_positive + false_negative)) * 100
# 62.06%
(f1 <- 2 * (precision * recall) / (precision + recall)) * 100
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

# The area under the curve, as extracted, is 0.87. It means that the classification model correctly predict 87% if an employee have their salary belong to the top 25%
# on a random employee with the needed information. Since the predition of top tier salary is not critical i.e. they only serve as statistical tools and not contributing
# in making critical decision, this value is accepted as excellent, which mean the model's predictability is good for the data.
# Source for it:  https://www.statology.org/what-is-a-good-auc-score/
