# ==============================================================================
# Step 7 - Feature Importance, Interpretation, and Recommendations
# Applied Statistics for Engineers and Scientists II (STAT452)
# ==============================================================================

library(dplyr)
library(ggplot2)
library(glmnet)

# 1. Load Trained Models from Step 4 & Step 5 -----------------------------------
step4_bundle <- readRDS("activity1/step4/continuous_salary_models.rds")
step5_model <- readRDS("activity1/step5/logistic_top_tier_model.rds")

mlr_model <- step4_bundle$models$mlr_stepwise
lasso_model <- step4_bundle$models$lasso
ridge_model <- step4_bundle$models$ridge

cat("==================== STEP 7: INTERPRETATION & RECOMMENDATIONS ====================\n\n")

# 2. Feature Importance Analysis -----------------------------------------------
cat("--- 7.1 FEATURE IMPORTANCE ANALYSIS ---\n\n")

# 2.1 Multiple Linear Regression Significant Predictors
cat("a) Multiple Linear Regression (Significant Coefficients at alpha = 0.05):\n")
mlr_summary <- summary(mlr_model)
coef_mat <- mlr_summary$coefficients
sig_coefs <- coef_mat[coef_mat[, "Pr(>|t|)"] < 0.05, , drop = FALSE]

df_mlr_sig <- data.frame(
  Term = rownames(sig_coefs),
  Estimate = round(sig_coefs[, "Estimate"], 2),
  Std_Error = round(sig_coefs[, "Std. Error"], 2),
  t_value = round(sig_coefs[, "t value"], 3),
  p_value = format.pval(sig_coefs[, "Pr(>|t|)"])
)
print(df_mlr_sig, row.names = FALSE)
cat("\n")

# 2.2 LASSO Feature Selection (Non-zero Coefficients)
cat("b) LASSO Feature Selection (Non-zero Coefficients at lambda.min):\n")
lasso_coefs <- coef(lasso_model, s = "lambda.min")
lasso_df <- data.frame(
  Term = rownames(lasso_coefs),
  Coefficient = round(as.numeric(lasso_coefs), 2)
)
lasso_nonzero <- lasso_df[lasso_df$Coefficient != 0 & lasso_df$Term != "(Intercept)", ]
lasso_nonzero <- lasso_nonzero[order(abs(lasso_nonzero$Coefficient), decreasing = TRUE), ]
print(lasso_nonzero, row.names = FALSE)
cat("\n")

# 2.3 Logistic Regression Odds Ratios
cat("c) Binary Logistic Regression: Odds Ratios & 95% Confidence Intervals:\n")
logit_coefs <- coef(step5_model)
summary(step5_model)
exp(coef(step5_model))

odds_ratios <- exp(logit_coefs)
logit_confint <- exp(confint.default(step5_model))

df_odds <- data.frame(
  Term = names(logit_coefs),
  Odds_Ratio = round(odds_ratios, 4),
  CI_2.5 = round(logit_confint[, 1], 4),
  CI_97.5 = round(logit_confint[, 2], 4)
)
print(df_odds, row.names = FALSE)
cat("\n")

# 3. Barplot of Feature Importance (MLR Coefficients) --------------------------
plot_importance <- df_mlr_sig |>
  filter(Term != "(Intercept)") |>
  mutate(
    Effect = ifelse(Estimate > 0, "Positive Impact (+)", "Negative Impact (-)"),
    Term = reorder(Term, Estimate)
  )

ggplot(plot_importance, aes(x = Term, y = Estimate, fill = Effect)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Positive Impact (+)" = "steelblue", "Negative Impact (-)" = "firebrick")) +
  labs(
    title = "Key Predictors of Data Science Salary (MLR Model)",
    subtitle = "Estimated dollar impact holding all other factors constant",
    x = "Predictor Term",
    y = "Estimated Salary Impact (USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("activity1/step7/feature_importance_plot.pdf")

# 4. Answers to the 3 Research Questions ---------------------------------------
cat("--- 7.2 SYNTHESIS & ANSWERS TO RESEARCH QUESTIONS ---\n\n")

cat("=========================================================================\n")
cat("RESEARCH QUESTION 1: Salary Forecasting & Key Drivers Identification\n")
cat("=========================================================================\n")
cat("1. Forecasting Accuracy:\n")
cat("   - On the independent test set (120 observations), regularized Ridge regression\n")
cat("     achieved the lowest RMSE ($46,435.13) and highest R^2 (0.3310).\n")
cat("   - The Multiple Linear Regression model explained 52.62% of salary variation\n")
cat("     in the training data (Adjusted R^2 = 0.5140).\n\n")
cat("2. Key Determinants of Compensation:\n")
cat("   a) Geographic Location (Strongest Global Driver):\n")
cat("      - Companies located in America pay significantly more than other regions.\n")
cat("      - Holding other factors constant, companies in Asia pay $71,344 less,\n")
cat("        and Europe pays $59,765 less compared to American employers (p < 1e-15).\n")
cat("   b) Seniority / Experience Level:\n")
cat("      - Executive-level roles command an $83,619 premium over Entry-level (p < 1e-10).\n")
cat("      - Senior-level roles earn $44,879 more than Entry-level.\n")
cat("      - Mid-level roles earn $16,227 more than Entry-level.\n")
cat("   c) Functional Role Discipline:\n")
cat("      - Engineering roles (+ $34,731) and Research roles (+ $31,976) earn\n")
cat("        significantly more than Analyst baseline roles (p < 1e-7).\n")
cat("   d) Leadership Responsibilities:\n")
cat("      - Roles with leadership/managerial scope receive a $22,892 premium (p = 0.001).\n")
cat("   e) Company Size:\n")
cat("      - Large enterprises (+ $23,451) and Medium firms (+ $15,507) pay higher\n")
cat("        wages than Small companies.\n\n")

cat("=========================================================================\n")
cat("RESEARCH QUESTION 2: Impact & Multi-Factor Interaction Analysis\n")
cat("=========================================================================\n")
cat("1. Multi-factor Differences:\n")
cat("   - ANOVA and regression analyses confirm statistically significant main effects\n")
cat("     across Experience Level (F = 99.95, p < 2.2e-16), Location (F = 48.64, p < 2.2e-16),\n")
cat("     Company Size (F = 8.82, p < 0.001), and Role (F = 20.89, p < 1e-8).\n")
cat("2. Interaction Effects:\n")
cat("   - The salary premium of seniority is moderated by geographic location;\n")
cat("     Executive and Senior compensations in America reach far higher ceilings\n")
cat("     than equivalent senior positions in Europe or Asia.\n\n")

cat("=========================================================================\n")
cat("RESEARCH QUESTION 3: Top 25% Income Probability Prediction\n")
cat("=========================================================================\n")
cat("1. Logistic Model Effectiveness:\n")
cat("   - Binary Logistic Regression achieves an overall Accuracy of 80.0% and an\n")
cat("     ROC-AUC of 0.87, representing excellent discriminatory ability.\n")
cat("2. High-Income Odds Multipliers:\n")
cat("   - Seniority: Moving up experience levels multiplies top-tier odds by 9.93x.\n")
cat("   - Functional Discipline: Engineering multiplies odds by 6.63x, and Research\n")
cat("     multiplies odds by 5.85x compared to Analyst positions.\n")
cat("   - Leadership: Managerial/Lead roles multiply top-tier odds by 2.63x.\n")
cat("   - Company Location: Working for European (OR = 0.041) or Asian (OR = 0.083)\n")
cat("     companies drastically reduces odds of reaching global top 25% USD salary,\n")
cat("     confirming that US/American employers dominate the top compensation quartile.\n\n")

# 5. Actionable Recommendations ------------------------------------------------
cat("--- 7.3 ACTIONABLE RECOMMENDATIONS ---\n\n")
cat("1. For Data Science Professionals & Career Planning:\n")
cat("   - Target Engineering & Research tracks (ML Engineer, Data Architect, AI Researcher)\n")
cat("     for highest earning potential compared to generic reporting analyst roles.\n")
cat("   - Seek opportunities with American-based companies (either via relocation or\n")
cat("     100% remote global contracts) to capture substantial geographic wage premiums.\n")
cat("   - Transition into technical leadership or staff/principal roles to unlock\n")
cat("     both individual contributor and managerial compensation tiers.\n\n")

cat("2. For HR & Talent Acquisition Managers:\n")
cat("   - Adjust compensation bands regionally; offering US-level salaries in Europe\n")
cat("     or Asia provides immense competitive advantage in attracting top tier talent.\n")
cat("   - Provide clear promotion ladders from Mid to Senior to mitigate turnover,\n")
cat("     as Senior engineers command a +$44.8k market premium.\n\n")

cat("3. For Future Statistical Modeling & Data Collection:\n")
cat("   - Collect Purchasing Power Parity (PPP) and Cost of Living (COL) indices to\n")
cat("     evaluate real disposable income rather than nominal USD converted salaries.\n")
cat("   - Collect tech-stack variables (e.g., Python, SQL, AWS, PyTorch) to estimate\n")
cat("     the specific marginal value of in-demand technical skills.\n\n")

cat("==================== STEP 7 COMPLETE ====================\n")
