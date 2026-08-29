# Activity 2 - Step 3: Does chest pain type, exercise-induced angina and
# smoker status affect the heart's working range (heart_rate_difference =
# peak minus resting), and do the factors interact?
#
# Input : ../dataset/cleaned_data.rds
# Output: heart_rate_anova_table.csv, heart_rate_interaction.pdf,
#         heart_rate_residuals.pdf
#
# Run with activity2/step3 as the working directory.

library(dplyr)
library(ggplot2)

data <- readRDS("../dataset/cleaned_data.rds") # Adjust this to fit the rds file location

# 3.1 Three-factor ANOVA on heart_rate_difference ----------------------------
# Tests each factor's main effect and every interaction in one model.
# Check cell counts first so the F tests are on a filled-enough design.
cell_counts <- table(data$chest_pain_type, data$exercise_induced_angina, data$smoker_status)
cat("cells:", length(cell_counts), " smallest cell:", min(cell_counts), "\n")

heart_rate_model <- aov(
  heart_rate_difference ~ chest_pain_type * exercise_induced_angina * smoker_status,
  data = data
)
anova_table <- as.data.frame(summary(heart_rate_model)[[1]])
names(anova_table) <- c("df", "sum_sq", "mean_sq", "f_value", "p_value")

significance_level <- 0.05
residual_df <- anova_table["Residuals", "df"]
anova_table$critical_f <- qf(
  significance_level, anova_table$df, residual_df,
  lower.tail = FALSE
)
print(round(anova_table, 4))
write.csv(anova_table, "heart_rate_anova_table.csv")

# Smallest cell has 63 patients (24 cells total) - filled enough to trust.

# 3.2 Interaction plot --------------------------------------------------------
# Faceted mean profile: parallel vs. crossing lines show interaction directly.
interaction_summary <- data |>
  group_by(chest_pain_type, exercise_induced_angina, smoker_status) |>
  summarise(mean_heart_rate_difference = mean(heart_rate_difference), .groups = "drop")

ggplot(
  interaction_summary,
  aes(
    x = chest_pain_type, y = mean_heart_rate_difference,
    color = exercise_induced_angina, group = exercise_induced_angina
  )
) +
  geom_line() +
  geom_point() +
  facet_wrap(~smoker_status) +
  labs(
    title = "Mean heart rate working range by chest pain type, angina and smoker status",
    x = "Chest pain type", y = "Mean heart_rate_difference (bpm)",
    color = "Exercise-induced angina"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("heart_rate_interaction.pdf", width = 10, height = 5)

# 3.3 Residual checks ---------------------------------------------------------
# F tests need constant variance and roughly normal residuals - verify both.
pdf("heart_rate_residuals.pdf", width = 8, height = 4)
par(mfrow = c(1, 2))
plot(heart_rate_model, which = 1)
plot(heart_rate_model, which = 2)
dev.off()
