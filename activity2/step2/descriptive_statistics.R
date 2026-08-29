library(ggplot2)
library(dplyr)
library(tidyr)
library(tseries)

data <- readRDS("activity2/dataset/cleaned_data.rds") # Change this relative to the file location

str(data)

numeric_raw <- c(
  "age", "resting_bp_systolic", "resting_bp_diastolic", "cholesterol_total",
  "hdl", "ldl", "triglycerides", "fasting_blood_sugar", "hba1c", "bmi",
  "resting_heart_rate", "max_heart_rate_achieved", "st_depression",
  "alcohol_units_per_week", "exercise_minutes_per_week", "sleep_hours",
  "stress_score", "daily_steps", "diet_quality_score"
)
numeric_derived <- c(
  "pulse_pressure", "non_hdl_cholesterol", "heart_rate_difference"
)
numeric_all <- c(numeric_raw, numeric_derived)

categorical_all <- c(
  "sex", "chest_pain_type", "exercise_induced_angina", "family_history",
  "smoker_status", "wearable_owner", "bmi_category", "bp_category",
  "glycemic_status", "age_group", "meets_activity_guideline"
)

prevalence <- mean(data$has_heart_disease_num)
cat("overall prevalence:", round(prevalence, 4), "\n")

# 30.3% of observations has heart disease

# Calculate the sknewness of each column
skewness <- function(x) mean((x - mean(x))^3) / sd(x)^3
excess_kurtosis <- function(x) mean((x - mean(x))^4) / sd(x)^4 - 3

describe_numeric <- function(x) {
  quartiles <- quantile(x, c(0.25, 0.75))
  c(
    n = length(x), mean = mean(x), sd = sd(x), cv = sd(x) / mean(x),
    min = min(x), q1 = quartiles[[1]], median = median(x),
    q3 = quartiles[[2]], max = max(x), iqr = quartiles[[2]] - quartiles[[1]],
    skewness = skewness(x), excess_kurtosis = excess_kurtosis(x)
  )
}

numeric_summary <- t(sapply(data[numeric_all], describe_numeric)) |>
  as.data.frame() |>
  round(3)
numeric_summary <- cbind(variable = rownames(numeric_summary), numeric_summary)
rownames(numeric_summary) <- NULL

print(numeric_summary, row.names = FALSE)
data |>
  select(where(is.numeric), -"has_heart_disease_num") |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)),
    bins = 30, fill = "steelblue", color = "black", alpha = 0.6
  ) +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Distributions of All Numeric Variables",
    x = "Value",
    y = "Density"
  )


write.csv(numeric_summary, "numeric_summary_table.csv", row.names = FALSE)

# Check for normal distribution of variables, using jarque bera and chi square

gof_normal <- function(x, n_bins = 10) {
  cut_points <- qnorm(
    seq(0, 1, length.out = n_bins + 1),
    mean = mean(x), sd = sd(x)
  )
  cut_points[1] <- -Inf
  cut_points[n_bins + 1] <- Inf

  observed <- as.numeric(table(cut(x, breaks = cut_points)))
  expected <- rep(length(x) / n_bins, n_bins)
  statistic <- sum((observed - expected)^2 / expected)
  c(chisq = statistic, df = n_bins - 1 - 2)
}

normality_report <- data.frame(
  variable = numeric_all,
  skewness = round(sapply(data[numeric_all], skewness), 3),
  excess_kurtosis = round(sapply(data[numeric_all], excess_kurtosis), 3),
  gof_chisq = round(sapply(data[numeric_all], function(x) gof_normal(x)["chisq"]), 1),
  jarque_bera = round(sapply(
    data[numeric_all], function(x) unname(jarque.bera.test(x)$statistic)
  ), 1),
  row.names = NULL
)
normality_report <- normality_report[order(normality_report$gof_chisq), ]
print(normality_report, row.names = FALSE)
write.csv(normality_report, "normality_report.csv", row.names = FALSE)

significance_level <- 0.05
critical_value <- qchisq(significance_level, df = 7, lower.tail = FALSE)
cat("5% critical value on 7 df:", round(critical_value, 2), "\n")
cat("variables exceeding it:", sum(normality_report$gof_chisq > critical_value), "\n")

# Can't use the shapiro wilk test here, since the sample size is large
# Observation of the plot combined with the tests gives the that alcohol units per week, cholesterol hdl ratio and the st depression is right skewed. The triglycedires is also a bit left skewed, as well as the bmi and exerciseminutes per week. The others are fine. The max heart reate achieved is a bit left skwed


long_numeric <- data |>
  select(all_of(numeric_all)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value")

ggplot(long_numeric, aes(value)) +
  geom_histogram(bins = 40) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of each numerical variable", x = NULL, y = "Patients") +
  theme_classic()
ggsave("numeric_histograms.pdf", width = 14, height = 10)

ggplot(long_numeric, aes(sample = value)) +
  stat_qq(size = 0.3) +
  stat_qq_line(colour = "red") +
  facet_wrap(~variable, scales = "free") +
  labs(
    title = "Normal QQ plot of each numerical variable",
    x = "Theoretical quantiles", y = "Observed value"
  ) +
  theme_classic()
ggsave("numeric_qq.pdf", width = 14, height = 10)


skewed <- c("alcohol_units_per_week", "st_depression")

transformation_report <- do.call(rbind, lapply(skewed, function(v) {
  x <- data[[v]]
  data.frame(
    variable = v,
    raw = round(skewness(x), 3),
    sqrt = round(skewness(sqrt(x)), 3),
    cube_root = round(skewness(x^(1 / 3)), 3),
    log = if (any(x == 0)) NA else round(skewness(log(x)), 3),
    log1p = round(skewness(log1p(x)), 3)
  )
}))
print(transformation_report, row.names = FALSE)

data |>
  transmute(
    `alcohol_units_per_week` = alcohol_units_per_week,
    `log(1 + alcohol)` = (alcohol_units_per_week)^(1 / 3),
    `st_depression` = st_depression,
    `st_depression^(1/3)` = st_depression^(1 / 3)
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable, levels = c(
    "alcohol_units_per_week", "log(1 + alcohol)",
    "st_depression", "st_depression^(1/3)"
  ))) |>
  ggplot(aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)),
    bins = 30, fill = "steelblue", color = "black", alpha = 0.6
  ) +
  geom_density(color = "red", linewidth = 1) +
  facet_wrap(~variable, scales = "free", nrow = 3) +
  theme_minimal() +
  labs(
    title = "Density distribution of skewed variables and the transformed",
    x = "Value",
    y = "Density"
  )
ggsave("skewed_variables_transformed.pdf", width = 10, height = 9)

# 2. The categorical variables

categorical_summary <- do.call(rbind, lapply(categorical_all, function(v) {
  counts <- table(data[[v]])
  data.frame(
    variable = v,
    level = names(counts),
    n = as.integer(counts),
    percent = round(100 * as.numeric(counts) / nrow(data), 2)
  )
}))
print(categorical_summary, row.names = FALSE)
write.csv(categorical_summary, "categorical_summary_table.csv", row.names = FALSE)

categorical_summary |>
  ggplot(aes(x = level, y = percent)) +
  geom_col() +
  facet_wrap(~variable, scales = "free_x") +
  labs(title = "Composition of the sample", x = NULL, y = "Percent of patients") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("categorical_distributions.pdf", width = 12, height = 8)

# observation has those who age between 50 65 being the most. People are most lily to be at normal weight or being overweight. Most of chest pain type are asymptiomatic, without angina. Almost 80% of family history don't have heart diseases problem. balanced in sex, most never smokes and having non-diabetic status (normal or prediabetic)

# 3. Comparison of numerical variables in each group of diagnosis
data |>
  select(all_of(numeric_all), has_heart_disease) |>
  pivot_longer(-has_heart_disease, names_to = "variable", values_to = "value") |>
  ggplot(aes(x = has_heart_disease, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Each numerical variable by diagnosis",
    x = "Diagnosed with heart disease", y = NULL
  ) +
  theme_classic()
ggsave("numeric_by_outcome_boxplots.pdf", width = 14, height = 10)

# Generally, those with heart disease has more ldl and less hdl, which correspodning to having more bad factor and less good factor in their bloodstream.
# They also has higher hba1c and fasting blood sugar, which indicate that their blood sugar is higher, associated risks of prediabete and diabete.
# Their physical qualities is less than those without heart disease, identicated via higher bmi, less daily steps and exercises time a week.  They are also older than those without heart disease and have less quality diet.
# For the cardiovascular statistics, their heart rate, including the max heart rate and the working range of heart rate, is signficantly lower than those without disease. This could be the cause or the consequence of the heart disease. However, their resting blood pressure are also higher than others in at both systolic phase and diastolic phase. The st_depression is also higher

top_three <- c(
  "max_heart_rate_achieved", "st_depression", "age"
)

data |>
  select(all_of(top_three), has_heart_disease) |>
  pivot_longer(-has_heart_disease, names_to = "variable", values_to = "value") |>
  ggplot(aes(value, fill = has_heart_disease)) +
  geom_histogram(bins = 40, position = "identity", alpha = 0.55) +
  facet_wrap(~variable, scales = "free") +
  labs(
    title = "The three most separating variables, by diagnosis",
    subtitle = "Patient counts, so the groups appear at their true relative size",
    x = NULL, y = "Patients", fill = "Heart disease"
  ) +
  theme_classic()
ggsave("top_predictors_histograms.pdf", width = 10, height = 8)

overlap_table <- do.call(rbind, lapply(top_four, function(v) {
  x <- data[[v]]
  g <- data$has_heart_disease
  range_no <- quantile(x[g == "No"], c(0.05, 0.95))
  range_yes <- quantile(x[g == "Yes"], c(0.05, 0.95))
  data.frame(
    variable = v,
    diseased_inside_healthy_range =
      round(mean(x[g == "Yes"] >= range_no[1] & x[g == "Yes"] <= range_no[2]), 3),
    healthy_inside_diseased_range =
      round(mean(x[g == "No"] >= range_yes[1] & x[g == "No"] <= range_yes[2]), 3)
  )
}))
print(overlap_table, row.names = FALSE)

# There are no clear seprator -- or cutoff value -- that are statisitcally significant to classify if a person have heart disease or not


# =============================================================================
# 4. Each categorical variable against the diagnosis
# =============================================================================
# Evaluate the risks and odd of people having heart disease in each categorical variable.

rate_by_level <- do.call(rbind, lapply(categorical_all, function(v) {
  data |>
    group_by(level = .data[[v]]) |>
    summarise(
      n = n(),
      disease_rate = round(mean(has_heart_disease_num), 3), .groups = "drop"
    ) |>
    mutate(variable = v, level = as.character(level)) |>
    select(variable, level, n, disease_rate)
}))
print(as.data.frame(rate_by_level), row.names = FALSE)
write.csv(rate_by_level, "categorical_by_outcome_table.csv", row.names = FALSE)

association_strength <- do.call(rbind, lapply(categorical_all, function(v) {
  rates <- tapply(data$has_heart_disease_num, data[[v]], mean)
  p_low <- min(rates)
  p_high <- max(rates)
  data.frame(
    variable = v,
    safest_level = names(which.min(rates)), rate_safest = round(p_low, 3),
    riskiest_level = names(which.max(rates)), rate_riskiest = round(p_high, 3),
    risk_difference = round(p_high - p_low, 3),
    odds_ratio_change = round((p_high / (1 - p_high)) / (p_low / (1 - p_low)), 2)
  )
}))
association_strength <-
  association_strength[order(-association_strength$risk_difference), ]
print(association_strength, row.names = FALSE)
write.csv(association_strength, "categorical_association_strength.csv",
  row.names = FALSE
)

rate_by_level |>
  ggplot(aes(x = level, y = disease_rate)) +
  geom_col(fill = "grey35") +
  geom_hline(yintercept = prevalence, linetype = "dashed", colour = "red") +
  geom_text(aes(label = sprintf("%.1f%%\nn=%d", 100 * disease_rate, n)),
    vjust = -0.25, size = 2.5, lineheight = 0.9
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) + # label headroom
  facet_wrap(~variable, scales = "free_x") +
  labs(
    title = "Disease rate within each category",
    subtitle = paste0(
      "Percent of the patients at each level who are diagnosed. Dashed line: ",
      "the ", round(100 * prevalence, 1), "% overall prevalence."
    ),
    x = NULL, y = "Proportion diagnosed"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("disease_rate_by_category.pdf", width = 13, height = 9)

# Based on the comparision and the plot, Those having bad health status, being old, have insufficient exercise or smokes has are more prone to having heart disease. Male tends to have more heart disease than woman. The heart disease diagnostic seem to be consistent with common sense:


# 5. Relationships among the predictors

correlation_matrix <- cor(data[numeric_all])

variable_order <- c(
  "resting_bp_systolic", "resting_bp_diastolic", "pulse_pressure",
  "cholesterol_total", "ldl", "non_hdl_cholesterol", "hdl", "triglycerides",
  "fasting_blood_sugar", "hba1c",
  "age", "max_heart_rate_achieved", "heart_rate_difference",
  "resting_heart_rate", "st_depression",
  "bmi", "exercise_minutes_per_week", "daily_steps", "diet_quality_score",
  "alcohol_units_per_week", "sleep_hours", "stress_score"
)
numeric_all
variable_order
stopifnot(setequal(variable_order, numeric_all)) # nothing dropped or repeated

correlation_matrix |>
  as.table() |>
  as.data.frame() |>
  setNames(c("row_variable", "column_variable", "correlation")) |>
  mutate(
    row_variable = factor(row_variable, levels = variable_order),
    column_variable = factor(column_variable, levels = variable_order)
  ) |>
  ggplot(aes(row_variable, column_variable, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1)) +
  labs(
    title = "Correlation between the numerical variables",
    subtitle = "Ordered so that variables measuring the same thing sit together",
    x = NULL, y = NULL
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("correlation_heatmap.pdf", width = 10, height = 9)

# There are noticable blocks of non derived variables: fastingh blood sugar and hba1c, age and max heart rate as well as herat rate difference, daily step and execise minutes per woeek, resting bp systolic and resting bp diastolic
#

data |>
  select(age, max_heart_rate_achieved) |>
  pivot_longer(-age, names_to = "variable", values_to = "value") |>
  ggplot(aes(age, value)) +
  geom_point(alpha = 0.06, size = 0.5) +
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    aes(colour = "linear")
  ) +
  scale_colour_manual(values = c(linear = "red", quadratic = "blue")) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Age and max heart rate",
    x = "Age (years)", y = NULL, colour = NULL
  ) +
  theme_classic() +
  theme(legend.position = "bottom")
ggsave("age_and_max_rate.pdf")

data |>
  select(fasting_blood_sugar, hba1c) |>
  pivot_longer(-fasting_blood_sugar, names_to = "variable", values_to = "value") |>
  ggplot(aes(fasting_blood_sugar, value)) +
  geom_point(alpha = 0.06, size = 0.5) +
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    aes(colour = "linear")
  ) +
  scale_colour_manual(values = c(linear = "red", quadratic = "blue")) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Fasting Blood Sugar and HbA1c",
    x = "Fasting Blood Sugar", y = NULL, colour = NULL
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("fbs_and_hba1c.pdf")

data |>
  select(daily_steps, exercise_minutes_per_week) |>
  pivot_longer(-daily_steps, names_to = "variable", values_to = "value") |>
  ggplot(aes(daily_steps, value)) +
  geom_point(alpha = 0.06, size = 0.5) +
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    aes(colour = "linear")
  ) +
  scale_colour_manual(values = c(linear = "red", quadratic = "blue")) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Daily Steps and Exercise Minutes",
    x = "Daily Steps", y = NULL, colour = NULL
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("steps_and_exercise.pdf")
data |>
  select(resting_bp_diastolic, resting_bp_systolic) |>
  pivot_longer(-resting_bp_diastolic, names_to = "variable", values_to = "value") |>
  ggplot(aes(resting_bp_diastolic, value)) +
  geom_point(alpha = 0.06, size = 0.5) +
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    aes(colour = "linear")
  ) +
  scale_colour_manual(values = c(linear = "red", quadratic = "blue")) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "Resting Blood Pressure: Diastolic vs Systolic",
    x = "Resting BP Diastolic", y = NULL, colour = NULL
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("resting_bp_sys_dia.pdf")
