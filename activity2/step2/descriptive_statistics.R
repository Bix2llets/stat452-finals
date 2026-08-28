# =============================================================================
# Activity 2 - Step 2: Descriptive statistics
# =============================================================================
# Input : ../dataset/cleaned_data.rds  (produced by ../step1/data_processing.R)
# Output: the tables and figures listed in task.md
#
# This file only describes the data. It reports centre, spread, shape, and the
# strength of the association between each variable and the diagnosis, using
# effect sizes rather than p-values. Formal hypothesis tests and the models
# belong to Step 3 - keeping the p-values out of here also keeps us from
# reading significance into a comparison that was chosen after seeing the plot.
#
# Run with activity2/step2 as the working directory.

library(ggplot2)
library(dplyr)
library(tidyr)
library(tseries) # jarque.bera.test

data <- readRDS("../dataset/cleaned_data.rds")

str(data)
dim(data)

# The variable groups used throughout the file. The derived columns from Step 1
# are described together with the raw ones, because the report quotes both.
numeric_raw <- c(
  "age", "resting_bp_systolic", "resting_bp_diastolic", "cholesterol_total",
  "hdl", "ldl", "triglycerides", "fasting_blood_sugar", "hba1c", "bmi",
  "resting_heart_rate", "max_heart_rate_achieved", "st_depression",
  "alcohol_units_per_week", "exercise_minutes_per_week", "sleep_hours",
  "stress_score", "daily_steps", "diet_quality_score"
)
numeric_derived <- c(
  "pulse_pressure", "non_hdl_cholesterol", "cholesterol_hdl_ratio",
  "heart_rate_reserve", "percent_predicted_max_hr"
)
numeric_all <- c(numeric_raw, numeric_derived)

categorical_raw <- c(
  "sex", "chest_pain_type", "exercise_induced_angina", "family_history",
  "smoker_status", "wearable_owner"
)
categorical_derived <- c(
  "bmi_category", "bp_category", "glycemic_status", "age_group",
  "meets_activity_guideline"
)
categorical_all <- c(categorical_raw, categorical_derived)


# =============================================================================
# 1. The response
# =============================================================================

table(data$has_heart_disease)
round(prop.table(table(data$has_heart_disease)), 4)

# 2,727 of 9,000 patients are cases: a prevalence of 30.3%. Every "disease
# rate" quoted below is compared against this 30.3% baseline - a subgroup is
# only interesting when its rate is far from it.
prevalence <- mean(data$has_heart_disease_num)
cat("overall prevalence:", round(prevalence, 4), "\n")


# =============================================================================
# 2. Univariate description of the numerical variables
# =============================================================================
# Sample skewness and kurtosis, written out rather than taken from a package so
# the file only needs what Step 1 already used. Kurtosis is reported in excess
# form, i.e. 0 for a normal distribution.

skewness <- function(x) mean((x - mean(x))^3) / sd(x)^3
excess_kurtosis <- function(x) mean((x - mean(x))^4) / sd(x)^4 - 3

describe_numeric <- function(x) {
  quartiles <- quantile(x, c(0.25, 0.75))
  c(
    n = length(x), mean = mean(x), sd = sd(x),
    cv = sd(x) / mean(x), # coefficient of variation, for comparing spreads
    min = min(x), q1 = quartiles[[1]], median = median(x),
    q3 = quartiles[[2]], max = max(x), iqr = quartiles[[2]] - quartiles[[1]],
    skewness = skewness(x), excess_kurtosis = excess_kurtosis(x)
  )
}

numeric_summary <- t(sapply(data[numeric_all], describe_numeric)) |>
  as.data.frame() |>
  round(3)
numeric_summary$variable <- rownames(numeric_summary)
numeric_summary <- numeric_summary[, c("variable", setdiff(names(numeric_summary), "variable"))]
rownames(numeric_summary) <- NULL

print(numeric_summary, row.names = FALSE)
write.csv(numeric_summary, "numeric_summary_table.csv", row.names = FALSE)

# What the table says.
#
# Centre. The clinical measurements sit where a middle-aged general population
# sits: median age 54, blood pressure 128/81 mmHg, BMI 25.3, total cholesterol
# 189 mg/dL, HbA1c 5.8%. So this is not a sample of already-sick patients; it
# is a screening population, which is what makes the 30.3% prevalence
# meaningful.
#
# Spread. The coefficient of variation separates two kinds of variable. The
# regulated ones vary little, because the body holds them in a narrow band:
# percent_predicted_max_hr has cv = 0.09, resting heart rate and systolic
# pressure 0.11, hba1c 0.12, sleep_hours 0.16. The behavioural ones vary far
# more: st_depression 0.95, alcohol_units_per_week 0.89, exercise_minutes 0.46,
# daily_steps 0.35. A model therefore has much more variation to work with in
# the lifestyle variables than in the vital signs.
#
# Shape. Most of the 24 variables are close to symmetric - 21 of them have
# |skewness| < 0.25 and |excess kurtosis| < 0.4. Exactly three are not:
#   st_depression           skewness +2.04, excess kurtosis +5.60
#                           - piled at 0 (193 patients) with a long right tail
#   alcohol_units_per_week  skewness +2.03, excess kurtosis +6.28
#                           - most patients drink little, a few drink heavily
#   cholesterol_hdl_ratio   skewness +1.41, excess kurtosis +4.25
#                           - a ratio, so it is skewed by construction: a low
#                             HDL in the denominator produces a very large value
# These three are the ones a linear model would need transformed, and the ones
# whose mean is a poor summary - for them the report quotes the median.
# Note that triglycerides is NOT among them (skewness 0.13): the usual lipid
# right tail is absent here, because Step 1 found the column truncated at
# 35 mg/dL and 390 mg/dL.

# 2.1 Are the distributions normal? ------------------------------------------
# Two warnings before reading the test results.
#
# First, shapiro.test refuses samples above 5,000, so it can only be run on a
# subsample here. Second, and more important: with n = 9,000 a normality test
# rejects a deviation far too small to matter for anything we do. The tests are
# reported for completeness, but the shape conclusions above come from the
# skewness / kurtosis figures and the QQ plots, which measure how big the
# departure is rather than whether it exists.

set.seed(6767)
shapiro_subsample <- sample(nrow(data), 5000)

normality_report <- data.frame(
  variable = numeric_all,
  skewness = round(sapply(data[numeric_all], skewness), 3),
  excess_kurtosis = round(sapply(data[numeric_all], excess_kurtosis), 3),
  jarque_bera_p = sapply(numeric_all, function(v) {
    signif(jarque.bera.test(data[[v]])$p.value, 3)
  }),
  shapiro_W_n5000 = sapply(numeric_all, function(v) {
    round(shapiro.test(data[[v]][shapiro_subsample])$statistic, 4)
  }),
  row.names = NULL
)
print(normality_report, row.names = FALSE)
write.csv(normality_report, "normality_report.csv", row.names = FALSE)

# Jarque-Bera rejects normality for 14 of the 24 columns at the 5% level and
# does not reject for the other 10 (cholesterol_total p = 0.85, hdl 0.60,
# hba1c 0.18, sleep_hours 0.22, stress_score 0.85, and so on), which is
# unusually normal-looking behaviour for clinical measurements.
#
# The Shapiro W statistic is the more useful number, because it measures how
# far from normal rather than whether. It is at or above 0.994 for 21 of the 24
# variables - a departure too small to affect anything - and drops only for the
# three skewed ones: st_depression 0.811, alcohol_units_per_week 0.816,
# cholesterol_hdl_ratio 0.915. Those are exactly the three flagged by the
# skewness column, so the two approaches agree.

# 2.2 Histograms and QQ plots ------------------------------------------------

long_numeric <- data |>
  select(all_of(numeric_all)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value")

pdf("numeric_histograms.pdf", width = 12, height = 8)
ggplot(long_numeric, aes(value)) +
  geom_histogram(bins = 40) +
  facet_wrap(~variable, scales = "free") +
  labs(
    title = "Distribution of each numerical variable",
    x = NULL, y = "Number of patients"
  ) +
  theme_classic()
dev.off()

pdf("numeric_qq.pdf", width = 12, height = 8)
ggplot(long_numeric, aes(sample = value)) +
  stat_qq(size = 0.3) +
  stat_qq_line(colour = "red") +
  facet_wrap(~variable, scales = "free") +
  labs(
    title = "Normal QQ plot of each numerical variable",
    x = "Theoretical quantiles", y = "Observed value"
  ) +
  theme_classic()
dev.off()

# The QQ plots make the Step 1 findings visible: max_heart_rate_achieved,
# triglycerides, bmi, hba1c and ldl run flat at one end, which is the recording
# limit documented in Step 1, and st_depression has a flat step at zero.

# 2.3 The three skewed variables, and what a transformation does -------------

pdf("skewed_variables_transformed.pdf", width = 10, height = 6)
data |>
  transmute(
    `alcohol_units_per_week` = alcohol_units_per_week,
    `log(1 + alcohol)` = log1p(alcohol_units_per_week),
    `cholesterol_hdl_ratio` = cholesterol_hdl_ratio,
    `log(cholesterol_hdl_ratio)` = log(cholesterol_hdl_ratio),
    `st_depression` = st_depression,
    `sqrt(st_depression)` = sqrt(st_depression)
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable, levels = c(
    "alcohol_units_per_week", "log(1 + alcohol)",
    "cholesterol_hdl_ratio", "log(cholesterol_hdl_ratio)",
    "st_depression", "sqrt(st_depression)"
  ))) |>
  ggplot(aes(value)) +
  geom_histogram(bins = 40) +
  facet_wrap(~variable, scales = "free", nrow = 3) +
  labs(
    title = "The three right-skewed variables, before and after a transformation",
    x = NULL, y = "Number of patients"
  ) +
  theme_classic()
dev.off()

cat("skewness alcohol raw / log1p:", round(skewness(data$alcohol_units_per_week), 3),
  "/", round(skewness(log1p(data$alcohol_units_per_week)), 3), "\n")
cat("skewness chol/HDL ratio raw / log:", round(skewness(data$cholesterol_hdl_ratio), 3),
  "/", round(skewness(log(data$cholesterol_hdl_ratio)), 3), "\n")
cat("skewness st_depression raw / sqrt:", round(skewness(data$st_depression), 3),
  "/", round(skewness(sqrt(data$st_depression)), 3), "\n")

# The log works on the two variables whose skew comes from a long tail: alcohol
# goes from +2.03 to +0.02 and the cholesterol / HDL ratio from +1.41 to +0.32,
# both effectively symmetric afterwards.
#
# The square root only halves the st_depression skew, from +2.04 to +0.65, and
# cannot do better - the problem there is not a tail but the 193 patients
# sitting exactly at zero, and no transformation moves a point mass. If Step 3
# needs st_depression to behave, the honest option is to split it into "any ST
# depression yes/no" plus the amount among the patients who have some.
#
# For contrast, a log applied to triglycerides would make things worse, not
# better (skewness +0.13 -> -1.09): the column is already symmetric, and the
# transformation would manufacture a left tail. Transformations are applied to
# the variables that need them, not by habit to everything that is a lipid.


# =============================================================================
# 3. Univariate description of the categorical variables
# =============================================================================

for (v in categorical_all) {
  cat("\n---", v, "---\n")
  print(table(data[[v]]))
  print(round(prop.table(table(data[[v]])), 4))
}

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

# The sample is close to balanced on sex (47.4% female). The clinical
# categories are not, and that is the point: 47.4% of the patients are
# asymptomatic and only 11.3% report typical angina, which is what a screening
# population looks like rather than a cardiology ward.
#
# The derived categories from Step 1 give the same picture from the clinical
# side: 61.6% of patients are already in a hypertension stage (33.5% stage 1,
# 28.1% stage 2), 58.1% are prediabetic or diabetic by HbA1c, 53.1% are
# overweight or obese, and 56.6% do not meet the 150-minute activity guideline.
# The cell counts are large everywhere - the smallest level in the entire table
# is Underweight with 549 patients - so no category has to be collapsed before
# Step 3, and every cell of a two-way table will still be well filled.

pdf("categorical_distributions.pdf", width = 11, height = 7)
categorical_summary |>
  ggplot(aes(x = level, y = percent)) +
  geom_col() +
  facet_wrap(~variable, scales = "free_x") +
  labs(
    title = "Composition of the sample on each categorical variable",
    x = NULL, y = "Percent of patients"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
dev.off()


# =============================================================================
# 4. Each numerical variable against the diagnosis
# =============================================================================
# The comparison is reported as a standardised mean difference (Cohen's d):
# the gap between the two group means measured in pooled standard deviations.
# It is used instead of a p-value because at n = 9,000 nearly every difference
# is "significant"; d says which ones are large enough to matter. The usual
# reading is 0.2 small, 0.5 medium, 0.8 large.

cohens_d <- function(x, group) {
  x1 <- x[group == "Yes"]
  x0 <- x[group == "No"]
  n1 <- length(x1)
  n0 <- length(x0)
  pooled_sd <- sqrt(((n1 - 1) * var(x1) + (n0 - 1) * var(x0)) / (n1 + n0 - 2))
  (mean(x1) - mean(x0)) / pooled_sd
}

outcome_comparison <- do.call(rbind, lapply(numeric_all, function(v) {
  x <- data[[v]]
  g <- data$has_heart_disease
  data.frame(
    variable = v,
    mean_no_disease = round(mean(x[g == "No"]), 2),
    sd_no_disease = round(sd(x[g == "No"]), 2),
    mean_disease = round(mean(x[g == "Yes"]), 2),
    sd_disease = round(sd(x[g == "Yes"]), 2),
    difference = round(mean(x[g == "Yes"]) - mean(x[g == "No"]), 2),
    cohens_d = round(cohens_d(x, g), 3)
  )
}))
outcome_comparison <- outcome_comparison[order(-abs(outcome_comparison$cohens_d)), ]
print(outcome_comparison, row.names = FALSE)
write.csv(outcome_comparison, "numeric_by_outcome_table.csv", row.names = FALSE)

# Ranked by |d|, the separation between diagnosed and undiagnosed patients is:
#
#   very large  percent_predicted_max_hr (-1.58), max_heart_rate_achieved
#               (-1.55), heart_rate_reserve (-1.54). All three say the same
#               thing: patients with heart disease cannot raise their heart
#               rate under exertion. Peak heart rate averages 146 bpm in the
#               diagnosed group against 173 bpm in the rest - a 27 bpm gap,
#               larger than one and a half standard deviations, and by far the
#               strongest signal in the dataset.
#   large       st_depression (+0.83), cholesterol_hdl_ratio (+0.82)
#   medium      age (+0.66), non_hdl_cholesterol (+0.62), ldl (+0.60),
#               hdl (-0.57), resting_bp_systolic (+0.56),
#               exercise_minutes_per_week (-0.56), hba1c (+0.50)
#   small       bmi (+0.48), fasting_blood_sugar (+0.45),
#               resting_bp_diastolic (+0.43), resting_heart_rate (+0.37),
#               cholesterol_total (+0.37), diet_quality_score (-0.34),
#               daily_steps (-0.34), pulse_pressure (+0.33),
#               triglycerides (+0.24), stress_score (+0.21)
#   negligible  sleep_hours (-0.11), alcohol_units_per_week (+0.09)
#
# Every sign points the way clinical knowledge says it should, which is a check
# that the data behaves sensibly. Two results are worth comment because they
# cut against what a reader expects:
#
#   - alcohol_units_per_week and sleep_hours, two of the variables a health
#     article would put first, separate the groups least of all 24. Whatever
#     they contribute, it is not visible marginally.
#   - the three heart-rate variables derived in Step 1 take the top three
#     places, ahead of every raw measurement. That is the evidence that those
#     derivations earned their place.
#
# Note that the top three are near-duplicates of each other (Step 1 measured
# their mutual correlations at 0.76-0.93), so this is one finding, not three.

pdf("numeric_by_outcome_boxplots.pdf", width = 12, height = 8)
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
dev.off()

# The four variables with the largest |d|, drawn as densities so the overlap
# between the two groups is visible. Even for the strongest variable the two
# distributions overlap heavily: no single measurement separates the groups,
# which is the argument for fitting a multivariable model in Step 3.
pdf("top_predictors_density.pdf", width = 10, height = 7)
data |>
  select(
    max_heart_rate_achieved, st_depression, age, cholesterol_hdl_ratio,
    has_heart_disease
  ) |>
  pivot_longer(-has_heart_disease, names_to = "variable", values_to = "value") |>
  ggplot(aes(value, fill = has_heart_disease)) +
  geom_density(alpha = 0.45) +
  facet_wrap(~variable, scales = "free") +
  labs(
    title = "The four most separating variables, by diagnosis",
    x = NULL, y = "Density", fill = "Heart disease"
  ) +
  theme_classic()
dev.off()


# =============================================================================
# 5. Each categorical variable against the diagnosis
# =============================================================================
# Reported as the disease rate within each level, against the 30.3% baseline,
# plus Cramer's V for the strength of the association. V runs from 0 to 1; for
# a 2 x 2 table it equals the phi coefficient.

cramers_v <- function(x, y) {
  tab <- table(x, y)
  chi_squared <- suppressWarnings(chisq.test(tab)$statistic)
  sqrt(chi_squared / (sum(tab) * (min(dim(tab)) - 1)))
}

rate_by_level <- do.call(rbind, lapply(categorical_all, function(v) {
  data |>
    group_by(level = .data[[v]]) |>
    summarise(
      n = n(),
      disease_rate = round(mean(has_heart_disease_num), 3),
      .groups = "drop"
    ) |>
    mutate(variable = v, level = as.character(level)) |>
    select(variable, level, n, disease_rate)
}))
print(as.data.frame(rate_by_level), row.names = FALSE)
write.csv(rate_by_level, "categorical_by_outcome_table.csv", row.names = FALSE)

association_strength <- data.frame(
  variable = categorical_all,
  cramers_v = round(sapply(categorical_all, function(v) {
    cramers_v(data[[v]], data$has_heart_disease)
  }), 3),
  row.names = NULL
)
association_strength <- association_strength[order(-association_strength$cramers_v), ]
print(association_strength, row.names = FALSE)
write.csv(association_strength, "categorical_association_strength.csv", row.names = FALSE)

# exercise_induced_angina dominates every other categorical variable
# (V = 0.45, against 0.28 for the next one): 67.8% of the patients who get
# angina during the stress test are diagnosed, against 19.1% of those who do
# not. Together with the heart-rate variables from Section 4, this says the
# exercise stress test carries most of the information in the dataset.
#
# The rest, in order of V:
#   age_group    (0.28)  9.5% at 18-34 rising to 51.1% at 65+, the cleanest
#                        monotone gradient in the table
#   bp_category  (0.21)  17.0% normal -> 43.9% at hypertension stage 2
#   glycemic_status (0.20)  20.7% -> 33.7% -> 45.6%
#   bmi_category (0.20)  15.8% underweight -> 47.2% obese
#   meets_activity_guideline (0.19)  20.1% vs 38.1%
#   smoker_status (0.17) never 25.2%, former 30.3%, current 46.4% - the
#                        ordering matches the exposure ordering Step 1 encoded,
#                        and former smokers sit between the two, as they should
#   chest_pain_type (0.16) asymptomatic 25.4% -> typical angina 47.9%
#   sex          (0.12)  male 35.4% vs female 24.7%
#   wearable_owner (0.09) 34.2% vs 25.4% - see Section 7.1 before using this
#   family_history (0.07) 35.6% vs 28.3%
#
# Two things stand out. chest_pain_type ranks only eighth, well below the four
# derived categories from Step 1, even though it is the variable a textbook
# heart-disease study leads with. And family_history is last: it does separate
# the groups, but by less than any other variable here.

pdf("disease_rate_by_category.pdf", width = 12, height = 8)
rate_by_level |>
  ggplot(aes(x = level, y = disease_rate)) +
  geom_col() +
  geom_hline(yintercept = prevalence, linetype = "dashed", colour = "red") +
  facet_wrap(~variable, scales = "free_x") +
  labs(
    title = "Disease rate within each category",
    subtitle = "Dashed line: the 30.3% overall prevalence",
    x = NULL, y = "Proportion diagnosed"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
dev.off()


# =============================================================================
# 6. Relationships among the predictors
# =============================================================================

correlation_matrix <- cor(data[numeric_all])
round(correlation_matrix, 2)

# Ordering the heatmap by a clustering of the correlations groups the variables
# that measure the same thing next to each other, which is easier to read than
# the alphabetical order used in the Step 1 version of this figure.
variable_order <- numeric_all[hclust(as.dist(1 - abs(correlation_matrix)))$order]

pdf("correlation_heatmap.pdf", width = 10, height = 9)
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
    title = "Pearson correlation between the numerical variables",
    subtitle = "Ordered so that variables measuring the same thing sit together",
    x = NULL, y = NULL
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# The blocks in the heatmap are the lipid panel, the two blood-sugar measures,
# the two blood pressures, and the heart-rate group - the same groups Step 1
# flagged. Everything outside those blocks is weakly correlated. In particular
# the strongest correlation between any lifestyle variable and any clinical one
# is only -0.31 (exercise_minutes_per_week with resting_heart_rate), and
# sleep_hours reaches at most 0.03 with anything clinical. The lifestyle block
# and the clinical block therefore carry largely separate information, so both
# belong in a model: neither is a proxy for the other.

# Age is worth a plot of its own, because it drives several other variables and
# is therefore a confounder for all of them.
pdf("age_relationships.pdf", width = 10, height = 7)
data |>
  select(
    age, max_heart_rate_achieved, resting_bp_systolic, ldl, hba1c
  ) |>
  pivot_longer(-age, names_to = "variable", values_to = "value") |>
  ggplot(aes(age, value)) +
  geom_point(alpha = 0.06, size = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "red") +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "How age moves the other measurements",
    x = "Age (years)", y = NULL
  ) +
  theme_classic()
dev.off()

# max_heart_rate_achieved falls almost linearly with age (r = -0.73), which is
# physiology, not disease. Since both age and peak heart rate are among the
# strongest correlates of the diagnosis, Step 3 has to fit them together - the
# marginal effect of either one on its own is partly the other one.


# =============================================================================
# 7. Three descriptive findings that need care
# =============================================================================

# 7.1 The wearable-owner gap is a confound, not an effect --------------------
# Owners have a lower disease rate. The obvious first guess is that they are
# younger; the tables below test that guess, and it turns out to be wrong.
table(data$wearable_owner, data$has_heart_disease)
data |>
  group_by(wearable_owner) |>
  summarise(
    n = n(), mean_age = round(mean(age), 1),
    mean_steps = round(mean(daily_steps)),
    mean_exercise_minutes = round(mean(exercise_minutes_per_week)),
    disease_rate = round(mean(has_heart_disease_num), 3)
  )
data |>
  group_by(age_group, wearable_owner) |>
  summarise(n = n(), disease_rate = round(mean(has_heart_disease_num), 3), .groups = "drop") |>
  pivot_wider(names_from = wearable_owner, values_from = c(n, disease_rate))

# The two groups are the same age: 53.8 years for owners against 54.1 for
# non-owners. Age is therefore not the explanation, and splitting by age group
# confirms it - the gap survives inside every band, and is if anything widest
# among the youngest patients (5.8% vs 12.6%).
#
# What does differ is activity: owners average 7,302 daily steps against 5,259,
# and 165 exercise minutes per week against 119. So wearable_owner is standing
# in for how active a patient is, and for the kind of patient who buys a
# tracker, rather than for their age.
#
# Owning a tracker cannot plausibly protect anyone. Any statement about
# wearable_owner in the report is an association only, and if Step 3 includes
# it, the activity variables have to be in the model beside it.

# 7.2 The sex gap is NOT explained by chest-pain type ------------------------
# Male patients are diagnosed more often (35.4% vs 24.7%). A natural
# explanation would be that men report the higher-risk kinds of chest pain more
# often. The first table tests that; the second asks whether the gap survives
# within each pain type.
round(prop.table(table(data$sex, data$chest_pain_type), margin = 1), 3)
data |>
  group_by(sex, chest_pain_type) |>
  summarise(n = n(), disease_rate = round(mean(has_heart_disease_num), 3), .groups = "drop") |>
  pivot_wider(names_from = sex, values_from = c(n, disease_rate))

# The explanation fails on both counts. The chest-pain profiles of the two
# sexes are nearly identical (typical angina 10.9% of women, 11.7% of men), and
# the male excess persists inside every pain type - 9.0, 10.0, 13.2 and 14.0
# percentage points from asymptomatic to typical angina. Sex acts on its own
# here, not through the symptom the patient reports.

# 7.3 The truncated columns distort their own summary statistics -------------
# Step 1 found five columns cut off at a recording limit. For those columns the
# reported maximum is the limit, not the largest patient value, and the sd is
# an underestimate. The share of patients sitting on the limit:
sapply(
  c("max_heart_rate_achieved", "triglycerides", "bmi", "hba1c", "ldl"),
  function(v) {
    x <- data[[v]]
    limit <- if (v == "max_heart_rate_achieved") max(x) else min(x)
    round(100 * mean(x == limit), 2)
  }
)

# Between 0.6% and 1.3% of patients sit exactly on each limit. The largest is
# max_heart_rate_achieved: 1.28% of patients are pinned at the 210 bpm ceiling.
# Since that variable carries the largest effect size in the entire dataset,
# the report states the ceiling explicitly - the truncation is at the healthy
# end of the scale, so the measured separation between the two groups is if
# anything an underestimate.
#
# The same truncation also explains why triglycerides came out symmetric in
# Section 2, when a lipid measurement is normally right skewed.


# =============================================================================
# 8. Summary of what Step 3 should take from here
# =============================================================================
# 1. The exercise stress test - peak heart rate, exercise-induced angina, ST
#    depression - separates the two groups far better than anything else.
# 2. Age is both a strong correlate of the diagnosis and a driver of several
#    other predictors, so it belongs in every model as a control.
# 3. The lifestyle block is almost uncorrelated with the clinical block, so it
#    is not redundant - but alcohol and sleep separate the groups barely at all
#    on their own. Whether they matter after adjustment is a question for
#    Step 3, not something the descriptives settle.
# 4. Three variables are right skewed (st_depression, alcohol,
#    cholesterol_hdl_ratio). A log fixes the last two; st_depression needs its
#    zero mass handled separately. Nothing else needs transforming, and
#    transforming triglycerides would make it worse.
# 5. The correlation blocks identified in Step 1 are confirmed here; one
#    variable per block goes into a model, not all of them.
