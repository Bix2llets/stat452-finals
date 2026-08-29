# Activity 2 - Step 2: Descriptive statistics
#
# Input : ../dataset/cleaned_data.rds  (produced by ../step1/data_processing.R)
# Output: the tables and figures listed in task.md
#
# Describes the data only. Associations are reported as effect sizes, not
# p-values: at n = 9,000 nearly any difference is significant, so a p-value
# would not say which variables matter. Tests and models belong to Step 3.
# Every association here is marginal - one variable at a time, nothing held
# fixed - and the data is observational, so nothing below is phrased as a cause.
#
# Run with activity2/step2 as the working directory.

library(ggplot2)
library(dplyr)
library(tidyr)
library(tseries) # jarque.bera.test

data <- readRDS("../dataset/cleaned_data.rds")

str(data)

# The Step 1 derived columns are described beside the raw ones.
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

categorical_all <- c(
  "sex", "chest_pain_type", "exercise_induced_angina", "family_history",
  "smoker_status", "wearable_owner", "bmi_category", "bp_category",
  "glycemic_status", "age_group", "meets_activity_guideline"
)

prevalence <- mean(data$has_heart_disease_num)
cat("overall prevalence:", round(prevalence, 4), "\n")

# 2,727 of 9,000 are cases: prevalence 30.3%. Every rate below is read against
# this baseline.


# =============================================================================
# 1. The numerical variables
# =============================================================================
# Skewness is the third standardised moment (0 = symmetric, positive = long
# right tail), kurtosis the fourth in excess form (0 for a normal).

skewness <- function(x) mean((x - mean(x))^3) / sd(x)^3
excess_kurtosis <- function(x) mean((x - mean(x))^4) / sd(x)^4 - 3

# cv = sd / mean is unit-free, so spread can be compared across variables
# measured in mmHg, mg/dL and bpm; sd cannot.
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
write.csv(numeric_summary, "numeric_summary_table.csv", row.names = FALSE)

# A screening population, not a ward: median age 54, 128/81 mmHg, BMI 25.3.
# The regulated quantities barely vary (cv 0.09-0.12), the behavioural ones
# swing (st_depression 0.95, alcohol 0.89). 21 of 24 variables are near
# symmetric; the exceptions are st_depression (+2.04), alcohol (+2.03) and
# cholesterol_hdl_ratio (+1.41).

# 1.1 Are the distributions normal? ------------------------------------------
# Chi-square goodness of fit: X2 = sum (obs - exp)^2 / exp over bins, on
# k - 1 - 2 df, two spent estimating the mean and sd. The bins are the deciles
# of the fitted normal, so every expected count is n/10 = 900.

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

# Jarque-Bera is reported beside it, so the choice of test can be checked.
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

# Written with alpha and the upper tail so the significance level is visible;
# qchisq(0.95, df) gives the same 14.07 but hides it.
significance_level <- 0.05
critical_value <- qchisq(significance_level, df = 7, lower.tail = FALSE)
cat("5% critical value on 7 df:", round(critical_value, 2), "\n")
cat("variables exceeding it:", sum(normality_report$gof_chisq > critical_value), "\n")

# Shapiro-Wilk cannot be used at all - R refuses samples above 5,000:
cat("shapiro.test on 9,000:", tryCatch(
  {
    shapiro.test(data$age)
    "ran"
  },
  error = function(e) conditionMessage(e)
), "\n")

# 17 of 24 are rejected, but read the SIZE of X2, not the verdict: ten sit at
# 7-30, eleven at 80-290 with |skewness| < 0.24, then cholesterol_hdl_ratio,
# st_depression and alcohol_units_per_week at 768, 3341 and 3744. Only those
# three are worth transforming.
#
# Jarque-Bera flags the same three (9766, 18017, 20972 against 102 or less), so
# it changes nothing. But it is built from the skewness and excess kurtosis
# alone, so it sees nothing in resting_heart_rate (0.6, skewness -0.019) where
# the binned test gives X2 = 285 - a shape departure the two moments cannot
# detect. The chi-square statistic is what the decisions are read from.

# 1.2 Histograms and QQ plots ------------------------------------------------

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
  labs(title = "Normal QQ plot of each numerical variable",
    x = "Theoretical quantiles", y = "Observed value") +
  theme_classic()
ggsave("numeric_qq.pdf", width = 14, height = 10)

# A truncated column shows as a flat run at one end of the QQ line. The five
# Step 1 flagged are each truncated at ONE end: max_heart_rate_achieved at the
# top (115 patients at 210 bpm), triglycerides, ldl, bmi and hba1c at the
# bottom. st_depression's step at zero is a point mass, not a scale boundary.

# 1.3 The three skewed variables ---------------------------------------------
# The ladder of powers - sqrt, cube root, log - pulls in a right tail more
# strongly at each rung. log is undefined at 0, so a column with zeros takes
# log1p(x) instead. Every rung is scored on the skewness it leaves behind.

skewed <- c("alcohol_units_per_week", "cholesterol_hdl_ratio", "st_depression")

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
    `log(1 + alcohol)` = log1p(alcohol_units_per_week),
    `cholesterol_hdl_ratio` = cholesterol_hdl_ratio,
    `log(cholesterol_hdl_ratio)` = log(cholesterol_hdl_ratio),
    `st_depression` = st_depression,
    `st_depression^(1/3)` = st_depression^(1 / 3)
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable, levels = c(
    "alcohol_units_per_week", "log(1 + alcohol)",
    "cholesterol_hdl_ratio", "log(cholesterol_hdl_ratio)",
    "st_depression", "st_depression^(1/3)"
  ))) |>
  ggplot(aes(value)) +
  geom_histogram(bins = 40) +
  facet_wrap(~variable, scales = "free", nrow = 3) +
  labs(title = "The three right-skewed variables, before and after",
    x = NULL, y = "Patients") +
  theme_classic()
ggsave("skewed_variables_transformed.pdf", width = 10, height = 9)

# Each wants a different rung: log1p for alcohol (+2.03 -> +0.02), plain log
# for cholesterol_hdl_ratio (+1.41 -> +0.32, and it is strictly positive so the
# plain log is available and beats log1p's +0.52), cube root for st_depression
# (+2.04 -> -0.05, where log overshoots).

# If the 193 zeros drove st_depression's skew, no monotone transformation could
# fix it and the variable would have to be split into yes/no plus an amount.
cat(
  "st_depression skewness, all / zeros dropped:",
  round(skewness(data$st_depression), 3), "/",
  round(skewness(data$st_depression[data$st_depression > 0]), 3), "\n"
)
# The converse: a symmetric variable is made worse by a log.
cat(
  "triglycerides skewness, raw / log:",
  round(skewness(data$triglycerides), 3), "/",
  round(skewness(log(data$triglycerides)), 3), "\n"
)

# Dropping the zeros moves the skewness by 0.01, so the spike is not the cause
# - it is a genuine long right tail, which a power transformation fixes, and no
# splitting is needed. Logging triglycerides takes +0.13 to -1.09: transforms
# go to the variables the skewness column flags, not by habit to every lipid.


# =============================================================================
# 2. The categorical variables
# =============================================================================

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

# Balanced on sex (47.4% female), not on the clinical categories: 47.4%
# asymptomatic, 11.3% typical angina. The smallest level anywhere is
# Underweight with 549 patients, so nothing needs collapsing before Step 3.


# =============================================================================
# 3. Each numerical variable against the diagnosis
# =============================================================================
# The standardised mean difference is the gap between the two group means over
# the pooled sd, s_pooled^2 = ((n1-1) s1^2 + (n0-1) s0^2) / (n1+n0-2), i.e.
# sqrt(MSE) from a one-factor ANOVA on the same split. It puts 24 variables
# measured in different units on one scale.

standardised_mean_difference <- function(x, group) {
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
    mean_disease = round(mean(x[g == "Yes"]), 2),
    std_mean_difference = round(standardised_mean_difference(x, g), 3)
  )
}))
outcome_comparison <-
  outcome_comparison[order(-abs(outcome_comparison$std_mean_difference)), ]
print(outcome_comparison, row.names = FALSE)
write.csv(outcome_comparison, "numeric_by_outcome_table.csv", row.names = FALSE)

# 3.1 The smd is a distance, not a test statistic ----------------------------
# It divides the gap by the spread of the PATIENTS, so it does not grow with n.
# The z statistic divides the same gap by its standard error, z = smd /
# sqrt(1/n1 + 1/n0), so it carries a factor of sqrt(n). 1.96 is a cut-off for z
# and says nothing about an smd.

scale_factor <- 1 / sqrt(1 / sum(data$has_heart_disease == "Yes") +
  1 / sum(data$has_heart_disease == "No"))
smallest <- min(abs(outcome_comparison$std_mean_difference))
cat("smd -> z multiplier:", round(scale_factor, 1),
  "| smallest |smd|", smallest, "-> z =", round(smallest * scale_factor, 1), "\n")

# The multiplier is 43.6, so even the smallest |smd| (0.09) reaches z = 3.9.
# All 24 clear 1.96 on the z scale and none reaches it on the smd scale.

# The 0.2 / 0.5 / 0.8 bands are a reporting convention, so translate them into
# patients: the share of diagnosed patients past the healthy median. Identical
# groups give 50%, complete separation 100%.
for (v in c("percent_predicted_max_hr", "st_depression", "age", "sleep_hours")) {
  x <- data[[v]]
  g <- data$has_heart_disease
  smd <- standardised_mean_difference(x, g)
  # Compare on the side the smd points to, so the number reads the same way
  # whether the disease raises or lowers the variable.
  beyond <- if (smd < 0) {
    mean(x[g == "Yes"] < median(x[g == "No"]))
  } else {
    mean(x[g == "Yes"] > median(x[g == "No"]))
  }
  cat(sprintf("%-26s smd %+6.2f -> %4.1f%% past the healthy median\n",
    v, smd, 100 * beyond))
}

# "Very large" is 93.2% for percent_predicted_max_hr; "negligible" is 51.9%
# for sleep_hours, barely off the 50% identical groups would give.

# 3.2 What the ranking says --------------------------------------------------
# Three exercise-test measurements lead: percent_predicted_max_hr (-1.58),
# max_heart_rate_achieved (-1.55), heart_rate_reserve (-1.54). They are near
# duplicates (mutual r 0.76-0.93), so this is ONE finding.
#
# All three come from the treadmill stress test, which is what "exertion" means
# here - there is no separate exertion variable. The data supports the gap, not
# a mechanism: diagnosed patients REACHED a lower peak, 146 bpm against 173.
# Whether they could not go higher, or the test was stopped for symptoms, or
# they were on rate-limiting medication, is not recorded.
#
# Then st_depression (+0.83) and cholesterol_hdl_ratio (+0.82), age (+0.66) and
# the lipid, pressure and glucose measures, and last sleep_hours (-0.11) and
# alcohol (+0.09). Every sign runs the way clinical knowledge predicts.

data |>
  select(all_of(numeric_all), has_heart_disease) |>
  pivot_longer(-has_heart_disease, names_to = "variable", values_to = "value") |>
  ggplot(aes(x = has_heart_disease, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Each numerical variable by diagnosis",
    x = "Diagnosed with heart disease", y = NULL) +
  theme_classic()
ggsave("numeric_by_outcome_boxplots.pdf", width = 14, height = 10)

top_four <- c(
  "max_heart_rate_achieved", "st_depression", "age", "cholesterol_hdl_ratio"
)

# Drawn on the patient-COUNT scale: a density curve rescales each group to unit
# area, inflating the smaller diagnosed group (2,727) to the height of the
# larger one (6,273).
data |>
  select(all_of(top_four), has_heart_disease) |>
  pivot_longer(-has_heart_disease, names_to = "variable", values_to = "value") |>
  ggplot(aes(value, fill = has_heart_disease)) +
  geom_histogram(bins = 40, position = "identity", alpha = 0.55) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "The four most separating variables, by diagnosis",
    subtitle = "Patient counts, so the groups appear at their true relative size",
    x = NULL, y = "Patients", fill = "Heart disease") +
  theme_classic()
ggsave("top_predictors_histograms.pdf", width = 10, height = 8)

# 3.3 How much do the two groups overlap? ------------------------------------
# A gap between MEANS says nothing about telling patients apart: two
# distributions can have distant means and cover the same range. For each group
# take its central range (5th to 95th percentile) and report the share of the
# other group inside it. Complete separation gives 0 in both columns; a variable
# carrying no information gives about 0.90.

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

# max_heart_rate_achieved is the best separator (0.556 / 0.549); the other
# three overlap almost completely. This RULES OUT a single-variable cut-off
# rule - no threshold avoids both missing many cases and flagging many healthy
# patients. It does NOT make the variables weak: logistic regression estimates
# a probability per patient rather than sorting them into boxes, and variables
# that overlap alone can separate together. Hence the model in Step 3.


# =============================================================================
# 4. Each categorical variable against the diagnosis
# =============================================================================
# Two summaries per variable, between its safest and riskiest level. Risk
# difference = highest rate minus lowest. Odds ratio = the odds at the riskiest
# over the odds at the safest, odds = p / (1 - p) - what logistic regression
# estimates, since exp(coefficient) of a dummy IS the odds ratio against the
# reference level. The odds ratio ranks variables the way a model will; the risk
# difference says whether that ranking moves many patients.
#
# Each percentage is a DISEASE RATE: of the patients at that level, the percent
# diagnosed. Not percent of the sample and not of the cases, so the levels of
# one variable do not add to 100.
#
# Each is computed one variable at a time with nothing held fixed, so it is
# MARGINAL and carries its confounding: if current smokers here are also older,
# part of their 46.4% is age. These rates cannot be added or chained across
# variables. Step 3 gives the adjusted version, and the gap between the two is
# the confounding the model removes.

rate_by_level <- do.call(rbind, lapply(categorical_all, function(v) {
  data |>
    group_by(level = .data[[v]]) |>
    summarise(n = n(),
      disease_rate = round(mean(has_heart_disease_num), 3), .groups = "drop") |>
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
    odds_ratio = round((p_high / (1 - p_high)) / (p_low / (1 - p_low)), 2)
  )
}))
association_strength <-
  association_strength[order(-association_strength$risk_difference), ]
print(association_strength, row.names = FALSE)
write.csv(association_strength, "categorical_association_strength.csv",
  row.names = FALSE)

# Each bar carries its rate and level size, so the figure needs no
# supplementary table beside it. n is shown because both summary measures
# ignore level size.
rate_by_level |>
  ggplot(aes(x = level, y = disease_rate)) +
  geom_col(fill = "grey35") +
  geom_hline(yintercept = prevalence, linetype = "dashed", colour = "red") +
  geom_text(aes(label = sprintf("%.1f%%\nn=%d", 100 * disease_rate, n)),
    vjust = -0.25, size = 2.5, lineheight = 0.9) +
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

# exercise_induced_angina leads on risk difference (0.487, No 19.1% -> Yes
# 67.8%), so with the heart-rate result the stress test carries most of the
# information here. age_group has the larger odds ratio (10.0 vs 8.9) but the
# smaller risk difference: multiplying its 9.5% base rate by 10 still leaves a
# small probability. chest_pain_type places sixth, family_history last.
#
# Every ordered variable rises monotonically - age_group 9.5 -> 18.0 -> 31.9 ->
# 51.1%, bp_category 17.0 -> 25.3 -> 30.4 -> 43.9%, glycemic_status 20.7 ->
# 33.7 -> 45.6%, smoker_status 25.2 -> 30.3 -> 46.4%.


# =============================================================================
# 5. Relationships among the predictors
# =============================================================================
# The heatmap is ordered by what each variable measures, written out by hand
# from the blocks Step 1 found, so the figure is reproducible.

correlation_matrix <- cor(data[numeric_all])

variable_order <- c(
  "resting_bp_systolic", "resting_bp_diastolic", "pulse_pressure",
  "cholesterol_total", "ldl", "non_hdl_cholesterol", "cholesterol_hdl_ratio",
  "hdl", "triglycerides",
  "fasting_blood_sugar", "hba1c",
  "age", "max_heart_rate_achieved", "heart_rate_reserve",
  "percent_predicted_max_hr", "resting_heart_rate", "st_depression",
  "bmi", "exercise_minutes_per_week", "daily_steps", "diet_quality_score",
  "alcohol_units_per_week", "sleep_hours", "stress_score"
)
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
  labs(title = "Correlation between the numerical variables",
    subtitle = "Ordered so that variables measuring the same thing sit together",
    x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("correlation_heatmap.pdf", width = 10, height = 9)

# Outside those blocks correlations are weak: the strongest lifestyle-clinical
# link is -0.31 and sleep_hours reaches at most 0.03, so the two blocks carry
# separate information and both belong in a model.

# If a relationship is linear the straight-line and quadratic fits coincide; a
# visible gap is the signal for a squared term. Age is plotted because it drives
# several predictors and so confounds them.
data |>
  select(age, max_heart_rate_achieved, resting_bp_systolic, ldl, hba1c) |>
  pivot_longer(-age, names_to = "variable", values_to = "value") |>
  ggplot(aes(age, value)) +
  geom_point(alpha = 0.06, size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
    aes(colour = "linear")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,
    aes(colour = "quadratic")) +
  scale_colour_manual(values = c(linear = "red", quadratic = "blue")) +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "How age moves the other measurements",
    x = "Age (years)", y = NULL, colour = NULL) +
  theme_classic() +
  theme(legend.position = "bottom")
ggsave("age_relationships.pdf", width = 9, height = 7)

for (v in c("max_heart_rate_achieved", "resting_bp_systolic", "ldl", "hba1c")) {
  fit <- lm(data[[v]] ~ data$age)
  cat(sprintf("%-24s slope %+8.3f per year   R^2 %.3f\n",
    v, coef(fit)[2], summary(fit)$r.squared))
}

# The two curves are indistinguishable in all four panels, so age enters Step 3
# linearly. Peak heart rate falls -1.196 bpm per year (R^2 0.53) - physiology,
# not disease - so Step 3 must fit age and peak rate together.


# =============================================================================
# 6. Two findings that need care
# =============================================================================

# 6.1 The wearable-owner gap is a confound -----------------------------------
# Owners have a lower disease rate. Test the obvious explanation, that they are
# younger, before accepting it.
data |>
  group_by(wearable_owner) |>
  summarise(
    n = n(), mean_age = round(mean(age), 1),
    mean_steps = round(mean(daily_steps)),
    mean_exercise_minutes = round(mean(exercise_minutes_per_week)),
    disease_rate = round(mean(has_heart_disease_num), 3)
  )

# It fails: 53.8 vs 54.1 years. Activity is what differs (7,302 vs 5,259 steps;
# 165 vs 119 exercise minutes), so wearable_owner stands in for how active a
# patient is. Association only - if Step 3 includes it, the activity variables
# must be beside it.

# 6.2 The sex gap is not explained by chest-pain type ------------------------
# Men are diagnosed more often (35.4% vs 24.7%). Test whether they report
# higher-risk pain types, then whether the gap survives within type.
round(prop.table(table(data$sex, data$chest_pain_type), margin = 1), 3)
data |>
  group_by(sex, chest_pain_type) |>
  summarise(disease_rate = round(mean(has_heart_disease_num), 3),
    .groups = "drop") |>
  pivot_wider(names_from = sex, values_from = disease_rate)

# Both fail: the profiles are nearly identical (typical angina 10.9% of women,
# 11.7% of men) and the male excess persists inside every type, 9.0 to 14.0
# percentage points.


# =============================================================================
# 7. What Step 3 takes from here
# =============================================================================
# 1. The exercise stress test - peak heart rate, exercise-induced angina, ST
#    depression - separates the groups far better than anything else.
# 2. Age belongs in every model as a control: a strong correlate, and a driver
#    of several other predictors.
# 3. The lifestyle block is nearly uncorrelated with the clinical block, so it
#    is not redundant - but alcohol and sleep separate the groups barely at all
#    on their own.
# 4. Transform three variables: log1p for alcohol, log for
#    cholesterol_hdl_ratio, cube root for st_depression. Nothing else.
# 5. One variable per correlation block, not all of them.
# 6. No single variable separates the groups, so a multivariable model rather
#    than a screening rule on one measurement.
