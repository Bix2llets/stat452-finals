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

# How to read the table. One row per variable, and the columns fall into four
# groups:
#
#   n                      number of patients contributing to the row. It is
#                          9,000 everywhere, because Step 1 found no missing
#                          values, so no row rests on a smaller sample than
#                          any other.
#   mean, median           the two measures of centre. When they agree the
#                          distribution is symmetric; when the mean sits above
#                          the median the variable has a right tail, and the
#                          median is then the honest summary.
#   sd, iqr, min/q1/q3/max spread. sd is in the variable's own unit, so it can
#                          be compared within a variable but never between two
#                          variables measured in different units.
#   cv = sd / mean         spread made unit-free, so that spreads CAN be
#                          compared across variables. Meaningful only for a
#                          variable on a ratio scale with a positive mean,
#                          which is true of every variable here.
#   skewness               0 for a symmetric distribution, positive when the
#                          long tail is on the right. Beyond about |0.5| the
#                          asymmetry is visible in a histogram; beyond |1| it
#                          is strong enough that the mean misleads.
#   excess_kurtosis        0 for a normal distribution, positive when the tails
#                          are heavier than normal, i.e. more extreme values
#                          than a normal curve of the same sd would predict.
#
# What the table is used for. Three questions are answered from it, and each is
# settled by a different group of columns:
#
#   "Who is in this sample?"       -> mean and median, read against the
#                                     clinical normal range of each measurement
#   "Which variables carry enough
#    variation to be worth
#    modelling?"                   -> cv. A predictor that barely varies cannot
#                                     explain variation in the response,
#                                     whatever its coefficient turns out to be.
#   "Which variables break the
#    assumptions of a linear
#    model?"                       -> skewness and excess_kurtosis. These flag
#                                     the variables needing the transformations
#                                     of Section 2.3, and are checked against
#                                     the QQ plots and the goodness-of-fit test
#                                     in Section 2.1.
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
# The tool for "does this sample follow this distribution" is the chi-square
# goodness-of-fit test. The data are cut into k bins, the count actually
# falling in each bin is compared with the count a normal distribution would
# put there, and
#
#     X2 = sum over bins of (observed - expected)^2 / expected
#
# is referred to a chi-square distribution. Mean and sd are estimated from the
# sample before the expected counts can be computed, and each estimated
# parameter costs one degree of freedom, so df = k - 1 - 2.
#
# The bins are the deciles of the fitted normal, i.e. they are equiprobable
# under the null, so every expected count is n / k = 900. Equiprobable bins are
# the standard choice: they stop the statistic from being driven by a nearly
# empty tail bin, and they satisfy the "expected count at least 5 in every bin"
# requirement with room to spare.

gof_normal <- function(x, n_bins = 10) {
  # Cut points: the deciles of the normal fitted to x. -Inf and Inf close the
  # two outer bins so that every observation lands in exactly one bin.
  cut_points <- qnorm(
    seq(0, 1, length.out = n_bins + 1),
    mean = mean(x), sd = sd(x)
  )
  cut_points[1] <- -Inf
  cut_points[n_bins + 1] <- Inf

  observed <- as.numeric(table(cut(x, breaks = cut_points)))
  expected <- rep(length(x) / n_bins, n_bins)

  statistic <- sum((observed - expected)^2 / expected)
  degrees_of_freedom <- n_bins - 1 - 2 # two parameters estimated from x
  c(
    chisq = statistic,
    df = degrees_of_freedom,
    p_value = pchisq(statistic, degrees_of_freedom, lower.tail = FALSE)
  )
}

gof_results <- t(sapply(data[numeric_all], gof_normal))

normality_report <- data.frame(
  variable = numeric_all,
  skewness = round(sapply(data[numeric_all], skewness), 3),
  excess_kurtosis = round(sapply(data[numeric_all], excess_kurtosis), 3),
  gof_chisq = round(gof_results[, "chisq"], 1),
  gof_df = gof_results[, "df"],
  gof_p_value = signif(gof_results[, "p_value"], 3),
  row.names = NULL
)
normality_report <- normality_report[order(normality_report$gof_chisq), ]
print(normality_report, row.names = FALSE)
write.csv(normality_report, "normality_report.csv", row.names = FALSE)

# One warning before reading the p-values. The test answers "is the departure
# from normality detectable", and at n = 9,000 it detects departures far too
# small to matter for anything done here. The size of X2 is therefore the
# useful number, not whether it clears the 5% critical value: a variable a
# little above the critical value is normal for every practical purpose, one
# with X2 in the hundreds is not.
critical_value <- qchisq(0.95, df = 7)
cat("5% critical value on 7 df:", round(critical_value, 2), "\n")
cat(
  "variables exceeding it:",
  sum(normality_report$gof_chisq > critical_value), "of", length(numeric_all), "\n"
)

# The ranking the test produces agrees with the skewness column, and the gap in
# that ranking is what matters. Reading the printed table from the top:
#
#   X2 =   7 to  30   ten variables, at or just above the critical value of
#                     14.07. Indistinguishable from normal even at n = 9,000.
#   X2 =  80 to 290   eleven variables. Formally rejected, but that is the
#                     sample size talking: every one of them has |skewness|
#                     below 0.24 and |excess kurtosis| below 0.37, a shape no
#                     plot would call non-normal.
#   X2 = 768, 3341, 3744   cholesterol_hdl_ratio, st_depression and
#                     alcohol_units_per_week - an order of magnitude clear of
#                     everything else, and the same three the skewness column
#                     flags.
#
# So 17 of the 24 variables are "rejected" at the 5% level and only three of
# those rejections mean anything. That is exactly why the size of X2 is read
# here rather than its p-value, and why Section 2.3 transforms three variables
# rather than seventeen.

# 2.2 Histograms and QQ plots ------------------------------------------------

long_numeric <- data |>
  select(all_of(numeric_all)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value")

ggplot(long_numeric, aes(value)) +
  geom_histogram(bins = 40) +
  facet_wrap(~variable, scales = "free") +
  labs(
    title = "Distribution of each numerical variable",
    x = NULL, y = "Number of patients"
  ) +
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

# The QQ plots make the Step 1 findings visible. A truncated variable shows up
# as a horizontal run of points at one end of the plot: many patients share the
# same observed value while the theoretical quantile keeps moving, so the curve
# flattens instead of following the reference line.
#
# Each of the five truncated columns is truncated at ONE end only, and the
# boundary counts in Step 1 say which end. A recording limit shows up there as
# a large tie count at the limit against a count of 1 at the opposite extreme:
#
#   max_heart_rate_achieved  115 patients at the 210 bpm maximum,  2 at the
#                            minimum -> flat at the TOP of the QQ plot
#   triglycerides            114 at the 35 mg/dL minimum,  1 at the maximum
#   ldl                       56 at the 35 mg/dL minimum,  1 at the maximum
#   bmi                       82 at the 15.0 minimum,      1 at the maximum
#   hba1c                     64 at the 4.0 % minimum,     1 at the maximum
#                            -> the last four are flat at the BOTTOM
#
# So no variable here is truncated at both ends; the four blood and body
# measurements have a floor, the stress-test peak has a ceiling. The direction
# matters for the interpretation, because a floor compresses the healthy tail
# and a ceiling compresses the unhealthy one - Section 7.3 returns to this.
#
# st_depression is a different shape again: it has a flat step at zero, not at
# an extreme of the measuring scale. That is the 193 patients with genuinely no
# ST depression, a point mass rather than a truncation, and Section 2.3 shows
# why no transformation removes it.
#
# 2.3 The three skewed variables, and what a transformation does -------------

data |>
  transmute(
    `alcohol_units_per_week` = alcohol_units_per_week,
    # log1p(x) is log(1 + x). The + 1 is needed because 29 patients report
    # exactly 0 units per week and log(0) is -Inf, which would drop them from
    # the plot and from every skewness figure below. Adding 1 before the log
    # maps 0 to 0 and leaves the shape of the rest of the distribution
    # essentially unchanged, since the values run up to 45.9 and adding 1 to a
    # large value barely moves its logarithm. The ratio below needs no such
    # shift: cholesterol / HDL is strictly positive, so a plain log is fine.
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
  labs(
    title = "The three right-skewed variables, before and after a transformation",
    x = NULL, y = "Number of patients"
  ) +
  theme_classic()
ggsave("skewed_variables_transformed.pdf", width = 10, height = 9)

# Rather than picking a transformation by habit, every candidate is scored on
# the same criterion - the skewness it leaves behind, which should be near 0.
# The ladder of powers is walked from the mildest correction to the strongest:
# square root, cube root, then log.
transformation_report <- do.call(rbind, lapply(
  c("alcohol_units_per_week", "cholesterol_hdl_ratio", "st_depression"),
  function(v) {
    x <- data[[v]]
    data.frame(
      variable = v,
      has_zeros = any(x == 0),
      raw = round(skewness(x), 3),
      sqrt = round(skewness(sqrt(x)), 3),
      cube_root = round(skewness(x^(1 / 3)), 3),
      # A plain log is only defined when the variable is strictly positive;
      # for the two columns containing zeros the shifted log1p is the
      # comparable candidate, and log is left as NA.
      log = if (any(x == 0)) NA else round(skewness(log(x)), 3),
      log1p = round(skewness(log1p(x)), 3)
    )
  }
))
print(transformation_report, row.names = FALSE)

# Each variable wants a different rung of the ladder, and the table says which.
#
#                            raw    sqrt   cube root   log    log1p
#   alcohol_units_per_week  +2.027  +0.666   +0.152     --    +0.021
#   cholesterol_hdl_ratio   +1.409  +0.814   +0.642   +0.323  +0.518
#   st_depression           +2.040  +0.652   -0.054     --    +0.775
#
# alcohol_units_per_week and cholesterol_hdl_ratio need the strongest
# correction, the log, which takes them to +0.02 and +0.32 respectively. The
# square root leaves far too much skew behind on both. These two have the
# classic long right tail.
#
# Note which log each of them takes. cholesterol_hdl_ratio is strictly
# positive, so a plain log applies and is the better of the two (+0.32 against
# +0.52 for log1p - the shift is not free, it flattens the low end). Alcohol
# contains zeros, so only log1p is available to it.
#
# st_depression needs the middle rung instead. The square root halves its skew
# and the log overshoots past zero in the other direction, but the cube root
# lands almost exactly on target (+2.04 -> -0.05). That is the transformation
# to carry into Step 3 for this variable.
#
# It is worth being explicit about WHY, because the obvious explanation is the
# wrong one. The natural guess is that the skew is caused by the patients
# sitting exactly at zero - a point mass that no monotone transformation can
# move, since every zero maps to the same new value. The data rejects that
# guess:
cat(
  "st_depression: patients at exactly 0 =", sum(data$st_depression == 0),
  sprintf("(%.1f%%)", 100 * mean(data$st_depression == 0)), "\n"
)
cat(
  "  skewness of the whole column          :",
  round(skewness(data$st_depression), 3), "\n"
)
cat(
  "  skewness with the zeros excluded      :",
  round(skewness(data$st_depression[data$st_depression > 0]), 3), "\n"
)

# Dropping the zeros changes the skewness by 0.01. The spike is only 2.1% of
# the sample, far too small to bend the third moment of 9,000 observations, so
# the skew is a genuine long right tail after all - the median is 0.7 mm and
# the largest 1% of patients reach 4.7 mm and beyond. A tail is exactly what a
# power transformation fixes, which is why the cube root works.
#
# So Step 3 does not need to split this variable into "any ST depression
# yes / no" plus an amount. That would only be necessary if the zeros were
# driving the shape, and they are not; the cube root of the column as it stands
# is enough.
#
# For contrast, a variable that is already symmetric is made WORSE by a log.
# triglycerides is the example, and the comparison is computed rather than
# asserted:
cat(
  "skewness triglycerides raw / log:",
  round(skewness(data$triglycerides), 3), "/",
  round(skewness(log(data$triglycerides)), 3), "\n"
)

# The raw column is nearly symmetric and the log manufactures a left tail out
# of nothing. Transformations go to the variables the skewness column flags,
# not by habit to everything that happens to be a lipid.


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
ggsave("categorical_distributions.pdf", width = 12, height = 8)


# =============================================================================
# 4. Each numerical variable against the diagnosis
# =============================================================================
# The comparison is reported as a standardised mean difference: the gap between
# the two group means, divided by the pooled standard deviation of the two
# groups. The pooled standard deviation is the same quantity that a two-group
# comparison of means uses, and it is the square root of the within-group mean
# square that a one-factor ANOVA on the same split would report:
#
#     s_pooled^2 = ((n1 - 1) s1^2 + (n0 - 1) s0^2) / (n1 + n0 - 2)
#
# Dividing by it makes the 24 gaps comparable with each other even though the
# variables are measured in mmHg, mg/dL, bpm and minutes. The number is used
# instead of a p-value because at n = 9,000 nearly every difference is
# "significant"; the standardised gap says which ones are large enough to
# matter. It is read as a number of standard deviations - a gap of 1 means the
# two group means are a full standard deviation apart - and the conventional
# rule of thumb for describing one in words is 0.2 small, 0.5 medium, 0.8
# large. That rule is a reporting convention, not a test: the ranking below is
# what is used, not the labels.

standardised_mean_difference <- function(x, group) {
  x1 <- x[group == "Yes"]
  x0 <- x[group == "No"]
  n1 <- length(x1)
  n0 <- length(x0)
  pooled_sd <- sqrt(((n1 - 1) * var(x1) + (n0 - 1) * var(x0)) / (n1 + n0 - 2))
  (mean(x1) - mean(x0)) / pooled_sd
}

# Compare the two groups on every numerical variable.
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
    std_mean_difference = round(standardised_mean_difference(x, g), 3)
  )
}))
outcome_comparison <-
  outcome_comparison[order(-abs(outcome_comparison$std_mean_difference)), ]
print(outcome_comparison, row.names = FALSE)
write.csv(outcome_comparison, "numeric_by_outcome_table.csv", row.names = FALSE)

# Ranked by the absolute standardised difference, the separation between
# diagnosed and undiagnosed patients is:
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
#     derivations earned their place. They are worth further inspection
#
# Note that the top three are near-duplicates of each other (Step 1 measured
# their mutual correlations at 0.76-0.93), so this is one finding, not three.

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

# The four variables with the largest standardised difference, drawn as
# overlaid histograms of the patient COUNT in each bin.
#
# Counting patients rather than plotting two separate densities matters here.
# The two groups are of very different size - 6,273 undiagnosed against 2,727
# diagnosed - and a density curve rescales each group to unit area, so the
# smaller group is silently inflated to the same height as the larger one and
# the picture exaggerates how much of the sample sits in the diagnosed range.
# On the count scale each bar is the number of real patients in it, so the two
# groups are drawn at their true relative sizes.
data |>
  select(
    max_heart_rate_achieved, st_depression, age, cholesterol_hdl_ratio,
    has_heart_disease
  ) |>
  pivot_longer(-has_heart_disease, names_to = "variable", values_to = "value") |>
  ggplot(aes(value, fill = has_heart_disease)) +
  geom_histogram(bins = 40, position = "identity", alpha = 0.55) +
  facet_wrap(~variable, scales = "free") +
  labs(
    title = "The four most separating variables, by diagnosis",
    subtitle = "Patient counts, so the two groups appear at their true relative size",
    x = NULL, y = "Number of patients", fill = "Heart disease"
  ) +
  theme_classic()
ggsave("top_predictors_histograms.pdf", width = 10, height = 8)

# How much do the two groups actually overlap? The plot alone cannot settle it,
# so the share of each group falling in the other group's central range is
# computed. For a variable that separated the groups completely both figures
# would be 0.
top_four <- c(
  "max_heart_rate_achieved", "st_depression", "age", "cholesterol_hdl_ratio"
)
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

# max_heart_rate_achieved is genuinely the best separator of the four - it is
# the only one where a large part of each group falls outside the other group's
# central 90% range, which is what a standardised difference of -1.55 looks
# like. Even so it does not separate the groups cleanly: the bulk of both
# groups still shares the same range of values, and the other three overlap
# almost completely. So the honest statement is not "everything overlaps", but
# that no single measurement here splits the two groups on its own - which is
# the argument for fitting a multivariable model in Step 3 rather than a
# screening rule on one measurement.


# =============================================================================
# 5. Each categorical variable against the diagnosis
# =============================================================================
# Reported as the disease rate within each level, against the 30.3% baseline,
# and summarised by two numbers per variable.
#
#   risk_difference  the highest disease rate across the variable's levels
#                    minus the lowest. It is a proportion, so it is already
#                    unit-free and comparable across variables, and it is read
#                    directly: "moving from the safest to the riskiest level of
#                    this variable changes the diagnosis rate by this much".
#
#   odds_ratio       the odds of diagnosis at the riskiest level divided by the
#                    odds at the safest level, where odds = p / (1 - p). This
#                    is the quantity a logistic regression estimates: the
#                    exponentiated coefficient of a dummy variable IS the odds
#                    ratio against the reference level. Quoting it here means
#                    Step 3's coefficients can be checked straight against this
#                    table - a marginal odds ratio and an adjusted one differ
#                    exactly by the confounding the model removes.
#
# The two are reported together on purpose. The odds ratio is insensitive to
# how common the outcome is and so ranks variables the way a model will, while
# the risk difference says whether that ranking matters in patients: a large
# odds ratio between two rare levels can still move very few people.
#
# The variables are ranked by risk difference below. Both measures ignore how
# many patients sit at each level, so the level sizes are carried in the table
# beside them - Section 3 already established that the smallest level here
# holds 549 patients, so no comparison rests on a thin cell.

risk_summary <- function(v) {
  rates <- tapply(data$has_heart_disease_num, data[[v]], mean)
  lowest <- names(which.min(rates))
  highest <- names(which.max(rates))
  p_low <- min(rates)
  p_high <- max(rates)
  data.frame(
    variable = v,
    n_levels = length(rates),
    safest_level = lowest,
    rate_safest = round(p_low, 3),
    riskiest_level = highest,
    rate_riskiest = round(p_high, 3),
    risk_difference = round(p_high - p_low, 3),
    odds_ratio = round((p_high / (1 - p_high)) / (p_low / (1 - p_low)), 2)
  )
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

association_strength <- do.call(rbind, lapply(categorical_all, risk_summary))
association_strength <-
  association_strength[order(-association_strength$risk_difference), ]
print(association_strength, row.names = FALSE)
write.csv(association_strength, "categorical_association_strength.csv",
  row.names = FALSE
)

# The table printed above carries every number quoted in the write-up: for each
# variable, its safest and riskiest level, the disease rate at each, and the two
# summary measures. Nothing below is quoted from anywhere else.
#
# exercise_induced_angina dominates every other categorical variable on both
# measures, and by a wide margin on each. Together with the heart-rate results
# of Section 4, this says the exercise stress test carries most of the
# information in the dataset.
#
# Two rankings are worth comment. chest_pain_type places well below the
# categories derived in Step 1, even though it is the variable a textbook
# heart-disease study leads with. And family_history is last: it does separate
# the groups, but by less than anything else here.
#
# Every variable whose levels have a natural order moves monotonically in the
# expected direction - the check that the data behaves sensibly. Printed rather
# than asserted, for the four ordered variables:
for (v in c("age_group", "bp_category", "glycemic_status", "smoker_status")) {
  cat("\n", v, ": ", sep = "")
  cat(paste0(
    levels(data[[v]]), " ",
    round(100 * tapply(data$has_heart_disease_num, data[[v]], mean), 1), "%"
  ), sep = " -> ")
  cat("\n")
}

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
ggsave("disease_rate_by_category.pdf", width = 12, height = 8)


# =============================================================================
# 6. Relationships among the predictors
# =============================================================================

correlation_matrix <- cor(data[numeric_all])
round(correlation_matrix, 2)

# The heatmap is ordered so that variables measuring the same physiological
# quantity sit next to each other, which is easier to read than the
# alphabetical order used in the Step 1 version of this figure. The grouping is
# written out by hand from the blocks Step 1 identified, rather than being
# discovered automatically, so the figure is reproducible and the reader can
# see exactly which variables the report claims belong together.
variable_order <- c(
  # blood pressure
  "resting_bp_systolic", "resting_bp_diastolic", "pulse_pressure",
  # lipid panel
  "cholesterol_total", "ldl", "non_hdl_cholesterol", "cholesterol_hdl_ratio",
  "hdl", "triglycerides",
  # glucose control
  "fasting_blood_sugar", "hba1c",
  # heart rate and the exercise test
  "age", "max_heart_rate_achieved", "heart_rate_reserve",
  "percent_predicted_max_hr", "resting_heart_rate", "st_depression",
  # body composition and lifestyle
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
  labs(
    title = "Pearson correlation between the numerical variables",
    subtitle = "Ordered so that variables measuring the same thing sit together",
    x = NULL, y = NULL
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("correlation_heatmap.pdf", width = 10, height = 9)

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
data |>
  select(
    age, max_heart_rate_achieved, resting_bp_systolic, ldl, hba1c
  ) |>
  pivot_longer(-age, names_to = "variable", values_to = "value") |>
  ggplot(aes(age, value)) +
  geom_point(alpha = 0.06, size = 0.5) +
  # A straight-line fit and a quadratic fit are drawn together. If the
  # relationship really is linear the two curves lie on top of each other; a
  # visible gap between them is the sign that a squared term in age would be
  # worth carrying into Step 3.
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    aes(colour = "linear: y ~ age")
  ) +
  geom_smooth(
    method = "lm", formula = y ~ poly(x, 2), se = FALSE,
    aes(colour = "quadratic: y ~ age + age^2")
  ) +
  scale_colour_manual(values = c(
    "linear: y ~ age" = "red",
    "quadratic: y ~ age + age^2" = "blue"
  )) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    title = "How age moves the other measurements",
    x = "Age (years)", y = NULL, colour = NULL
  ) +
  theme_classic() +
  theme(legend.position = "bottom")
ggsave("age_relationships.pdf", width = 9, height = 7)

# max_heart_rate_achieved falls almost linearly with age (r = -0.73), which is
# physiology, not disease. Since both age and peak heart rate are among the
# strongest correlates of the diagnosis, Step 3 has to fit them together - the
# marginal effect of either one on its own is partly the other one.
#
# The straight line and the quadratic are indistinguishable in all four panels,
# so age enters as a linear term; nothing here asks for a polynomial. The
# slopes themselves, in units per year of age:
for (v in c("max_heart_rate_achieved", "resting_bp_systolic", "ldl", "hba1c")) {
  fit <- lm(data[[v]] ~ data$age)
  cat(
    sprintf(
      "%-24s slope %+8.3f per year   R^2 %.3f\n",
      v, coef(fit)[2], summary(fit)$r.squared
    )
  )
}


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
# 4. Three variables are right skewed, and each wants a different rung of the
#    ladder of powers: a log for alcohol_units_per_week and
#    cholesterol_hdl_ratio, a cube root for st_depression. All three end up
#    with |skewness| < 0.35. Nothing else needs transforming, and transforming
#    triglycerides would make it worse.
# 5. The correlation blocks identified in Step 1 are confirmed here; one
#    variable per block goes into a model, not all of them.
