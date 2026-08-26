# =============================================================================
# Step 2 - Descriptive statistics and EDA
# =============================================================================
# WHAT CHANGED IN THIS REVISION (see report/review-step23.md for the evidence):
#
# 1. The response analysed here is now `observed_salary_in_usd`, the UNTOUCHED
#    salary, not `salary_in_usd`, which Step 1 winsorised at the 1.5*IQR fences.
#    Reason: the winsorising collapses 10 rows (1.7% of 600) onto one identical
#    cap value and cuts the SD by 12.6%. Every normality statement in the old
#    version of this file was therefore a statement about a variable we created,
#    not about salaries. Step 4 already models the observed response, so this
#    also puts Steps 2, 3 and 4 back on the same response.
#    The winsorised column is still compared against the observed one below, so
#    the report can show the effect of the cleaning choice instead of hiding it.
#
# 2. Several comments described the data as it looked BEFORE Step 1 collapsed
#    it (fragmented country codes, a "Contract" employment type, an
#    "Unspecified" role). Those categories do not exist in cleaned_data.rds.
#    They have been rewritten to describe the two-level factors we actually plot.
#
# 3. `remote_ratio` is now plotted. The assignment brief lists it as a required
#    EDA variable and it was missing entirely.
# =============================================================================

# ggplot2 is the only part of the tidyverse this file needs (the data handling
# here is all base R), so it is loaded on its own. Step 3 does the same.
# tseries supplies jarque.bera.test.
library(ggplot2)
library(tseries)

data <- readRDS("../dataset/cleaned_data.rds")

print(str(data))

# `salary` is the analysis response for the whole file: the observed, uncapped
# salary in USD. Defined once here so there is a single place to change it.
data$salary <- data$observed_salary_in_usd


# -----------------------------------------------------------------------------
# Effect of the Step 1 winsorising (kept so the report can justify the choice)
# -----------------------------------------------------------------------------
cat("\n--- Winsorised vs observed response ---\n")
cat("rows altered by the 1.5*IQR clamp:", sum(data$salary != data$salary_in_usd), "\n")
cat("observed max:", max(data$salary), " clamp value:", max(data$salary_in_usd), "\n")
cat("sd observed:", sd(data$salary), " sd winsorised:", sd(data$salary_in_usd), "\n")
# 10 rows are clamped, the 600,000 USD maximum is pulled down to 280,911, and
# the SD falls from 70,852 to 61,901. Everything below uses the observed column.


# -----------------------------------------------------------------------------
# Descriptive statistics and the distribution of the response
# -----------------------------------------------------------------------------

print(summary(data$salary))

ggplot(data, aes(salary)) +
  geom_histogram(bins = 20) +
  labs(x = "Salary in USD", y = "Count") +
  theme_classic()
ggsave("salary_histogram.pdf")

ggplot(data, aes(sqrt(salary))) +
  geom_histogram(bins = 20) +
  labs(x = "sqrt(Salary in USD)", y = "Count") +
  theme_classic()
ggsave("salary_histogram_sqrt.pdf")

# The raw histogram is clearly right skewed. The square root pulls the long
# right tail in and the shape becomes roughly symmetric. QQ plots and formal
# tests below.

ggplot(data, aes(sample = salary)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical quantiles", y = "Salary in USD") +
  theme_classic()
ggsave("salary_qq.pdf")

ggplot(data, aes(sample = sqrt(salary))) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical quantiles", y = "sqrt(Salary in USD)") +
  theme_classic()
ggsave("salary_sqrt_qq.pdf")

print(shapiro.test(data$salary))
print(jarque.bera.test(data$salary))
print(shapiro.test(sqrt(data$salary)))
print(jarque.bera.test(sqrt(data$salary)))

# Sample skewness, which is easier to read than the test statistics.
skewness <- function(x) mean((x - mean(x))^3) / sd(x)^3
cat("skewness raw :", skewness(data$salary), "\n")
cat("skewness sqrt:", skewness(sqrt(data$salary)), "\n")

# BOTH tests still reject normality after the square root (Shapiro W = 0.987,
# p = 4.5e-05). We do NOT claim the square root makes salary normal - the
# earlier draft of this file made that claim, but only because it was testing
# the winsorised column, whose tail had been cut off (there, W = 0.992 and
# Jarque-Bera sat right on the boundary at p = 0.051).
#
# What the square root does achieve is what actually matters for Step 3: it
# removes the skew. Skewness falls from about +1.68 to about +0.19, i.e. from
# strongly right skewed to near symmetric, and the QQ plot follows the reference
# line over the whole central range and departs only in the extreme upper tail.
# With n = 600 the F test in Step 3 is robust to a residual departure of this
# size. The transform is justified by Box-Cox in Step 3 (lambda ~= 0.4 on the
# observed response), not by a normality test that passes.


# -----------------------------------------------------------------------------
# Salary against each factor
# -----------------------------------------------------------------------------

ggplot(data, aes(salary, experience_level)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Experience level") +
  theme_classic()
ggsave("salary_by_exp.pdf")

# Salary rises steadily across the four experience levels. This is the clearest
# monotone pattern in the whole EDA and Step 3 confirms it (F = 72, p < 2.2e-16
# in the experience x role model).

print(table(data$employment_type))
ggplot(data, aes(salary, employment_type)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Employment type") +
  theme_classic()
ggsave("salary_by_employment.pdf")

# NOTE: Step 1 collapsed employment_type to two levels, Fulltime (581) and
# Other (19), so this plot shows two boxes, not the original FT/PT/CT/FL.
# "Other" pools part time, contract and freelance. Its box is wide, but with 19
# observations that spread is mostly sampling noise, so employment_type is not
# carried into the Step 3 ANOVA models.

# work_year is kept numeric in Step 1 (it has to stay numeric for the Step 4
# forecast), so it must be turned into a factor *here* to get one labelled box
# per year. Mapping it with group = only dodges the boxes and leaves the axis
# showing the dodge offsets (-0.2 / 0.0 / 0.2) instead of the years.
ggplot(data, aes(salary, as.factor(work_year))) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Work year") +
  theme_classic()
ggsave("salary_by_work_year.pdf")

# 2020 and 2021 look alike; 2022 is visibly higher. Step 3 tests whether that
# rise is real once company location, company size and role are accounted for.

# The remote work ratio is a required EDA variable in the brief. Step 1 keeps it
# numeric (0 / 50 / 100), so it is factored here for one box per tier.
print(table(data$remote_ratio))
ggplot(data, aes(salary, as.factor(remote_ratio))) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Remote work ratio (%)") +
  theme_classic()
ggsave("salary_by_remote_ratio.pdf")

# The three tiers hold 127 / 96 / 377 observations. The middle tier (50%) looks
# much lower paid than the other two, but that is a confound, not an effect:
# the cross-tab below shows the 50% group is 79% non-US, and location is the
# strongest salary driver in this dataset. Step 3 fits remote_ratio jointly with
# company_location and finds no remote effect once location is held fixed.
print(table(data$remote_ratio, data$company_location,
  dnn = c("remote_ratio", "company_location")
))

print(table(data$employee_residence))
ggplot(data, aes(salary, employee_residence)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Employee residence") +
  theme_classic()
ggsave("salary_by_employee_residence.pdf")

print(table(data$company_location))
ggplot(data, aes(salary, company_location)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Company location") +
  theme_classic()
ggsave("salary_by_company_location.pdf")

# NOTE: Step 1 collapsed both geographic variables to US / Other, so these are
# two-level factors with 331 / 269 and 353 / 247 observations. An earlier draft
# of this file said the groups were "fragmented" with "many 1-observation"
# categories and concluded that nothing could be said about location - that
# described the raw ISO country codes, before the collapse, and it is wrong for
# what is plotted here.
# Both boxplots show a large, well supported US premium, and Step 3 finds
# company_location to be the single strongest effect in the entire analysis
# (F = 292, p < 2.2e-16, a premium of roughly 67,000-85,000 USD in every role).

# The residence / location pair is almost perfectly collinear - see the cross-tab
# below, whose off-diagonal cells are nearly empty (US resident at a non-US
# company: 2 observations). Only one of the two is used as a factor in Step 3.
print(table(data$employee_residence, data$company_location,
  dnn = c("employee_residence", "company_location")
))

ggplot(data, aes(salary, company_size)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Company size") +
  theme_classic()
ggsave("salary_by_company_size.pdf")

# Small companies pay least. Medium and large look similar to each other, and
# Step 3 confirms that medium vs large is never significant.

ggplot(data, aes(salary, leadership)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Leadership role") +
  theme_classic()
ggsave("salary_by_leadership_role.pdf")

# Leadership / management titles are paid clearly more than non-leadership ones.

print(table(data$role))
ggplot(data, aes(salary, role)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Role") +
  theme_classic()
ggsave("salary_by_role.pdf")

# NOTE: there is no "Unspecified" role on this plot. Step 1 drops the rows whose
# job title could not be matched to one of the three disciplines (seven
# management titles such as "Head of Data"), so only Analyst (127),
# Engineering (248) and Research (225) remain. That selective loss is a real
# limitation of the study and is recorded in the Step 4 audit, but it is not
# something this boxplot can show.
# Analyst sits below the other two; Engineering and Research are close. Step 3
# confirms exactly that ordering (Analyst is significantly below both; the
# Engineering vs Research gap is never significant).
