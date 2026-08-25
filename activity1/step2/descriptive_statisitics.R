library(tidyverse)
library(tseries)

data <- readRDS("../dataset/cleaned_data.rds")

# Step 1 stores the ordinal variables as bare integer codes ("1"/"2"/"3",
# "0"/"1"), which makes every axis below unreadable. Restore the original
# S/M/L, EN/MI/SE/EX and leadership labels (same level order, so nothing about
# the data changes except how the levels are printed).
source("../shared/factor_labels.R")
data <- apply_factor_labels(data)

print(str(data))


# Descriptive statisitic

print(summary(data$salary_in_usd))
ggplot(data, aes(salary_in_usd)) +
  geom_histogram(bins = 20)
ggsave("salary_histogram.pdf")


ggplot(data, aes(sqrt(salary_in_usd))) +
  geom_histogram(bins = 20)
ggsave("salary_histogram_sqrt.pdf")

# By observign the histogram, we see that there is a tendency of forming a the bell curve, although right skewed. To confirm it, we use the test for narmality

# QQ plots + normality tests for the raw and the square-root transformed salary.
ggplot(data, aes(sample = salary_in_usd)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical quantiles", y = "Salary in USD") +
  theme_classic()
ggsave("salary_qq.pdf")

ggplot(data, aes(sample = sqrt(salary_in_usd))) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical quantiles", y = "sqrt(Salary in USD)") +
  theme_classic()
ggsave("salary_sqrt_qq.pdf")

print(shapiro.test(data$salary_in_usd))
print(jarque.bera.test(data$salary_in_usd))
print(shapiro.test(sqrt(data$salary_in_usd)))
print(jarque.bera.test(sqrt(data$salary_in_usd)))

# For the raw salary both tests reject normality decisively (Shapiro-Wilk
# W = 0.970, p = 1.0e-09; Jarque-Bera p = 8.0e-08). After the square root the
# evidence is far weaker: W rises to 0.992 (p = 0.0036) and Jarque-Bera no longer
# rejects at 0.05 (p = 0.051). With n = 600 these tests are extremely sensitive -
# they detect deviations too small to matter in practice - so the QQ plots are the
# primary evidence here.
# The raw QQ plot bends away from the reference line at both tails, while the
# square-root QQ plot follows the line closely except at the clamped upper tail.
# The square root is therefore the better working scale, which is also what the
# Box-Cox analysis in Step 3 concludes.


ggplot(data, aes(salary_in_usd, experience_level)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Experience level") +
  theme_classic()
ggsave("salary_by_exp.pdf")

# There is a difference between workers of different experience level. From the boxplot, we see that the salary in used generally increases

ggplot(data, aes(salary_in_usd, employment_type)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Employment type") +
  theme_classic()
ggsave("salary_by_employment.pdf")

# There seem to be a large variance in the contract worker type. However, note that there are little observations in non-fulltime jobs, thus it is less accurate representation of the distirbution there.

# C

# work_year is kept numeric in Step 1 (it has to stay numeric for the Step 4
# forecast), so it must be turned into a factor *here* to get one labelled box
# per year. Mapping it with group = only dodges the boxes and leaves the axis
# showing the dodge offsets (-0.2 / 0.0 / 0.2) instead of the years.
ggplot(data, aes(salary_in_usd, as.factor(work_year))) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Work year") +
  theme_classic()
ggsave("salary_by_work_year.pdf")

# In the first two year, there are no difference, but the final year have visible difference in salary (incrases)


print(table(data$employee_residence))
ggplot(data, aes(salary_in_usd, employee_residence)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Employee residence") +
  theme_classic()
ggsave("salary_by_employee_residence.pdf")

# The observation group is fragmented and clutered in US. There are many 1-observation, thus we cannot conclude any on this
print(table(data$company_location))
ggplot(data, aes(salary_in_usd, company_location)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Company location") +
  theme_classic()
ggsave("salary_by_company_location.pdf")
# Similarly, there are fragmentation and clustering in company location, with heavilty unbalanced observation. Therefore we cannot conclude any on this

# The residence / location pair is almost perfectly collinear - see the cross-tab
# below, whose off-diagonal cells are nearly empty. Only one of the two is used
# as a factor in Step 3.
print(table(data$employee_residence, data$company_location))

ggplot(data, aes(salary_in_usd, company_size)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Company size") +
  theme_classic()

ggsave("salary_by_company_size.pdf")
# The small companies seem to offer loweest salary, while the medium companies offer highest salary, differs from the large companies' salary slightly.
ggplot(data, aes(salary_in_usd, leadership)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Leadership role") +
  theme_classic()
ggsave("salary_by_leadership_role.pdf")
# The boxplot shows those being at leader / management role has higher salary than those are not
ggplot(data, aes(salary_in_usd, role)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Role") +
  theme_classic()
ggsave("salary_by_role.pdf")

# There seem to be no signficant differences between mean salary of roles in data science.. However, those being analyst has its 3rd quartile being lower than the other. We can't conclude anything on the Unspecified roles, since its sample size is small and their role are not explicitly mentioned.
