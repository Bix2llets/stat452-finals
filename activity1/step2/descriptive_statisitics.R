library(ggplot2)
library(tseries)

data <- readRDS("activity1/dataset/cleaned_data.rds")

print(str(data))

# `salary` is the analysis response for the whole file: the observed, uncapped
# salary in USD. Defined once here so there is a single place to change it.
data$salary <- data$observed_salary_in_usd

# -----------------------------------------------------------------------------
# Descriptive statistics and the distribution of the response
# -----------------------------------------------------------------------------

print(summary(data$salary_in_usd))
ggplot(data, aes(salary)) +
  geom_histogram(bins = 20) +
  labs(x = "Salary in USD", y = "Count") +
  theme_classic()
ggsave("pre_salary_histogram.pdf")

ggplot(data, aes(salary_in_usd)) +
  geom_histogram(bins = 20) +
  labs(x = "Salary in USD", y = "Count") +
  theme_classic()
ggsave("salary_histogram.pdf")

ggplot(data, aes(sqrt(salary_in_usd))) +
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

skewness <- function(x) mean((x - mean(x))^3) / sd(x)^3
cat("skewness raw :", skewness(data$salary), "\n")
cat("skewness sqrt:", skewness(sqrt(data$salary)), "\n")

# The transformation reduces the skew from 16.8 to 0.19


# -----------------------------------------------------------------------------
# Salary against each factor
# -----------------------------------------------------------------------------

ggplot(data, aes(salary, experience_level)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Experience level") +
  theme_classic()
ggsave("salary_by_exp.pdf")

# Salary rises steadily across the four experience levels.
print(table(data$employment_type))
ggplot(data, aes(salary, employment_type)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Employment type") +
  theme_classic()
ggsave("salary_by_employment.pdf")
# Fulltime employee has greater salary in average:

ggplot(data, aes(salary, as.factor(work_year))) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Work year") +
  theme_classic()
ggsave("salary_by_work_year.pdf")

# An increase in 2022, its mean salary is different from those in 2021 and 2020

print(table(data$remote_ratio))
ggplot(data, aes(salary, as.factor(remote_ratio))) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Remote work ratio (%)") +
  theme_classic()
ggsave("salary_by_remote_ratio.pdf")

# Fully remote and fully on site companies pays more, hybrid working pay the least

print(table(data$remote_ratio, data$company_location,
  dnn = c("remote_ratio", "company_location")
))

print(table(data$employee_residence))
ggplot(data, aes(salary, employee_residence)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Employee residence") +
  theme_classic()
ggsave("salary_by_employee_residence.pdf")

# We can conclude that those living in US continent get paid more than other
print(table(data$company_location))
ggplot(data, aes(salary, company_location)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Company location") +
  theme_classic()
ggsave("salary_by_company_location.pdf")

# We can conclude that those working in US continent get paid more than others

print(table(data$employee_residence, data$company_location,
  dnn = c("employee_residence", "company_location")
))

ggplot(data, aes(salary, company_size)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Company size") +
  theme_classic()
ggsave("salary_by_company_size.pdf")

# Small companies pay least. Medium and large look similar to each other, but large companies' payment has more variance
# Step 3 confirms that medium vs large is never significant.

ggplot(data, aes(salary, leadership)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Leadership role") +
  theme_classic()
ggsave("salary_by_leadership_role.pdf")

# Leadership / management titles are paid more than non-leadership ones.

print(table(data$role))
ggplot(data, aes(salary, role)) +
  geom_boxplot() +
  labs(x = "Salary in USD", y = "Role") +
  theme_classic()
ggsave("salary_by_role.pdf")

# The plots shows that the mean salary of three roles are the same. However, the research and engineering role has their 3rd quartile larger than the analyst role
