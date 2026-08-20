library(tidyverse)
library(tseries)

data <- readRDS("../dataset/cleaned_data.rds")
data
str(data)


# Descriptive statisitic

summary(data$salary_in_usd)
ggplot(data, aes(salary_in_usd)) +
  geom_histogram(bins = 20)
ggsave("salary_hisotogram.pdf")


ggplot(data, aes(salary_in_usd)) +
  geom_boxplot()
# By observign the histogram, we see that there is a tendency of forming a the bell curve. To confirm it, we use the test for narmality
shapiro.test(data$salary_in_usd)
jarque.bera.test(data$salary_in_usd)

# Both test gives p-value very small, so we conclude the salary is not normally distirbuted

ggplot(data, aes(salary_in_usd, experience_level)) +
  geom_boxplot() +
  theme_classic()
ggsave("salary_by_exp.pdf")

# There is a difference between workers of different experience level. From the boxplot, we see that the salary in used generally increases

ggplot(data, aes(salary_in_usd, employment_type)) +
  geom_boxplot() +
  theme_classic()
ggsave("salary_by_employment.pdf")

# There seem to be a large variance in the contract worker type. However, note that there are little observations in non-fulltime jobs, thus it is less accurate representation of the distirbution there.

# C

ggplot(data, aes(salary_in_usd, group = work_year)) +
  geom_boxplot() +
  theme_classic()
ggsave("salary_by_work_year.pdf")

# In the first two year, there are no difference, but the final year have visible difference in salary (incrases)


table(data$employee_residence)
ggplot(data, aes(salary_in_usd, employee_residence)) +
  geom_boxplot() +
  theme_classic()
ggsave("salary_by_employee_residence.pdf")

# The observation group is fragmented and clutered in US. There are many 1-observation, thus we cannot conclude any on this
table(data$company_location)
ggplot(data, aes(salary_in_usd, company_location)) +
  geom_boxplot() +
  theme_classic()
ggsave("salary_by_company_location.pdf")
# Similarly, there are fragmentation and clustering in company location, with heavilty unbalanced observation. Therefore we cannot conclude any on this
ggplot(data, aes(salary_in_usd, company_size)) +
  geom_boxplot() +
  theme_classic()

ggsave("salary_by_company_size.pdf")
# The small companies seem to offer loweest salary, while the medium companies offer highest salary, differs from the large companies' salary slightly.
ggplot(data, aes(salary_in_usd, leadership)) +
  geom_boxplot() +
  theme_classic()
ggsave("salary_by_leadership_role.pdf")
# The boxplot shows those being at leader / management role has higher salary than those are not
ggplot(data, aes(salary_in_usd, role)) +
  geom_boxplot() +
  theme_classic()
ggsave("salary_by_role.pdf")

# There seem to be no signficant differences between mean salary of roles in data science.. However, those being analyst has its 3rd quartile being lower than the other. We can't conclude anything on the Unspecified roles, since its sample size is small and their role are not explicitly mentioned.
