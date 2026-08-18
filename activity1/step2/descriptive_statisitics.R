library(tidyverse)
library(tseries)

data <- readRDS("../dataset/cleaned_data.rds")
data
str(data)


# Descriptive statisitic

summary(data$salary_in_usd)
ggplot(data, aes(salary_in_usd)) +
  geom_histogram(bins = 20)


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

table(data$employment_type)
ggplot(data, aes(salary_in_usd, employment_type)) +
  geom_boxplot() +
  theme_classic()

# There seem to be a large variance in the contract worker type. However, note that there are little observations in non-fulltime jobs, thus it is less accurate representation of the distirbution there.

# C

ggplot(data, aes(salary_in_usd, group = work_year)) +
  geom_boxplot() +
  theme_classic()

# In the first two year, there are no difference, but the final year have visible difference in salary (incrases)


ggplot(data, aes(salary_in_usd, employee_residence)) +
  geom_boxplot() +
  theme_classic()

# The observation group is fragmented and clutered in US. There are many 1-observation, thus we cannot conclude any on this
table(data$company_location)
ggplot(data, aes(salary_in_usd, company_location)) +
  geom_boxplot() +
  theme_classic()
# Similarly, there are fragmentation and clustering in company location, with heavilty unbalanced observation. Therefore we cannot conclude any on this
ggplot(data, aes(salary_in_usd, company_size)) +
  geom_boxplot() +
  theme_classic()

# The small companies seem to offer loweest salary, while the medium companies offer highest salary, differs from the large companies' salary slightly.
ggplot(data, aes(salary_in_usd, leadership)) +
  geom_boxplot() +
  theme_classic()
# The boxplot shows those being at leader / management role has higher salary than those are not
ggplot(data, aes(salary_in_usd, role)) +
  geom_boxplot() +
  theme_classic()

# There seem to be no signficant differences between roles in data science.
