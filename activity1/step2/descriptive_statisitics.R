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

ggplot(data, aes(salary_in_usd, employment_type)) +
  geom_boxplot() +
  theme_classic()
ggplot(data, aes(salary_in_usd, work_year)) +
  geom_point() +
  theme_classic()
ggplot(data, aes(salary_in_usd, employee_residence)) +
  geom_boxplot() +
  theme_classic()
ggplot(data, aes(salary_in_usd, company_location)) +
  geom_boxplot() +
  theme_classic()
ggplot(data, aes(salary_in_usd, company_size)) +
  geom_boxplot() +
  theme_classic()
ggplot(data, aes(salary_in_usd, discipline)) +
  geom_boxplot() +
  theme_classic()
ggplot(data, aes(salary_in_usd, leadership)) +
  geom_boxplot() +
  theme_classic()
ggplot(data, aes(salary_in_usd, role)) +
  geom_boxplot() +
  theme_classic()
