# Data inspection
#
data <- read.csv("../dataset/ds_salaries.csv")
head(data)
str(data)
table(data$job_title)

boxplot(data$X)
boxplot(data$remote_ratio)
boxplot(data$salary)
boxplot(data$salary_in_usd)
boxplot(data$work_year)

duplicated(data)
# No duplication
sum(is.na(data$X))
sum(is.na(data$work_year))
sum(is.na(data$experience_level))
sum(is.na(data$employment_type))
sum(is.na(data$job_title))
sum(is.na(data$salary))
sum(is.na(data$salary_currency))
sum(is.na(data$salary_in_usd))
sum(is.na(data$employee_residence))
sum(is.na(data$remote_ratio))
sum(is.na(data$company_location))
sum(is.na(data$company_size))

# No NA value
