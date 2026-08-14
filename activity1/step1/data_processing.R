# Data inspection
#
library(tidyverse)
data <- read.csv("../dataset/ds_salaries.csv")

# First, we use head and str to get the sample of data and their types
head(data)
str(data)


# To unify the units, we will use the converesion of salary to USD and remove the salary in other units, as well as the unit
# The X column is also not being used since it seems like a No. numbering for each entry

data <- data |> select(!c("X", "salary", "salary_currency"))

str(data)

# Now we inpsect the data to find outliers

boxplot(data$salary_in_usd)
# The salary in usd has a few outliers according to the boxplot, but they are useful information on how the salary is evalutated, thsu we won't handle them

table(data$work_year)
# The data is seem to be collected in 2020 - 2022 period, thus the work year should remain as numerical value so as to predict the salary of the following years

table(data$remote_ratio)

# The remote ratio only has 3 value, how is it collected. Either way, this also should remain as numerical value
# In real life, the remote ration is calculated as day of remote work : total working day, which make the ratio may take other value than 0, 50 and 100
#

nrow(data[duplicated(data) == TRUE, ])
# 42 duplicated entries. This would not happen if the X value is not removed, which mean these value is likely independent and not a result of faulty collection.
# Therefore, the duplicated values should remain
#
# Check for NA
sum(is.na(data))
# No NA value

# Check for the job titles
table(data$job_title)
# The job title has many value with small frequencies, suggesting they are collected from how people call their working position
# We unify how the job title by string matching

# Chosen dispcipline: Artificial Intelligence, Machine Learning, Data
# Chosen roles in their discipline: scientist, engineer,
# Leadership: Is leader or not
data <- (data |> mutate(
  discipline = case_when(
    str_detect(job_title, "\\b(Data|ETL|Analytics|Scientist)\\b") ~ "Data Science",
    str_detect(job_title, "\\b(AI|Artificial Intelligence|Computer Vision|NLP)\\b") ~ "Artificial Intelligence",
    str_detect(job_title, "\\b(ML|Machine Learning)\\b") ~ "Machine Learning",
    .default = "Other"
  ),
  role = case_when(
    str_detect(job_title, "\\b(Engineer|Developer|Engineering|Architect|Specialist)\\b") ~ "Engineering",
    str_detect(job_title, "\\b(Scientist|Research|Researcher|Science)\\b") ~ "Research",
    str_detect(job_title, "\\b(Analyst|Analytics)\\b") ~ "Analyst",
    .default = "Unspecified"
  ),
  leadership = case_when(
    .default = FALSE,
    str_detect(job_title, "\\b(Lead|Director|Manager|Principal|Head|Staff)\\b") ~ TRUE
  )
))

# Check for non-classified roles
data |> select(job_title, discipline, role, leadership)
# Clearing the job_title
data <- data |> mutate(job_title = NULL)

# Inspect the distribution of roles

table(data$role)
table(data$leadership)
table(data$discipline)
table(data$discipline, data$role)

# The data shows that the job categories leans toward the non-management roles in data science field, where most of the job title is different roles in data science. The number of data in other field is much less.

# Factorize the variables
str(data)

data <- data |>
  mutate(company_size = case_when(
    company_size == "S" ~ 1,
    company_size == "M" ~ 2,
    company_size == "L" ~ 3,
    .default = 0
  )) |>
  mutate(leadership = case_when(
    leadership == FALSE ~ 0,
    leadership == TRUE ~ 1,
    .default = 0
  )) |>
  mutate(experience_level = case_when(
    experience_level == "EN" ~ 0,
    experience_level == "MI" ~ 1,
    experience_level == "SE" ~ 2,
    experience_level == "EX" ~ 3,
    .default = -1
  ))


str(data)

data <- data |>
  mutate(experience_level = as.factor(experience_level)) |>
  mutate(employment_type = as.factor(employment_type)) |>
  mutate(employee_residence = as.factor(employee_residence)) |>
  mutate(company_location = as.factor(company_location)) |>
  mutate(company_size = as.factor(company_size)) |>
  mutate(discipline = as.factor(discipline)) |>
  mutate(role = as.factor(role)) |>
  mutate(leadership = as.factor(leadership))
str(data)

# Standardizing independent numerical variables

data <- data |>
  mutate(standardized_year = (work_year - mean(work_year)) / sqrt(var(work_year))) |>
  mutate(standardized_remote_ratio = (remote_ratio - mean(remote_ratio)) / sqrt(var(remote_ratio)))

str(data)
