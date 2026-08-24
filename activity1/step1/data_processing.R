# Data inspection
#
library(tidyverse)
data <- read.csv("../dataset/ds_salaries.csv")
set.seed(6767)

# First, we use head and str to get the sample of data and their types
head(data)
str(data)


# To unify the units, we will use the converesion of salary to USD and remove the salary in other units, as well as the unit
# The X column is also not being used since it seems like a No. numbering for each entry
data <- data |> dplyr::select(!c("X", "salary", "salary_currency"))

str(data)

# Now we inpsect the data to find outliers

nrow(data)
ggplot(data, aes(salary_in_usd)) +
  geom_boxplot()
# The salary in usd has a few outliers according to the boxplot. They only take around 1% of observation but having much larger value (from 3e5 to 6e5, while the mean is at 1e5), thus we should clamp them to 1.5 of IQR range.


handle_outlier <- function(data) { # Handle outlier by clamping them in 1.5 IQR
  data_quantile <- quantile(data, c(0.25, 0.75))

  iqr <- data_quantile[2] - data_quantile[1]
  lower_bound <- data_quantile[1] - 1.5 * iqr
  upper_bound <- data_quantile[2] + 1.5 * iqr
  pmax(lower_bound, pmin(data, upper_bound))
}
data <- data |>
  mutate(salary_in_usd = handle_outlier(salary_in_usd))

ggplot(data, aes(salary_in_usd)) +
  geom_boxplot()
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

# Chosen roles in their discipline: scientist, engineer,
# Leadership: Is leader or not
data <- (data |> mutate(
  role = case_when(
    str_detect(job_title, "\\b(Engineer|Developer|Engineering|Architect|Specialist)\\b") ~ "Engineering",
    str_detect(job_title, "\\b(Scientist|Research|Researcher|Science)\\b") ~ "Research",
    str_detect(job_title, "\\b(Analyst|Analytics)\\b") ~ "Analyst",
    .default = NA
  ),
  leadership = case_when(
    .default = FALSE,
    str_detect(job_title, "\\b(Lead|Director|Manager|Principal|Head|Staff)\\b") ~ TRUE
  ),
  employment_type = case_when(
    .default = "Other",
    str_detect(employment_type, "FT") ~ "Fulltime"
  ),
  company_location = case_when(
    .default = "Other",
    str_detect(company_location, "US") ~ "US"
  ),
  employee_residence = case_when(
    .default = "Other",
    str_detect(employee_residence, "US") ~ "US"
  )
))

# The roles of those having NA is those who have job title of "Head of something". It mean there role are unclear, only know that they are leader. Therefore, we decided to give their role as NA then filter them out, as they are missing value and they only take a small proporiton of the whole observation
#
# Check for non-classified roles
data |> dplyr::select(job_title, role, leadership)
data |> filter(is.na(role) == TRUE)
data <- data |> filter(is.na(role) == FALSE)

# Clearing the job_title
data <- data |> mutate(job_title = NULL)

# Inspect the distribution of roles

table(data$role)
table(data$leadership)

# The data shows that the job categories leans toward the non-management roles in data science field, where most of the job title is different roles in data science. The number of data in other field is much less.
# There are also large bias in collected data toward the data science role, where the number of observation there is almost 10 times the second largest (540 and 56). This suggest splitting them by discipline is not a good idea.
# The factor of discipline is thus put at question for its ability to explain the variance in salary
# Moreover, since the collected data is named "Data Science Job Salaries", it further reinforce the idea that the variance in job title is actually how people perceive their job to be.
# Therefore, we conclude that grouping the disciplines based on the job title is not correct.

# Factorize the variables

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

data <- data |>
  mutate(experience_level = as.factor(experience_level)) |>
  mutate(employment_type = as.factor(employment_type)) |>
  mutate(employee_residence = as.factor(employee_residence)) |>
  mutate(company_location = as.factor(company_location)) |>
  mutate(company_size = as.factor(company_size)) |>
  mutate(role = as.factor(role)) |>
  mutate(leadership = as.factor(leadership))

# Standardizing independent numerical variables

data <- data |>
  mutate(standardized_year = (work_year - mean(work_year)) / sqrt(var(work_year))) |>
  mutate(standardized_remote_ratio = (remote_ratio - mean(remote_ratio)) / sqrt(var(remote_ratio)))

str(data)
saveRDS(data, "../dataset/cleaned_data.rds")
