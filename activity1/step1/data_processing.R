# Data inspection
#
library(tidyverse)
data <- read.csv("../dataset/ds_salaries.csv")
set.seed(6767)

# First, we use head and str to get the sample of data and their types
head(data)
str(data)


# To unify the units, we use salary in USD and remove salary in other units.
# Preserve the source row ID, observed response, and original category codes so
# Step 4 can split first and learn all preprocessing from training data. The
# existing collapsed variables remain below for the Step 2/3 analyses.
data <- data |>
  rename(source_row_id = X) |>
  mutate(
    observed_salary_in_usd = salary_in_usd,
    employment_type_code = employment_type,
    employee_residence_code = employee_residence,
    company_location_code = company_location
  ) |>
  dplyr::select(!c("salary", "salary_currency"))

str(data)

# Now we inspect the data for potential outliers.

nrow(data)
ggplot(data, aes(salary_in_usd)) +
  geom_boxplot()
# The legacy Step 2/3 response is winsorized at the 1.5-IQR limits. The
# untouched observed response is retained in observed_salary_in_usd and is the
# only response used by Step 4, so the held-out outcomes are not overwritten.


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

duplicate_check <- data |> dplyr::select(!source_row_id)
nrow(duplicate_check[duplicated(duplicate_check) == TRUE, ])
# There are 42 exact duplicate substantive records after ignoring the source
# row ID. We cannot tell whether they are collection duplicates or repeated
# observations, so they remain in the descriptive data. Step 4 assigns
# identical modeling records to the same partition to prevent train/test twins.
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


# Seven management titles cannot be assigned to one of the three functional
# disciplines by this rule (five Head of Data, one Head of Machine Learning,
# and one Machine Learning Manager). They are removed for compatibility with
# the existing Step 2/3 role analysis; this selective loss is documented as a
# remaining limitation in the Step 4 audit.
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

# The retained functional categories are reasonably represented: Analyst 127,
# Engineering 248, and Research 225. Leadership is modeled separately because
# management seniority and functional discipline capture different information.

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
  mutate(experience_level = factor(experience_level, ordered = TRUE, labels = c("Entry", "Mid-level", "Senior", "Executive"))) |>
  mutate(employment_type = as.factor(employment_type)) |>
  mutate(employee_residence = as.factor(employee_residence)) |>
  mutate(company_location = as.factor(company_location)) |>
  mutate(company_size = factor(company_size, ordered = TRUE, labels = c("Small", "Medium", "Large"))) |>
  mutate(role = as.factor(role)) |>
  mutate(leadership = factor(leadership, ordered = TRUE, labels = c("No", "Yes")))

# Legacy full-data standardized copies retained for Steps 2/3 compatibility.
# Step 4 explicitly excludes these columns and estimates scaling from training
# data only.

data <- (data |>
  mutate(standardized_year = (work_year - mean(work_year)) / sqrt(var(work_year))) |>
  mutate(standardized_remote_ratio = (remote_ratio - mean(remote_ratio)) / sqrt(var(remote_ratio))))

str(data)
saveRDS(data, "../dataset/cleaned_data.rds")
