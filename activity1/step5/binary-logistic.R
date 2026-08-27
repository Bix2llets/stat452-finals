logistic_data <- readRDS("../dataset/cleaned_data.rds") # Change the directory to the cleaned_data.rds
library(tidyverse)

#
cutoff <- quantile(logistic_data$salary_in_usd, 0.75)
logistic_data <- logistic_data |> mutate(
  is_top_tier = salary_in_usd >= cutoff
)

# Given the employee's working roles, seniority and working environment, what is the probability that the professional enter the top 25% salary bracket
# Inspection
head(
  logistic_data |>
    count(is_top_tier, experience_level, leadership, role, company_size, employment_type, company_location) |>
    arrange(desc(is_top_tier), desc(n), desc(experience_level), desc(leadership)) |>
    filter(is_top_tier == TRUE) # Sorts to show the most common combinations first
  , 10
)

# Top 10 combination of top_tier salary: Senior at medium-large company in any role, only one with leadership, then the executive and mid level in engineering and research. Company_size are all medium and large. They are all fulltime employee at US
#

head(
  logistic_data |>
    count(is_top_tier, experience_level, leadership, role, company_size, employment_type, company_location) |>
    arrange(desc(is_top_tier), desc(n), desc(experience_level), desc(leadership)) |>
    filter(is_top_tier == FALSE), 10
)
# Also medium and large companies. mostly at senior and mid-level, wihout leadership, fulltime employee at equal rate between US and other locations of company

# Create model
role_senior_env_model <- glm(is_top_tier ~ experience_level + leadership + role + company_size + employment_type + company_location, family = "binomial", data = logistic_data)

summary(role_senior_env_model)
role_senior_env_model
levels(logistic_data$role)
levels(logistic_data$company_location)
levels(logistic_data$employment_type)
levels(logistic_data$leadership)
levels(logistic_data$experience_level)
levels(logistic_data$company_size)

exp(role_senior_env_model$coefficients)
exp(confint(role_senior_env_model))

# The model give that experienece level, leadership, role, company size and company location affect the odds of reaching top 25% of salary, while the employement does not affect. In those elements that affects, only the linear change (i.e. transitioning to the next larger value) significantly affect the odds.
# As the seniority increases, the likelihood of falling into the top 25% salary bracket increases by 10.81 times
# Being at research role multiplies the odds of falling tinto the top 25% salary bracket to almost 2 times, while being at engineering role multiplies the odds by almost 0.29
# As the company size increases, the odds of having salaries falling into top 25% multiplies by 2.18
# The odds of having top tier salary when working at US companies is 16.41 more than working at non-US companies.
#
#
# We will try to remove the variable employement_type, as its p-value is quite high: above 0.2
