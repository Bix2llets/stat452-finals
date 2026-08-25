library(tidyverse)
library(car)
library(ggfortify)
library(MASS)
library(lmtest)
library(emmeans)


data <- readRDS("../dataset/cleaned_data.rds")
set.seed(6767)
options(contrasts = c("contr.sum", "contr.poly"))


# Cannot do > 2 factors, since it results in some 0 observation, which make it unable to check for interaction
draw_interaction <- function(x, line, response) {
  print(table(x, line))

  # 2. Create a temporary dataframe for ggplot
  plot_df <- data.frame(
    x_factor = as.factor(x),
    trace_factor = as.factor(line),
    salary = response # Assumes 'data' exists in the global environment
  )

  # 3. Generate the ggplot
  ggplot(plot_df, aes(x = x_factor, y = salary, group = trace_factor, color = trace_factor)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1) +
    stat_summary(fun = mean, geom = "point", size = 2.5) +
    labs(
      x = "X Factor",
      y = "Mean Salary in USD",
      color = "Trace Factor"
    ) +
    theme_classic()
}
check_assumption <- function(model) {
  print(shapiro.test(residuals(model)))
  print(bptest(model))
}

# Since the data is randomly collected fromm people, we don't need the Durbin Watson test
plot_diagnostic <- function(model) {
  par(mfrow = c(2, 2))
  plot(model)
  par(mfrow = c(1, 1))
}
draw_interaction(x = data$company_size, line = data$leadership, response = data$salary_in_usd)
model <- lm(salary_in_usd ~ company_size * leadership, data = data)
plot_diagnostic(model)
check_assumption(model)
boxcox(model)

# Not normal, boxcox suggest square root

draw_interaction(x = data$company_size, line = data$leadership, response = sqrt(data$salary_in_usd))
model <- lm(sqrt(salary_in_usd) ~ company_size * leadership, data = data)
Anova(model, type = 3)
check_assumption(model)
emmeans(model, ~ company_size * leadership)
pairs(regrid(emmeans(model, ~ company_size * leadership)), simple = "leadership", by = "company_size")
pairs(regrid(emmeans(model, ~ company_size * leadership)), simple = "company_size", by = "leadership")

#  Using the significant level of 0.05
#  There is a difference of salary between leadership and non leadership roles in companies, where those without leadership have their salaries less than those having leadership. At small scale, it is 73500 USD difference, 36953 USD difference at medium scale, and finally 62488 USD difference at large scale.
#  However, only the salary of non-leadership roles between small and other company scales are different, by 46072 and 34316 USD for medium and large scales. There are no difference between the non-leadership roles salary of medium scale and large scale. Also, the leadership role at any company scale receive similar salaries.
#  Therefore, we conclude that the leadership roles' salary is consistent at any company scale, is larger than non-leadership roles. For non-leadership role, their salary in medium company and large company is the same.

# The Box-cox also suggest that we should transform the salary_in_usd by square root, as the distribution of the square root of the salary is closer to the normal distribution and the normality of the residual is better

# The model violates the assumption of normality

draw_interaction(x = data$company_size, line = data$role, response = sqrt(data$salary_in_usd))
model <- lm(sqrt(salary_in_usd) ~ company_size * role, data = data)
car::Anova(model, type = 3)
plot_diagnostic(model)
check_assumption(model)
# The normality  check is passed at signifcance level of 0.05, however, the residual is not homoscedastic.
# This is caused by the salary at large company size having larger variance than the salary at other company sizes

pairs(regrid(emmeans(model, ~ company_size * role)), simple = "role", by = "company_size")
pairs(regrid(emmeans(model, ~ company_size * role)), simple = "company_size", by = "role")

# There are almost no salary difference between roles in each company size: The only difference is between data analyst and data engineering roles at small company
# Similar to the conclusion before, for each role, the significant differences in salary between companies is between small and medium or small and large companies. Between medium and large companies, there are no significance differences.
#
draw_interaction(x = data$company_size, line = data$experience_level, response = sqrt(data$salary_in_usd))
model <- lm(sqrt(salary_in_usd) ~ company_size * experience_level, data = data)
plot_diagnostic(model)
check_assumption(model)
# Observation on the scale-location graph shows fairly stable residual
car::Anova(model, type = 3) # Interaction shows near 0.05 p-value, but greater. Hence
pairs(regrid(emmeans(model, ~ company_size * experience_level)), simple = "company_size", group = "experience_level")
pairs(regrid(emmeans(model, ~ company_size * experience_level)), simple = "experience_level", group = "company_size")
# P value adjustment: tukey method for comparing a family of 4 estimates
# Only experienece level 1, or Junior, has significant difference in salary between company scales
# At any company scale, there are differences between Entry level and Senior, Mid level and Seniror, and Mid-level and Executive. No different between senior and executive, suggest
draw_interaction(x = data$company_size, line = data$company_location, response = (sqrt(data$salary_in_usd)))
model <- lm(sqrt(salary_in_usd) ~ company_size * company_location, data = data)
check_assumption(model)
plot_diagnostic(model)
# The visual line is almost flat, contrary to the test. We choose to trust the plot
car::Anova(model, type = 3)
# An interaction

pairs(regrid(emmeans(model, ~ company_size * company_location)), simple = "company_location", group = "company_size")
pairs(regrid(emmeans(model, ~ company_size * company_location)), group = "company_location", simple = "company_size")
# For each company size, there is a significant difference between the salary of US companies and non-US companies
# In the US, company give difference salary based on their scale: small company pay less than medium and large companies. In non-US countries, there are no difference

# Does the employee resident and where they work interacts.
draw_interaction(x = data$employee_residence, line = data$company_location, response = sqrt(data$salary_in_usd))
model <- lm(sqrt(salary_in_usd) ~ employee_residence * company_location, data = data)
check_assumption(model)
plot_diagnostic(model)
# residual line flat contrary to the test result 0.007

Anova(model, type = 3)
# There are no interaction, as the interaction term and the company_location both has high p-value.


draw_interaction(x = data$experience_level, line = data$leadership, response = sqrt(data$salary_in_usd))
# No interaction, since no leadership is at entry level

draw_interaction(x = data$experience_level, line = data$role, response = data$salary_in_usd)
model <- lm(sqrt(salary_in_usd) ~ experience_level * role, data = data)
check_assumption(model)
# The test statistic is high, yet the p-value is low. Plotting gives that the residue mostly follow the normal distribution
plot_diagnostic(model)
Anova(model, type = 3)

draw_interaction(x = data$leadership, line = data$company_location, response = sqrt(data$salary_in_usd))
model <- lm(sqrt(salary_in_usd) ~ leadership * company_location, data = data)
check_assumption(model)
plot_diagnostic(model) # High W, but low p-value . The data can be observed to follow the normal distribution.
Anova(model, type = 3)

# There is an interaction
pairs(regrid(emmeans(model, ~ leadership * company_location)), simple = "company_location", by = "leadership")
pairs(regrid(emmeans(model, ~ leadership * company_location)), simple = "leadership", by = "company_location")
#  Using the significant level of 0.05
#  In both non-US and US companies, non-leadership roles earn less than leadership roles, by 54466 USD in non-US companies and 39082 USD in US companies.
#  Similarly, at both leadership and non-leadership roles, US companies pay more than non-US companies, by 73416 USD for non-leadership roles and 58033 USD for leadership roles.
#  Although the interaction was detected, the difference pattern goes in the same direction everywhere: the gap between locations is larger for non-leadership roles than
#  for leadership roles, and the leadership premium is slightly larger outside the US. Hence, both factors raise salary independently, only the magnitude differs.

draw_interaction(x = data$leadership, line = data$role, response = sqrt(data$salary_in_usd))
model <- lm(sqrt(salary_in_usd) ~ leadership * role, data = data)
check_assumption(model) # High W, but low p-value . The data can be observed to follow the normal distribution.
plot_diagnostic(model)
# Shrink at the middle, but expand at two ends for the residual.
Anova(model, type = 3)
# NO difference in role nor interaction, although the interaction plot shows a difference trend in research role

draw_interaction(x = data$role, line = data$company_location, response = data$salary_in_usd)
model <- lm(sqrt(salary_in_usd) ~ role * company_location, data = data)
check_assumption(model)
plot_diagnostic(model) # Near 1 W, but low p-value. The plot shows that it follows the normal dist. The residual have small dips
Anova(model, type = 3)
# Both have impact, but no interaction
pairs(regrid(emmeans(model, ~ role * company_location)), simple = "company_location", by = "role")
pairs(regrid(emmeans(model, ~ role * company_location)), simple = "role", by = "company_location")
# P value adjustment: tukey method for comparing a family of 3 estimates
#  Using the significant level of 0.05
#  For all three roles, US companies pay more than non-US companies: by 65416 USD for analysts, 83059 USD for engineers and 75813 USD for researchers.
#  Within a location, analyst is the lowest paid role; in non-US companies analysts earn less than both engineering and research roles (by about 16-21 thousand USD),
#  and in the US the gap widens to around 31-34 thousand USD. Engineering and research roles are paid the same at either location.

# Analyze salary trends across work_year
data$work_year <- as.factor(data$work_year)
draw_interaction(x = data$work_year, line = data$experience_level, response = sqrt(data$salary_in_usd))
model <- lm(sqrt(salary_in_usd) ~ work_year * experience_level, data = data)
check_assumption(model)
plot_diagnostic(model) # There is a decrease in residual  at high experienece level (expert). However, the line is still erelatively flat. No pattern
Anova(model, type = 3) # There are difference in experienece level, but almost no difference in salary over years in general
pairs(regrid(emmeans(model, ~ work_year * experience_level)), simple = "work_year", by = "experience_level")
# Fine inspection by pairs give that the p-values are all high, thus there should be no difference

draw_interaction(x = data$work_year, line = data$company_location, response = sqrt(data$salary_in_usd))
model <- lm(sqrt(salary_in_usd) ~ work_year * company_location, data = data)
check_assumption(model) # Normal but  the residual dereases  at 2022, US and Other. No pattern
plot_diagnostic(model)
Anova(model, type = 3)
# Both impact, but no interaction
pairs(regrid(emmeans(model, ~ work_year * company_location)), simple = "work_year", by = "company_location")
# Both don't have salary increases in 2021. In 2022, the salary increases for non-US people by 14658 USB. This might corresponds to the recovery in economics after the COVID-19 pandemics

draw_interaction(x = data$work_year, line = data$company_size, response = sqrt(data$salary_in_usd))
model <- lm(sqrt(salary_in_usd) ~ work_year * company_size, data = data)
check_assumption(model)
plot_diagnostic(model) # The residue drops at medium company in 2022. No visible pattern
Anova(model, type = 3) # Has both  main effect interaction of both variables and their interactions
pairs(regrid(emmeans(model, ~ work_year * company_size)), simple = "work_year", by = "company_size")
# All company pays their employees stably in between 2020 and 2021. In 2022, employee of medium companies receive 55084 salary increase in average.

draw_interaction(x = data$work_year, line = data$role, response = sqrt(data$salary_in_usd))
model <- lm(sqrt(salary_in_usd) ~ work_year * role, data = data)
check_assumption(model)
plot_diagnostic(model) # Normal, but the residue is a few at the left, suggesting misisng data distribution. There are no visible patterns. The residue decreases, then being stable
Anova(model, type = 3) # Has impact on their own. Have interaction at significance levvel 0.1
pairs(regrid(emmeans(model, ~ work_year * role)), simple = "work_year", by = "role")
# The analyst get big salary jump in 2022 compared to 2020. Other has salary incease in 2022 and is also a big jump to 2020

draw_interaction(x = data$work_year, line = data$leadership, response = sqrt(data$salary_in_usd))
model <- lm(sqrt(salary_in_usd) ~ work_year * leadership, data = data)
check_assumption(model)
plot_diagnostic(model) # Normal, stable residual
Anova(model, type = 3) # Interaction effect, and indiviual effect
pairs(regrid(emmeans(model, ~ work_year * leadership)), simple = "work_year", by = "leadership")

# Leader don't have salary increase throughout the year. Non-leader has salary increases in 2022
