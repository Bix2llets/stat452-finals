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

# company_size = 1:
#  contrast                  estimate    SE  df t.ratio p.value
#  leadership0 - leadership1   -73500 22100 594  -3.323  0.0009
#
# company_size = 2:
#  contrast                  estimate    SE  df t.ratio p.value
#  leadership0 - leadership1   -36953 14800 594  -2.503  0.0126
#
# company_size = 3:
#  contrast                  estimate    SE  df t.ratio p.value
#  leadership0 - leadership1   -62488 14200 594  -4.413 <0.0001
#
# leadership = 0:
#  contrast                      estimate    SE  df t.ratio p.value
#  company_size1 - company_size2   -46072  6380 594  -7.221 <0.0001
#  company_size1 - company_size3   -34316  6960 594  -4.928 <0.0001
#  company_size2 - company_size3    11756  5660 594   2.076  0.0957
#
# leadership = 1:
#  contrast                      estimate    SE  df t.ratio p.value
#  company_size1 - company_size2    -9525 25800 594  -0.369  0.9277
#  company_size1 - company_size3   -23305 25300 594  -0.920  0.6277
#  company_size2 - company_size3   -13779 19700 594  -0.701  0.7630
#
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


# > pairs(regrid(emmeans(model, ~ company_size * role)), simple = "role", by = "company_size")
# company_size = 1:
#  contrast               estimate    SE  df t.ratio p.value
#  Analyst - Engineering    -42465 14300 591  -2.968  0.0088
#  Analyst - Research       -23944 13600 591  -1.758  0.1847
#  Engineering - Research    18522 12300 591   1.506  0.2890
#
# company_size = 2:
#  contrast               estimate    SE  df t.ratio p.value
#  Analyst - Engineering    -12657  8410 591  -1.505  0.2894
#  Analyst - Research       -18180  9110 591  -1.996  0.1141
#  Engineering - Research    -5523  8390 591  -0.659  0.7875
#
# company_size = 3:
#  contrast               estimate    SE  df t.ratio p.value
#  Analyst - Engineering    -10628 12500 591  -0.851  0.6712
#  Analyst - Research       -21968 12400 591  -1.773  0.1794
#  Engineering - Research   -11341  9700 591  -1.169  0.4720
#
# P value adjustment: tukey method for comparing a family of 3 estimates
# > pairs(regrid(emmeans(model, ~ company_size * role)), simple = "company_size", by = "role")
# role = Analyst:
#  contrast                      estimate    SE  df t.ratio p.value
#  company_size1 - company_size2   -57720 12700 591  -4.547 <0.0001
#  company_size1 - company_size3   -49071 15100 591  -3.257  0.0034
#  company_size2 - company_size3     8649 12200 591   0.708  0.7590
#
# role = Engineering:
#  contrast                      estimate    SE  df t.ratio p.value
#  company_size1 - company_size2   -27912 10700 591  -2.610  0.0251
#  company_size1 - company_size3   -17233 11600 591  -1.491  0.2959
#  company_size2 - company_size3    10678  8790 591   1.215  0.4449
#
# role = Research:
#  contrast                      estimate    SE  df t.ratio p.value
#  company_size1 - company_size2   -51956 10400 591  -5.017 <0.0001
#  company_size1 - company_size3   -47096 10600 591  -4.454 <0.0001
#  company_size2 - company_size3     4861  9330 591   0.521  0.8612
#
# P value adjustment: tukey method for comparing a family of 3 estimates
# >
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
# > pairs(regrid(emmeans(model, ~ company_size * experience_level)), simple = "company_size", group = "experience_level")
# experience_level = 0:
#  contrast                      estimate    SE  df t.ratio p.value
#  company_size1 - company_size2    10514  9470 588   1.110  0.5081
#  company_size1 - company_size3    -9748 10500 588  -0.931  0.6212
#  company_size2 - company_size3   -20262  9930 588  -2.040  0.1037
#
# experience_level = 1:
#  contrast                      estimate    SE  df t.ratio p.value
#  company_size1 - company_size2   -35397  8130 588  -4.353 <0.0001 HERE
#  company_size1 - company_size3   -39677  8400 588  -4.724 <0.0001 ONLY HAVE DIFFERENCE HERE
#  company_size2 - company_size3    -4280  6990 588  -0.612  0.8134
#
# experience_level = 2:
#  contrast                      estimate    SE  df t.ratio p.value
#  company_size1 - company_size2   -24617 12300 588  -2.001  0.1130
#  company_size1 - company_size3   -26604 13600 588  -1.962  0.1226
#  company_size2 - company_size3    -1988  8420 588  -0.236  0.9698
#
# experience_level = 3:
#  contrast                      estimate    SE  df t.ratio p.value
#  company_size1 - company_size2   -26090 40800 588  -0.640  0.7981
#  company_size1 - company_size3   -41286 43600 588  -0.947  0.6110
#  company_size2 - company_size3   -15197 31600 588  -0.481  0.8803
#
# P value adjustment: tukey method for comparing a family of 3 estimates
# > pairs(regrid(emmeans(model, ~ company_size * experience_level)), simple = "experience_level", group = "company_size")
# company_size = 1:
#  contrast                              estimate    SE  df t.ratio p.value
#  experience_level0 - experience_level1     8653  9710 588   0.891  0.8095
#  experience_level0 - experience_level2   -54055 13500 588  -4.001  0.0004 HERE
#  experience_level0 - experience_level3   -89265 36500 588  -2.445  0.0701
#  experience_level1 - experience_level2   -62707 13300 588  -4.726 <0.0001 HERE
#  experience_level1 - experience_level3   -97918 36400 588  -2.688  0.0370 HERE
#  experience_level2 - experience_level3   -35211 37600 588  -0.936  0.7855
#
# company_size = 2:
#  contrast                              estimate    SE  df t.ratio p.value
#  experience_level0 - experience_level1   -37258  7840 588  -4.750 <0.0001 HERE
#  experience_level0 - experience_level2   -89186  7650 588 -11.651 <0.0001 HERE
#  experience_level0 - experience_level3  -125869 20500 588  -6.155 <0.0001 HERE
#  experience_level1 - experience_level2   -51927  6440 588  -8.062 <0.0001 HERE
#  experience_level1 - experience_level3   -88610 20000 588  -4.424 <0.0001 HERE
#  experience_level2 - experience_level3   -36683 20000 588  -1.838  0.2564
#
# company_size = 3:
#  contrast                              estimate    SE  df t.ratio p.value
#  experience_level0 - experience_level1   -21277  9270 588  -2.294  0.1005
#  experience_level0 - experience_level2   -70911 10500 588  -6.729 <0.0001 HERE
#  experience_level0 - experience_level3  -120804 26000 588  -4.637 <0.0001 HERE
#  experience_level1 - experience_level2   -49634  8850 588  -5.608 <0.0001 HERE
#  experience_level1 - experience_level3   -99527 25400 588  -3.916  0.0006 HERE
#  experience_level2 - experience_level3   -49892 25900 588  -1.926  0.2181
#
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
# > pairs(regrid(emmeans(model, ~ company_size * company_location)), simple = "company_location", group = "company_size")
# company_size = 1:
#  contrast   estimate   SE  df t.ratio p.value
#  Other - US   -37775 9840 594  -3.837  0.0001
#
# company_size = 2:
#  contrast   estimate   SE  df t.ratio p.value
#  Other - US   -74614 5380 594 -13.866 <0.0001
#
# company_size = 3:
#  contrast   estimate   SE  df t.ratio p.value
#  Other - US   -80714 7090 594 -11.384 <0.0001
pairs(regrid(emmeans(model, ~ company_size * company_location)), group = "company_location", simple = "company_size")
# > pairs(regrid(emmeans(model, ~ company_size * company_location)), group = "company_location", simple = "company_size")
# company_location = Other:
#  contrast                      estimate    SE  df t.ratio p.value
#  company_size1 - company_size2    -7751  6260 594  -1.237  0.4315
#  company_size1 - company_size3    -8875  6530 594  -1.359  0.3634
#  company_size2 - company_size3    -1125  5560 594  -0.202  0.9777
#
# company_location = US:
#  contrast                      estimate    SE  df t.ratio p.value
#  company_size1 - company_size2   -44590  9310 594  -4.791 <0.0001
#  company_size1 - company_size3   -51814 10200 594  -5.068 <0.0001
#  company_size2 - company_size3    -7225  6950 594  -1.040  0.5519
#
# P value adjustment: tukey method for comparing a family of 3 estimates
#
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
# > pairs(regrid(emmeans(model, ~ leadership * company_location)), simple = "company_location", by = "leadership")
# leadership = 0:
#  contrast   estimate    SE  df t.ratio p.value
#  Other - US   -73416  3940 596 -18.639 <0.0001
#
# leadership = 1:
#  contrast   estimate    SE  df t.ratio p.value
#  Other - US   -58033 14500 596  -3.997 <0.0001
#
# > pairs(regrid(emmeans(model, ~ leadership * company_location)), simple = "leadership", by = "company_location")
# company_location = Other:
#  contrast                  estimate    SE  df t.ratio p.value
#  leadership0 - leadership1   -54466 11100 596  -4.912 <0.0001
#
# company_location = US:
#  contrast                  estimate    SE  df t.ratio p.value
#  leadership0 - leadership1   -39082 10200 596  -3.845  0.0001
#
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
# > pairs(regrid(emmeans(model, ~ role * company_location)), simple = "company_location", by = "role")
# role = Analyst:
#  contrast   estimate   SE  df t.ratio p.value
#  Other - US   -65416 7650 594  -8.553 <0.0001
#
# role = Engineering:
#  contrast   estimate   SE  df t.ratio p.value
#  Other - US   -83059 6200 594 -13.400 <0.0001
#
# role = Research:
#  contrast   estimate   SE  df t.ratio p.value
#  Other - US   -75813 6530 594 -11.612 <0.0001
#
# > pairs(regrid(emmeans(model, ~ role * company_location)), simple = "role", by = "company_location")
# company_location = Other:
#  contrast               estimate   SE  df t.ratio p.value
#  Analyst - Engineering    -16530 6530 594  -2.532  0.0311
#  Analyst - Research       -20823 6740 594  -3.092  0.0059
#  Engineering - Research    -4294 5370 594  -0.800  0.7034
#
# company_location = US:
#  contrast               estimate   SE  df t.ratio p.value
#  Analyst - Engineering    -34173 7370 594  -4.637 <0.0001
#  Analyst - Research       -31220 7470 594  -4.181 <0.0001
#  Engineering - Research     2953 7230 594   0.409  0.9121
#
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
