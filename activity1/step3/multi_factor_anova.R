library(tidyverse)
library(car)
library(ggfortify)
library(MASS)
library(lmtest)
library(emmeans)


data <- readRDS("../dataset/cleaned_data.rds")
options(contrasts = c("contr.sum", "contr.poly"))

assumption_check <- function(model) {
  print("Shapiro-Wilk: ")
  print(shapiro.test(residuals(model)))
  print("Beusch-Pagan: ")
  print(bptest(model))
  print("Durbin-Watson: ")
  print(car::durbinWatsonTest(model))
}



# We first select which factor would affect the salary in isolation and in combination with other factors.
# We consider the effect of leadership and expereience in salary

table(data$leadership, data$experience_level)
interaction.plot(response = data$salary_in_usd, trace.factor = data$leadership, x.factor = data$experience_level)


# The interaction plot shows that there is effects fromt he leadership (the salary goes up when there is leadership), but ther lines between difference experence leve is almost parallel.
# It suggests that the leader ship and the expereince has no interactoion, for the expereienece level to not affect the salary
# To confirm it, we will use the anova of type 2, as there are no effect jointly caused by two factors. There are also no observation of leadership at experience level 0, which make type 3 anova imposible

salary_lsel <- lm(salary_in_usd ~ leadership + experience_level, data = data)
Anova(salary_lsel, type = 2)

par(mfrow = c(2, 2))
plot(salary_lsel)
par(mfrow = c(1, 1))
assumption_check(salary_lsel)
# The result shows that both leadership and expereience level factors independently has impact on the salary at significance level 0.01. The interaction between them can't be tested with anova due to missing value, and the interaction plot shows no interaction, thus we can conlcude that there are no interaction between these variables.
# The normality of resiudal hypothesis is not satisfied, as the jarque bera test and shapiro test gives. Also, the scale-location plot is not completely flat, as there is varying standardied residual given by the red line. This suggest us to try remedial method to make the assumptions holds
# We observe that the Q-Q residual and the histogram shows that the salary_in_used is left skewed. This suggest us to use square root transformation on the salary_in_usd to make it distributes normally
boxcox(salary_lsel, plotit = TRUE)

sqrt_salary_lsel <- lm(sqrt(salary_in_usd) ~ leadership + experience_level, data = data)

Anova(sqrt_salary_lsel, type = 2)

par(mfrow = c(2, 2))
plot(sqrt_salary_lsel)
par(mfrow = c(1, 1))


assumption_check(sqrt_salary_lsel)
# The plot and test result shows that the normality of the residual is fixed, as it mostly follow the expected quatiles.
# Also, consider the scale-location plot, the values are scattered around with no patterns, suggesting that they are indeed random. Also, the standarded resiudal is flat with slight variance. The assumption check also shows higher than 0.05 p value in all homoscedasticity, independent and normality, thus we can conclude based on the anova result that both leadersijp and experience level affect the salary at 0.05 significant level.
#

emmeans(sqrt_salary_lsel, pairwise ~ leadership, type = "response")
# At 5% significant level, we accept that the salary of those being leader is 19835 USD greater than those who are not.
emmeans(sqrt_salary_lsel, pairwise ~ experience_level, type = "response")
# At 5% signfiicant level, there are no difference between experienec elevel 3 and those with expereience level 2. Other pairs of expereience level all have differences:
# Between 0 and 1
# Between 0 and 2
# Between 0 and 3
# Between 1 and 2
# Betewen 1 and 3


# -------------------------------------------------------------------------------------
#
# Next, we will consider the effect of role and experience on the salary

table(data$role, data$experience_level)

# The data is also unbalanced.

interaction.plot(trace.factor = data$role, x.factor = data$experience_level, response = data$salary_in_usd)

# We see that there is variation caused by each of role and expereience level, as well as their interaction, shown with non-identical segments.

# Simple model
salary_ex_ro <- lm(salary_in_usd ~ role * experience_level, data = data)
Anova(salary_ex_ro, type = 3)

par(mfrow = c(2, 2))
plot(salary_ex_ro)
par(mfrow = c(1, 1))

assumption_check(salary_ex_ro)
# Despite the anova model's viable results of low p-value, the scale location and Q-Q plot shows as the values getting higher, the Q-Q model is right-skered and the residual value increases, violating both the normality and the homoscedasticity

boxcox(salary_ex_ro, plotit = TRUE)

# The boxcox suggest the power of 0.5, which mean to use square root. This agree with our previous find of square root of the salary in usd fits better to the expected quantiles line.

interaction.plot(x.factor = data$role, trace.factor = data$experience_level, response = sqrt(data$salary_in_usd))
sqrt_salary_ex_ro <- lm(sqrt(salary_in_usd) ~ role * experience_level, data = data)
Anova(sqrt_salary_ex_ro, type = 3)

par(mfrow = c(2, 2))
plot(sqrt_salary_ex_ro)
par(mfrow = c(1, 1))
assumption_check(sqrt_salary_ex_ro)

# The residual is normal, homoscedastic and independent   at 0.001 significace level. Also, we see that the scale-location graph is roughly the same as the scatter is random enough.
# Therefore, from the anova result, we conclude that only experience level affeet the salary_in_usd when considering its interaction with role. The differences w.r.t. the salary is checked above, so we won't check it here


# ---------------------------------------------------------------------

# Check the impact of leaderhip and role on salary


table(data$leadership, data$role)

interaction.plot(response = data$salary_in_usd, x.factor = data$leadership, trace.factor = data$role)


# THe line of research is different from other, suggsting the effect of role
# The leadership clearly affect the salary, as shown above

salary_role_leader <- lm(data$salary_in_usd ~ data$role * data$leadership)
Anova(salary_role_leader, type = 3)
assumption_check(salary_role_leader)

# Shapiro-Wilk shows that the data is not normal as the p-value is very small, as well as dependent data

boxcox(salary_role_leader)

sqrt_salary_role_leader <- lm(sqrt(data$salary_in_usd) ~ data$role * data$leadership)
Anova(sqrt_salary_role_leader)
assumption_check(sqrt_salary_role_leader)

# Even after the data is normal (0.001 s-level), it is still depdendent. Suggest that the model is untrustworthy


# -----
table(data$company_size, data$leadership, data$role)

interaction.plot(response = data$salary_in_usd, x.factor = data$company_size, trace.factor = data$leadership)
salary_size_leader <- lm(salary_in_usd ~ company_size * leadership, data = data)
Anova(salary_size_leader, type = 3)
assumption_check(salary_size_leader)
boxcox(salary_size_leader)

salary_size_leader <- lm(sqrt(salary_in_usd) ~ company_size * leadership, data = data)
Anova(salary_size_leader, type = 3)
assumption_check(salary_size_leader)

interaction.plot(response = data$salary_in_usd, x.factor = data$company_size, trace.factor = data$role)

salary_size_role <- lm(salary_in_usd ~ company_size * role, data = data)
Anova(salary_size_role, type = 3)
assumption_check(salary_size_role)
boxcox(salary_size_role)

salary_size_role <- lm(sqrt(salary_in_usd) ~ company_size * role, data = data)
Anova(salary_size_role, type = 3)
assumption_check(salary_size_role)


# ----------------------------------------------------------
#
data$work_year <- as.factor(data$work_year)
table(data$company_size, data$work_year)

interaction.plot(response = data$salary_in_usd, x.factor = data$work_year, trace.factor = data$company_size)
interaction.plot(response = data$salary_in_usd, trace.factor = data$role, x.factor = data$work_year)
interaction.plot(response = data$salary_in_usd, trace.factor = data$experience_level, x.factor = data$work_year)
interaction.plot(response = data$salary_in_usd, trace.factor = data$is_fulltime, x.factor = data$work_year)
salary_size_leader <- lm(salary_in_usd ~ company_size * leadership, data = data)
