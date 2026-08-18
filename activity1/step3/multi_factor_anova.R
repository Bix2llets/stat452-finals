library(tidyverse)
library(car)

data <- readRDS("../dataset/cleaned_data.rds")


# We first select which factor would affect the salary in isolation and in combination with other factors.
# We examine the factors work_year and expereience level

table(data$work_year, data$experience_level)

# We that the data is imbalanced between years and levels
# Based on the descriptive statisitics, we see that each of them already affect the salary. We use the interaction plot to visually see how they interact with each other to affect the salary

interaction.plot(x.factor = data$work_year, trace.factor = data$experience_level, response = data$salary_in_usd)

# The lines are almost parallel, yet they have slight differences

# Since the data is highly imbalanced, we use the type 3 Anova as it would not be affected by order we insert the factors

options(contrasts = c("contr.sum", "contr.poly"))

salary_yl <- lm(salary_in_usd ~ work_year * experience_level, data = data)
salary_yl.anova <- Anova(salary_year_level_model, type = 3)

salary_yl.anova

# The anova type 3 shows that the interaction between work_year and expereience level does not affect the salary at significance level of 0.05, as their p-value are all very large
#

# Next, we consider the effect of leadership and expereience in salary

table(data$leadership, data$experience_level)
interaction.plot(response = data$salary_in_usd, x.factor = data$leadership, trace.factor = data$experience_level)

# The interaction plot shows that there is effects fromt he leadership (the salary goes up when there is leadership), but ther lines between difference experence leve is almost parallel.
# It suggests that the leader ship and the expereince has no interactoion, for the expereienece level to not affect the salary
# To confirm it, we will use the anova of type 2, as there are no effect jointly caused by two factors. There are also no observation of leadership at experience level 0, which make type 3 anova imposible

salary_lsel <- lm(salary_in_usd ~ leadership + experience_level, data = data)
Anova(salary_lsel, type = 2)

# The result shows that both leadership and expereience level factors independently has impact on the salary at significance level 0.01. The interaction between them can't be tested with anova due to missing value, and the interaction plot shows no interaction, thus we can conlcude that there are no interaction between these variables.
