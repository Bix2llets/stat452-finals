library(ggplot2)
library(car)
library(MASS)
library(lmtest)
library(emmeans)

data <- readRDS("activity1/dataset/cleaned_data.rds")

set.seed(6767)

old_contrasts <- options(contrasts = c("contr.sum", "contr.poly"))

data$salary <- data$observed_salary_in_usd
data$work_year_f <- factor(data$work_year)



# Cannot do > 2 factors, since it results in some 0 observation cells, which
# make the interaction between factors missing data.
draw_interaction <- function(x, line, response,
                             y_lab = "Mean sqrt(salary in USD)") {
  x_name <- sub("^data\\$", "", deparse(substitute(x)))
  line_name <- sub("^data\\$", "", deparse(substitute(line)))

  print(table(x, line, dnn = c(x_name, line_name)))

  plot_df <- data.frame(
    x_factor = as.factor(x),
    trace_factor = as.factor(line),
    salary = response
  )

  p <- ggplot(plot_df, aes(x = x_factor, y = salary, group = trace_factor, color = trace_factor)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1) +
    stat_summary(fun = mean, geom = "point", size = 2.5) +
    labs(x = x_name, y = y_lab, color = line_name) +
    theme_classic()

  print(p)
  invisible(p)
}

plot_diagnostic <- function(model) {
  par(mfrow = c(2, 2))
  plot(model)
  par(mfrow = c(1, 1))
}

# print the report of the model and return the boolean value dpeending on if the omdel is heteroscadastic
report_model <- function(model) {
  print(shapiro.test(residuals(model)))
  bp <- bptest(model)
  print(bp)

  heteroscedastic <- bp$p.value < 0.05
  if (heteroscedastic) {
    cat(
      "\n*** Breusch-Pagan rejects (p =", signif(bp$p.value, 3), ").",
      "Use HC3 adjustment"
    )
    print(Anova(model, type = 3, white.adjust = "hc3"))
  } else {
    cat(
      "\n*** Breusch-Pagan accept (p =", signif(bp$p.value, 3), ").",
      ""
    )
    print(Anova(model, type = 3))
  }
  invisible(heteroscedastic)
}

# Set the robust to TRUE if the anova require white adj = hc3 to be valid
simple_effects <- function(model, spec, simple, robust = FALSE) {
  vc <- if (robust) car::hccm(model, type = "hc3") else stats::vcov(model)
  emm <- emmeans(model, spec, vcov. = vc)
  print(pairs(regrid(emm), simple = simple))
}



draw_interaction(
  x = data$company_size, line = data$leadership, response = data$salary,
  y_lab = "Mean salary in USD"
)
model <- lm(salary ~ company_size * leadership, data = data)
plot_diagnostic(model)
report_model(model)
boxcox(model)

# The boxcox peaked near 0.5. We choose 0.5 as it correspond tot the square root, which is eaiser to interpret the transformation


# --- company_size x leadership ---------------------------------------------

draw_interaction(x = data$company_size, line = data$leadership, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ company_size * leadership, data = data)
plot_diagnostic(model)
het <- report_model(model)
print(emmeans(model, ~ company_size * leadership))
simple_effects(model, ~ company_size * leadership, "leadership", het)
simple_effects(model, ~ company_size * leadership, "company_size", het)
# Observation of the Q Q residual plots show that from -2 to 2 theoritical quantiles, it follows the normal distubtion. At greater than 2 quantile it has sudden jumps, corresponding to the group of very high salaries
# Thus we choose to trust the plot and not the shapiro wilk p-value. Its test value is near 1

# The effect of company size and leadershipo are all significant, but their interaction does not (only 0.08 p-value)
# The leader's salary in every company is greater than normal employee: 82010 increase in small companies, 36564 in medium and 71224 in large companies

# --- company_size x role ----------------------------------------------------

draw_interaction(x = data$company_size, line = data$role, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ company_size * role, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ company_size * role, "role", het)
simple_effects(model, ~ company_size * role, "company_size", het)
# Both are significant, but no interaction
# Only role difference at small companies 42465
# Between small and medium companies there are salary difference at all role 57720 28100 51234


# --- company_size x experience_level ---------------------------------------

draw_interaction(x = data$company_size, line = data$experience_level, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ company_size * experience_level, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ company_size * experience_level, "company_size", het)
simple_effects(model, ~ company_size * experience_level, "experience_level", het)
# Experience gradient, consistent across every company size: entry < mid < senior,
# with the entry-to-senior step the largest (about 54,000 / 89,000 / 74,000 USD at
# small / medium / large, all p < 0.002). Senior vs Executive is never significant
# The only place company size still separates is at mid-level, where small pays
# 36,219 USD less than medium and 41,464 USD less than large (both p < 0.0001).


# --- company_size x company_location ---------------------------------------

draw_interaction(x = data$company_size, line = data$company_location, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ company_size * company_location, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ company_size * company_location, "company_location", het)
simple_effects(model, ~ company_size * company_location, "company_size", het)
# US premium at every company size: 40,030 USD small (p = 0.003), 75,226 USD
# medium and 86,908 USD large (both p < 0.0001).
# The size contrasts are significant only within US companies (small - medium
# -42,947, p = 0.002; small - large -55,754, p = 0.0003) and not within non-US
# companies.

# We don't choose residence versus the the lcoation as they are strongly colinear: They are highly likely to have the same value
print(table(data$employee_residence, data$company_location,
  dnn = c("employee_residence", "company_location")
))


data$remote_ratio_f <- factor(data$remote_ratio)
draw_interaction(x = data$remote_ratio_f, line = data$company_location, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ remote_ratio_f * company_location, data = data)
plot_diagnostic(model)
het <- report_model(model)
# Only the company location is important
simple_effects(model, ~ remote_ratio_f * company_location, "company_location", het)
# Companies that are in the US pays more than those at other countries for all remote ratio.
draw_interaction(x = data$experience_level, line = data$leadership, response = sqrt(data$salary))

# They have missing values


draw_interaction(x = data$experience_level, line = data$role, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ experience_level * role, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ experience_level * role, "experience_level", het)
simple_effects(model, ~ experience_level * role, "role", het)
# Senior role are paid more than entry and mid level. There are no salary difference between the senior and the executive. Only at the research role that mid-level are paid significantly more than entry level
# Only senior roles between analyst and research get paid differently

draw_interaction(x = data$leadership, line = data$company_location, response = sqrt(data$salary))
# A leverage of 1 in the observation (leadership in other contginent), with prevents the hc3 correction to work

# --- leadership x role ------------------------------------------------------

draw_interaction(x = data$leadership, line = data$role, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ leadership * role, data = data)
plot_diagnostic(model)
het <- report_model(model)
# Role is  insignificant here and
#  no interaction (p = 0.69).

draw_interaction(x = data$role, line = data$company_location, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ role * company_location, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ role * company_location, "company_location", het)
simple_effects(model, ~ role * company_location, "role", het)
# An interaction here
# Overall, the salary in US are higher than the salary in all other countries at all roles
# In every continents, data analyst is the one with least salary, while engineering and research's are high. Between engineering and research, there are no difference. However, the difference between analyst and the other roles is different per continent, where the difference is 16k - 20k in other countries and 33k in the US


draw_interaction(x = data$work_year_f, line = data$experience_level, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ work_year_f * experience_level, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ work_year_f * experience_level, "work_year_f", het)
# No year effect


draw_interaction(x = data$work_year_f, line = data$company_location, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ work_year_f * company_location, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ work_year_f * company_location, "work_year_f", het)
# In the US, there are no difference in salary across years. Hoever, in other countries, there is an increase of 14k USD between 2021 and 2022

draw_interaction(x = data$work_year_f, line = data$company_size, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ work_year_f * company_size, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ work_year_f * company_size, "work_year_f", het)
# The interaction appear in medium companies, where 2021 -> 2022 salary increase is
# worth 55,189 USD (p < 0.0001). #


draw_interaction(x = data$work_year_f, line = data$role, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ work_year_f * role, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ work_year_f * role, "work_year_f", het)
# All significant, but no interaction
# All three disciplines rise into 2022, which is why there is a main effect but
# no interaction:
#   Analyst      2020 -> 2022  +50,467 USD (p = 0.0015)
#   Engineering  2020 -> 2022  +35,097 USD (p = 0.004); 2021 -> 2022 +30,461 (p = 0.003)
#   Research     2021 -> 2022  +51,843 USD (p < 0.0001)


draw_interaction(x = data$work_year_f, line = data$leadership, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ work_year_f * leadership, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ work_year_f * leadership, "work_year_f", het)

# Non-leadership roles have their salary increased in 2022, while the leader aren't
options(old_contrasts)
