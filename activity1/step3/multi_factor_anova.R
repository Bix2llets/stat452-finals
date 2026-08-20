library(tidyverse)
library(car)
library(ggfortify)
library(MASS)
library(lmtest)
library(emmeans)


data <- readRDS("../dataset/cleaned_data.rds")
set.seed(6767)
options(contrasts = c("contr.sum", "contr.poly"))


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
  print(t.test(residuals(model), mu = 0))
  print(shapiro.test(residuals(model)))
  print(bptest(model))
  print(car::durbinWatsonTest(model))
}
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

# It is equal to 0, but is not normal
draw_interaction(x = data$company_size, line = data$leadership, response = data$salary_in_usd)
model <- lm(sqrt(salary_in_usd) ~ company_size * leadership, data = data)
plot_diagnostic(model)
check_assumption(model)
