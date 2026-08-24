library(ggplot2)
data <- readRDS("../dataset/cleaned_data.rds")
str(data)
attach(data)


draw_interaction <- function(x, line, response) {
  print(table(x, line))

  plot_df <- data.frame(
    x_factor = as.factor(x),
    trace_factor = as.factor(line),
    salary = response
  )

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

table(employee_residence, company_location)
# Thus we will only use one of them as factor
draw_interaction(x = company_size, line = leadership, response = salary_in_usd)
draw_interaction(x = company_size, line = role, response = salary_in_usd)
draw_interaction(x = company_size, line = experience_level, response = salary_in_usd)
draw_interaction(x = company_size, line = company_location, response = salary_in_usd)
draw_interaction(x = experience_level, line = leadership, response = salary_in_usd)
draw_interaction(x = experience_level, line = company_location, response = salary_in_usd)
draw_interaction(x = experience_level, line = role, response = salary_in_usd)
draw_interaction(x = leadership, line = company_location, response = salary_in_usd)
draw_interaction(x = leadership, line = role, response = salary_in_usd)
draw_interaction(x = role, line = company_location, response = salary_in_usd)

# The effect of work_year
draw_interaction(x = work_year, line = experience_level, response = salary_in_usd)
draw_interaction(x = work_year, line = company_location, response = salary_in_usd)
draw_interaction(x = work_year, line = company_size, response = salary_in_usd)
draw_interaction(x = work_year, line = role, response = salary_in_usd)
draw_interaction(x = work_year, line = leadership, response = salary_in_usd)
detach(data)
