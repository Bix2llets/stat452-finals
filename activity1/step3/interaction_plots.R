library(ggplot2)
data <- readRDS("../dataset/cleaned_data.rds")
str(data)
attach(data)


draw_interaction <- function(x, line) {
  print(table(x, line))

  # 2. Create a temporary dataframe for ggplot
  plot_df <- data.frame(
    x_factor = as.factor(x),
    trace_factor = as.factor(line),
    salary = data$salary_in_usd # Assumes 'data' exists in the global environment
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

table(employee_residence, company_location)
# Thus we will only use one of them as factor
draw_interaction(x = company_size, line = leadership)
draw_interaction(x = company_size, line = role)
draw_interaction(x = company_size, line = experience_level)
draw_interaction(x = company_size, line = company_location)
draw_interaction(x = experience_level, line = leadership)
draw_interaction(x = experience_level, line = employment_type)
draw_interaction(x = experience_level, line = company_location)
draw_interaction(x = experience_level, line = role)
draw_interaction(x = employment_type, line = company_location)
draw_interaction(x = leadership, line = company_location)
draw_interaction(x = leadership, line = role)
draw_interaction(x = leadership, line = employment_type)
draw_interaction(x = role, line = employment_type)
draw_interaction(x = role, line = company_location)
draw_interaction(x = remote_ratio, line = role)
draw_interaction(x = remote_ratio, line = employment_type)
draw_interaction(x = remote_ratio, line = company_location)
draw_interaction(x = remote_ratio, line = employment_type)
draw_interaction(x = remote_ratio, line = company_size)
draw_interaction(x = remote_ratio, line = experience_level)

# The effect of work_year
draw_interaction(x = work_year, line = experience_level)
draw_interaction(x = work_year, line = employment_type)
draw_interaction(x = work_year, line = company_location)
draw_interaction(x = work_year, line = company_size)
draw_interaction(x = work_year, line = role)
draw_interaction(x = work_year, line = leadership)
draw_interaction(x = work_year, line = employment_type)
detach(data)
