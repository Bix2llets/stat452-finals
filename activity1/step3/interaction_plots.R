library(ggplot2)

data <- readRDS("activity1/dataset/cleaned_data.rds")
str(data)

data$salary <- data$observed_salary_in_usd

draw_interaction <- function(x, line, response,
                             y_lab = "Mean salary in USD") {
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

  # print() so the plot is drawn under source() / RStudio's plain "Source" too,
  # not only under Rscript's top-level autoprint.
  print(p)
  invisible(p)
}

print(table(data$employee_residence, data$company_location,
  dnn = c("employee_residence", "company_location")
))

draw_interaction(x = data$company_size, line = data$leadership, response = data$salary)
draw_interaction(x = data$company_size, line = data$role, response = data$salary)
draw_interaction(x = data$company_size, line = data$experience_level, response = data$salary)
draw_interaction(x = data$company_size, line = data$company_location, response = data$salary)

# experience_level x leadership: the Entry x Yes cell is EMPTY, so one trace has
# no starting point. That is why multi_factor_anova.R does not fit this model -
# the interaction is inestimable, which is not the same as absent.
draw_interaction(x = data$experience_level, line = data$leadership, response = data$salary)

draw_interaction(x = data$experience_level, line = data$company_location, response = data$salary)
draw_interaction(x = data$experience_level, line = data$role, response = data$salary)
draw_interaction(x = data$leadership, line = data$company_location, response = data$salary)
draw_interaction(x = data$leadership, line = data$role, response = data$salary)
draw_interaction(x = data$role, line = data$company_location, response = data$salary)

# remote_ratio is numeric in Step 1 (0 / 50 / 100), factored here for one trace
# per tier. The middle tier looks badly paid, but it is 79% non-US - see the
# model in multi_factor_anova.R, which finds no remote effect once location is
# held fixed.
data$remote_ratio_f <- factor(data$remote_ratio)
draw_interaction(x = data$remote_ratio_f, line = data$company_location, response = data$salary)

# The effect of work_year. Numeric in Step 1 because Step 4 needs it numeric, so
# a separate factor column is added rather than overwriting the original.
data$work_year_f <- factor(data$work_year)
draw_interaction(x = data$work_year_f, line = data$experience_level, response = data$salary)
draw_interaction(x = data$work_year_f, line = data$company_location, response = data$salary)
draw_interaction(x = data$work_year_f, line = data$company_size, response = data$salary)
draw_interaction(x = data$work_year_f, line = data$role, response = data$salary)
draw_interaction(x = data$work_year_f, line = data$leadership, response = data$salary)
