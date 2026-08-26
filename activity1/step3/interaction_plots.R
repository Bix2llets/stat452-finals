# =============================================================================
# Step 3 - interaction plots (screen reading only)
# =============================================================================
# Companion to multi_factor_anova.R. This file draws nothing but pictures, so
# you can eyeball which factor pairs look like they interact before reading the
# formal tests. No model is fitted here.
#
# WHAT CHANGED IN THIS REVISION (see report/review-step23.md):
#  - Uses `observed_salary_in_usd`, the untouched salary, to match Steps 2/3/4.
#    `salary_in_usd` is the Step 1 winsorised copy and is no longer analysed.
#  - draw_interaction() now takes a `y_lab`, so a plot can never be labelled
#    with a scale it is not actually drawn on.
#  - remote_ratio added (required by the brief, previously missing).
#
# SCALE: these plots are drawn on the RAW USD scale because that is what a
# reader can interpret. multi_factor_anova.R fits every model on sqrt(salary)
# and draws its own copies of these plots on that same square-root scale, so the
# picture there matches the test there. Use this file to see the shape of the
# pattern, and that file to see whether the shape is real.
# =============================================================================

library(ggplot2)

data <- readRDS("../dataset/cleaned_data.rds")
str(data)

# Analysis response for this file. Defined once, matching Steps 2 and 3.
data$salary <- data$observed_salary_in_usd

# The plots are deliberately NOT written to disk. Step 3's committed artifact is
# the ANOVA output (anova_output.txt), not the figures. Run via Rscript they
# collect in a gitignored Rplots.pdf; run in RStudio they go to the Plots pane.
# The axis / legend titles are taken from the expressions passed in, so a plot
# never ends up labelled with the generic "X Factor" / "Trace Factor".
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

# The off-diagonal cells of this cross-tab are nearly empty (US resident /
# non-US company has 2 observations), so residence and location carry almost the
# same information and an interaction between them cannot be estimated with any
# power. Thus we only use one of them as a factor: company_location, the
# location the salary is actually paid in. multi_factor_anova.R records the same
# decision and does not fit the residence x location model.
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
