# Does the working capacity of the heart different between people with health condidtion, and do they have compounding effect

library(tidyverse)
library(tseries)
library(lmtest)
library(car)
library(MASS)
library(emmeans)


data <- readRDS("activity2/dataset/cleaned_data.rds") # Adjust this to fit the rds file location

# Hypothesis:
# The weight condition,  blood sugar: The health condition.
draw_interaction <- function(x, line, response,
                             y_lab = "") {
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

  # Shown next to the model it belongs to, not written to disk - Step 3's
  # committed artifact is the ANOVA output, not the figures.
  print(p)
  invisible(p)
}



plot_diagnostic <- function(model) {
  op <- par(mfrow = c(2, 2))
  on.exit(par(op)) # restores the layout even if plot() errors partway through
  plot(model)
}
# The data is > 9000. By the central limit theorem, the data is likely to follow the normal distrbution regardless of the original distribution
report_model <- function(model) {
  bp <- bptest(model)
  print(bp)
  print(Anova(model, type = 3))

  heteroscedastic <- bp$p.value < 0.05
  if (heteroscedastic) {
    print(Anova(model, type = 3, white.adjust = "hc3"))
  }
  invisible(heteroscedastic)
}
simple_effects <- function(model, spec, simple, robust = FALSE) {
  vc <- if (robust) car::hccm(model, type = "hc3") else stats::vcov(model)
  emm <- emmeans(model, spec, vcov. = vc)
  print(pairs(regrid(emm), simple = simple))
}
# -----------------------------
table(data$bmi_category, data$glycemic_status)
draw_interaction(x = data$glycemic_status, line = data$bmi_category, response = data$heart_rate_difference)

model <- lm(heart_rate_difference ~ bmi_category * glycemic_status, data = data)
het <- report_model(model)

# There are no interaction between these variables. There are difference of heart's working capability in groups of bmi category and group of glycemic status

simple_effects(model, ~ bmi_category * glycemic_status, c("bmi_category"), het)
simple_effects(model, ~ bmi_category * glycemic_status, c("glycemic_status"), het)

# The heart working capacbility decreases as people's glycemic status transition from normal to prediabetic then diabetic
# Underweight and normal people has the same heart working cababilities. However, when the weight is above normal (i.e. overweight or obese), then the heart working capacbility decreases
# There are a noticable for diabetic underweight people: Their heart's working capability drops significantly.
# Reduction of heart rate difference is associated with the increased chance of having heart disease, since those having heart disease has their average heart rate difference lower than those who are not

# Does the age, sex and lifestyle (smoking, meetiing activy guideline) affect the heart working condition

table(data$age_group, data$sex, data$smoker_status, data$meets_activity_guideline)
model <- lm(heart_rate_difference ~ age_group * sex * smoker_status * meets_activity_guideline, data = data)
het <- report_model(model)
emmeans(model, pairwise ~ age_group)
emmeans(model, pairwise ~ age_group | sex * smoker_status * meets_activity_guideline)
# There are decrease of average heart_rate_difference when age increases
# When age is included, only the age is signficant.
# Removing the age casues
model <- lm(heart_rate_difference ~ sex * smoker_status * meets_activity_guideline, data = data)
het <- report_model(model)
# The smoker status and meets activty guideline is signficant on their own, and on their interaction with sex
emmeans(model, pairwise ~ sex | smoker_status * meets_activity_guideline)
# Formerly smoking female and male don't have any working heart capability difference. Others have
# There is a difference between ex then they are smoking and not meeing activty guideline, or when they never smoke at meet acitivity guideline.
# There, female generally has higher heart capabilities than male
emmeans(model, pairwise ~ smoker_status | sex * meets_activity_guideline)
# Between sexes, those that are currently smokes has their working heart capabilities reduced. The reduction effect is stronger on male
emmeans(model, pairwise ~ meets_activity_guideline | sex * smoker_status)
# Those who meet the activity guideline has their heart working capabilities increased significantly


# Does the working heart capability different between people having chest pain during exercise, their chest pain type and their blood pressure category
# That is, between people having sign of chest pain, if the pain happens during heavy works, and their blood pressure category(normal, hypertension, etc)

table(data$chest_pain_type, data$exercise_induced_angina, data$bp_category)
draw_interaction(x = data$chest_pain_type, line = data$exercise_induced_angina, response = data$heart_rate_difference)
draw_interaction(x = data$bp_category, line = data$exercise_induced_angina, response = data$heart_rate_difference)
draw_interaction(x = data$chest_pain_type, line = data$bp_category, response = data$heart_rate_difference)

# The intraction plots show that there is interaction between variable, shows via non-parallel lines . Additionally, the plots in each lines are separated and visibly far from each other, which mean there are significant factrors
model <- lm(heart_rate_difference ~ chest_pain_type * bp_category * exercise_induced_angina, data = data)
het <- report_model(model)

# Exercise induced angina and bp_category have their effects significant. There are no interaction

emmeans(model, pairwise ~ bp_category)
emmeans(model, pairwise ~ bp_category | exercise_induced_angina)
emmeans(model, pairwise ~ bp_category | chest_pain_type)
emmeans(model, pairwise ~ bp_category | exercise_induced_angina * chest_pain_type)
# Stepwise, between those wtih elevated blood pressure and those with hypertension stage 1, there are no difference in heart working capabilities, except between those with asymptomatic chest pain where therea re differenece bewtween elevated bloord pressure and hypertension stage 1


emmeans(model, pairwise ~ exercise_induced_angina)
emmeans(model, pairwise ~ exercise_induced_angina | bp_category)
emmeans(model, pairwise ~ exercise_induced_angina | chest_pain_type)
emmeans(model, pairwise ~ exercise_induced_angina | bp_category * chest_pain_type)

# Those having execise induced angina have their heart working capababilities reduced by 12.5 bpm, which is a significant value
