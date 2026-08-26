# =============================================================================
# Step 3 - Multi-factor (factorial) ANOVA
# =============================================================================
# WHAT CHANGED IN THIS REVISION (evidence: report/review-step23.md)
#
# 1. RESPONSE. Every model now uses `observed_salary_in_usd`, the untouched
#    salary, instead of `salary_in_usd`, which Step 1 winsorised at the 1.5*IQR
#    fences. Ten rows (1.7% of 600) were clamped onto one identical cap value,
#    which cut the SD by 12.6% and was enough to CREATE a significant
#    leadership x company_location interaction (p = 0.034 winsorised vs
#    p = 0.12 observed). Step 4 already models the observed response, so this
#    also puts Steps 2, 3 and 4 back on the same response.
#
# 2. HETEROSCEDASTICITY IS NOW REMEDIED, NOT ARGUED AWAY. The old file ran
#    bptest() and, when it rejected, wrote "we choose to trust the plot". That
#    reasoning is fine for normality but not for equal variance here, because
#    the designs are badly unbalanced (cells run from 3 to 258). Unequal
#    variance PLUS unequal cell sizes is exactly where the classical F test
#    stops being robust. Wherever Breusch-Pagan rejects, report_model() now also
#    prints an HC3 heteroscedasticity-consistent Type III table, and the simple
#    effects are taken from the same HC3 covariance. The brief asks us to
#    "propose remedy methods if any assumption is violated"; this is that remedy.
#
# 3. remote_ratio is now tested. It is a required variable in the brief and was
#    missing from the whole analysis.
#
# 4. Interaction plots are drawn on the SAME scale as the model that tests them
#    (square root), and the y axis is labelled with that scale. The old helper
#    hard-coded "Mean Salary in USD" even when it was handed sqrt values.
#
# 5. The global contrasts option is saved and restored at the end of the file so
#    it cannot leak into Step 4, whose glmnet penalty depends on the coding.
# =============================================================================

# ggplot2 is the only part of the tidyverse this file uses. Loading it alone
# (instead of the whole tidyverse) also avoids MASS masking dplyr::select.
library(ggplot2)
library(car)
library(MASS)
library(lmtest)
library(emmeans)

data <- readRDS("../dataset/cleaned_data.rds")

set.seed(6767)

# Type III tests are only the hypotheses we mean if the factor contrasts sum to
# zero, so contr.sum is set for the unordered factors (role, company_location).
# Step 1 stores company_size, experience_level and leadership as ORDERED
# factors, which take contr.poly instead - also sum-to-zero and orthogonal, so
# the Type III sums of squares are unaffected. Verified: refitting with those
# three coerced to unordered contr.sum factors reproduces every Type III
# Sum Sq / F / p exactly.
# This line is load-bearing: without it the same model reports company_size
# F = 22.1 instead of F = 3.7. It is a GLOBAL option, so we keep the previous
# value and restore it at the bottom of the file (see change note 5).
old_contrasts <- options(contrasts = c("contr.sum", "contr.poly"))

# The analysis response for the whole file. Defined once so there is a single
# place to change it. `salary_in_usd` (winsorised) is deliberately not used.
data$salary <- data$observed_salary_in_usd

# work_year is numeric in Step 1 because Step 4 needs it numeric. A separate
# factor column is added here rather than overwriting the original, so that
# re-running a block halfway down the file cannot silently change the data.
data$work_year_f <- factor(data$work_year)


# --------------------------------------------------------------------------
# Caveats that apply to EVERY model in this file. Stated once here instead of
# being re-argued at each model.
#
# 1. Large-sample sensitivity of the NORMALITY test. With n = 600 Shapiro-Wilk
#    rejects on deviations far too small to disturb the F test. So where W is
#    near 1 and the QQ plot is straight, we trust the plot. This argument
#    applies to normality ONLY. It is explicitly NOT used for equal variance
#    any more - see change note 2 at the top.
#
# 2. No multiplicity correction ACROSS models. Tukey adjusts the pairwise
#    contrasts *within* each by-group, but nothing adjusts across the ~16
#    models fitted below, so the family-wise error rate of the whole sweep is
#    well above 0.05. Worse, when a simple-effect family contains only ONE
#    comparison (e.g. "No - Yes" within each company size) Tukey adjusts
#    nothing at all, so those p-values are completely raw. This sweep is
#    exploratory: a borderline result (p roughly 0.01-0.05) is a lead to
#    confirm, not a confirmed finding. The large repeated effects - leadership,
#    company location, experience level - are far too strong to be explained by
#    multiplicity.
#
# 3. 45 exactly duplicated substantive records remain in the data (Step 1 could
#    not tell collection duplicates from genuine repeated observations). They
#    are pseudo-replication and mildly inflate every F statistic below.
# --------------------------------------------------------------------------

# Cannot do > 2 factors, since it results in some 0 observation cells, which
# makes the interaction inestimable.
# Everything below is wrapped in print() so the output is the same whether the
# file is run with Rscript, with source(), or with RStudio's plain "Source"
# button - the last two use print.eval = FALSE and would otherwise silently
# drop every Anova / emmeans / pairs table.

# --- helpers ---------------------------------------------------------------

# Draws the interaction plot. `y_lab` must describe the scale actually passed in
# as `response`: all the calls below pass sqrt(salary) because that is the scale
# the models are fitted on, so the picture and the test agree.
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

# One call per model: normality check, equal-variance check, Type III table,
# and - only when Breusch-Pagan rejects - the HC3 robust Type III table as the
# remedy. Returns TRUE when the model is heteroscedastic, so the caller knows to
# take its simple effects from the robust covariance too.
report_model <- function(model) {
  print(shapiro.test(residuals(model)))
  bp <- bptest(model)
  print(bp)
  print(Anova(model, type = 3))

  heteroscedastic <- bp$p.value < 0.05
  if (heteroscedastic) {
    cat("\n*** Breusch-Pagan rejects (p =", signif(bp$p.value, 3), ").",
      "Read the HC3 table below, not the classical one above. ***\n")
    print(Anova(model, type = 3, white.adjust = "hc3"))
  }
  invisible(heteroscedastic)
}

# Tukey-adjusted simple effects on the USD scale.
# `robust = TRUE` swaps in the HC3 covariance so the standard errors match the
# ANOVA table we actually read for that model.
# NOTE ON SCALE: emmeans detects the sqrt() on the response, so regrid() maps
# the estimates back to USD. Squaring a mean of square roots is not an
# arithmetic mean (Jensen's inequality) - it lands about 8-13% below it. Read
# every "estimate" below as a difference of back-transformed marginal means,
# i.e. a median-like centre, not a difference of mean salaries.
simple_effects <- function(model, spec, simple, robust = FALSE) {
  vc <- if (robust) car::hccm(model, type = "hc3") else stats::vcov(model)
  emm <- emmeans(model, spec, vcov. = vc)
  print(pairs(regrid(emm), simple = simple))
}


# --- choice of transformation ----------------------------------------------

draw_interaction(
  x = data$company_size, line = data$leadership, response = data$salary,
  y_lab = "Mean salary in USD"
)
model <- lm(salary ~ company_size * leadership, data = data)
plot_diagnostic(model)
report_model(model)
boxcox(model)
# Box-Cox on the untouched response peaks at lambda ~= 0.4, and the residuals of
# the untransformed model are clearly right skewed. lambda = 0.5 (the square
# root) is inside the confidence interval and is far easier to explain in the
# report than lambda = 0.4, so the square root is used for every model below.
# Note this differs from the winsorised response, where Box-Cox peaked at 0.6 -
# another reason not to fit on the clamped column.


# --- company_size x leadership ---------------------------------------------

draw_interaction(x = data$company_size, line = data$leadership, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ company_size * leadership, data = data)
plot_diagnostic(model)
het <- report_model(model)
print(emmeans(model, ~ company_size * leadership))
simple_effects(model, ~ company_size * leadership, "leadership", het)
simple_effects(model, ~ company_size * leadership, "company_size", het)
# Breusch-Pagan does NOT reject here (p = 0.069), so the classical table stands.
# Significance level 0.05 throughout.
#
# NO interaction (p = 0.080). Leadership and company size act additively.
# Leadership main effect is very strong (p = 1.8e-10); company size is weak
# (p = 0.038).
#
# Leadership premium, at every company size (back-transformed, see note above):
#   Small  82,010 USD (p = 0.0007)
#   Medium 36,564 USD (p = 0.019)   <- see caveat 2: this is a family of ONE
#                                      comparison, so Tukey adjusted nothing.
#                                      Treat it as a lead, not a firm finding.
#   Large  71,224 USD (p < 0.0001)
#
# Company size only matters for NON-leadership staff: small pays 46,461 USD less
# than medium and 36,049 USD less than large (both p < 0.0001), while medium vs
# large is not significant (p = 0.19). Among leadership staff no size contrast is
# significant at all - but note only 10 / 24 / 30 observations, so that is low
# power, not evidence of equality.


# --- company_size x role ----------------------------------------------------

draw_interaction(x = data$company_size, line = data$role, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ company_size * role, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ company_size * role, "role", het)
simple_effects(model, ~ company_size * role, "company_size", het)
# Breusch-Pagan rejects (p = 0.011) - driven by the wider salary spread at large
# companies - so the HC3 table is the one to read, and the contrasts above use
# the HC3 covariance too.
#
# NO interaction (HC3 p = 0.17). Both main effects are real: company size
# p = 2.1e-09, role p = 0.0085.
#
# Role differences are small and only surface at small companies, where analysts
# earn 42,465 USD less than engineers (p = 0.0024). Nothing else is significant.
# Company size shows the same shape as before for every role: small is below
# medium and large, and medium vs large is never significant (p = 0.44 - 0.97).
#   Analyst     small - medium -57,720 (p < 0.0001), small - large -54,034 (p = 0.005)
#   Engineering small - medium -28,100 (p = 0.012)
#   Research    small - medium -51,234 (p = 0.0002), small - large -48,158 (p = 0.0009)


# --- company_size x experience_level ---------------------------------------

draw_interaction(x = data$company_size, line = data$experience_level, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ company_size * experience_level, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ company_size * experience_level, "company_size", het)
simple_effects(model, ~ company_size * experience_level, "experience_level", het)
# Breusch-Pagan rejects (p = 0.0019), so read the HC3 table.
#
# THIS IS A CASE WHERE THE REMEDY CHANGES THE ANSWER, and it is worth putting in
# the report:
#   company_size   classical p = 0.011  ->  HC3 p = 0.22  (no longer significant)
#   interaction    classical p = 0.062  ->  HC3 p = 0.060 (not significant either way)
#   experience     p < 2e-16 under both  (unmoved)
# The design is very unbalanced (cells from 3 to 185), so the classical company
# size effect was resting on the equal-variance assumption the model failed.
# Conclusion: once experience level is in the model and variance is handled
# honestly, company size adds nothing. Experience level is doing the work.
#
# Experience gradient, consistent across every company size: entry < mid < senior,
# with the entry-to-senior step the largest (about 54,000 / 89,000 / 74,000 USD at
# small / medium / large, all p < 0.002). Senior vs Executive is never significant
# - there are only 3 / 12 / 8 executives per size, so this is a power limit, not
# evidence that the two levels are paid alike.
# The only place company size still separates is at mid-level, where small pays
# 36,219 USD less than medium and 41,464 USD less than large (both p < 0.0001).


# --- company_size x company_location ---------------------------------------

draw_interaction(x = data$company_size, line = data$company_location, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ company_size * company_location, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ company_size * company_location, "company_location", het)
simple_effects(model, ~ company_size * company_location, "company_size", het)
# Breusch-Pagan rejects (p = 0.0015), so read the HC3 table.
#
# ANOTHER CASE WHERE THE REMEDY CHANGES THE ANSWER:
#   interaction    classical p = 0.022  ->  HC3 p = 0.072  (NOT significant)
# The previous version of this file reported "An interaction" here and built a
# conclusion on it. It was an artefact of assuming equal variance across cells
# that run 31 / 51 / 88 / 105 / 108 / 217. We therefore report NO interaction and
# treat the two factors as additive.
#
# Both main effects are strong and survive the remedy: company location
# p < 2.2e-16 (by far the largest effect anywhere in this analysis) and company
# size p = 0.0013.
#
# US premium at every company size: 40,030 USD small (p = 0.003), 75,226 USD
# medium and 86,908 USD large (both p < 0.0001).
# The size contrasts are significant only within US companies (small - medium
# -42,947, p = 0.002; small - large -55,754, p = 0.0003) and not within non-US
# companies. Since the interaction is NOT significant, describe that as an
# observed pattern in the simple effects, NOT as evidence that company size
# behaves differently inside and outside the US.


# --- why residence x location is NOT fitted --------------------------------
# The cross-tab below shows the two variables carry almost the same information
# after the US/Other collapse, and one off-diagonal cell holds only 2
# observations. R fits that model without any rank-deficiency warning, but the
# interaction would be estimated off a 2-observation cell and the two main
# effects cannot be separated from each other at all. We keep company_location
# as the single geographic factor throughout: it is the location the salary is
# actually paid in. interaction_plots.R records the same decision.
print(table(data$employee_residence, data$company_location,
  dnn = c("employee_residence", "company_location")
))


# --- remote_ratio x company_location ---------------------------------------
# Required by the brief and previously missing from the whole analysis.
# remote_ratio is numeric in Step 1 (0 / 50 / 100) and is factored here so the
# three tiers are treated as groups rather than as a straight line.
# It is paired with company_location on purpose: the raw boxplot in Step 2 makes
# the 50% tier look badly paid, but that tier is 79% non-US, so location has to
# be in the model before the remote effect means anything.
data$remote_ratio_f <- factor(data$remote_ratio)
draw_interaction(x = data$remote_ratio_f, line = data$company_location, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ remote_ratio_f * company_location, data = data)
plot_diagnostic(model)
het <- report_model(model)
# Breusch-Pagan does not reject (p = 0.50), so the classical table stands.
#
# remote_ratio has NO effect on salary: main effect p = 0.53, interaction with
# location p = 0.48. Company location remains overwhelming (p < 2e-16).
#
# This is the useful answer to the confound flagged in Step 2. The raw boxplot
# makes the 50%-remote tier look about 28,000 USD worse off than the other two,
# but 76 of its 96 observations are non-US. Once location is held fixed the
# apparent remote effect disappears entirely. Report the null, and report why the
# raw plot is misleading - that contrast is more informative than either on its own.


# --- experience_level x leadership: NOT fitted -----------------------------

draw_interaction(x = data$experience_level, line = data$leadership, response = sqrt(data$salary))
# The Entry x Yes cell contains ZERO observations, so the interaction is
# INESTIMABLE - there is no data from which to estimate that cell's mean. This
# is not the same as finding no interaction, and the model is therefore not
# fitted. (Substantively it makes sense: nobody holds a Lead/Director/Manager
# title at entry level.)


# --- experience_level x role -----------------------------------------------

draw_interaction(x = data$experience_level, line = data$role, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ experience_level * role, data = data)
plot_diagnostic(model)
het <- report_model(model)
# Breusch-Pagan rejects (p = 0.032), so read the HC3 table. Here the remedy
# STRENGTHENS a result rather than removing one - a useful counter-example
# against the idea that robust standard errors just make everything conservative:
#   role   classical p = 0.052 (not significant)  ->  HC3 p = 0.0053 (significant)
#
# NO interaction (p = 0.62): the experience gradient has the same shape in all
# three disciplines. Experience level dominates (p < 2.2e-16), role is a real but
# much smaller additive effect.


# --- leadership x company_location ------------------------------------------

draw_interaction(x = data$leadership, line = data$company_location, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ leadership * company_location, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ leadership * company_location, "company_location", het)
simple_effects(model, ~ leadership * company_location, "leadership", het)
# Breusch-Pagan does not reject (p = 0.25), so the classical table stands.
#
# NO interaction (p = 0.12). This is the model that changed most when we moved
# off the winsorised response: on the clamped column the interaction came out at
# p = 0.034 and the old file interpreted it at length. Ten clamped rows were
# producing it. On the observed response there is nothing there, and leadership
# and location are cleanly additive.
#
# Both main effects are very strong (leadership p = 1.2e-11, location p < 2.2e-16):
#   US premium:         74,900 USD for non-leadership, 68,067 USD for leadership
#   Leadership premium: 54,466 USD outside the US,     47,633 USD inside the US
#   (all four p < 0.0001)
# Additive reading: being in the US and holding a leadership title each add to
# salary independently, and the two premiums simply stack.


# --- leadership x role ------------------------------------------------------

draw_interaction(x = data$leadership, line = data$role, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ leadership * role, data = data)
plot_diagnostic(model)
het <- report_model(model)
# Breusch-Pagan does not reject (p = 0.090), so the classical table stands.
#
# Leadership is strong (p = 9.4e-08). Role is NOT significant here (p = 0.17) and
# there is no interaction (p = 0.69). Compare with the role x company_location
# model below, where role IS significant (p = 9.9e-06): role only separates once
# location is in the model, because location is the dominant driver and the three
# roles are not spread evenly across US / non-US. No simple effects are extracted
# here since nothing but leadership is significant.


# --- role x company_location ------------------------------------------------

draw_interaction(x = data$role, line = data$company_location, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ role * company_location, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ role * company_location, "company_location", het)
simple_effects(model, ~ role * company_location, "role", het)
# Breusch-Pagan does not reject (p = 0.094), so the classical table stands.
#
# Both main effects, NO interaction (p = 0.77). Location p < 2.2e-16, role
# p = 9.9e-06.
#
# US premium in every discipline: 67,224 USD analysts, 84,647 USD engineers,
# 79,500 USD researchers (all p < 0.0001).
# Within a location, analysts are the lowest paid:
#   non-US: analyst - engineering -16,530 (p = 0.048), analyst - research -20,823 (p = 0.011)
#   US:     analyst - engineering -33,953 (p < 0.0001), analyst - research -33,100 (p = 0.0001)
# Engineering vs Research is never significant at either location (p = 0.74, 0.99).
# The US gaps look about twice the non-US gaps, but the interaction test says that
# difference is not distinguishable from noise, so do not claim the gap widens in
# the US - report it as an additive analyst penalty.


# ===========================================================================
# Salary trends over work_year (required task 3c)
# ===========================================================================

draw_interaction(x = data$work_year_f, line = data$experience_level, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ work_year_f * experience_level, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ work_year_f * experience_level, "work_year_f", het)
# Breusch-Pagan rejects (p = 7.4e-05), so read the HC3 table.
#
# NO year effect at all in this model (HC3 p = 0.75) and no interaction
# (p = 0.996). Experience level dominates (p < 2.2e-16). None of the 9 pairwise
# year contrasts is significant.
#
# Read together with the work_year x company_size and work_year x role models
# below, which DO find a 2022 rise: the year effect is not a uniform raise, it is
# concentrated in particular company sizes and roles, and it does not show up
# once observations are grouped by experience level.


draw_interaction(x = data$work_year_f, line = data$company_location, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ work_year_f * company_location, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ work_year_f * company_location, "work_year_f", het)
# Breusch-Pagan rejects (p = 1.6e-05), so read the HC3 table.
#
# Both main effects, NO interaction (p = 0.56). Year p = 0.0086, location
# p < 2.2e-16.
# The only significant year contrast is outside the US: 2021 -> 2022 is worth
# 14,658 USD (p = 0.017); 2020 -> 2022 is borderline at 17,273 USD (p = 0.057).
# Inside the US no year contrast is significant. A plausible reading is post-COVID
# catch-up in non-US markets, but with no interaction we cannot claim the two
# locations trended differently - state it as a description of the simple effects.


draw_interaction(x = data$work_year_f, line = data$company_size, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ work_year_f * company_size, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ work_year_f * company_size, "work_year_f", het)
# Breusch-Pagan rejects (p = 0.0019), so read the HC3 table.
#
# This is the ONE interaction in the whole file that survives the robust remedy:
#   interaction  classical p = 0.021  ->  HC3 p = 0.029  (still significant)
# Main effects also hold (year p = 0.0011, company size p = 0.00015).
#
# The interaction is driven entirely by medium companies, where 2021 -> 2022 is
# worth 55,189 USD (p < 0.0001). No year contrast is significant at small or
# large companies.
#
# CAUTION for the report: the medium/2022 cell holds 258 of the 600 observations
# while small/2022 holds 12. Much of this "interaction" may be the sample
# composition changing over time rather than pay changing. Flag it; do not sell
# it as a firm causal finding.


draw_interaction(x = data$work_year_f, line = data$role, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ work_year_f * role, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ work_year_f * role, "work_year_f", het)
# Breusch-Pagan rejects (p = 0.00059), so read the HC3 table.
#
#   interaction  classical p = 0.069  ->  HC3 p = 0.12   (not significant either way)
# Both main effects hold: year p = 3.7e-09, role p = 0.014.
#
# All three disciplines rise into 2022, which is why there is a main effect but
# no interaction:
#   Analyst      2020 -> 2022  +50,467 USD (p = 0.0015)
#   Engineering  2020 -> 2022  +35,097 USD (p = 0.004); 2021 -> 2022 +30,461 (p = 0.003)
#   Research     2021 -> 2022  +51,843 USD (p < 0.0001)
# This is the clearest evidence in the file for a genuine 2022 salary rise.


draw_interaction(x = data$work_year_f, line = data$leadership, response = sqrt(data$salary))
model <- lm(sqrt(salary) ~ work_year_f * leadership, data = data)
plot_diagnostic(model)
het <- report_model(model)
simple_effects(model, ~ work_year_f * leadership, "work_year_f", het)
# Breusch-Pagan rejects (p = 0.022), so read the HC3 table.
#
#   interaction  classical p = 0.048 (significant)  ->  HC3 p = 0.053 (not)
# This one lands almost exactly on the 0.05 line and flips across it. Do not
# report it as either "there is" or "there is no" interaction - report it as
# borderline and inconclusive, and note that caveat 2 applies with force: with
# ~16 models fitted and no correction across them, a p-value sitting on 0.05 is
# exactly what multiplicity produces by chance.
#
# The main effects are solid (year p = 0.014, leadership p = 2.5e-08).
# Descriptively, the 2022 rise sits with non-leadership staff (2020 -> 2022
# +40,580 USD and 2021 -> 2022 +43,107 USD, both p < 0.0001) while no year
# contrast is significant for leadership staff - but there are only 10 / 37 / 17
# leaders per year, so that is weak power rather than a flat salary.


# Restore whatever contrasts setting the session had before this file ran, so
# sourcing Step 3 and then Step 4 in one session cannot change Step 4's results.
options(old_contrasts)
