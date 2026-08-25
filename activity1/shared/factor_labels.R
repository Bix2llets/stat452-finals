# Shared factor-label helper for Step 2 / Step 3.
#
# Step 1 recodes the ordinal variables to integers (S/M/L -> 1/2/3,
# EN/MI/SE/EX -> 0/1/2/3, leadership -> 0/1) and then calls as.factor(), so the
# cleaned data set carries bare digits as level names. That is fine for the
# model algebra, but it makes every plot axis and every emmeans contrast label
# unreadable ("company_size1 - company_size2" instead of "S - M").
#
# This helper restores human-readable level names *without* changing the level
# ORDER, so the levels stay ordinal in the intended sense and every estimate /
# contrast sign stays exactly the same as before relabelling. The factors are
# deliberately left unordered: Type III ANOVA below is run under contr.sum, and
# an ordered factor would silently switch to polynomial contrasts instead.
#
# It is idempotent - re-sourcing it, or loading data that has already been
# relabelled, leaves the data unchanged.

relabel_if <- function(x, from, to) {
  if (identical(levels(x), from)) {
    factor(as.character(x), levels = from, labels = to)
  } else {
    x
  }
}

apply_factor_labels <- function(data) {
  if (!is.null(data$company_size)) {
    data$company_size <- relabel_if(
      data$company_size,
      c("1", "2", "3"), c("S", "M", "L")
    )
  }
  if (!is.null(data$experience_level)) {
    data$experience_level <- relabel_if(
      data$experience_level,
      c("0", "1", "2", "3"), c("EN", "MI", "SE", "EX")
    )
  }
  if (!is.null(data$leadership)) {
    data$leadership <- relabel_if(
      data$leadership,
      c("0", "1"), c("NonLead", "Lead")
    )
  }
  data
}
