# Record of work

| Name | Work |
| ---- | ---- |
| Phat |      |
| Bao  |      |
| Dung | Perform descriptive statistics, summarize some tables and figures |

### Tables (CSV)

| File | What it holds |
| ---- | ------------- |
| `numeric_summary_table.csv` | n, mean, sd, CV, min, Q1, median, Q3, max, IQR, skewness, excess kurtosis for the 24 numerical variables |
| `normality_report.csv` | Skewness, excess kurtosis, Jarque-Bera p, Shapiro W (n = 5,000 subsample) |
| `categorical_summary_table.csv` | Count and percent of every level of the 11 categorical variables |
| `numeric_by_outcome_table.csv` | Mean and sd per diagnosis group, plus Cohen's d, sorted by \|d\| |
| `categorical_by_outcome_table.csv` | Disease rate within every category level |
| `categorical_association_strength.csv` | Cramer's V of each categorical variable with the diagnosis |

### Figures

| File | What it shows |
| ---- | ------------- |
| `numeric_histograms.pdf` | Distribution of each numerical variable |
| `numeric_qq.pdf` | Normal QQ plot of each numerical variable |
| `skewed_variables_transformed.pdf` | The three skewed variables, before and after a transformation |
| `categorical_distributions.pdf` | Composition of the sample on each categorical variable |
| `numeric_by_outcome_boxplots.pdf` | Each numerical variable split by diagnosis |
| `top_predictors_density.pdf` | The four most separating variables, showing how much the groups still overlap |
| `disease_rate_by_category.pdf` | Disease rate per category against the 30.3% baseline |
| `correlation_heatmap.pdf` | Correlations, ordered by clustering |
| `age_relationships.pdf` | How age moves the other measurements |

### Outcome in one line

The exercise stress test carries most of the information — peak heart rate
separates the two groups by 1.55 pooled standard deviations and
exercise-induced angina by Cramer's V = 0.45, far ahead of everything else —
while alcohol and sleep separate them least; three variables are right skewed
and need transforming, no single variable separates the groups on its own, and
the wearable-owner and sex gaps are both confounded in ways Section 6 of the
write-up spells out.

### Deliberate choices

- Effect sizes (Cohen's d, Cramer's V) are reported instead of p-values. At
  n = 9,000 nearly any difference is significant, so a p-value would not tell
  us which variables matter. The tests belong in Step 3.
- `shapiro.test` caps at 5,000 observations, so it is run on a seeded random
  subsample; the shape conclusions rest on skewness and the QQ plots instead.
