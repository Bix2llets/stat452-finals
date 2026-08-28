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
| `normality_report.csv` | Skewness, excess kurtosis, and the chi-square goodness-of-fit statistic against a fitted normal |
| `categorical_summary_table.csv` | Count and percent of every level of the 11 categorical variables |
| `numeric_by_outcome_table.csv` | Mean and sd per diagnosis group, plus the standardised mean difference, sorted by its size |
| `categorical_by_outcome_table.csv` | Disease rate within every category level |
| `categorical_association_strength.csv` | Safest and riskiest level of each categorical variable, with the risk difference and odds ratio between them |

### Figures

| File | What it shows |
| ---- | ------------- |
| `numeric_histograms.pdf` | Distribution of each numerical variable |
| `numeric_qq.pdf` | Normal QQ plot of each numerical variable |
| `skewed_variables_transformed.pdf` | The three skewed variables, before and after their transformation |
| `categorical_distributions.pdf` | Composition of the sample on each categorical variable |
| `numeric_by_outcome_boxplots.pdf` | Each numerical variable split by diagnosis |
| `top_predictors_histograms.pdf` | The four most separating variables, on the patient-count scale so the two groups appear at their true relative size |
| `disease_rate_by_category.pdf` | Disease rate per category against the 30.3% baseline |
| `correlation_heatmap.pdf` | Correlations, grouped by what the variables measure |
| `age_relationships.pdf` | How age moves the other measurements |

### Outcome in one line

The exercise stress test carries most of the information — peak heart rate
separates the two groups by 1.55 pooled standard deviations and
exercise-induced angina moves the diagnosis rate from 19.1 % to 67.8 %, far
ahead of everything else — while alcohol and sleep separate them least; three
variables are right skewed and need transforming, no single variable separates
the groups on its own, and the wearable-owner and sex gaps are both confounded
in ways Section 6 of the write-up spells out.

### Deliberate choices

- Effect sizes are reported instead of p-values: the standardised mean
  difference for the numerical variables, the risk difference and odds ratio
  for the categorical ones. At n = 9,000 nearly any difference is significant,
  so a p-value would not tell us which variables matter. The tests belong in
  Step 3. The odds ratio is quoted because it is the quantity Step 3's logistic
  regression estimates, so its coefficients can be checked against this table.
- Normality is judged by the chi-square goodness-of-fit test against a fitted
  normal, read by the size of the statistic rather than by its p-value — at
  n = 9,000 the test rejects departures far too small to matter, and it does so
  for 17 of 24 variables when only 3 of them are worth acting on. The QQ plots
  and the skewness column carry the conclusion.
- Every method used here is one the course covers: the goodness-of-fit test,
  odds ratios and dummy coding from the logistic-regression chapter, the pooled
  standard deviation from the ANOVA chapter, and the transformations from the
  polynomial-regression-and-transformation chapter.
