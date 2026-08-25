# Activity 1 Step 4 improvement proposal

## Scope and source of truth

This audit uses `FinalProject_2026.md` as the authoritative specification. It
covers Activity 1 Steps 1-4 only. Activity 2, every `task.md` file, and Activity
1 Steps 5-7 are outside scope.

The Step 4 requirement is to make an appropriate training/testing partition and
fit at least two continuous regression models for `salary-in-usd`. Model metrics
and model selection belong to Step 6, so Step 4 should create held-out
predictions and reusable model artifacts without calculating RMSE, MAE, or
R-squared.

## Requirement-to-implementation map

| Step | Requirement | Current implementation | Status and Step 4 consequence |
| --- | --- | --- | --- |
| 1 | Check missing values | `data_processing.R` reports zero missing values. The raw file has no blank/NA cells. | Complete. |
| 1 | Check and handle duplicates | The script finds 42 duplicates after removing non-model columns but retains them because the index differs. | Incomplete. The index is only a row identifier and does not establish independence. Identical modeling records must not cross the Step 4 split. |
| 1 | Check and handle outliers | Salaries above the full-data 1.5-IQR limit are clamped to USD 280,911. | Unsuitable for Step 4. Ten observed salaries are overwritten using a bound learned before splitting, leaking test-response information and preventing later evaluation against observed salaries. |
| 1 | Group job titles | Titles are reduced to `Analyst`, `Engineering`, and `Research`, with a separate leadership flag. | Partly complete. Seven management/head records become unclassified and are dropped, including all five `Head of Data` records. |
| 1 | Encode categorical variables | Several variables are converted to human-readable ordered factors. Employment type and both country variables are collapsed to two levels. | Partly complete. Step 4 must deliberately create nominal dummy variables; it must not inherit ordered polynomial contrasts accidentally. The broad location/employment collapses lose potentially useful predictive information. |
| 1 | Scale numeric features as necessary | Full-data standardized copies of year and remote ratio are saved alongside the raw variables. | Unsuitable for Step 4. These copies leak full-data means/SDs and are exactly collinear with their raw variables. Step 4 must ignore them and learn scaling from training data only. |
| 2 | Describe `salary-in-usd` | A six-number summary, raw/square-root histograms, QQ plots, and normality tests are produced. | Substantially complete, but n, SD, and IQR are not reported in a reproducible grouped table/text output. |
| 2 | Visualize key relationships | Plots cover experience, employment, year, residence, company location, company size, leadership, and role. | Incomplete: the required salary-versus-remote-ratio view is absent. Several comments use inferential wording (for example, “no difference” or “no significant difference”) based only on plots. |
| 3 | Factorial ANOVA with interactions | `multi_factor_anova.R` fits a broad set of two-factor interaction models and Type III tests. | Present, but the analysis is an exploratory battery rather than a clearly selected primary factorial model. No adjustment is made across the model battery. |
| 3 | Check assumptions and propose remedies | Shapiro-Wilk, Breusch-Pagan, diagnostic plots, Box-Cox, and square-root models are used. | Present but incomplete as a remedy: 12 of 14 replayed square-root models still reject homoscedasticity at 0.05. Some comments override tests by visual judgment without robust SEs or another formal remedy. |
| 3 | Analyze salary over time | Year-by-factor models and pairwise comparisons are fitted after treating year as categorical. | Present. This compares years but does not estimate a numeric trend with an effect size and confidence interval. |
| 4 | Reproducible train/test split | `activity1/step4/` is empty. | Missing. |
| 4 | At least two continuous salary models | No Step 4 model exists. | Missing. |

## Data audit

- The raw CSV has 607 observations, 11 substantive variables, and one artificial
  index column. It has no missing values.
- Ignoring the index, there are 42 beyond-first exact duplicate rows in 29
  duplicate groups; 71 rows participate in those groups and the largest group
  has six rows. All 42 remain after the current seven-row role filter.
- The observed salary ranges from USD 2,859 to USD 600,000. Ten observations
  exceed the 1.5-IQR upper limit of USD 280,911. These values are plausible
  high-earning records rather than data-entry errors evident from this file.
- The current cleaned RDS has 600 rows and 12 columns. It has no missing values,
  but it has 52 beyond-first identical rows after lossy category collapsing and
  salary clamping.
- Seven rows are removed solely because title matching does not assign a role:
  five `Head of Data`, one `Head of Machine Learning`, and one `Machine Learning
  Manager`. All seven are leaders, three are executives, and their mean observed
  salary is about USD 142,422. Their selective removal can understate leadership
  and executive effects.
- The actual retained role counts are Analyst 127, Engineering 248, and Research
  225. Comments claiming a roughly 540-to-56 imbalance are stale.
- Employment type is highly unbalanced (588 FT versus 19 combined PT/CT/FL in
  the raw file). Geography is high-cardinality: 50 company-country and 57
  residence-country codes, with many rare levels. Training-only rare-level
  pooling is preferable to a global irreversible US/Other collapse for Step 4.
- Residence and company location are strongly associated. In the cleaned data,
  574 of 600 rows agree on US/Other status, and one off-diagonal cell has only two
  rows. Including both in an ordinary least-squares model would make individual
  geographic coefficients unstable.
- Remote ratio has only three observed values (0, 50, and 100). Treating it as a
  number imposes a linear effect; that modeling choice must be explicit.

## Errors and improvements in Steps 1-3

### Step 1

The central Step 4 blocker is that the observed outcome is discarded. The
cleaned `salary_in_usd` is a full-data-winsorized value, so an untouched test
outcome cannot be recovered from `cleaned_data.rds`. A minimal supporting change
is necessary: retain the source row identifier, the observed salary, and the
original employment/location codes as additional columns before the existing
legacy transformations. The existing winsorized and collapsed variables can
remain unchanged for Steps 2-3, avoiding a silent change to their results.

The full-data `standardized_year` and `standardized_remote_ratio` columns must
not enter Step 4. Step 4 will select raw numeric predictors explicitly and fit
scaling parameters on training data. Duplicate records will be retained for the
existing descriptive/inferential work, but identical modeling records will be
assigned to a common split group so no exact twin appears on both sides of the
holdout. This avoids rewriting the earlier analyses while directly addressing
Step 4 leakage.

The seven dropped management records, stale comments, and broad category
collapses are real Step 1 weaknesses. Retaining the original employment and
location codes supports a better Step 4 model without changing Steps 2-3. The
management-role classification needs a domain decision because changing the
meaning/levels of `role` would alter multiple Step 3 models; it should remain a
clearly stated limitation in this scoped pass rather than being guessed.

### Step 2

The required remote-ratio relationship is missing. The current cleaned-data
group summaries are: ratio 0, n = 127 and mean USD 105,023; ratio 50, n = 96 and
mean USD 77,430; ratio 100, n = 377 and mean USD 119,419. A future Step 2 pass
should add this plot and reproducible grouped summaries, but neither is required
to execute Step 4, so no Step 2 source change is proposed here.

The script is working-directory dependent and initially lacked an installed
`tseries` dependency. Its 12 checked-in PDFs correspond to current explicit
`ggsave()` calls. The square-root distribution is closer to normal, but marginal
outcome normality is not itself a regression assumption and must not be the sole
reason for a Step 4 transformation.

### Step 3

The factorial models, diagnostics, Box-Cox investigation, and time comparisons
cover the requested topics. Important limitations are the many unadjusted model
tests, remaining heteroscedasticity, a sparse residence-by-location model,
several stale or unsupported interpretations, and dollar-scale interpretations
that square transformed emmeans without a bias adjustment. Ordered factors also
use polynomial contrasts rather than ordinary dummy coding.

These issues do not block Step 4. Step 4 should use the defensible part of the
earlier work—the square-root scale as a candidate variance/skewness remedy—while
estimating a training-only back-transformation correction and keeping all model
evaluation on the original dollar scale for Step 6.

## Proposed Step 4 implementation

1. Load the cleaned RDS, assert its required schema and data quality, and select
   only the untouched observed outcome plus explicitly named predictors. Exclude
   the winsorized target, artificial identifier, and full-data standardized
   columns from the model formula.
2. Create a deterministic signature from the selected predictors and observed
   outcome. Use it as the grouping variable for a seeded 80/20 stratified split,
   keeping identical modeling records entirely in training or testing.
3. Use company location as the geographic predictor in the primary feature set;
   do not put the strongly associated residence field into the ordinary linear
   model. Keep the original employment and company-country codes preserved by
   Step 1.
4. Fit a preprocessing recipe on training data only. It will pool rare training
   categories, create explicit unknown/novel handling, one-hot encode nominal
   variables, remove zero-variance columns, and normalize numeric predictors.
   The prepared recipe will then be applied unchanged to testing data.
5. Model `sqrt(salary_in_usd)` to reduce skew and the leverage of plausible high
   earners without deleting or clamping them. Fit three continuous models:
   multiple linear regression, ridge regression, and LASSO regression.
6. Tune ridge and LASSO with seeded grouped cross-validation using training rows
   only. Identical records must also stay together inside tuning folds. The
   untouched test set must not influence preprocessing, lambda choice, or
   back-transformation correction.
7. Convert square-root predictions to dollars with a nonnegative square and an
   empirical training-only smearing correction. Store both transformed, naive,
   and corrected dollar predictions so Step 6 can transparently assess the
   choice.
8. Save one R-native artifact containing split identifiers, the trained recipe,
   fitted model objects, selected lambdas, transformation corrections, and
   held-out predictions. A short text training log may record counts and model
   construction details, but it must not calculate Step 6 metrics.

## Validation criteria

- Exactly one `.R` implementation file exists in `activity1/step4/`.
- Re-running Step 1 followed by Step 4 with seed 6767 reproduces the same source
  row IDs, split, tuning folds, lambdas, and predictions.
- Approximately 80% of rows are in training and 20% in testing; both sets are
  nonempty and their source IDs and duplicate groups are disjoint.
- All observed salary groups are represented adequately by stratification, and
  no test response contributes to a learned preprocessing or tuning parameter.
- Baked training/testing matrices have identical predictor columns, contain no
  missing or infinite values, and accept unseen categorical values without an
  error.
- The linear model is full rank; ridge and LASSO converge; every stored held-out
  prediction is finite and nonnegative on the dollar scale.
- Step 4 produces no RMSE, MAE, R-squared, model ranking, or best-model choice.
- No CSV is created in a results folder.

## Planned scope of changes

- `activity1/step1/data_processing.R`: only preserve fields needed for a
  leakage-safe Step 4 and clarify the related comments.
- `activity1/dataset/cleaned_data.rds`: regenerate through Step 1; existing
  Steps 2-3 analysis variables remain unchanged.
- `activity1/step4/continuous_salary_regression.R`: the sole Step 4 R script.
- Step 4 R-native/text artifacts, this audit, the requested change log, and
  `summary.md` in Steps 1-4.
- No planned edits to Step 2 or Step 3 source, Activity 2, Activity 1 Steps 5-7,
  any `task.md`, or unrelated files.
