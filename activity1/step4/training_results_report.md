# Step 4 Training Results

## Data split

The models were trained on 285 observations from 2020-2021 and produced predictions for 315 observations from the future year 2022.

| Set | 2020 | 2021 | 2022 | Total |
|---|---:|---:|---:|---:|
| Training | 71 | 214 | 0 | 285 |
| Testing | 0 | 0 | 315 | 315 |

The strict temporal check `max(training year) < min(testing year)` passed. All modeled categorical variables remained factors, and no unseen factor level occurred in the 2022 data.

All factors use treatment coding. Reference levels are Entry, Fulltime, Other company location, Small company, Analyst, and No leadership.

## Multiple linear regression

The fitted model is full rank: 13 coefficients and rank 13.

| Training statistic | Result |
|---|---:|
| Residual standard error | 61,364.57 USD |
| Multiple R-squared | 0.4439 |
| Adjusted R-squared | 0.4194 |
| Overall F statistic | 18.09 on 12 and 272 df |
| Overall p-value | < 2.2e-16 |

Terms with training p-values below 0.05 were:

| Term | Estimate (USD) | p-value |
|---|---:|---:|
| `experience_levelSenior` | 44,863.21 | 8.25e-05 |
| `experience_levelExecutive` | 127,012.86 | 3.23e-08 |
| `company_locationUS` | 73,802.23 | < 2e-16 |
| `company_sizeLarge` | 19,640.61 | 0.0363 |
| `roleEngineering` | 26,632.18 | 0.0164 |
| `roleResearch` | 30,575.05 | 0.00579 |

With treatment coding, each estimate is a direct difference from its reference category while the other predictors are held fixed. For example, the fitted Senior salary is 44,863.21 USD above Entry, conditional on the other model variables. These are in-sample model results and do not measure performance on 2022.

## MLR diagnostic results

The diagnostics use training residuals only. No observation was removed and the fitted formula was not changed.

| Check | Result | Reading |
|---|---:|---|
| Mean residual | -8.74e-13 USD | Numerically zero, as expected for OLS with an intercept |
| Residual-mean t-test | p = 1.000 | No evidence that the mean differs from zero |
| Shapiro-Wilk | W = 0.79755, p < 2.2e-16 | Residual normality is rejected |
| Breusch-Pagan | BP = 26.127, df = 12, p = 0.0103 | Evidence of non-constant variance |
| Maximum adjusted GVIF | 1.2083 | No concerning multicollinearity signal |

Adjusted GVIF values were low for every model term:

| Term | Adjusted GVIF |
|---|---:|
| `work_year` | 1.0161 |
| `experience_level` | 1.0780 |
| `employment_type` | 1.0188 |
| `remote_ratio` | 1.0476 |
| `company_location` | 1.0761 |
| `company_size` | 1.0276 |
| `role` | 1.0252 |
| `leadership` | 1.2083 |

Influence rules flagged observations for investigation:

| Rule | Cutoff | Flagged observations |
|---|---:|---:|
| Cook's distance > 4/n | 0.01404 | 18 |
| Leverage > 2p/n | 0.09123 | 23 |
| Absolute studentized residual > 3 | 3.00000 | 7 |

The maximum Cook's distance was 0.308 for source row 252. The maximum leverage was 0.1685, and the maximum absolute studentized residual was 6.0408.

The diagnostic plots are saved in `mlr_diagnostics.pdf`. Residuals vs Fitted shows a mild curved pattern and wider spread at larger fitted values. The Q-Q plot has a strong upper-tail departure. Scale-Location also shows increasing spread. Residuals vs Leverage identifies several observations that warrant investigation.

These findings make coefficient p-values and confidence intervals less reliable under the usual constant-variance normal-error assumptions. Reasonable follow-up options are a log-salary model, HC3 heteroscedasticity-robust standard errors, robust regression, and a sensitivity analysis of influential observations. They are recommendations only; Step 4 does not automatically delete observations or replace the two required models.

## LASSO regression

Lambda was tuned with deterministic five-fold cross-validation using only the 285 training observations. Every fold contains both training years, so `work_year` varies in every fitting subset:

| Work year | Fold 1 | Fold 2 | Fold 3 | Fold 4 | Fold 5 |
|---|---:|---:|---:|---:|---:|
| 2020 | 15 | 14 | 14 | 14 | 14 |
| 2021 | 43 | 43 | 43 | 43 | 42 |

The minimum-CV-error penalty was `lambda.min = 658.633` (mean CV MSE `4,030,796,712.17`). The final penalty uses the more conservative one-standard-error rule: `lambda.1se = 12,929.225` (mean CV MSE `4,619,173,198.09`). This rule favors a more stable and parsimonious model whose estimated error is within one standard error of the minimum.

After refitting on all 2020-2021 data, the nonzero coefficients were:

| Term | Coefficient (USD) |
|---|---:|
| Intercept | 67,674.70 |
| `experience_levelSenior` | 8,600.50 |
| `experience_levelExecutive` | 47,196.42 |
| `company_locationUS` | 50,157.55 |
| `leadershipYes` | 19,663.96 |

All other LASSO slope coefficients were zero at the selected lambda.

The internal folds mix 2020 and 2021, so their CV error is not a temporal-forecast estimate. This is the explicit trade-off for using all training observations and allowing `work_year` to vary during tuning. The untouched 2022 set remains the only strict future-year evaluation set.

## Prediction sanity checks

Both models produced 315 finite predictions for 2022.

| Model | Minimum prediction | Maximum prediction | Negative predictions |
|---|---:|---:|---:|
| Multiple linear regression | -9,002.37 USD | 249,848.26 USD | 2 |
| LASSO | 67,674.70 USD | 184,692.63 USD | 0 |

The two negative MLR predictions are not realistic salaries. They are a known limitation of unconstrained linear regression and should be discussed when Step 6 compares the models.

## What is not reported here

Test RMSE, MAE, R-squared, and final model selection are intentionally not calculated in Step 4. They belong to Step 6. Therefore, this report does not claim that either model predicts 2022 better.
