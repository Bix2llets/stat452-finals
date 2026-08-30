# Step 4 Training Results: Continuous Salary Regression Modeling
## Applied Statistics for Engineers and Scientists II (STAT452)

---

## 1. Data Partitioning (Train/Test Split)

The cleaned dataset (600 observations) was partitioned into an 80% Training set (480 observations) and a 20% Testing set (120 observations) using `set.seed(6767)` for full reproducibility.

| Dataset Partition | Number of Observations | Proportion |
|---|---:|---:|
| **Training Set** | 480 | 80.0% |
| **Testing Set** | 120 | 20.0% |
| **Total** | 600 | 100.0% |

Reference levels for dummy coding:
- `experience_level`: Entry
- `employment_type`: Fulltime
- `company_location`: America
- `company_size`: Small
- `role`: Analyst
- `leadership`: No (Individual Contributor)

---

## 2. Model 1: Multiple Linear Regression (MLR)

### 2.1 Full Model Summary
- **Multiple $R^2$:** 0.5286 ($52.86\%$)
- **Adjusted $R^2$:** 0.5144 ($51.44\%$)
- **Residual Standard Error (RSE):** \$43,970 on 465 DF
- **Overall Model Utility Test:** $F(14, 465) = 37.24, \quad p\text{-value} < 2.2 \times 10^{-16}$

### 2.2 Multicollinearity Diagnostics (Generalized VIF)
All $\text{GVIF}^{1/(2 \cdot \text{Df})} < 1.30$, confirming that multicollinearity is negligible ($\text{VIF} < 4.0$).

---

## 3. Best Subset Selection using Mallow's $C_p$ Criterion & Stepwise AIC

Best subset regression (`leaps::regsubsets`) was performed across all subset sizes $k = 1, \dots, 14$:

| Number of Predictors ($k$) | Mallow's $C_p$ | Adjusted $R^2$ | BIC | Residual Sum of Squares (RSS) |
|---|---:|---:|---:|---:|
| 1 | 329.85 | 0.1813 | -84.67 | $1.558 \times 10^{12}$ |
| 2 | 207.83 | 0.3058 | -158.71 | $1.318 \times 10^{12}$ |
| 3 | 155.56 | 0.3597 | -192.35 | $1.213 \times 10^{12}$ |
| 4 | 86.07 | 0.4315 | -244.23 | $1.075 \times 10^{12}$ |
| 5 | 69.89 | 0.4489 | -254.01 | $1.040 \times 10^{12}$ |
| 6 | 47.17 | 0.4731 | -270.42 | $9.921 \times 10^{11}$ |
| 7 | 34.61 | 0.4870 | -278.06 | $9.640 \times 10^{11}$ |
| 8 | 24.97 | 0.4979 | -283.23 | $9.415 \times 10^{11}$ |
| 9 | 18.74 | 0.5053 | -285.23 | $9.256 \times 10^{11}$ |
| 10 | 13.70 | 0.5116 | **-286.17** | $9.120 \times 10^{11}$ |
| **11 (Optimal $C_p$)** | **12.18** | **0.5142** | -283.59 | **$9.052 \times 10^{11}$** |
| 12 | 12.69 | 0.5147 | -278.96 | $9.023 \times 10^{11}$ |
| 13 | 13.88 | 0.5145 | -273.61 | $9.007 \times 10^{11}$ |
| 14 (Full) | 15.00 | 0.5144 | -268.34 | $8.990 \times 10^{11}$ |

* **Key Finding:** Mallow's $C_p$ achieves its minimum at **$C_p = 12.18 \approx p = 12$** (11 predictor terms + intercept).
* **Consensus with Stepwise AIC:** The 11 variables selected by Mallow's $C_p$ are **100% identical** to those chosen by Backward Stepwise AIC, eliminating `remote_ratio` and `work_year` as non-essential predictors:

| Predictor Term | Coefficient ($\hat{\beta}$) | Std. Error | $t$-value | $p$-value | Significance |
|---|---:|---:|---:|---:|---|
| **(Intercept)** | +\$57,925 | \$8,452 | 6.853 | $2.29 \times 10^{-11}$ | *** |
| **`experience_levelMid-level`** | +\$16,227 | \$6,388 | 2.540 | 0.0114 | * |
| **`experience_levelSenior`** | +\$44,879 | \$6,758 | 6.640 | $8.71 \times 10^{-11}$ | *** |
| **`experience_levelExecutive`** | +\$83,619 | \$12,360 | 6.766 | $3.99 \times 10^{-11}$ | *** |
| **`employment_typeOther`** | -\$20,587 | \$11,146 | -1.847 | 0.0654 | . |
| **`company_locationAsia`** | -\$71,344 | \$8,235 | -8.663 | $< 2.0 \times 10^{-16}$ | *** |
| **`company_locationEurope`** | -\$59,765 | \$5,114 | -11.687 | $< 2.0 \times 10^{-16}$ | *** |
| **`company_locationOther`** | -\$17,018 | \$18,918 | -0.900 | 0.3688 | |
| **`company_sizeMedium`** | +\$15,507 | \$6,384 | 2.429 | 0.0155 | * |
| **`company_sizeLarge`** | +\$23,451 | \$6,649 | 3.527 | 0.00046 | *** |
| **`roleEngineering`** | +\$34,731 | \$5,527 | 6.284 | $7.59 \times 10^{-10}$ | *** |
| **`roleResearch`** | +\$31,976 | \$5,673 | 5.637 | $3.00 \times 10^{-8}$ | *** |
| **`leadershipYes`** | +\$22,892 | \$6,961 | 3.288 | 0.00108 | ** |

- **Stepwise / $C_p$ Multiple $R^2$:** 0.5262 ($52.62\%$)
- **Adjusted $R^2$:** 0.5140 ($51.40\%$)
- **RSE:** \$43,990

---

## 4. Model 2: Polynomial Regression (Degree 2)

Second-degree orthogonal polynomial expansion was applied to the continuous feature `remote_ratio`:
$$\text{salary\_in\_usd} = \beta_0 + \sum \beta_j X_j + \gamma_1 \cdot \text{remote\_ratio} + \gamma_2 \cdot \text{remote\_ratio}^2 + \epsilon$$

- **Polynomial Multiple $R^2$:** 0.5296 ($52.96\%$)
- **Polynomial Adjusted $R^2$:** 0.5144 ($51.44\%$)
- **RSE:** \$43,970
- On the Test Set, Polynomial Regression achieved $\text{RMSE} = \$46,859.64$ and $R^2 = 0.3187$, improving upon Linear OLS ($R^2 = 0.3153$).

---

## 5. MLR Assumption Diagnostics

| Assumption | Test / Metric | Result | Statistical Decision |
|---|---|---|---|
| **1. Normality of Residuals** | Shapiro-Wilk Test | $W = 0.9739, \quad p = 1.47 \times 10^{-7}$ | Departures in upper tail (skewed salaries) |
| **2. Homoscedasticity** | Breusch-Pagan Test | $BP = 44.501, \quad p = 1.25 \times 10^{-5}$ | Higher variance among senior earners |
| **3. Independence** | Durbin-Watson Test | $DW = 1.8408, \quad p = 0.076$ | Fail to reject $H_0$ (No autocorrelation) |

All 4 standard diagnostic plots have been rendered and saved to `mlr_diagnostics.pdf`.

---

## 6. Regularized Regression Models (Ridge, LASSO, Elastic Net Grid Search)

1. **Ridge Regression ($\alpha = 0$):**
   - $\lambda_{\min} = 2696.415, \quad \lambda_{1\text{se}} = 20877.36$
   - Shrinks all coefficients smoothly to mitigate variance without eliminating variables.
2. **LASSO Regression ($\alpha = 1$):**
   - $\lambda_{\min} = 63.7564, \quad \lambda_{1\text{se}} = 4194.748$
   - Performs automatic feature selection.
3. **Elastic Net Grid Search ($\alpha \in [0.0, 1.0]$ with $\Delta\alpha = 0.05$):**
   - Optimal CV mixing parameter on training data: **$\alpha^* = 0.55$** ($\lambda_{\min} = 96.2395, \text{CV-MSE} = 1.986 \times 10^9$).
   - Standard academic comparison: $\alpha = 0.50$ ($\lambda_{\min} = 96.4588$).

---

## 7. Artifacts Generated
- Model Bundle: `activity1/step4/continuous_salary_models.rds`
- Diagnostic Plots: `activity1/step4/mlr_diagnostics.pdf`
- Formal Test Set Evaluation (RMSE, MAE, $R^2$) is conducted in **Step 6**.
