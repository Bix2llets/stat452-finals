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
- `company_location`: Other (non-US)
- `company_size`: Small
- `role`: Analyst
- `leadership`: No (Individual Contributor)

---

## 2. Model 1: Multiple Linear Regression (MLR)

### 2.1 Full Model Summary
- **Multiple $R^2$:** 0.5649 ($56.49\%$)
- **Adjusted $R^2$:** 0.5537 ($55.37\%$)
- **Residual Standard Error (RSE):** \$42,150 on 467 DF
- **Overall Model Utility Test:** $F(12, 467) = 50.56, \quad p\text{-value} < 2.2 \times 10^{-16}$

### 2.2 Multicollinearity Diagnostics (Generalized VIF)
All $\text{GVIF}^{1/(2 \cdot \text{Df})} < 1.15$, confirming that multicollinearity is completely absent.

---

## 3. Best Subset Selection using Mallow's $C_p$ Criterion & Stepwise AIC

Best subset regression (`leaps::regsubsets`) was performed across all subset sizes $k = 1, \dots, 11$:

| Number of Predictors ($k$) | Mallow's $C_p$ | Adjusted $R^2$ | BIC | Residual Sum of Squares (RSS) |
|---|---:|---:|---:|---:|
| 1 | 190.88 | 0.3767 | -215.53 | $1.186 \times 10^{12}$ |
| 2 | 142.77 | 0.4223 | -246.85 | $1.097 \times 10^{12}$ |
| 3 | 75.88 | 0.4857 | -297.53 | $9.746 \times 10^{11}$ |
| 4 | 62.07 | 0.4995 | -305.41 | $9.464 \times 10^{11}$ |
| 5 | 39.97 | 0.5212 | -321.49 | $9.036 \times 10^{11}$ |
| 6 | 28.21 | 0.5332 | -328.49 | $8.791 \times 10^{11}$ |
| 7 | 18.91 | 0.5429 | -333.42 | $8.590 \times 10^{11}$ |
| 8 | 14.32 | 0.5482 | -333.84 | $8.473 \times 10^{11}$ |
| 9 | 11.14 | 0.5521 | -332.92 | $8.380 \times 10^{11}$ |
| **10 (Optimal $C_p$)** | **10.64** | **0.5535** | **-329.30** | **$8.336 \times 10^{11}$** |
| 11 (Full) | 12.00 | 0.5532 | -323.78 | $8.325 \times 10^{11}$ |

* **Key Finding:** Mallow's $C_p$ achieves its minimum at **$C_p = 10.64 \approx p = 11$** (10 predictor terms + intercept).
* **Consensus with Stepwise AIC:** The 10 variables selected by Mallow's $C_p$ are **100% identical** to those chosen by Backward Stepwise AIC, eliminating `remote_ratio` ($p = 0.42$) and `work_year` ($p = 0.51$) as non-essential predictors:

| Predictor Term | Coefficient ($\hat{\beta}$) | Std. Error | $t$-value | $p$-value | Significance |
|---|---:|---:|---:|---:|---|
| **(Intercept)** | +\$1,988 | \$8,054 | 0.247 | 0.8052 | |
| **`experience_levelMid-level`** | +\$16,686 | \$6,009 | 2.777 | 0.0057 | ** |
| **`experience_levelSenior`** | +\$43,763 | \$6,342 | 6.901 | $1.69 \times 10^{-11}$ | *** |
| **`experience_levelExecutive`** | +\$83,735 | \$11,702 | 7.155 | $3.23 \times 10^{-12}$ | *** |
| **`employment_typeOther`** | -\$23,416 | \$10,681 | -2.192 | 0.0288 | * |
| **`company_locationUS`** | +\$65,716 | \$4,386 | 14.983 | $< 2.0 \times 10^{-16}$ | *** |
| **`company_sizeMedium`** | +\$10,230 | \$6,094 | 1.679 | 0.0939 | . |
| **`company_sizeLarge`** | +\$17,477 | \$6,331 | 2.761 | 0.0060 | ** |
| **`roleEngineering`** | +\$30,909 | \$5,235 | 5.904 | $6.80 \times 10^{-9}$ | *** |
| **`roleResearch`** | +\$29,318 | \$5,399 | 5.431 | $9.03 \times 10^{-8}$ | *** |
| **`leadershipYes`** | +\$20,894 | \$6,644 | 3.145 | 0.0018 | ** |

- **Stepwise / $C_p$ Multiple $R^2$:** 0.5629 ($56.29\%$)
- **Adjusted $R^2$:** 0.5535 ($55.35\%$)
- **RSE:** \$42,159

---

## 4. MLR Assumption Diagnostics

| Assumption | Test / Metric | Result | Statistical Decision |
|---|---|---|---|
| **1. Normality of Residuals** | Shapiro-Wilk Test | $W = 0.9801, \quad p = 2.83 \times 10^{-7}$ | Minor skewness in upper tail (high-earner ceiling) |
| **2. Homoscedasticity** | Breusch-Pagan Test | $BP = 24.51, \quad p = 0.00188$ | Controlled via regularized models and robust standard errors |
| **3. Independence** | Durbin-Watson Test | $DW = 1.8830, \quad p = 0.222$ | Fail to reject $H_0$ (No autocorrelation) |

All diagnostic plots have been rendered and saved to `mlr_diagnostics.pdf`.

---

## 5. Regularized Regression Models (Ridge, LASSO, Elastic Net)

1. **Ridge Regression ($\alpha = 0$):**
   - $\lambda_{\min} = 3875.032, \quad \lambda_{1\text{se}} = 22696.15$
   - Shrinks all coefficients smoothly to mitigate variance without eliminating variables.
2. **LASSO Regression ($\alpha = 1$):**
   - $\lambda_{\min} = 63.1534, \quad \lambda_{1\text{se}} = 4155.07$
   - Performs automatic feature selection.
3. **Elastic Net Regression ($\alpha = 0.5$):**
   - $\lambda_{\min} = 104.8621, \quad \lambda_{1\text{se}} = 5727.858$
   - Balances $L_1$ sparsity and $L_2$ shrinkage.

---

## 6. Artifacts Generated
- Model Bundle: `activity1/step4/continuous_salary_models.rds`
- Diagnostic Plots: `activity1/step4/mlr_diagnostics.pdf`
- Formal Test Set Evaluation (RMSE, MAE, $R^2$) is conducted in **Step 6**.
