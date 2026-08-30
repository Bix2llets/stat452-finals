# Step 7: Feature Importance, Interpretation, and Recommendations
## Activity 1: Data Science Job Salaries Analysis

---

## 1. Feature Importance Analysis

### 1.1 Significant Predictors from Multiple Linear Regression (Stepwise MLR)

Under the optimal Multiple Linear Regression model selected via AIC and Mallow's $C_p$, the estimated dollar coefficients ($\hat{\beta}$) holding all other factors constant are summarized below (Reference categories: `Entry-level`, `Non-US Company / Other`, `Small Company`, `Analyst`, `Individual Contributor / No Leadership`):

| Predictor Term | Estimated Impact ($\hat{\beta}$) | Std. Error | $t$-value | $p$-value | Significance |
|---|---:|---:|---:|---:|---|
| **`experience_levelMid-level`** | +\$16,686 | \$6,009 | 2.777 | 0.0057 | ** |
| **`experience_levelSenior`** | +\$43,763 | \$6,342 | 6.901 | $1.69 \times 10^{-11}$ | *** |
| **`experience_levelExecutive`** | +\$83,735 | \$11,702 | 7.155 | $3.23 \times 10^{-12}$ | *** |
| **`employment_typeOther`** | -\$23,416 | \$10,681 | -2.192 | 0.0288 | * |
| **`company_locationUS`** | **+\$65,716** | **\$4,386** | **14.983** | **$< 2.0 \times 10^{-16}$** | *** |
| **`company_sizeLarge`** | +\$17,477 | \$6,331 | 2.761 | 0.0060 | ** |
| **`roleEngineering`** | +\$30,909 | \$5,235 | 5.904 | $6.80 \times 10^{-9}$ | *** |
| **`roleResearch`** | +\$29,318 | \$5,399 | 5.431 | $9.03 \times 10^{-8}$ | *** |
| **`leadershipYes`** | +\$20,894 | \$6,644 | 3.145 | 0.0018 | ** |

---

### 1.2 Non-Zero Feature Selection from LASSO Regression

LASSO regularized regression ($\alpha = 1, \lambda_{\min} = 63.1534$) identified the top non-zero predictive features ranked by absolute coefficient magnitude:
1. `experience_levelExecutive`: +\$81,403.01
2. `company_locationUS`: +\$64,610.21
3. `experience_levelSenior`: +\$41,807.82
4. `roleEngineering`: +\$30,404.89
5. `roleResearch`: +\$29,432.18
6. `employment_typeOther`: -\$22,912.68
7. `leadershipYes`: +\$22,056.10
8. `company_sizeLarge`: +\$16,765.82
9. `experience_levelMid-level`: +\$15,764.75
10. `company_sizeMedium`: +\$6,596.87
11. `work_year`: +\$3,796.36
12. `remote_ratio`: +\$37.77

---

### 1.3 Odds Ratios from Binary Logistic Regression (Top 25% Earner Bracket)

| Factor / Term | Odds Ratio ($\exp(\hat{\beta})$) | 95% Wald CI | Statistical Interpretation |
|---|---:|---|---|
| **`company_locationUS`** | **15.8720** | $[7.69, 32.77]$ | US companies multiply top-tier odds by 15.87x vs non-US |
| **`experience_level.L` (Seniority)** | **10.8565** | $[3.64, 32.42]$ | Advancing in seniority multiplies top-tier odds by ~10.9x |
| **`roleEngineering`** | **6.7453** | $[3.35, 13.57]$ | Engineering tracks have 6.75x higher odds than Analysts |
| **`roleResearch`** | **5.8804** | $[2.90, 11.91]$ | Research/Scientist tracks have 5.88x higher odds than Analysts |
| **`leadershipYes` (Management)** | **3.5013** | $[1.70, 7.20]$ | Leadership roles multiply top-tier odds by 3.50x |
| **`company_size.L`** | **2.2947** | $[1.15, 4.58]$ | Larger company size increases top-tier odds by 2.29x |

---

## 2. Answers to the 3 Core Research Questions

### 📌 Research Question 1: Salary Forecasting & Key Drivers
* **Forecasting Accuracy:** On the 120-observation independent test set, **Ridge Regression** achieved the best predictive performance ($\text{RMSE} = \$46,128.66, \text{MAE} = \$33,852.67, R^2 = 0.3398$). Elastic Net ($\text{RMSE} = \$46,600.54, R^2 = 0.3262$) and LASSO ($\text{RMSE} = \$46,607.82, R^2 = 0.3260$) also outperformed standard OLS ($\text{RMSE} = \$46,910.31, R^2 = 0.3173$).
* **Model Selection Consensus:** **Mallow's $C_p$ criterion** ($C_p = 10.64$) and **Backward Stepwise AIC** reached 100% agreement on selecting the identical 10-variable parsimonious model ($R^2_{\text{train}} = 56.29\%, \text{Adjusted } R^2 = 55.35\%$).
* **Key Determinants:** Geographic location (US premium $+\$65,716$) and seniority (Executive $+\$83,735$, Senior $+\$43,763$) are the dominant determinants, followed by discipline (Engineering $+\$30,909$, Research $+\$29,318$) and leadership ($+\$20,894$).

### 📌 Research Question 2: Impact & Multi-Factor Interaction
* Factorial ANOVA and regression models establish statistically significant differences across experience levels ($p < 10^{-16}$), company location ($p < 10^{-16}$), company sizes ($p < 0.01$), and functional roles ($p < 10^{-7}$).
* Geographic location and seniority interact substantially: high-earning ceilings for Senior and Executive professionals are heavily concentrated in US-based companies.

### 📌 Research Question 3: Top 25% Income Probability Prediction
* Binary Logistic Regression achieved **$81.0\%$ Accuracy**, **$\text{AUC} = 0.87$**, **Precision $62.07\%$**, and **Recall $69.23\%$**, showing excellent discrimination.
* The probability of entering the top 25% bracket is highest for **Senior/Executive professionals in Engineering or Research roles at Large US companies with leadership responsibilities**.

---

## 3. Actionable Recommendations

### 3.1 For Data Science Professionals & Career Planning
1. **Target US-Based Employers:** Working for US companies provides an unmatched $+\$65,716$ wage premium and multiplies top-tier odds by 15.87x.
2. **Specialize in Engineering & Research:** Transitioning into Data Engineering, Machine Learning Engineering, or Applied Science offers a significant earnings uplift (+\$29,300–\$30,900) over generalist reporting analysis.
3. **Strive for Technical Leadership:** Stepping into Lead/Manager/Staff positions adds an average +\$20,894 premium and increases top-tier odds by 3.50x.

### 3.2 For Recruitment & HR Compensation Strategy
1. **Regional Compensation Benchmarking:** Non-US firms seeking global top-tier talent must narrow the wage gap by offering flexible remote equity/dollar-denominated bonuses.
2. **Retention Strategies for Mid-to-Senior:** Because Senior professionals command a +\$43,763 market premium, structured promotion pipelines and retention incentives at the 3–5 year experience mark are critical to reduce costly turnover.

### 3.3 For Statistical Modeling & Future Data Collection
1. **Purchasing Power Parity (PPP):** Future analyses should incorporate local cost-of-living indices to reflect real purchasing power rather than nominal converted USD.
2. **Granular Skillsets:** Collecting specific technology stack proficiencies (e.g., PyTorch, AWS, Kubernetes, Spark) will allow estimation of marginal skill premiums.
