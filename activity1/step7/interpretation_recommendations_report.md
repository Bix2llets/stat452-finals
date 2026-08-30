# Step 7: Feature Importance, Interpretation, and Recommendations
## Activity 1: Data Science Job Salaries Analysis

---

## 1. Feature Importance Analysis

### 1.1 Significant Predictors from Multiple Linear Regression (Stepwise MLR)

Under the optimal Multiple Linear Regression model selected via AIC and Mallow's $C_p$, the estimated dollar coefficients ($\hat{\beta}$) holding all other factors constant are summarized below (Reference categories: `Entry-level`, `America`, `Small Company`, `Analyst`, `Individual Contributor / No Leadership`):

| Predictor Term | Estimated Impact ($\hat{\beta}$) | Std. Error | $t$-value | $p$-value | Significance |
|---|---:|---:|---:|---:|---|
| **(Intercept)** | +\$57,925 | \$8,452 | 6.853 | $2.29 \times 10^{-11}$ | *** |
| **`experience_levelMid-level`** | +\$16,227 | \$6,388 | 2.540 | 0.0114 | * |
| **`experience_levelSenior`** | +\$44,879 | \$6,758 | 6.640 | $8.71 \times 10^{-11}$ | *** |
| **`experience_levelExecutive`** | +\$83,619 | \$12,360 | 6.766 | $3.99 \times 10^{-11}$ | *** |
| **`company_locationAsia`** | -\$71,344 | \$8,235 | -8.663 | $< 2.0 \times 10^{-16}$ | *** |
| **`company_locationEurope`** | -\$59,765 | \$5,114 | -11.687 | $< 2.0 \times 10^{-16}$ | *** |
| **`company_sizeMedium`** | +\$15,507 | \$6,384 | 2.429 | 0.0155 | * |
| **`company_sizeLarge`** | +\$23,451 | \$6,649 | 3.527 | 0.00046 | *** |
| **`roleEngineering`** | +\$34,731 | \$5,527 | 6.284 | $7.59 \times 10^{-10}$ | *** |
| **`roleResearch`** | +\$31,976 | \$5,673 | 5.637 | $3.00 \times 10^{-8}$ | *** |
| **`leadershipYes`** | +\$22,892 | \$6,961 | 3.288 | 0.00108 | ** |

---

### 1.2 Non-Zero Feature Selection from LASSO Regression

LASSO regularized regression ($\alpha = 1, \lambda_{\min} = 63.7564$) identified the top non-zero predictive features ranked by absolute coefficient magnitude:
1. `experience_levelExecutive`: +\$81,212.65
2. `company_locationAsia`: -\$69,621.97
3. `company_locationEurope`: -\$58,229.37
4. `experience_levelSenior`: +\$42,969.46
5. `roleEngineering`: +\$33,984.38
6. `roleResearch`: +\$32,160.24
7. `leadershipYes`: +\$23,881.74
8. `company_sizeLarge`: +\$22,667.80
9. `employment_typeOther`: -\$20,392.17
10. `company_sizeMedium`: +\$12,516.67

---

### 1.3 Odds Ratios from Binary Logistic Regression (Top 25% Earner Bracket)

| Factor / Term | Odds Ratio ($\exp(\hat{\beta})$) | 95% Wald CI | Statistical Interpretation |
|---|---:|---|---|
| **`experience_level.L` (Seniority)** | **9.9253** | $[3.28, 30.06]$ | Advancing in seniority multiplies top-tier odds by ~9.9x |
| **`roleEngineering`** | **6.6346** | $[3.33, 13.23]$ | Engineering tracks have 6.63x higher odds than Analysts |
| **`roleResearch`** | **5.8520** | $[2.91, 11.78]$ | Research/Scientist tracks have 5.85x higher odds than Analysts |
| **`leadership.L` (Management)** | **2.6342** | $[1.57, 4.41]$ | Leadership roles multiply top-tier odds by 2.63x |
| **`company_size.L`** | **2.6853** | $[1.33, 5.41]$ | Larger company size increases top-tier odds by 2.69x |
| **`company_locationAsia`** | **0.0831** | $[0.017, 0.398]$ | Asian firms have 91.7% lower odds vs American employers |
| **`company_locationEurope`** | **0.0415** | $[0.015, 0.118]$ | European firms have 95.8% lower odds vs American employers |

---

## 2. Answers to the 3 Core Research Questions

### 📌 Research Question 1: Salary Forecasting & Key Drivers
* **Forecasting Accuracy:** On the 120-observation independent test set, **Ridge Regression** achieved the best predictive performance ($\text{RMSE} = \$46,435.13, \text{MAE} = \$33,980.78, R^2 = 0.3310$). Elastic Net ($\text{RMSE} = \$46,882.60, R^2 = 0.3181$) and LASSO ($\text{RMSE} = \$46,883.71, R^2 = 0.3180$) also outperformed standard OLS ($\text{RMSE} = \$46,977.65, R^2 = 0.3153$).
* **Model Selection Consensus:** **Mallow's $C_p$ criterion** ($C_p = 12.18$) and **Backward Stepwise AIC** reached 100% agreement on selecting the identical 11-variable parsimonious model ($R^2_{\text{train}} = 52.62\%, \text{Adjusted } R^2 = 51.40\%$).
* **Key Determinants:** Geographic location (American premium $+\$59.8\text{k}-\$71.3\text{k}$) and seniority (Executive $+\$83.6\text{k}$, Senior $+\$44.9\text{k}$) are the dominant determinants, followed by discipline (Engineering/Research $+\$32\text{k}-\$34.7\text{k}$) and leadership ($+\$22.9\text{k}$).

### 📌 Research Question 2: Impact & Multi-Factor Interaction
* Factorial ANOVA and regression models establish statistically significant differences across experience levels ($p < 10^{-16}$), company locations ($p < 10^{-16}$), and company sizes ($p < 0.001$).
* Geographic location and seniority interact substantially: high-earning ceilings for Senior and Executive professionals are heavily concentrated in American-based companies.

### 📌 Research Question 3: Top 25% Income Probability Prediction
* Binary Logistic Regression achieved **$80.0\%$ Accuracy** and **$\text{AUC} = 0.87$**, showing excellent discrimination.
* The probability of entering the top 25% bracket is highest for **Senior/Executive professionals in Engineering or Research roles at Medium/Large American companies**.

---

## 3. Actionable Recommendations

### 3.1 For Data Science Professionals & Career Planning
1. **Specialize in Engineering & Research:** Transitioning into Data Engineering, Machine Learning Engineering, or Applied Science offers a significant earnings uplift (+\$32,000–\$34,700) over generalist reporting analysis.
2. **Target Global / American Employers:** Prioritize remote contracts or relocation with US/American firms to capture the massive geographic compensation differential.
3. **Strive for Technical Leadership:** Stepping into Lead/Manager/Staff positions adds an average +\$22,892 premium and increases top-tier odds by 2.63x.

### 3.2 For Recruitment & HR Compensation Strategy
1. **Regional Compensation Benchmarking:** European and Asian firms seeking global top-tier talent must narrow the wage gap by offering flexible remote equity/dollar-denominated bonuses.
2. **Retention Strategies for Mid-to-Senior:** Because Senior professionals command a +\$44,879 market premium, structured promotion pipelines and retention incentives at the 3–5 year experience mark are critical to reduce costly turnover.

### 3.3 For Statistical Modeling & Future Data Collection
1. **Purchasing Power Parity (PPP):** Future analyses should incorporate local cost-of-living indices to reflect real purchasing power rather than nominal converted USD.
2. **Granular Skillsets:** Collecting specific technology stack proficiencies (e.g., PyTorch, AWS, Kubernetes, Spark) will allow estimation of marginal skill premiums.
