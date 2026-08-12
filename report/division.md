# Task division

1. Salary Forecasting & Key Drivers Identification: How accurately can we predict the salary (salary-in-usd) of a Data Science professional globally, and which key factors play a crucial role in determining income levels?

2. Impact & Multi-Factor Interaction Analysis: Are there statistically significant differences in salary across experience levels, company sizes, or geographic locations? Do these factors operate independently, or do they exhibit interaction effects with one
   another in shaping compensation?

3. Top 25% Income Probability Prediction (Top-tier): What specific attributes (e.g., job role, employment type, experience level) increase or decrease the likelihood of a professional entering the top 25% salary bracket, and how effectively can a Logistic Regression model predict this probability?

## Workflow

Theo phase, output ra folder chung bằng saveRDS, loadRDS (model) hoặc write.csv(), read.csv() cho file dữ liệu

## Task list

### Phase 1 Step 1 Data Preprocessing

- Check for and handle missing values, duplicate records, and potential outliers.
- Group the granular job titles (job-title) into major, meaningful job categories (e.g., Data Analyst, Data Scientist, Data Engineer, Machine Learning/AI Engineer, Management/Lead).
- Encode categorical variables (using Ordinal/One-Hot Encoding) and scale numerical features as necessary.

Phat, Dung

### Phase 1 Step 2 Exploratory Data Analysis (EDA)

- Perform descriptive statistics and visualize the data to understand the distribution of salary-in-usd.
- Visualize the relationships between salary and key factors, including job category, experience level, employment type, remote work ratio, company size, and location.

Phat, Dung

### Phase 2 Step 3 Inferential Statistics & Factorial ANOVA

Select and justify appropriate statistical techniques to:

- Implement a Multi-Factor ANOVA (Factorial ANOVA) model to evaluate mean salary differences across groups while assessing both main effects and interaction effects (e.g., Experience Level × Company Size, or Experience Level × Job Category) on salary-in-usd.
- Test the underlying assumptions of ANOVA (normality of residuals, homogeneity of variance) and propose remedy methods if any assumption is violated.
- Analyze trends in salary changes over time (work-year).

Phat, Dung

### Phase 2 Step 4 Continuous Salary Regression Modeling

- Partition the dataset into Training and Testing sets using an appropriate split
  ratio.
- Construct at least two continuous regression models to predict salary-in-usd (e.g.,
  Multiple Linear Regression, Polynomial Regression, Ridge Regression, LASSO
  Regression).

  Phat, Bao

### Phase 2 Step 5 Binary Logistic Regression Modeling

- Frame a classification problem: Create a binary target variable Y (is-top-tier)
  based on the 75th percentile (Q3) of salary-in-usd (Y = 1 if the salary falls into
  the Top 25% highest earning tier, Y = 0 otherwise).
- Fit a Binary Logistic Regression model to estimate the probability of a professional
  reaching the top-tier salary bracket.

- Interpret the estimated coefficients using Odds Ratios (exp(β)) to identify key
  factors that significantly increase or decrease the likelihood of achieving a high
  salary.

  Phat, Bao

### Phase 3 Step 6 Model Evaluation & Comparison

- Evaluate the continuous regression models from step 4 using RMSE, MAE,
  and R2 metrics.
- Evaluate the Logistic Regression model from step 5 using Accuracy, Precision,
  Recall, F1-Score, and ROC-AUC.
- Select the best model for each task and justify your choice.

Phat,

### Phase 3 Step 7 Interpretation and Recommendations

- Feature Importance Interpretation: Interpret the significance and impact of key
  variables in the selected models.
- Propose potential avenues for model improvement or alternative analytical ap-
  proaches to achieve better predictive performance

Phat,

### Writing report

## Activity 2

Dataset:

Phat

### Clean + Description

Detailed report + Reproducible step for cleaning
Phat

### Descriptive statistic

Plots, tables, etc
Phat

### Raise question + apply model

Phat

### Write report

Phat
