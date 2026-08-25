# Final Project — Applied Statistics for Engineers and Scientists II

## What This Project Is

This is a group final project for the course **Applied Statistics for Engineers and Scientists II**. Each group is assigned one topic containing **two activities**, and must produce a single combined report analyzing real-world data using statistical and machine learning methods learned in the course.

The overall goal is to demonstrate the full data analysis workflow — from cleaning raw data, to exploring it visually, to testing hypotheses, to building and evaluating predictive models — and to communicate the findings in a clear, well-structured scientific report.

## Tech Stack / Tools

- **Language:** R
- **Environment:** RStudio
- **Code formats accepted:** `.R` script, `.Rmd` (R Markdown), or exported `.html`
- No other programming language is permitted for the analysis — all statistical work must be done in R/RStudio.

## What the Project Involves

### Activity 1 (6 points) — Data Science Job Salaries
Analyze the Kaggle **"Data Science Job Salaries"** dataset (`ds-salaries.csv`, 607 rows × 11 columns) to understand what drives Data Science salaries worldwide. This involves:
- Cleaning and preprocessing the data (missing values, duplicates, outliers, job-title grouping, encoding)
- Exploratory Data Analysis (EDA) and visualization
- Factorial ANOVA to test for salary differences across experience level, company size, location, and their interactions
- Building regression models (e.g., Multiple Linear, Polynomial, Ridge, LASSO) to predict salary
- Building a Logistic Regression model to predict whether someone falls in the top 25% salary bracket
- Evaluating and comparing all models with standard metrics (RMSE, MAE, R², Accuracy, Precision, Recall, F1, ROC-AUC)
- Interpreting results and giving recommendations

### Activity 2 (4 points) — Own Dataset
Each group picks its **own dataset** (ideally related to their major, or sourced from UCI/Kaggle) containing at least 2 categorical and 4 numerical variables, then:
- Cleans and describes the data
- Runs descriptive statistics
- Applies appropriate statistical models from the course to answer a research question the group defines themselves
- Interprets results and proposes conclusions/recommendations

## Deliverables

One combined report (max 60 pages, excluding appendices) covering both activities, including:
- Cover page (group members' full names + student IDs)
- Table of contents
- Data collection/cleaning, data description, and statistical analysis sections
- R code, references, data sources
- Breakdown of individual member contributions

**Submitted as one compressed file to Moodle containing:**
1. The report as a PDF
2. A folder with the R code (`.R`, `.Rmd`, or `.html`)
3. A folder with the data used

## Key Rules

- Work independently — copying another group's work is not allowed.
- AI tools may only be used as a study aid; submitted code, visuals, interpretations, and writing must be the group's own and in the group's own words. Directly copying AI-generated output results in a **zero grade**.
- **Deadline:** August 30, 2026

## Related Files

- [`project-requirement-activity1.md`](./project-requirement-activity1.md) — full detailed requirements for Activity 1
- [`project-requirement-activity2.md`](./project-requirement-activity2.md) — full detailed requirements for Activity 2
