# CodeSamples
The code samples (written in R) are taken from an ongoing project on evaluating the impacts of child marriage on wellbeing among adolescents in Peru, Ethiopia, India, and Vietnam in the Young Lives project.

The [Young Lives project](https://www.younglives.org.uk) is a longitudinal dataset following around 2000 children in each country, collecting data on a variety of indicators including health, education, and labour outcomes.

This code preprocesses, conducts multiple imputation, and a number of analyses on the research question. 

# What it contains
The repository contains three files:
- Data_preprocessing
- Multiple_imputation
- Data_analyses
  
## Data_preprocessing
This is the first coding sample for the. It is the data pre-processing file for PERU. 
Here are the key steps:

Data cleaning
1. Adding marital status, wellbeing, mental health data and covariates to the dataset
2. Transforming variables if necessary for standardisation across cohorts and rounds
3. Creating a child marriage variable (CM) and age of marriage (age_marriage_combined)
4. Stacking the cohorts according to ages (e.g. age 8 is round 1 for 1994-cohort but round 3 for 2001-cohort)

Descriptive statistics and plots 
1. Pre-imputation descriptive statistics - word file
2. Visualisation plots for wellbeing score (grouping by age, cohort and child marriage status) 

Save dataset for multiple imputation

## Multiple_imputation
This is the second coding sample, it is the multiple imputation file for PERU. 
Here are the key steps:

1. Check data structure
2. Transform variables before imputation (sparse values, convert to factor)
3. Conduct imputations for two cohorts separately - specify imputation methods
4. Check imputation diagnostics
5. Save imputation data

## Data_analyses
This is the third coding sample for, it is the main analyses file for PERU. 
Here are the key steps:

Cleaning & descriptives 
1. Transform imputed datasets - 2001-cohort; 1994-cohort; combined cohorts
2. Descriptive plots across ages (post-imputation)
3. Descriptive statistics (post-imputation)

Analyses (child marriage as exposure)
1. Linear regression predicting wellbeing at age 19
2. Linear regression predicting average wellbeing across ages 19-26 (will be up to 29 with round 7 data)
3. Linear regression predicting trajectories of wellbeing from age 19-26
4. Difference in differences with 2001-cohort only
5. Propensity score matching & repeating analyses i-iv. (iv not possible as parallel trends violated)
Analyses are conducted with combined cohort, then separate cohorts for robustness checks 

