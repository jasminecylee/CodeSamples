## This is the third coding sample for a current project on the impacts of child marriage on wellbeing across Peru, India, Ethiopia, and Vietnam.

## This is the main analyses file for PERU. 

# Here are the key steps:

# Cleaning & descriptives 
# 1. Transform imputed datasets - 2001-cohort; 1994-cohort; combined cohorts
# 2. Descriptive plots across ages (post-imputation)
# 3. Descriptive statistics (post-imputation)

# Analyses (child marriage as exposure)
    # 1.    Linear regression predicting wellbeing at age 19 
    # 2.   Linear regression predicting average wellbeing across ages 19-26 (will be up to 29 with round 7 data) 
    # 3.  Linear regression predicting trajectories of wellbeing from age 19-26
    # 4.   Difference in differences with 2001-cohort only 
    # 5.    Propensity score matching & repeating analyses i-iv. (iv not possible as parallel trends violated)

# Analyses are conducted with combined cohort, then separate cohorts for robustness checks 

#--------------------------- LOAD LIBRARIES AND DATA -----------------------

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(mice)
library(miceadds)
library(MatchIt)  
library(cobalt)    
library(fixest)
library(lme4)
library(lmerTest) 

# NOTE: raw data files are not included due to data access restrictions.
# Code is provided to demonstrate workflow, structure, and methods.

## Load data

# Load long data
data_long <- readRDS("data_long.rds")

# Load 2001-cohort data (younger cohort, yc)
imputed_long_data_yc <- readRDS("imputed_long_data_yc.rds")
original_data_yc <- imputed_long_data_yc[[1]]  # Save the original dataset separately
imputed_long_data_yc <- imputed_long_data_yc[-1]  # Remove the first dataset
# imputed_long_data_yc[[19]] %>% view()

# Load 1994-cohort data (older cohort, oc)
imputed_long_data_oc <- readRDS("imputed_long_data_oc.rds")
original_data_oc <- imputed_long_data_oc[[1]]  # Save the original dataset separately
imputed_long_data_oc <- imputed_long_data_oc[-1]  # Remove the first dataset

# imputed_long_data_oc[[19]] %>% view() # No more 20, only up until 19 




# ---------------- TRANSFORM DATASETS FOR ANALYSES ------------------------------

## --- Convert long to wide data for each cohort (for model 1) ---

## 2001-cohort
imputed_wide_data_yc <- lapply(imputed_long_data_yc, function(data) {
  data %>%
    pivot_wider(
      id_cols = c(childid, CM, age_married_combined, clustid, birthorder),
      names_from = equivalent_round,
      values_from = c(cladder, chhealth, typesite, chethnic, chldrel, hhsize, wi, hq, cd, enrol, caredu, careladder, debt, paid, parentasp, aspirations)
    )
})
imputed_wide_data_yc[[10]] %>% nrow() # 930
imputed_wide_data_yc[[10]] %>% colnames() 

imputed_long_data_yc <- lapply(imputed_wide_data_yc, function(data) {
  
  # Keep only `_Age 12` baseline covariates
  baseline_vars <- data %>%
    select(childid, CM, clustid, `cladder_Age 12`, 
           `typesite_Age 12`, `chethnic_Age 12`, `chldrel_Age 12`, 
           `hhsize_Age 12`, `wi_Age 12`, `hq_Age 12`, `cd_Age 12`, 
           `enrol_Age 12`, `caredu_Age 12`, `careladder_Age 12`, 
           `debt_Age 12`, `paid_Age 12`, `parentasp_Age 12`, `aspirations_Age 12`,
           `birthorder`) %>%
    distinct(childid, .keep_all = TRUE)  # Ensure unique values per child
  
  # Keep only cladder as time-varying variable
  long_data <- data %>%
    select(childid, matches("^cladder_Age \\d+$")) %>%  # Keep only cladder over time
    pivot_longer(
      cols = -childid,  
      names_to = c(".value", "timepoint"),  
      names_pattern = "(.*)_Age (\\d+)"
    ) %>%
    mutate(timepoint = as.numeric(timepoint))  # Convert timepoint to numeric
  
  # Merge back only the baseline `_Age 12` covariates
  long_data <- left_join(long_data, baseline_vars, by = "childid")
  
  return(long_data)
})

# Convert ID columns back to factors for Younger Cohort
imputed_long_data_yc <- lapply(imputed_long_data_yc, function(data) {
  data %>%
    mutate(
      childid = as.factor(childid),  
      clustid = as.factor(clustid),
      across(where(is.character), as.factor)  # Convert any remaining character columns to factor
    )
})

## 1994-cohort 
imputed_wide_data_oc <- lapply(imputed_long_data_oc, function(data) {
  data %>%
    pivot_wider(
      id_cols = c(childid, CM, age_married_combined, clustid, birthorder),
      names_from = equivalent_round,
      values_from = c(cladder, chhealth, typesite, chethnic, chldrel, hhsize, wi, hq, cd, enrol, caredu, careladder, debt, paid, parentasp, aspirations)
    )
})
imputed_wide_data_oc[[19]] %>% nrow() # 517

imputed_long_data_oc <- lapply(imputed_wide_data_oc, function(data) {
  
  # Keep only `_Age 12` baseline covariates
  baseline_vars <- data %>%
    select(childid, age_married_combined, CM, clustid, `cladder_Age 12`, 
           `typesite_Age 12`, `chethnic_Age 12`, `chldrel_Age 12`, 
           `hhsize_Age 12`, `wi_Age 12`, `hq_Age 12`, `cd_Age 12`, 
           `enrol_Age 12`, `caredu_Age 12`, `careladder_Age 12`, 
           `debt_Age 12`, `paid_Age 12`, `parentasp_Age 12`, `aspirations_Age 12`,
           `birthorder`) %>%
    distinct(childid, .keep_all = TRUE)  # Ensure unique values per child
  
  # Keep only cladder as time-varying variable
  long_data <- data %>%
    select(childid, matches("^cladder_Age \\d+$")) %>%  # Keep only cladder over time
    pivot_longer(
      cols = -childid,  
      names_to = c(".value", "timepoint"),  
      names_pattern = "(.*)_Age (\\d+)"
    ) %>%
    mutate(timepoint = as.numeric(timepoint))  # Convert timepoint to numeric
  
  # Merge back only the baseline `_Age 12` covariates
  long_data <- left_join(long_data, baseline_vars, by = "childid")
  
  return(long_data)
})

# Convert ID columns back to factors for Older Cohort
imputed_long_data_oc <- lapply(imputed_long_data_oc, function(data) {
  data %>%
    mutate(
      childid = as.factor(childid),  
      clustid = as.factor(clustid),
      across(where(is.character), as.factor)  # Convert any remaining character columns to factor
    )
})

## Combined cohorts
imputed_wide_data_combined <- lapply(seq_along(imputed_wide_data_oc), function(i) {
  # Extract the i-th imputed dataset from both cohorts
  data_oc <- imputed_wide_data_oc[[i]] %>%
    mutate(cohort = "1994-Cohort")  # Mark older cohort
  
  data_yc <- imputed_wide_data_yc[[i]] %>%
    mutate(cohort = "2001-Cohort")  # Mark younger cohort
  
  # Bind the datasets together
  bind_rows(data_oc, data_yc)
})

# Check structure
str(imputed_wide_data_combined[[1]])  # Should show both cohorts in one dataset
imputed_wide_data_combined[[10]] %>% nrow() # 1447 - correct!
# imputed_wide_data_combined[[10]] %>% view()


## --- In long data create timepoint variable + keep covariates at age 12 from wide data ---

# Create function
convert_to_long <- function(imputed_wide_data) {
  lapply(imputed_wide_data, function(data) {
    
    # Keep only `_Age 12` baseline covariates
    baseline_vars <- data %>%
      select(childid, age_married_combined, CM, clustid, age_married_combined, `cladder_Age 12`, 
             `typesite_Age 12`, `chethnic_Age 12`, `chldrel_Age 12`, 
             `hhsize_Age 12`, `wi_Age 12`, `hq_Age 12`, `cd_Age 12`, 
             `enrol_Age 12`, `caredu_Age 12`, `careladder_Age 12`, 
             `debt_Age 12`, `paid_Age 12`, `parentasp_Age 12`, `aspirations_Age 12`,
             `birthorder`) %>%
      distinct(childid, .keep_all = TRUE)  # Ensure unique values per child
    
    # Convert wide → long for **time-varying** `cladder`
    long_data <- data %>%
      select(childid, matches("^cladder_Age \\d+$")) %>%  # Keep only cladder over time
      pivot_longer(
        cols = -childid,  
        names_to = c(".value", "timepoint"),  
        names_pattern = "(.*)_Age (\\d+)"
      ) %>%
      mutate(timepoint = as.numeric(timepoint))  # Convert timepoint to numeric
    
    # Merge back only the `_Age 12` baseline covariates
    long_data <- left_join(long_data, baseline_vars, by = "childid")
    
    return(long_data)
  }) %>%
    lapply(function(data) {
      data %>%
        mutate(
          childid = as.factor(childid),  
          clustid = as.factor(clustid),
          across(where(is.character), as.factor)  # Convert remaining character vars to factors
        )
    })
}

## --- Convert each cohort to long format ---
imputed_long_data_combined <- convert_to_long(imputed_wide_data_combined)  # Combined cohort
imputed_long_data_yc <- convert_to_long(imputed_wide_data_yc)  # 2001 cohort (younger)
imputed_long_data_oc <- convert_to_long(imputed_wide_data_oc)  # 1994 cohort (older)

# Check the datasets are all correct
# imputed_long_data_combined[[20]] %>% view()





# ----------- 1) LINEAR REGRESSION PREDICTING WELLBEING AT *AGE 19** ---------------------------
# First with combined cohorts, then individual cohorts as robustness check

## --- Filter only to those who were married after 12 (otherwise not a true pre-treatment group at age 12) ---
imputed_wide_data_yc <- lapply(imputed_wide_data_yc, function(data) {
  data %>%
    filter(age_married_combined > 12)
})

## --- Combined Cohorts ---

## Unadjusted
wellbeing_age19_unadjusted <- lapply(imputed_wide_data_combined, function(data) {
  lm_robust(`cladder_Age 19` ~ CM + `cladder_Age 12`, data = data, clusters = clustid)
})

wellbeing_age19_unadjusted <- pool(wellbeing_age19_unadjusted)
summary(wellbeing_age19_unadjusted)
# Coefficient -0.524, p-value 0.0004

## Adjusted - Step 1 Demographics
wellbeing_age19_adjusted1 <- lapply(imputed_wide_data_combined, function(data) {
  lm_robust(`cladder_Age 19` ~ CM + `cladder_Age 12` + `typesite_Age 12` + `chethnic_Age 12` + `chldrel_Age 12` + `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + `enrol_Age 12` + `caredu_Age 12` +  `debt_Age 12` + `paid_Age 12` +  `birthorder`, 
            data = data, clusters = clustid)  
})

wellbeing_age19_adjusted1 <- pool(wellbeing_age19_adjusted1)
summary(wellbeing_age19_adjusted1)
# Coefficient -0.26, p-value 0.02
# Adjusting for first set of demographics, child marriage poorer predicted wellbeing at age 19. 

## Adjusted - Full
wellbeing_age19_fulladjusted <- lapply(imputed_wide_data_combined, function(data) {
  lm_robust(`cladder_Age 19` ~ CM + `cladder_Age 12` + `typesite_Age 12` + `chethnic_Age 12` + `chldrel_Age 12` + `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + `enrol_Age 12` + `caredu_Age 12` + `careladder_Age 12` + `debt_Age 12` + `paid_Age 12` + `parentasp_Age 12` + `aspirations_Age 12` + `birthorder`, 
data = data, clusters = clustid)  
})

wellbeing_age19_fulladjusted <- pool(wellbeing_age19_fulladjusted)
summary(wellbeing_age19_fulladjusted)
# Coefficient -0.21, p-value 0.06
# Adjusting for full set of demographics, child marriage not significantly poorer predicted wellbeing at age 19. 

## --- Robustness Checks - Separate Cohorts --

### 1994-cohort 

## Unadjusted 
wellbeing_age19_oc_unadjusted <- lapply(imputed_wide_data_oc, function(data) {
  lm_robust(`cladder_Age 19` ~ CM + `cladder_Age 12`, data = data, clusters = clustid)
})

wellbeing_age19_oc_unadjusted <- pool(wellbeing_age19_oc_unadjusted)
summary(wellbeing_age19_oc_unadjusted)
# Coefficient -0.68, p-value 0.0003

## Adjusted - Step 1 Demographics
wellbeing_age19_oc_adjusted1 <- lapply(imputed_wide_data_oc, function(data) {
  lm_robust(`cladder_Age 19` ~ CM + `cladder_Age 12` + `typesite_Age 12` + `chethnic_Age 12` + `chldrel_Age 12` + 
              `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + `enrol_Age 12` + `caredu_Age 12` + `debt_Age 12` + `paid_Age 12` + `birthorder`, 
            data = data, clusters = clustid)
})

wellbeing_age19_oc_adjusted1 <- pool(wellbeing_age19_oc_adjusted1)
summary(wellbeing_age19_oc_adjusted1)
# Coefficient -0.42, p-value 0.025

## Adjusted - Full
wellbeing_age19_oc_fulladjusted <- lapply(imputed_wide_data_oc, function(data) {
  lm_robust(`cladder_Age 19` ~ CM + `cladder_Age 12` + `typesite_Age 12` + `chethnic_Age 12` + `chldrel_Age 12` + 
              `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + `enrol_Age 12` + `caredu_Age 12` + `careladder_Age 12` + `debt_Age 12` + `paid_Age 12` + `parentasp_Age 12` + `aspirations_Age 12` + `birthorder`, 
            data = data, clusters = clustid)
})

wellbeing_age19_oc_fulladjusted <- pool(wellbeing_age19_oc_fulladjusted)
summary(wellbeing_age19_oc_fulladjusted)
# Coefficient -0.39, p-value 0.039
# Adjusting for full set of demographics, child marriage significantly poorer predicted wellbeing at age 19 (older cohort). 


### 2001-cohort 

## Unadjusted
wellbeing_age19_yc_unadjusted <- lapply(imputed_wide_data_yc, function(data) {
  lm_robust(`cladder_Age 19` ~ CM + `cladder_Age 12`, data = data, clusters = clustid)
})

wellbeing_age19_yc_unadjusted<- pool(wellbeing_age19_yc_unadjusted)
summary(wellbeing_age19_yc_unadjusted)
# Coefficient -0.29, p-value 0.14

## Adjusted - Step 1 Demographics
wellbeing_age19_yc_adjusted1 <- lapply(imputed_wide_data_yc, function(data) {
  lm_robust(`cladder_Age 19` ~ CM + `cladder_Age 12` + `typesite_Age 12` + `chethnic_Age 12` + `chldrel_Age 12` + 
              `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + `enrol_Age 12` + `caredu_Age 12` + `debt_Age 12` + `paid_Age 12` + `birthorder`, 
            data = data, clusters = clustid)
})

wellbeing_age19_yc_adjusted1 <- pool(wellbeing_age19_yc_adjusted1)
summary(wellbeing_age19_yc_adjusted1)
# Coefficient -0.22, p-value 0.28

## Adjusted - Full
wellbeing_age19_yc_fulladjusted <- lapply(imputed_wide_data_yc, function(data) {
  lm_robust(`cladder_Age 19` ~ CM + `cladder_Age 12` + `typesite_Age 12` + `chethnic_Age 12` + `chldrel_Age 12` + 
              `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + `enrol_Age 12` + `caredu_Age 12` + `careladder_Age 12` + `debt_Age 12` + `paid_Age 12` + `parentasp_Age 12` + `aspirations_Age 12` + `birthorder`, 
            data = data, clusters = clustid)
})

wellbeing_age19_yc_fulladjusted <- pool(wellbeing_age19_yc_fulladjusted)
summary(wellbeing_age19_yc_fulladjusted)
# Coefficient -0.21, p-value 0.36
# Adjusting for full set of demographics, child marriage not significantly poorer predicted wellbeing at age 19 (younger cohort). 


## Only older cohort significantly poorer predicted age 19 wellbeing. 




# --------------------- 2) LINEAR REGRESSION PREDICTING *AVERAGE WELLBEING FROM AGE 19 - 30* ----------------------------

## --- Combined cohort ---

# Check that timepoint variable is created (for different ages)
imputed_long_data_combined[[19]]$timepoint %>% unique() # 8 12 15 19 22 26 (will add 29)

# Filter this dataset to only include timepoints 19, 22 and 26 (and 29 later)
imputed_long_data_combined <- lapply(imputed_long_data_combined, function(data) {
  data %>%
    filter(timepoint %in% c(19, 22, 26))  # Keep only relevant timepoints
})

## Unadjusted
wellbeing_avg_unadjusted <- lapply(imputed_long_data_combined, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + timepoint + 
         (1 | childid) + (1 | clustid),  # clustering
       data = data,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# lmer doesn't work well with mice pooling so have to perform manually

# Function to extract results (including p-values)
extract_lmer_results <- function(model) {
  model_summary <- summary(model)  # Get summary output
  model_params <- as.data.frame(coef(summary(model)))  # Extract fixed effects table
  
  # Rename columns for clarity
  model_params <- model_params %>%
    rownames_to_column(var = "Parameter") %>%
    select(Parameter, Estimate, `Std. Error`, `Pr(>|t|)`) %>%
    rename(SE = `Std. Error`, p_value = `Pr(>|t|)`)
  
  return(model_params)
}

# Extract results from all imputations
wellbeing_avg_unadjusted <- lapply(wellbeing_avg_unadjusted, extract_lmer_results)
wellbeing_avg_unadjusted <- bind_rows(wellbeing_avg_unadjusted, .id = "imputation")

# Function to pooled estimates using Rubin's rules
compute_pooled_summary <- function(pooled_results, m = 20) {
  pooled_results %>%
    group_by(Parameter) %>%
    summarise(
      pooled_mean = mean(Estimate),           # Mean of coefficient estimates
      within_var = mean(SE^2),                # Within-imputation variance
      between_var = var(Estimate),            # Between-imputation variance
      pooled_p = mean(p_value),               # Approximate mean p-value across imputations
      .groups = "drop"
    ) %>%
    mutate(
      total_var = within_var + (1 + (1 / m)) * between_var,  # Rubin’s formula
      pooled_SE = sqrt(total_var),
      lower.CL = pooled_mean - 1.96 * pooled_SE,  # 95% CI lower bound
      upper.CL = pooled_mean + 1.96 * pooled_SE   # 95% CI upper bound
    ) %>%
    select(Parameter, pooled_mean, pooled_SE, lower.CL, upper.CL, pooled_p)  # Organize columns
}

# Display final results
wellbeing_avg_unadjusted <- compute_pooled_summary(wellbeing_avg_unadjusted)
wellbeing_avg_unadjusted
# Coefficient -0.385 (-0.552, -0.218), p-value 0.00000626
# Child marriage associated with significantly reduced wellbeing across ages 19, 22 and 26


## Adjusted - Step 1 Demographics
# For some reason need to specify variables that are factors for emmeans to work
wellbeing_avg_adjusted1 <- lapply(imputed_long_data_combined, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + timepoint + 
         factor(`typesite_Age 12`) + factor(`chethnic_Age 12`) + 
         factor(`chldrel_Age 12`) + factor(`enrol_Age 12`) + 
         factor(`caredu_Age 12`) + factor(`debt_Age 12`) + 
         factor(`paid_Age 12`) + 
         `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + 
          + `birthorder` + 
         (1 | childid) + (1 | clustid),  
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# Extract results from all imputations
wellbeing_avg_adjusted1 <- lapply(wellbeing_avg_adjusted1, extract_lmer_results)
wellbeing_avg_adjusted1 <- bind_rows(wellbeing_avg_adjusted1, .id = "imputation")

# Display final results
wellbeing_avg_adjusted1 <- compute_pooled_summary(wellbeing_avg_adjusted1)
wellbeing_avg_adjusted1 %>% print(n = 27)
# Coefficient -0.170 (-0.331, -0.0078), p-value 0.040 significant


## Adjusted - Full
wellbeing_avg_fulladjusted <- lapply(imputed_long_data_combined, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + timepoint + 
         factor(`typesite_Age 12`) + factor(`chethnic_Age 12`) + 
         factor(`chldrel_Age 12`) + factor(`enrol_Age 12`) + 
         factor(`caredu_Age 12`) + factor(`debt_Age 12`) + 
         factor(`paid_Age 12`) + factor(`parentasp_Age 12`) + 
         factor(`aspirations_Age 12`) + 
         `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + 
         `careladder_Age 12` + `birthorder` + 
         (1 | childid) + (1 | clustid),  
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# Extract results from all imputations
wellbeing_avg_fulladjusted <- lapply(wellbeing_avg_fulladjusted, extract_lmer_results)
wellbeing_avg_fulladjusted <- bind_rows(wellbeing_avg_fulladjusted, .id = "imputation")

# Display final results
wellbeing_avg_fulladjusted <- compute_pooled_summary(wellbeing_avg_fulladjusted)
wellbeing_avg_fulladjusted %>% print(n = 27)
# Coefficient -0.118 (-0.280, 0.045), p-value 0.15 insignificant
# Much of the effect was absorbed by parent aspirations and child aspirations


### 1994-cohort

# Check that timepoint variable is created (for different ages)
imputed_long_data_oc[[19]]$timepoint %>% unique() # 8 12 15 19 22 26 (will add 29)

# Filter this dataset to only include timepoints 19, 22 and 26 (and 29 later)
imputed_long_data_oc <- lapply(imputed_long_data_oc, function(data) {
  data %>%
    filter(timepoint %in% c(19, 22, 26))  # Keep only relevant timepoints
})

## Unadjusted
wellbeing_avg_oc_unadjusted <- lapply(imputed_long_data_oc, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + timepoint + 
         (1 | childid) + (1 | clustid),  # clustering
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# Extract results from all imputations
wellbeing_avg_oc_unadjusted <- lapply(wellbeing_avg_oc_unadjusted, extract_lmer_results)
wellbeing_avg_oc_unadjusted <- bind_rows(wellbeing_avg_oc_unadjusted, .id = "imputation")

# Pool results
wellbeing_avg_oc_unadjusted <- compute_pooled_summary(wellbeing_avg_oc_unadjusted)
wellbeing_avg_oc_unadjusted
# Coefficient -0.502 (-0.714, -0.290), p-value 0.00000422

## Adjusted - Step 1 Demographics
wellbeing_avg_oc_adjusted1 <- lapply(imputed_long_data_oc, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + timepoint + 
         factor(`typesite_Age 12`) + factor(`chethnic_Age 12`) + 
         factor(`chldrel_Age 12`) + factor(`enrol_Age 12`) + 
         factor(`caredu_Age 12`) + factor(`debt_Age 12`) + 
         factor(`paid_Age 12`) + `hhsize_Age 12` + `wi_Age 12` +                `hq_Age 12` + `cd_Age 12` + `birthorder` + 
         (1 | childid) + (1 | clustid),  
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# Extract results from all imputations
wellbeing_avg_oc_adjusted1 <- lapply(wellbeing_avg_oc_adjusted1, extract_lmer_results)
wellbeing_avg_oc_adjusted1 <- bind_rows(wellbeing_avg_oc_adjusted1, .id = "imputation")

# Display final results
wellbeing_avg_oc_adjusted1 <- compute_pooled_summary(wellbeing_avg_oc_adjusted1)
wellbeing_avg_oc_adjusted1 %>% print(n = 26)
# Coefficient -0.26 (-0.461, -0.058), p-value 0.01 significant


## Adjusted - Full
wellbeing_avg_oc_fulladjusted <- lapply(imputed_long_data_oc, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + timepoint + 
         factor(`typesite_Age 12`) + factor(`chethnic_Age 12`) + 
         factor(`chldrel_Age 12`) + factor(`enrol_Age 12`) + 
         factor(`caredu_Age 12`) + factor(`debt_Age 12`) + 
         factor(`paid_Age 12`) + factor(`parentasp_Age 12`) + 
         factor(`aspirations_Age 12`) + 
         `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + 
         `careladder_Age 12` + `birthorder` + 
         (1 | childid) + (1 | clustid),  
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# Extract results from all imputations
wellbeing_avg_oc_fulladjusted <- lapply(wellbeing_avg_oc_fulladjusted, extract_lmer_results)
wellbeing_avg_oc_fulladjusted <- bind_rows(wellbeing_avg_oc_fulladjusted, .id = "imputation")

# Display final results
wellbeing_avg_oc_fulladjusted <- compute_pooled_summary(wellbeing_avg_oc_fulladjusted)
wellbeing_avg_oc_fulladjusted %>% print(n = 26)
# Coefficient -0.218 (-0.423, -0.013), p-value 0.036 significant


### 2001-cohort

# Cannot run this as it is the same as the first model - we only have up to age 19 for this cohort 


## ---  Separate plots ---

# Function to extract coefficients from models 1 and 2, with their unadjusted, partially adjusted and fully adjusted forms

extract_cm_results <- function(pooled_model, model_type, model_number) {
  # Convert pooled summary to a data frame
  if ("m" %in% names(pooled_model)) {
    pooled_summary <- summary(pooled_model, conf.int = TRUE) %>% as.data.frame()
  } else {
    pooled_summary <- pooled_model
  }
  
  # Debugging: Print column names
  print(paste("Columns for", model_type, ":"))
  print(colnames(pooled_summary))
  
  if (model_number == 1) {
    # Model 1: Wellbeing at Age 19 (Linear Regression)
    if (!"term" %in% colnames(pooled_summary)) {
      pooled_summary <- pooled_summary %>% rownames_to_column("term") # Add only if missing
    }
    
    return(pooled_summary %>%
             filter(term == "CMChild bride") %>%  
             mutate(
               HR = estimate,           # Effect size
               LowerCI = `2.5 %`,       # Lower confidence interval
               UpperCI = `97.5 %`,      # Upper confidence interval
               Model = model_type
             ) %>%
             select(HR, LowerCI, UpperCI, p.value, Model))
    
  } else if (model_number == 2) {
    # Model 2: Average Wellbeing (Multilevel Model)
    return(pooled_summary %>%
             filter(Parameter == "CMChild bride") %>%
             mutate(
               HR = pooled_mean,  
               LowerCI = lower.CL,  
               UpperCI = upper.CL,  
               Model = model_type
             ) %>%
             select(HR, LowerCI, UpperCI, pooled_p, Model))
  }
}

# Model 1: Wellbeing at 19
results_age19 <- extract_cm_results(wellbeing_age19_unadjusted, "Predicted Wellbeing at 19 (Unadjusted)", 1) %>%
  bind_rows(extract_cm_results(wellbeing_age19_adjusted1, "Predicted Wellbeing at 19 (Adjusted - Social demographic)", 1)) %>%
  bind_rows(extract_cm_results(wellbeing_age19_fulladjusted, "Predicted Wellbeing at 19 (Adjusted - Social demographic + Psychological)", 1))

# Model 2: Average wellbeing across ages 19, 22 and 26
results_avg <- extract_cm_results(wellbeing_avg_unadjusted, "Predicted Average Wellbeing (Unadjusted)", 2) %>%
  bind_rows(extract_cm_results(wellbeing_avg_adjusted1, "Predicted Average Wellbeing (Adjusted - Social demographic)", 2)) %>%
  bind_rows(extract_cm_results(wellbeing_avg_fulladjusted, "Predicted Average Wellbeing (Adjusted - Social demographic + Psychological)", 2))

# Print results
print(results_age19)
print(results_avg)


# Model 1 
model1 <- ggplot(results_age19,
                 aes(x = Model, y = HR, color = Model)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(title = "Predicted difference in wellbeing score at age 19 for child brides compared to non-child brides",
       y = "Coefficient", x = NULL) +
  scale_color_manual(values = c("#fdae61", "#7fcdbb", "#4575b4")) +
  theme_minimal() +
  theme(legend.position = "none")

print(model1)
ggsave("child_marriage_age19.png", plot = model1, width = 12, height = 10, dpi = 300, bg = "white")


# Model 2 
model2 <- ggplot(results_avg,
                 aes(x = Model, y = HR, color = Model)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(title = "Predicted difference in average wellbeing score from ages 19-26 for child brides compared to non-child brides",
       y = "Coefficient", x = NULL) +
  scale_color_manual(values = c("#fdae61", "#7fcdbb", "#4575b4")) +
  theme_minimal() +
  theme(legend.position = "none")

print(model2)
ggsave("child_marriage_ave19-26.png", plot = model2, width = 12, height = 10, dpi = 300, bg = "white")





#--------------------------------- 3) LINEAR REGRESSION  PREDICTING WELLBEING *TRAJECTORIES FROM AGE 19 - 26* ------------------------------------

## --- Combined cohort ---

# Check that the dataset is still filtered correctly
imputed_long_data_combined[[19]]$timepoint %>% unique() # 19, 22, 26 (will add 29)

## Unadjusted
wellbeing_traj_unadjusted <- lapply(imputed_long_data_combined, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + factor(timepoint) + CM * factor(timepoint) + # add interaction term; make sure timepoint is a factor to get trajectory
         (1 | childid) + (1 | clustid),  # clustering
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# Function to get estimated marginal means from the model
get_emmeans <- function(model) {
  emmeans(model, ~ CM * timepoint, 
          nuisance = c("cladder_Age 12", "typesite_Age 12", 
                       "chethnic_Age 12", "chldrel_Age 12", "hhsize_Age 12", 
                       "wi_Age 12", "hq_Age 12", "cd_Age 12", "enrol_Age 12", 
                       "caredu_Age 12", "careladder_Age 12", "debt_Age 12", 
                       "paid_Age 12", "parentasp_Age 12", "aspirations_Age 12", 
                       "birthorder"),
          type = "response") %>% 
    as.data.frame()  # Convert to dataframe for pooling
}

# Extract emmeans for plotting
emmeans_unadjusted <- lapply(wellbeing_traj_unadjusted, get_emmeans)

# Pool imputed results
pooled_emmeans_unadjusted <- bind_rows(emmeans_unadjusted, .id = "imputation") %>%
  group_by(CM, timepoint) %>%
  summarise(
    pooled_emmean = mean(emmean),  # Mean estimated marginal mean
    pooled_SE = sqrt(mean(SE^2)),  # Pooled standard error
    lower.CL = pooled_emmean - 1.96 * pooled_SE,
    upper.CL = pooled_emmean + 1.96 * pooled_SE,
    .groups = "drop"
  )

# Extract results 
wellbeing_traj_unadjusted <- lapply(wellbeing_traj_unadjusted, extract_lmer_results)
wellbeing_traj_unadjusted <- bind_rows(wellbeing_traj_unadjusted, .id = "imputation")

# Create summary statistics 
wellbeing_traj_unadjusted_sum <- compute_pooled_summary(wellbeing_traj_unadjusted)
wellbeing_traj_unadjusted_sum
# At age 19, child brides significantly poorer wellbeing -0.374 (p = 0.000239)
# From age 19-22, CM wellbeing improved 0.194 more than non-CM but not significant (p = 0.24)
# From age 19-26, child brides wellbeing declined more than non-child brides by 0.289 (p = 0.090)


## Adjusted - Full
wellbeing_traj_adjusted <- lapply(imputed_long_data_combined, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + factor(timepoint) + CM * factor(timepoint) +  
         factor(`typesite_Age 12`) + factor(`chethnic_Age 12`) + 
         factor(`chldrel_Age 12`) + factor(`enrol_Age 12`) + 
         factor(`caredu_Age 12`) + factor(`debt_Age 12`) + 
         factor(`paid_Age 12`) + factor(`parentasp_Age 12`) + 
         factor(`aspirations_Age 12`) + 
         `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + 
         `careladder_Age 12` + `birthorder` + 
         (1 | childid) + (1 | clustid),  
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# Extract estimated marginal means 
emmeans_adjusted <- lapply(wellbeing_traj_adjusted, get_emmeans)

# Pool imputed results
pooled_emmeans_adjusted <- bind_rows(emmeans_adjusted, .id = "imputation") %>%
  group_by(CM, timepoint) %>%
  summarise(
    pooled_emmean = mean(emmean),  
    pooled_SE = sqrt(mean(SE^2)),  
    lower.CL = pooled_emmean - 1.96 * pooled_SE,
    upper.CL = pooled_emmean + 1.96 * pooled_SE,
    .groups = "drop"
  )


# Extract results from all imputations
wellbeing_traj_adjusted <- lapply(wellbeing_traj_adjusted, extract_lmer_results)
wellbeing_traj_adjusted <- bind_rows(wellbeing_traj_adjusted, .id = "imputation")

# Compute summary statistics
wellbeing_traj_adjusted_sum <- compute_pooled_summary(wellbeing_traj_adjusted)
wellbeing_traj_adjusted_sum %>% print(n = 30)
# At age 19, child brides poorer wellbeing -0.101 but not significant (p = 0.309)
# From age 19-22, CM wellbeing improved 0.187 more than non-CM but not significant (p = 0.25)
# From age 19-26, child brides wellbeing declined more than non-child brides by 0.296 (p = 0.080)


# Line plot (Model 3 Trajectories - unadjusted and adjusted)
pooled_emmeans_unadjusted$model <- "Unadjusted"
pooled_emmeans_adjusted$model <- "Adjusted - Social demographic + Psychological"
pooled_emmeans_combined <- bind_rows(pooled_emmeans_unadjusted, pooled_emmeans_adjusted)
pooled_emmeans_combined$model <- factor(pooled_emmeans_combined$model, levels = c("Unadjusted", "Adjusted - Social demographic + Psychological"))

traj_plot <- ggplot(pooled_emmeans_combined, aes(x = timepoint, y = pooled_emmean, color = factor(CM), group = factor(CM))) +
  geom_point() +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  facet_wrap(~ model, nrow = 1) +  # Split into Adjusted vs. Unadjusted
  labs(
    title = "Predicted Wellbeing Trajectories from Ages 19-26",
    x = "Age (Timepoint)",
    y = "Predicted Wellbeing Score",
    color = "Child Marriage Status"
  ) + 
  scale_y_continuous(limits = c(3.5, 5.5)) + 
  scale_x_continuous(breaks = c(19, 22, 26), limits = c(18,27)) +
  scale_color_manual(values = c("Child bride" = "#d73027", "Non-child-bride" = "#2c7fb8")) +
  theme_minimal()

# Save the plot
ggsave("child_marriage_trajectory.png", plot = traj_plot, width = 12, height = 10, dpi = 300, bg = "white")

# Display the plot
print(traj_plot)


## --- 1994-cohort ---

# Check that the dataset is still filtered correctly
imputed_long_data_oc[[19]]$timepoint %>% unique() # 19, 22, 26 (will add 29)

## Unadjusted
wellbeing_traj_oc_unadjusted <- lapply(imputed_long_data_oc, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + factor(timepoint) + CM * factor(timepoint) + # add interaction term; make sure timepoint is a factor to get trajectory
         (1 | childid) + (1 | clustid),  # clustering
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# Extract results 
wellbeing_traj_oc_unadjusted <- lapply(wellbeing_traj_oc_unadjusted, extract_lmer_results)
wellbeing_traj_oc_unadjusted <- bind_rows(wellbeing_traj_oc_unadjusted, .id = "imputation")

# Display final results
wellbeing_traj_oc_unadjusted <- compute_pooled_summary(wellbeing_traj_oc_unadjusted)
wellbeing_traj_oc_unadjusted
# At age 19, child brides poorer wellbeing -0.518  (p = 0.0006)
# From age 19-22, CM wellbeing improved 0.265 more than non-CM but not significant (p = 0.149)
# From age 19-26, child brides wellbeing declined more than non-child brides by 0.218 but not significant (p = 0.244)


## Adjusted - Full
wellbeing_traj_oc_adjusted1 <- lapply(imputed_long_data_oc, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + factor(timepoint) + CM * factor(timepoint) +  
         factor(`typesite_Age 12`) + factor(`chethnic_Age 12`) + 
         factor(`chldrel_Age 12`) + factor(`enrol_Age 12`) + 
         factor(`caredu_Age 12`) + factor(`debt_Age 12`) + 
         factor(`paid_Age 12`) + factor(`parentasp_Age 12`) + 
         factor(`aspirations_Age 12`) + 
         `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + 
         `careladder_Age 12` + `birthorder` + 
         (1 | childid) + (1 | clustid),  
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# Extract results from all imputations
wellbeing_traj_oc_adjusted1 <- lapply(wellbeing_traj_oc_adjusted1, extract_lmer_results)
wellbeing_traj_oc_adjusted1 <- bind_rows(wellbeing_traj_oc_adjusted1, .id = "imputation")

# Compute pooled summary statistics
wellbeing_traj_oc_adjusted1 <- compute_pooled_summary(wellbeing_traj_oc_adjusted1)
wellbeing_traj_oc_adjusted1 %>% print(n = 29)
# At age 19, child brides poorer wellbeing -0.234 (p = 0.11)
# From age 19-22, CM wellbeing improved 0.265 more than non-CM but not significant (p = 0.15)
# From age 19-26, child brides wellbeing declined more than non-child brides by 0.218 but not significant (p = 0.24)


# 2001-cohort no age 22 data 



# ------------------------- 4) DIFFERENCE IN DIFFERENCES FROM AGE 12 - 19 (2001-COHORT) -------------------------

## --- Parallel trends --- 
# Prerequisite to DiD analysis for 12 vs 19

# Check original data
data_long <- readRDS("data_long.rds")

# Plot parallel trends graph for older cohort 8 vs 12 years old
data_yc_8_12 <- data_long %>%
  filter(yc == 1) %>% 
  filter(equivalent_round %in% c("Age 8", "Age 12")) %>% 
  group_by(equivalent_round, CM) %>%
  filter(!is.na(CM)) %>% 
  summarise(mean_cladder = mean(cladder, na.rm = TRUE), .groups = 'drop')
head(data_yc_8_12)
# dropped NA from plotting

ggplot(data_yc_8_12, aes(x = equivalent_round, y = mean_cladder, color = CM, group = CM)) + 
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  labs(x = "Age", y = "Average Wellbeing Score",
       title = "Average Wellbeing Score Across Ages by Child Marriage Status (2001-Cohort)") +
  scale_color_manual(values = c('#00BFC4', '#F8766D')) + 
  scale_y_continuous(limits = c(1,9)) +
  theme_minimal()
# Parallel trends

# Use only those whose age of marriage >12
imputed_long_did_yc <- lapply(imputed_long_data_yc, function(data) {
  data %>%
    group_by(childid) %>%
    filter(is.na(age_married_combined) | age_married_combined > 12) %>%
    ungroup()
})


## --- Unadjusted  ---

# Define post-treatment indicator (1 for 12 and 19)  
imputed_long_did_yc <- lapply(imputed_long_did_yc, function(data) {
  data %>%
    mutate(age_12_19 = case_when(
      timepoint == 19 ~ 1,
      timepoint == 12 ~ 0,
      TRUE ~ NA_real_
    )) %>% 
      mutate(age_12_19 = as.factor(age_12_19))
})

# Filter to ages 12 and 19 only 
imputed_long_did_yc <- lapply(imputed_long_did_yc, function(data) {
  data %>% 
    filter(timepoint %in% c(12, 19))
})
imputed_long_did_yc$'3'$childid %>% n_distinct() # 924 girls in younger cohort - correct after dropping those married young! 
imputed_long_did_yc$'3' %>% head()

# Run unadjusted DiD model
did_model_unadj <- lapply(imputed_long_did_yc, function(data) {
  feols(cladder ~ CM * age_12_19, data = data, cluster = ~childid + clustid)
})
did_model_unadj 

# Pool the results 
pooled_unadj <- pool(did_model_unadj) # Can ignore warning
summary(pooled_unadj) # Negative and significant at 0.05 level (coefficient = -0.325, p = 0.09)

# Overall from ages 12 to 19, wellbeing improves
# But child brides experience a smaller improvement (0.3 points smaller than non-child brides)


## --- Adjusted full  ---

did_model_adj <- lapply(imputed_long_did_yc, function(data) {
  feols(cladder ~ CM * age_12_19 + `typesite_Age 12` + `chethnic_Age 12` + `chldrel_Age 12` + 
          `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + `enrol_Age 12` + `caredu_Age 12` + `careladder_Age 12` + `debt_Age 12` + `paid_Age 12` + `parentasp_Age 12` + `aspirations_Age 12` + `birthorder`, 
        data = data, cluster = ~childid + clustid)
})
etable(did_model_adj) 

# Pool the results 
pooled_adj <- pool(did_model_adj)
summary(pooled_adj) # Interaction negative and  significant (coefficient = -0.325, p = 0.096)

# At age 12 no difference in wellbeing between CM and non-CM
# In general, wellbeing significantly improves from ages 12 to 19
# However, child brides experience a (borderline significant) slightly smaller improvement (0.3 points smaller than non-child brides)


## --- Plot results  ---

# Compute EMMs for each imputed dataset
em_means_list <- lapply(seq_along(did_model_adj), function(i) {
  emmeans(did_model_adj[[i]], ~ CM * age_12_19, data = imputed_long_did_yc[[i]], nuisance = c("cladder_Age 12", "typesite_Age 12", 
                                                                                              "chethnic_Age 12", "chldrel_Age 12", "hhsize_Age 12", "wi_Age 12", "hq_Age 12", 
                                                                                              "cd_Age 12", "enrol_Age 12", "caredu_Age 12", "careladder_Age 12", "debt_Age 12", 
                                                                                              "paid_Age 12", "parentasp_Age 12", "aspirations_Age 12", "birthorder"))
})

# Convert each emmeans output to a data frame
emmeans_results <- lapply(em_means_list, function(em) as.data.frame(summary(em)))

# Combine all imputed datasets’ EMM results into one data frame
combined_emmeans <- bind_rows(emmeans_results, .id = "imputation")

# Pool the EMMs across imputations using Rubin’s rules
pooled_emmeans <- combined_emmeans %>%
  group_by(CM, age_12_19) %>%
  summarise(
    emmean = mean(emmean, na.rm = TRUE),   # Mean of estimates
    SE = sqrt(mean(SE^2, na.rm = TRUE)),   # SE pooling using Rubin’s rules
    LowerCI = emmean - 1.96 * SE,          # 95% CI lower bound
    UpperCI = emmean + 1.96 * SE           # 95% CI upper bound
  ) %>%
  ungroup()

# Print pooled results
print(pooled_emmeans)

# Convert age_12_19 to numeric before plotting
pooled_emmeans <- pooled_emmeans %>%
  mutate(age_12_19 = as.numeric(as.character(age_12_19)))  # Convert factor to numeric for x-axis

# Plot the marginal means
did_plot <- ggplot(pooled_emmeans, aes(x = age_12_19, y = emmean, color = factor(CM), group = factor(CM))) +
  geom_point() +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  labs(
    title = "Predicted Wellbeing from Ages 12 to 19 (2001-Cohort)",
    subtitle = "Adjusted - Social demographic + Psychological",
    x = "Age",
    y = "Predicted Wellbeing Score",
    color = "Child Marriage"
  ) + 
  scale_x_continuous(breaks = c(0, 1), labels = c("12", "19")) + 
  scale_y_continuous(limits = c(3.5, 5.5)) + 
  scale_color_manual(values = c("Non-child-bride" = "#2c7fb8", "Child bride" = "#d73027"), 
                                labels = c("Non-child-bride", "Child bride")) + 
  theme_minimal()

did_plot

ggsave("did_models.png", plot = did_plot, width = 12, height = 10, dpi = 300, bg = "white")






#---------------------------------- 5) PROPENSITY SCORE MATCHING & REPEAT ANALYSES ---------------


## --- Use original data for matching ---

# Load 2001-cohort data (younger cohort, yc)
imputed_long_data_yc <- readRDS("imputed_long_data_yc.rds")
original_data_yc <- imputed_long_data_yc[[1]]  # Save the original dataset separately
imputed_long_data_yc <- imputed_long_data_yc[-1]  # Remove the first dataset
# imputed_long_data_yc[[19]] %>% view()

# Load 1994-cohort data (older cohort, oc)
imputed_long_data_oc <- readRDS("imputed_long_data_oc.rds")
original_data_oc <- imputed_long_data_oc[[1]]  # Save the original dataset separately
imputed_long_data_oc <- imputed_long_data_oc[-1]  # Remove the first dataset

# imputed_long_data_oc[[19]] %>% view() # No more 20, only up until 19 



## Convert long to wide data for each cohort (for model 1)

# 2001-cohort
imputed_wide_data_yc <- lapply(imputed_long_data_yc, function(data) {
  data %>%
    pivot_wider(
      id_cols = c(childid, CM, age_married_combined, clustid, birthorder),
      names_from = equivalent_round,
      values_from = c(cladder, chhealth, typesite, chethnic, chldrel, hhsize, wi, hq, cd, enrol, caredu, careladder, debt, paid, parentasp, aspirations)
    )
})
imputed_wide_data_yc[[10]] %>% nrow() # 930
imputed_wide_data_yc[[10]] %>% colnames() 

imputed_long_data_yc <- lapply(imputed_wide_data_yc, function(data) {
  
  # Keep only `_Age 12` baseline covariates
  baseline_vars <- data %>%
    select(childid, CM, age_married_combined, clustid, `cladder_Age 12`, 
           `typesite_Age 12`, `chethnic_Age 12`, `chldrel_Age 12`, 
           `hhsize_Age 12`, `wi_Age 12`, `hq_Age 12`, `cd_Age 12`, 
           `enrol_Age 12`, `caredu_Age 12`, `careladder_Age 12`, 
           `debt_Age 12`, `paid_Age 12`, `parentasp_Age 12`, `aspirations_Age 12`,
           `birthorder`) %>%
    distinct(childid, .keep_all = TRUE)  # Ensure unique values per child
  
  # Keep only cladder as time-varying variable
  long_data <- data %>%
    select(childid, matches("^cladder_Age \\d+$")) %>%  # Keep only cladder over time
    pivot_longer(
      cols = -childid,  
      names_to = c(".value", "timepoint"),  
      names_pattern = "(.*)_Age (\\d+)"
    ) %>%
    mutate(timepoint = as.numeric(timepoint))  # Convert timepoint to numeric
  
  # Merge back only the baseline `_Age 12` covariates
  long_data <- left_join(long_data, baseline_vars, by = "childid")
  
  return(long_data)
})

# Convert ID columns back to factors for Younger Cohort
imputed_long_data_yc <- lapply(imputed_long_data_yc, function(data) {
  data %>%
    mutate(
      childid = as.factor(childid),  
      clustid = as.factor(clustid),
      across(where(is.character), as.factor)  # Convert any remaining character columns to factor
    )
})

# 1994-cohort 
imputed_wide_data_oc <- lapply(imputed_long_data_oc, function(data) {
  data %>%
    pivot_wider(
      id_cols = c(childid, CM, age_married_combined, clustid, birthorder),
      names_from = equivalent_round,
      values_from = c(cladder, chhealth, typesite, chethnic, chldrel, hhsize, wi, hq, cd, enrol, caredu, careladder, debt, paid, parentasp, aspirations)
    )
})
imputed_wide_data_oc[[19]] %>% nrow() # 517

imputed_long_data_oc <- lapply(imputed_wide_data_oc, function(data) {
  
  # Keep only `_Age 12` baseline covariates
  baseline_vars <- data %>%
    select(childid, CM, age_married_combined, clustid, `cladder_Age 12`, 
           `typesite_Age 12`, `chethnic_Age 12`, `chldrel_Age 12`, 
           `hhsize_Age 12`, `wi_Age 12`, `hq_Age 12`, `cd_Age 12`, 
           `enrol_Age 12`, `caredu_Age 12`, `careladder_Age 12`, 
           `debt_Age 12`, `paid_Age 12`, `parentasp_Age 12`, `aspirations_Age 12`,
           `birthorder`) %>%
    distinct(childid, .keep_all = TRUE)  # Ensure unique values per child
  
  # Keep only cladder as time-varying variable
  long_data <- data %>%
    select(childid, matches("^cladder_Age \\d+$")) %>%  # Keep only cladder over time
    pivot_longer(
      cols = -childid,  
      names_to = c(".value", "timepoint"),  
      names_pattern = "(.*)_Age (\\d+)"
    ) %>%
    mutate(timepoint = as.numeric(timepoint))  # Convert timepoint to numeric
  
  # Merge back only the baseline `_Age 12` covariates
  long_data <- left_join(long_data, baseline_vars, by = "childid")
  
  return(long_data)
})

# Convert ID columns back to factors for Older Cohort
imputed_long_data_oc <- lapply(imputed_long_data_oc, function(data) {
  data %>%
    mutate(
      childid = as.factor(childid),  
      clustid = as.factor(clustid),
      across(where(is.character), as.factor)  # Convert any remaining character columns to factor
    )
})

# Combined cohorts
imputed_wide_data_combined <- lapply(seq_along(imputed_wide_data_oc), function(i) {
  # Extract the i-th imputed dataset from both cohorts
  data_oc <- imputed_wide_data_oc[[i]] %>%
    mutate(cohort = "1994-Cohort")  # Mark older cohort
  
  data_yc <- imputed_wide_data_yc[[i]] %>%
    mutate(cohort = "2001-Cohort")  # Mark younger cohort
  
  # Bind the datasets together
  bind_rows(data_oc, data_yc)
})


## Check CM groups 
imputed_wide_data_yc[[10]]$CM %>% table()
imputed_wide_data_oc[[10]]$CM %>% table()



## --- Perform matching  ---

# Define the matching function
perform_matching <- function(data) {
  # Check if all necessary variables exist
  required_vars <- c("CM", "chethnic_Age 12", "chldrel_Age 12", "typesite_Age 12", "wi_Age 12", 
                     "caredu_Age 12", "enrol_Age 12")
  
  missing_vars <- setdiff(required_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables in dataset:", paste(missing_vars, collapse = ", ")))
  }
  
  # Define the matching formula
  formula <- CM ~ `chethnic_Age 12` +  `chldrel_Age 12` + `typesite_Age 12` + `wi_Age 12` + 
    `caredu_Age 12` + `enrol_Age 12`
  
  # Run optimal matching to ensure the best match for each individual
  matched_model <- matchit(formula, data = data, method = "optimal",  ratio = 1)  
  
  # Extract matched dataset
  matched_data <- match.data(matched_model)
  
  # Return a list containing both the matched dataset and the matchit object
  return(list(matched_data = matched_data, matchit_object = matched_model))
}

matched_yc <- lapply(imputed_wide_data_yc, perform_matching)
matched_oc <- lapply(imputed_wide_data_oc, perform_matching)

# Check results 
summary(matched_yc[[1]])
summary(matched_oc[[1]])

# 2001-Cohort diagnostics 
# Check balance summary for the first imputed dataset
summary(matched_yc[[1]]$matchit_object) # Matched n = 138
# Check balance plot
plot(summary(matched_yc[[1]]$matchit_object), abs = TRUE)
# Love plot for detailed visualization
love.plot(matched_yc[[1]]$matchit_object, abs = TRUE, thresholds = c(0.1, 0.2), stars = "std")

# 1994-Cohort diagnostics 
summary(matched_oc[[1]]$matchit_object) # Matched n = 140
plot(summary(matched_oc[[1]]$matchit_object), abs = TRUE)
love.plot(matched_oc[[1]]$matchit_object, abs = TRUE, thresholds = c(0.1, 0.2), stars = "std")



## --- Run analyses with matched data  ---

## Combine to become matched cohorts
matched_combined <- lapply(seq_along(matched_yc), function(i) {
  bind_rows(matched_yc[[i]]$matched_data, matched_oc[[i]]$matched_data)
})
summary(matched_combined[[1]])

## Run model 1 with matched groups (wellbeing at age 19)

# Unadjusted 
wellbeing_age19_unadjusted_matched <- lapply(matched_combined, function(data) {
  lm_robust(`cladder_Age 19` ~ CM + `cladder_Age 12`, 
            data = data, clusters = clustid)
})
wellbeing_age19_unadjusted_matched <- pool(wellbeing_age19_unadjusted_matched)
summary(wellbeing_age19_unadjusted_matched) # Coefficient -0.35, p-value 0.049

# Adjusted - Full (Adjust for whatever wasn't accounted for in matching)
wellbeing_age19_adjusted_matched <- lapply(matched_combined, function(data) {
  lm_robust(`cladder_Age 19` ~ CM + `cladder_Age 12` +  `hhsize_Age 12` + `hq_Age 12` + `cd_Age 12` + 
              `careladder_Age 12` + `debt_Age 12` + 
              `paid_Age 12` + `parentasp_Age 12` + `aspirations_Age 12` + `birthorder`, 
            data = data, clusters = clustid)  
})
wellbeing_age19_adjusted_matched <- pool(wellbeing_age19_adjusted_matched)
summary(wellbeing_age19_adjusted_matched) # Coefficient -0.32, p-value 0.07


### Run model 2 with matched groups (average wellbeing across ages 19, 22, 26)

# Convert to long data and include only timepoints 19, 22, 26
matched_long_combined <- lapply(matched_combined, function(data) {
  data %>%
    pivot_longer(cols = c(`cladder_Age 19`, `cladder_Age 22`, `cladder_Age 26`),  # Convert only outcome ages
                 names_to = "timepoint",
                 names_pattern = "cladder_Age (\\d+)",
                 values_to = "cladder") %>%
    mutate(timepoint = as.numeric(timepoint)) %>%   # Ensure timepoint is numeric
    filter(timepoint %in% c(19, 22, 26)) # Filter for relevant timepoints (19, 22, 26)
})
  
# Unadjusted 
wellbeing_avg_unadjusted_matched <- lapply(matched_long_combined, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + timepoint + 
         (1 | childid) + (1 | clustid),  
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

wellbeing_avg_unadjusted_matched <- lapply(wellbeing_avg_unadjusted_matched, extract_lmer_results)
wellbeing_avg_unadjusted_matched <- bind_rows(wellbeing_avg_unadjusted_matched, .id = "imputation")
wellbeing_avg_unadjusted_matched <- compute_pooled_summary(wellbeing_avg_unadjusted_matched)
wellbeing_avg_unadjusted_matched # Coefficient -0.238, p-value 0.04

# Adjusted
wellbeing_avg_adjusted_matched <- lapply(matched_long_combined, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + timepoint + `hhsize_Age 12` + `hq_Age 12` + `cd_Age 12` + 
         `careladder_Age 12` + `debt_Age 12` + 
         `paid_Age 12` + `parentasp_Age 12` + `aspirations_Age 12` + `birthorder` +  
         (1 | childid) + (1 | clustid),  
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

wellbeing_avg_adjusted_matched <- lapply(wellbeing_avg_adjusted_matched, extract_lmer_results)
wellbeing_avg_adjusted_matched <- bind_rows(wellbeing_avg_adjusted_matched, .id = "imputation")
wellbeing_avg_adjusted_matched <- compute_pooled_summary(wellbeing_avg_adjusted_matched)
wellbeing_avg_adjusted_matched # Coefficient -0.203, p-value 0.07


# Model 1: Wellbeing at 19
results_age19_matched <- extract_cm_results(wellbeing_age19_unadjusted_matched, "Predicted Wellbeing at 19 (Unadjusted)", 1) %>%
  bind_rows(extract_cm_results(wellbeing_age19_adjusted_matched, "Predicted Wellbeing at 19 (Adjusted - Social demographic + Psychological)", 1)) 

# Model 2: Average wellbeing across ages 19, 22 and 26
results_avg_matched <- extract_cm_results(wellbeing_avg_unadjusted_matched, "Predicted Average Wellbeing (Unadjusted)", 2) %>%
  bind_rows(extract_cm_results(wellbeing_avg_adjusted_matched, "Predicted Average Wellbeing (Adjusted - Social demographic + Psychological)", 2))

# Print results
print(results_age19_matched)
print(results_avg_matched)

# Plot results for model 1 
# Model 1 
model1 <- ggplot(results_age19_matched,
                 aes(x = Model, y = HR, color = Model)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(title = "Predicted difference in wellbeing score at age 19 for child brides compared to non-child brides", subtitle = "Matched Groups",
       y = "Coefficient", x = NULL) +
  scale_color_manual(values = c("#fdae61", "#7fcdbb", "#4575b4")) +
  theme_minimal() +
  theme(legend.position = "none")

print(model1)
ggsave("child_marriage_age19_matched.png", plot = model1, width = 12, height = 10, dpi = 300, bg = "white")


# Model 2 
model2 <- ggplot(results_avg_matched,
                 aes(x = Model, y = HR, color = Model)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(title = "Predicted difference in average wellbeing score from ages 19-26 for child brides compared to non-child brides", subtitle = "Matched Groups",
       y = "Coefficient", x = NULL) +
  scale_color_manual(values = c("#fdae61", "#7fcdbb", "#4575b4")) +
  theme_minimal() +
  theme(legend.position = "none")

print(model2)
ggsave("child_marriage_ave19-26_matched.png", plot = model2, width = 12, height = 10, dpi = 300, bg = "white")


## Run model 3 with matched groups (wellbeing trajectories across ages 19, 22, 26)

# Check that the dataset is still filtered correctly
matched_long_combined[[19]]$timepoint %>% unique() # 19, 22, 26 (will add 29)

## Unadjusted
wellbeing_traj_unadjusted_matched <- lapply(matched_long_combined, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + factor(timepoint) + CM * factor(timepoint) + # add interaction term; make sure timepoint is a factor to get trajectory
         (1 | childid) + (1 | clustid),  # clustering
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# Function to get estimated marginal means from the model
get_emmeans <- function(model) {
  emmeans(model, ~ CM * timepoint, 
          nuisance = c("cladder_Age 12", "typesite_Age 12", 
                       "chethnic_Age 12", "chldrel_Age 12", "hhsize_Age 12", 
                       "wi_Age 12", "hq_Age 12", "cd_Age 12", "enrol_Age 12", 
                       "caredu_Age 12", "careladder_Age 12", "debt_Age 12", 
                       "paid_Age 12", "parentasp_Age 12", "aspirations_Age 12", 
                       "birthorder"),
          type = "response") %>% 
    as.data.frame()  # Convert to dataframe for pooling
}

# Extract emmeans for plotting
emmeans_unadjusted <- lapply(wellbeing_traj_unadjusted_matched, get_emmeans)

# Pool imputed results
pooled_emmeans_unadjusted_matched <- bind_rows(emmeans_unadjusted, .id = "imputation") %>%
  group_by(CM, timepoint) %>%
  summarise(
    pooled_emmean = mean(emmean),  # Mean estimated marginal mean
    pooled_SE = sqrt(mean(SE^2)),  # Pooled standard error
    lower.CL = pooled_emmean - 1.96 * pooled_SE,
    upper.CL = pooled_emmean + 1.96 * pooled_SE,
    .groups = "drop"
  )

# Extract results 
wellbeing_traj_unadjusted_matched <- lapply(wellbeing_traj_unadjusted_matched, extract_lmer_results)
wellbeing_traj_unadjusted_matched <- bind_rows(wellbeing_traj_unadjusted_matched, .id = "imputation")

# Create summary statistics 
wellbeing_traj_unadjusted_sum <- compute_pooled_summary(wellbeing_traj_unadjusted_matched)
wellbeing_traj_unadjusted_sum
# At age 19, child brides significantly poorer wellbeing -0.28 (p = 0.056)
# From age 19-22, CM wellbeing improved 0.333 more than non-CM but not significant (p = 0.14)
# From age 19-26, child brides wellbeing declined more than non-child brides by 0.16 (p = 0.43)


## Adjusted - Full
wellbeing_traj_adjusted_matched <- lapply(matched_long_combined, function(data) {
  lmer(cladder ~ CM + `cladder_Age 12` + factor(timepoint) + CM * factor(timepoint) +  
         factor(`typesite_Age 12`) + factor(`chethnic_Age 12`) + 
         factor(`chldrel_Age 12`) + factor(`enrol_Age 12`) + 
         factor(`caredu_Age 12`) + factor(`debt_Age 12`) + 
         factor(`paid_Age 12`) + factor(`parentasp_Age 12`) + 
         factor(`aspirations_Age 12`) + 
         `hhsize_Age 12` + `wi_Age 12` + `hq_Age 12` + `cd_Age 12` + 
         `careladder_Age 12` + `birthorder` + 
         (1 | childid) + (1 | clustid),  
       data = data,
       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
})

# Extract estimated marginal means 
emmeans_adjusted <- lapply(wellbeing_traj_adjusted_matched, get_emmeans)

# Pool imputed results
pooled_emmeans_adjusted_matched <- bind_rows(emmeans_adjusted, .id = "imputation") %>%
  group_by(CM, timepoint) %>%
  summarise(
    pooled_emmean = mean(emmean),  
    pooled_SE = sqrt(mean(SE^2)),  
    lower.CL = pooled_emmean - 1.96 * pooled_SE,
    upper.CL = pooled_emmean + 1.96 * pooled_SE,
    .groups = "drop"
  )


# Extract results from all imputations
wellbeing_traj_adjusted_matched <- lapply(wellbeing_traj_adjusted_matched, extract_lmer_results)
wellbeing_traj_adjusted_matched <- bind_rows(wellbeing_traj_adjusted_matched, .id = "imputation")

# Compute summary statistics
wellbeing_traj_adjusted_sum <- compute_pooled_summary(wellbeing_traj_adjusted_matched)
wellbeing_traj_adjusted_sum %>% print(n = 30)
# At age 19, child brides poorer wellbeing -0.26 but not significant (p = 0.07)
# From age 19-22, CM wellbeing improved 0.34 more than non-CM but not significant (p = 0.12)
# From age 19-26, child brides wellbeing declined more than non-child brides by 0.15 but not significant (p = 0.45)


# Line plot (Model 3 Trajectories - unadjusted and adjusted)
pooled_emmeans_unadjusted_matched$model <- "Unadjusted"
pooled_emmeans_adjusted_matched$model <- "Adjusted - Social demographic\n + Psychological"
pooled_emmeans_combined_matched <- bind_rows(pooled_emmeans_unadjusted_matched, pooled_emmeans_adjusted_matched)
pooled_emmeans_combined_matched$model <- factor(pooled_emmeans_combined_matched$model, levels = c("Unadjusted", "Adjusted - Social demographic\n + Psychological"))

traj_plot <- ggplot(pooled_emmeans_combined_matched, aes(x = timepoint, y = pooled_emmean, color = factor(CM), group = factor(CM))) +
  geom_point() +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  facet_wrap(~ model, nrow = 1) +  # Split into Adjusted vs. Unadjusted
  labs(
    title = "Predicted Wellbeing Trajectories from Ages 19-26",
    subtitle = "Matched Groups",
    x = "Age (Timepoint)",
    y = "Predicted Wellbeing Score",
    color = "Child Marriage Status"
  ) + 
  scale_y_continuous(limits = c(3.5, 6)) + 
  scale_x_continuous(breaks = c(19, 22, 26), limits = c(18,27)) +
  scale_color_manual(values = c("Child bride" = "#d73027", "Non-child-bride" = "#2c7fb8")) +
  theme_minimal()

# Save the plot
ggsave("child_marriage_trajectory_matched.png", plot = traj_plot, width = 12, height = 10, dpi = 300, bg = "white")

# Display the plot
print(traj_plot)


## Run diff in diff with matched groups (age 12-19)

matched_yc_data <- lapply(matched_yc, function(x) x$matched_data)
matched_yc_data <- lapply(matched_yc_data, function(data) {
  filter(data, age_married_combined > 12)
  })

matched_yc_long <- lapply(matched_yc_data, function(df) {
  pivot_longer(df,
               cols = starts_with("cladder_Age"),
               names_to = "equivalent_round",
               values_to = "cladder") %>%
    mutate(equivalent_round = gsub("cladder_", "", equivalent_round))
})

# --- Parallel trends ---
# Prerequisite to DiD analysis for 12 vs 19
matched_yc_parallel_list <- lapply(matched_yc_long, function(data) {
  data %>%
    filter(equivalent_round %in% c("Age 8", "Age 12"), !is.na(CM)) %>%
    group_by(equivalent_round, CM) %>%
    summarise(mean_cladder = mean(cladder, na.rm = TRUE), .groups = 'drop')
})

# Combine all into one dataframe with imputation ID
matched_yc_parallel_combined <- bind_rows(matched_yc_parallel_list, .id = "imputation")

# Pool across imputations by averaging
matched_yc_parallel_pooled <- matched_yc_parallel_combined %>%
  group_by(equivalent_round, CM) %>%
  summarise(mean_cladder = mean(mean_cladder, na.rm = TRUE), .groups = "drop")

matched_yc_parallel_pooled$equivalent_round <- factor(
  matched_yc_parallel_pooled$equivalent_round,
  levels = c("Age 8", "Age 12")
)

# Plot parallel trends
ggplot(matched_yc_parallel_pooled, aes(x = equivalent_round, y = mean_cladder,
                                       color = factor(CM), group = CM)) + 
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(x = "Age", y = "Average Wellbeing Score",
       title = "Parallel Trends Check (Matched Younger Cohort)",
       color = "Child Marriage") +
  scale_color_manual(values = c("Non-child-bride" = "#00BFC4", "Child bride" = "#F8766D"),
                     labels = c("Non-child-bride", "Child bride")) +
  scale_y_continuous(limits = c(1, 9)) +
  theme_minimal()

# Cannot conduct DiD after matching because parallel trends assumption not met - will capture the effect of pre-existing disadvantages rather than the effect of child marriage


#---------------------------------- Session information ---------------
sessionInfo()
