## This is the second coding sample for a current project on the impacts of child marriage on wellbeing across Peru, India, Ethiopia, and Vietnam.

## This is the multiple imputation file for PERU. 
# 1. Check data structure
# 2. Transform variables before imputation (sparse values, convert to factor)
# 3. Conduct imputations for two cohorts separately - specify imputation methods
# 4. Check imputation diagnostics
# 5. Save imputation data


# ----------------------------- LOAD LIBRARIES AND DATA -------------------

# Load necessary libraries
library(dplyr)
library(sjPlot)
library(tidyr)
library(graphics)
library(rstatix)
library(tidyverse)
library(ggplot2)
library(nnet)
library(MASS)
library(naniar)
library(lubridate)
library(fixest)
library(naniar)
library(emmeans)
library(gtsummary)
library(tidyverse)
library(kableExtra)
library(gt)
library(survey)
library(mice)
library(miceadds)
library(flextable)
options(scipen = 999)

# NOTE: raw data files are not included due to data access restrictions.
# Code is provided to demonstrate workflow, structure, and methods.

# Load Dataset
data_long <- readRDS("data_long.rds")
data_long %>% colnames()
data_long$childid %>% n_distinct() # Number of unique individuals: 1353 girls - correct! 


# ---------------------------- DATA STRUCTURE AND CORRELATIONS-------------------------

## --- Check data structure ---

## Number of girls that are child brides vs non-child brides
data_long %>%
  group_by(CM) %>%
  summarise(count = n_distinct(childid))

# CM                     count
# Non-child-bride         1127
# Child bride              178
# NA                      48  # Missing CM data for 48 girls 

## Number of girls in each cohort
data_long %>%
  group_by(yc) %>%
  summarise(count = n_distinct(childid))

#    yc count
#    0   328
#    1   1025


### --- Correlations and distributions ---

data_oc <- data_long %>% filter(yc == 0) # Older cohort
data_yc <- data_long %>% filter(yc == 1) # Younger cohort

# Include variables of interest for correlations
variables_check <- c("cladder", "typesite", "chethnic", "hhsize", 
                     "wi", "hq", "cd", "enrol", "caredu", "careladder", "birthorder", "debt", "paid")

## Correlations for older cohort 
cor_matrix_oc <- data_oc %>%
  select(all_of(variables_check)) %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")
corrplot::corrplot(cor_matrix_oc, method = "circle", title = NULL)
title("1994-Cohort Correlation Matrix", line = 2, cex.main = 1.2)  

## Correlations for younger cohort
cor_matrix_yc <- data_yc %>% 
   select(all_of(variables_check)) %>%
   select(where(is.numeric)) %>%
   cor(use = "complete.obs")
corrplot::corrplot(cor_matrix_yc, method = "circle", title = NULL)
title("2001-Cohort Correlation Matrix", line = 2, cex.main = 1.2)  

# Visualise cladder distributions by cohort (pre-imputation) 
ggplot(data_long, aes(x = cladder, fill = factor(yc))) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.6) +
  labs(title = "Distribution of Cladder Scores by Cohort",
       x = "Cladder Score", y = "Count") +
  facet_wrap(~ yc, labeller = labeller(yc = c("0" = "1994-Cohort", "1" = "2001-Cohort"))) +
  theme_minimal()

# Boxplot for cladder scores by CM status
ggplot(data_long, aes(x = CM, y = cladder, fill = CM)) +
  geom_boxplot() +
  facet_wrap(~ yc) +
  labs(title = "Cladder Score by Child Marriage Status and Cohort",
       x = "Child Marriage Status", y = "Cladder Score") +
  theme_minimal()

# Doesn't really seem like any difference at the moment


#---------------------------- TRANSFORM VARIABLES BEFORE IMPUTATION ------------------------

## Check class of variables 
str(data_long)
data_long <- data_long %>% select(-missing_cladder, -pre_post_marriage)

# Convert to factors
data_long <- data_long %>% mutate(yc = factor(yc, levels = c(0, 1), labels = c("1994-Cohort", "2001-Cohort")))
data_long <- data_long %>% mutate(chsex = factor(chsex, levels = c(1, 2), labels = c("Boys", "Girls")))
data_long <- data_long %>% mutate(typesite = factor(typesite, levels = c(1, 2), labels = c("Urban", "Rural")))
data_long <- data_long %>% mutate(birth = factor(birth, levels = c(0, 1), labels = c("No", "Yes")))

## Ethnic group
data_long$chethnic %>% table() 
# 31 - White, 32 - Mestizo, 33 - Native of the amazon, 34 - Negro, 35 - Asiatic

data_long <- data_long %>% 
  mutate(
    chethnic = case_when(
      chethnic == 31 ~ "White", 
      chethnic == 32 ~ "Mestizo",
      chethnic == 33 ~ "Native of the Amazon", 
      chethnic == 34 ~ "Black",
      chethnic == 35 ~ "Asian"
    )
  )

## Language
data_long$chlang %>% table()
# 31 - spanish, 32 - quechua, 33 - aymara, 34 - native from jungle, 35 - spanish & quechua, 36 - spanish & aymara, 37 - nomatsiguenga (native language in jungle), 10 - other 
# Combine 10 and 34 

data_long <- data_long %>% 
  mutate(
    chlang = case_when(
      chlang %in% c(10, 34) ~ "Native language / others",
    ),
    chlang = as.factor(chlang)
  )

## Religion
data_long$chldrel %>% table()
# 4 - hindu, 5 - catholic, 9 - evangelist, 10 - mormon, 14 - none, 15 - other
# Combine 4, 10, 15

data_long <- data_long %>% 
  mutate(chldrel = case_when(
    chldrel == 5 ~ "Catholic", 
    chldrel == 9 ~ "Evangelist", 
    chldrel == 14 ~ "None", 
    chldrel %in% c(4, 10, 15) ~ "Others/Hindu/Mormon"
  ),
  chldrel = as.factor(chldrel)
  )



## Marital status
data_long$marrcohab %>% table()
data_long <- data_long %>% mutate(marrcohab = factor(marrcohab, levels = c(0, 1), labels = c("Never married", "Ever married")))

data_long$marstatcov %>% table()
data_long <- data_long %>% 
  mutate(
    marstatcov = factor(case_when(
      marstatcov %in% 0 ~ "Never married", 
      marstatcov %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ "Ever married",
      TRUE ~ as.character(marstatcov) 
    ))
  )

data_long$ever_child_marriage <- as.factor(data_long$ever_child_marriage)

data_long$preg1cov %>% table()
data_long <- data_long %>% mutate(preg1cov = factor(preg1cov, levels = c(0, 1), labels = c("No", "Yes")))

## Education
data_long$enrol %>% table()
data_long <- data_long %>% mutate(
  enrol = factor(enrol, levels = c(0, 1),
                 labels = c("No", "Yes"))
)

data_long$engrade %>% table()
data_long <- data_long %>% 
  mutate(engrade = case_when(
    engrade == 0 ~ "None",
    engrade %in% c(1:11) ~ "Primary-secondary",
    engrade %in% c(13, 21) ~ "Incomplete technical",
    engrade %in% c(14, 16, 22) ~ "Complete university/technical", 
    engrade == 15 ~ "Incomplete university", 
    engrade == 17 ~ "Adult literacy", 
    engrade == 18 ~ "Other", 
    engrade == 19 ~ "Postgraduate"
  ))
data_long$engrade <- as.factor(data_long$engrade)

data_long$entype %>% table()
data_long <- data_long %>% 
  mutate(entype = case_when(
    entype == 1 ~ "Private", 
    entype == 2 ~ "Public", 
    entype %in% c(3, 4)  ~ "Half public/private or Other",
  ))
data_long$entype <- as.factor(data_long$entype)

## Child's aspirations 
data_long <- data_long %>% 
  mutate(aspirations = case_when(
    aspirations %in% c("Primary", "Secondary") ~ "Primary-secondary", 
    aspirations %in% c("Adult literacy", "Incomplete technical", "Others", "Complete technical") ~ "Complete/incomplete technical/Adult literacy/Other",
    TRUE ~ aspirations
  ))
data_long$aspirations <- as.factor(data_long$aspirations)
data_long$aspirations %>% table()

## Parent aspirations for the child
data_long$parentasp %>% table() 
# Combine none with adult literacy, incomplete technical and others (sparse categories)
data_long <- data_long %>% 
  mutate(parentasp = case_when(
    parentasp %in% c("Primary", "Secondary") ~ "Primary-secondary", 
    parentasp %in% c("Complete technical", "Adult literacy", "Incomplete technical", "None", "Others") ~ "Complete/incomplete technical/Adult literacy/None/Other",
    TRUE ~ parentasp
  ))
data_long$parentasp <- as.factor(data_long$parentasp)

## Caregiver education
data_long$caredu %>% table()
data_long <- data_long %>% 
  mutate(caredu = case_when(
    caredu %in% c(1:11) ~ "Primary-secondary", 
    caredu %in% c(13, 28, 30) ~ "Incomplete technical/Adult literacy/Other",
    caredu == 14 ~ "Complete technical", 
    caredu == 15 ~ "Incomplete university", 
    caredu == 16 ~ "Complete university"
  ))
data_long$caredu <- as.factor(data_long$caredu)


## Review data 
str(data_long)
data_oc <- data_long %>% filter(yc == "1994-Cohort") # Older cohort
data_yc <- data_long %>% filter(yc == "2001-Cohort") # Younger cohort


#------------------------------- 1994-COHORT: MULTIPLE IMPUTATION------------------------

## --- Convert to wide data for imputation --

data_oc %>% str()

# Constant variables
variables <- data_oc %>% 
  group_by(childid) %>%  
  summarise(
    across(
      c(chsex, chethnic, CM, marrcohab, age_married_combined, ever_married_combined, birth, birth_age, clustid, birthorder, chldrel), 
      ~ first(na.omit(.))  # Take the first non-NA value
    ), 
    .groups = "drop"  # Drop grouping after summarizing
  )

# Reshape to wide data for imputation
data_oc_wide_pre <- data_oc %>% 
  pivot_wider(
    id_cols = c(childid),
    names_from = round,
    values_from = c(equivalent_round, typesite, chlang, hhsize, chhrel, chhealth, cladder, wi, hq, cd, enrol, caredu, careladder, gad1, gad2, gad3, gad4, gad5, gad6, gad7, phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8, emo, debt, parentasp, paid, aspirations)) %>%  # no SDQ data
  left_join(variables, by = "childid")

data_oc_wide_pre <- data_oc_wide_pre %>% relocate(names(variables)[-1], .after = childid)

data_oc_wide_pre %>% str()


## --- Select variables for imputation  -- 

data_oc_wide <- data_oc_wide_pre %>% select(childid, chsex, clustid, chethnic, birthorder, marrcohab, age_married_combined, ever_married_combined, birth, birth_age, CM, typesite_2, chlang_2, chldrel, hhsize_1, hhsize_2, hhsize_3, hhsize_4, hhsize_5, chhrel_1, chhrel_2, chhealth_3, chhealth_4, chhealth_5, cladder_2, cladder_3, cladder_4, cladder_5, cladder_6, wi_2, hq_2, cd_2, wi_3, hq_3, cd_3, enrol_1, enrol_2, enrol_3, enrol_4, enrol_5, caredu_2, careladder_2, gad1_6, gad2_6, gad3_6, gad4_6, gad5_6, gad6_6, gad7_6, phq1_6, phq2_6, phq3_6, phq4_6, phq5_6, phq6_6, phq7_6, phq8_6, emo_3, emo_4, emo_5, equivalent_round_1, equivalent_round_2, equivalent_round_3, equivalent_round_4, equivalent_round_5, equivalent_round_6, debt_2, parentasp_2, paid_2, aspirations_2)

# Make sure age of marriage is only imputed for those who were ever married
data_oc_wide <- data_oc_wide %>%
  mutate(age_married_combined = ifelse(marrcohab == 0, NA, age_married_combined))

# Rename chlang
data_oc_wide <- data_oc_wide %>% rename(chlang = chlang_2)


## --- Specify methods for imputation -- 

# Inspect default methods
default_methods <- mice(data_oc_wide, maxit = 0)$method  # This retrieves default methods for your variables
methods_oc <- default_methods
methods_oc

# Update imputation methods based on variable types
methods_oc[c("childid", "chsex", "clustid", "birthorder", "chethnic")] <- "" # Don't impute constant cols
methods_oc[c("aspirations_2", "chlang", "caredu_2","parentasp_2")] <- "polyreg" # Polynomial regression 
methods_oc["CM"] <- "logreg" # Child bride or not
methods_oc["typesite_2"] <- "logreg" # Urban/rural
methods_oc[c("enrol_1", "enrol_2", "enrol_3", "enrol_4", "enrol_5")] <- "logreg"       # Enrolled in school/not
methods_oc[c("birth", "debt_2", "paid_2", "marrcohab")] <- "logreg" # Logistic regression for binary variables
#methods_oc["age_married_combined"] <- "" # Age of marriage shouldn't be imputed

methods_oc # Check updated methods



## --- Check and remove any variables which are constant and collinear --

# Check which variables are constant
constant_vars <- data_oc_wide %>%
  select_if(is.numeric) %>%  # Only numeric variables
  summarise(across(everything(), ~ sd(., na.rm = TRUE))) %>%  # Calculate SD for each variable
  pivot_longer(cols = everything(), names_to = "variable", values_to = "sd") %>%  # Reshape to long format
  filter(sd == 0)  # Filter variables with SD = 0
constant_vars

# Calculate correlation matrix
cor_matrix <- data_oc_wide %>%
  select_if(is.numeric) %>%
  cor(use = "pairwise.complete.obs")

# Extract highly correlated variable pairs (absolute correlation > 0.8)
high_corr_pairs <- which(abs(cor_matrix) > 0.8 & lower.tri(cor_matrix), arr.ind = TRUE)

# Create a dataframe with the variable names and correlation values
high_corr_df <- data.frame(
  Var1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
  Var2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
  Correlation = cor_matrix[high_corr_pairs]
)

# View the top correlated pairs
high_corr_df <- high_corr_df[order(-abs(high_corr_df$Correlation)), ]
high_corr_df 
# Consumer durables, housing quality and wealth indices - makes sense



## --- Create predictor matrix and run imputation -- 

# Create predictor matrix
pred_matrix <- quickpred(data_oc_wide, mincor = 0.1)
pred_matrix

# Exclude specific variables from being imputed and used as predictors
pred_matrix[c("childid", "chsex", "equivalent_round_1", "equivalent_round_2", "equivalent_round_3", "equivalent_round_4", "equivalent_round_5", "equivalent_round_6", "chethnic", "clustid"), ] <- 0  # Exclude these as predictors
pred_matrix[, c("childid", "chsex", "equivalent_round_1", "equivalent_round_2", "equivalent_round_3", "equivalent_round_4", "equivalent_round_5", "equivalent_round_6", "chethnic", "clustid")] <- 0  # Ensure they are not used as predictors

# Only those who were married should have age of marriage
pred_matrix["ever_married_combined", ] <- 0  # Do not impute ever_married_combined
pred_matrix[, "ever_married_combined"] <- 1  # Use it as a predictor
pred_matrix["age_married_combined", "ever_married_combined"] <- 1  # Condition age_married_combined on ever_married

#imputed_oc$method["caredu_2"] <- "polyreg" # force it to impute caredu_2
# Ensure `caredu_2` has predictors assigned
#pred_matrix["caredu_2", ] <- 0  # Reset first
#pred_matrix["caredu_2", c("wi_2", "hq_2", "cd_2", "hhsize_2", "enrol_2", "parentasp_2", "careladder_2")] <- 1  # Key predictors
#pred_matrix["caredu_2", "caredu_2"] <- 0  # Prevent self-prediction
#print(pred_matrix["caredu_2", ])  # Should NOT be all 0s

methods_oc

## Run mice model
imputed_oc <- mice(data_oc_wide, m = 20, maxit = 20, method = methods_oc, predictorMatrix = pred_matrix, seed = 123, printFlag = TRUE)

# View summary and warnings 
summary(imputed_oc)
imputed_oc$loggedEvents 

## Modify only imputed values directly within the MICE structure - only those who were married should have age of marriage
for (i in 1:20) {
  missing_idx <- which(is.na(data_oc_wide$age_married_combined))  # Get missing indices
  imputed_oc$imp$age_married_combined[[i]] <- 
    ifelse(data_oc_wide$ever_married_combined[missing_idx] == "Not Married", 
           NA, 
           imputed_oc$imp$age_married_combined[[i]])
}

# Extract wide-form imputed datasets
imputed_wide_oc <- complete(imputed_oc, action = "all")  # Should now work!

# Verify row count is correct
nrow(imputed_wide_oc$"20")  # 328 girls - correct! 

# Re-do CM variable 
imputed_wide_oc <- lapply(imputed_wide_oc, function(data) {
  data %>%
    mutate(
      CM = case_when(
        ever_married_combined == "Not Married" ~ "Non-child-bride",
        !is.na(age_married_combined) & age_married_combined < 18 ~ "Child bride",
        !is.na(age_married_combined) & age_married_combined >= 18 ~ "Non-child-bride",
        TRUE ~ NA_character_  # Catch unexpected cases
      )
    )
})


## --- Compare observed vs imputed data -- 

# Save all convergence plots to a PDF file
pdf("imputed_oc_diagnostics.pdf", width = 14, height = 10)  # Adjust size as needed
plot(imputed_oc)  # Generate the plot
dev.off()  # Close the PDF device

# Save all density plots to a PDF file 
pdf("density_plots_oc.pdf", width = 14, height = 10)  # Adjust size as needed
densityplot(imputed_oc, ~ age_married_combined + wi_2 + hq_2 + cd_2 + emo_3 + emo_4 + emo_5) # Density plots available only for numerical variables
dev.off()  # Close the PDF device

summary(data_oc_wide$age_married_combined)  # Observed values
summary(complete(imputed_oc, action = 1)$age_married_combined)  # Imputed values

plot(imputed_oc, c("age_married_combined"))
densityplot(imputed_oc, ~ age_married_combined) 

summary(data_oc_wide$cladder_4)  # Observed values
summary(complete(imputed_oc, action = 1)$cladder_4)  # Imputed values

## --- Reshape to long data -- 

imputed_long_data_oc <- lapply(imputed_wide_oc, function(data) {
  data %>%
    pivot_longer(
      cols = starts_with(c("equivalent_round_", "typesite_", "hhsize_", "chhrel_", "chhealth_", 
                           "cladder_", "wi_", "hq_", "cd_", "enrol_", 
                           "caredu_", "careladder_", "gad1_", "gad2_", 
                           "gad3_", "gad4_", "gad5_", "gad6_", "gad7_", 
                           "phq1_", "phq2_", "phq3_", "phq4_", "phq5_", 
                           "phq6_", "phq7_", "phq8_", "emo_", "debt_", "parentasp_", "paid_", "aspirations_")),  # Include only multi-round variables
      names_to = c(".value", "round"),  # `.value` creates a single row per round
      names_pattern = "(.+)_(\\d+)"     # Extract variable name and round number
    ) 
})

imputed_long_data_oc <- lapply(imputed_long_data_oc, function(data) {
  data %>% relocate(equivalent_round, .after = childid) })
imputed_long_data_oc$"20" # Long data, correct! 


## --- Save imputation data -- 

saveRDS(imputed_long_data_oc, "imputed_long_data_oc.rds")

imputed_long_combined <- bind_rows(
  lapply(seq_along(imputed_long_data_oc), function(i) {
    imputed_long_data_oc[[i]] %>% mutate(.imp = i) # Add imputation number as sa column 
  })
)
saveRDS(imputed_long_combined, "imputed_long_combined_oc.rds")



#------------------------ 2001-COHORT: MULTIPLE IMPUTATION----------------------

## --- Convert to wide data for imputation --

data_yc %>% colnames()

# Constant variables
variables <- data_yc %>% group_by(childid) %>%  summarise(across(c(chsex, chethnic, CM, ever_married_combined, age_married_combined, clustid, birthorder), first)) # Take the first value within each group - constant across rounds

# Reshape to wide data for imputation
data_yc_wide_pre <- data_yc %>% 
  pivot_wider(
    id_cols = childid,
    names_from = round,
    values_from = c(equivalent_round, typesite, chlang, chldrel, hhsize, chhrel, chhealth, cladder, wi, hq, cd, enrol, marstatcov, preg1cov, preg3cov, caredu, careladder, gad1, gad2, gad3, gad4, gad5, gad6, gad7, phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8, debt, parentasp, paid, aspirations)) %>%  # no SDQ data
  left_join(variables, by = "childid")

data_yc_wide_pre <- data_yc_wide_pre %>% relocate(names(variables)[-1], .after = childid)

data_yc_wide_pre %>% str()

## --- Select variables for imputation  -- 

# Select relevant round variables
data_yc_wide <- data_yc_wide_pre %>% select(childid, chsex, clustid, chethnic, birthorder, marstatcov_6, preg1cov_6, preg3cov_6, age_married_combined, ever_married_combined, CM, typesite_4, chlang_4, chldrel_3, hhsize_3, hhsize_4, hhsize_5, chhrel_5, chhealth_3, chhealth_4, chhealth_5, cladder_3, cladder_4, cladder_5, cladder_6, wi_4, hq_4, cd_4, enrol_3, enrol_4, enrol_5, gad1_6, gad2_6, gad3_6, gad4_6, gad5_6, gad6_6, gad7_6, phq1_6, phq2_6, phq3_6, phq4_6, phq5_6, phq6_6, phq7_6, phq8_6, caredu_4, careladder_3, careladder_4, careladder_5, equivalent_round_3, equivalent_round_4, equivalent_round_5, equivalent_round_6, debt_4, parentasp_4, paid_4, aspirations_4)

# Age of marriage is all N/A so no imputation, but need to keep for when combined with older cohort

# Rename chlang
data_yc_wide <- data_yc_wide %>% rename(chlang = chlang_4)
data_yc_wide <- data_yc_wide %>% rename(chldrel = chldrel_3)


## --- Specify methods for imputation -- 

# Inspect default methods
default_methods <- mice(data_yc_wide, maxit = 0)$method  # This retrieves default methods for your variables
methods_yc <- default_methods
methods_yc

# Update imputation methods based on variable types
methods_yc[c("childid", "chsex", "clustid", "birthorder", "chethnic")] <- "" # Don't impute constant cols
methods_yc[c("typesite_4", "preg1cov_6", "enrol_3", "enrol_4", "enrol_5", "debt_4", "paid_4")] <- "logreg" # Binary
methods_yc["age_married_combined"] <- " " # Age of marriage shouldn't be imputed
methods_yc["chlang"] <- "polyreg"  # Language
methods_yc["parentasp_4"] <- "polyreg" # Parent's aspirations for child
methods_yc["marstatcov_6"] <- "polyreg"        # Marital status
methods_yc["aspirations_4"] <- "polyreg"        # Aspirations
methods_yc["caredu_4"] <- "polyreg"        # Caregiver education

methods_yc # Check updated methods




## --- Check and remove any variables which are constant and collinear --

# Check which variables are constant
constant_vars <- data_yc_wide %>%
  select_if(is.numeric) %>%  # Only numeric variables
  summarise(across(everything(), ~ sd(., na.rm = TRUE))) %>%  # Calculate SD for each variable
  pivot_longer(cols = everything(), names_to = "variable", values_to = "sd") %>%  # Reshape to long format
  filter(sd == 0)  # Filter variables with SD = 0
constant_vars

# Calculate correlation matrix
cor_matrix <- data_yc_wide %>%
  select_if(is.numeric) %>%
  cor(use = "pairwise.complete.obs")

# Extract highly correlated variable pairs (absolute correlation > 0.8)
high_corr_pairs <- which(abs(cor_matrix) > 0.8 & lower.tri(cor_matrix), arr.ind = TRUE)

# Create a dataframe with the variable names and correlation values
high_corr_df <- data.frame(
  Var1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
  Var2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
  Correlation = cor_matrix[high_corr_pairs]
)

# View the top correlated pairs
high_corr_df <- high_corr_df[order(-abs(high_corr_df$Correlation)), ]
high_corr_df 
# Correlation between consumer durables, housing quality and wealth index - makes sense 


## --- Create predictor matrix and run imputation -- 

# Create predictor matrix
pred_matrix_yc <- quickpred(data_yc_wide, mincor = 0.1) 

# Exclude specific variables from being imputed and used as predictors
pred_matrix_yc[c("childid", "chsex", "equivalent_round_3", "equivalent_round_4", "equivalent_round_5", "equivalent_round_6", "chethnic", "clustid"), ] <- 0  # Exclude these as predictors
pred_matrix_yc[, c("childid", "chsex", "equivalent_round_3", "equivalent_round_4", "equivalent_round_5", "equivalent_round_6", "chethnic", "clustid")] <- 0  # Ensure they are not used as predictors

pred_matrix_yc["ever_married_combined", ] <- 0  # Do not impute ever_married_combined
pred_matrix_yc[, "ever_married_combined"] <- 1  # Use it as a predictor

pred_matrix["caredu_2", ] <- 1  # Allow `caredu_2` to be predicted
pred_matrix["caredu_2", "caredu_2"] <- 0  # But it should NOT predict itself

methods_yc

# Run mice model
imputed_yc <- mice(data_yc_wide, m = 20, maxit = 20, method = methods_yc, predictorMatrix = pred_matrix_yc, seed = 123, printFlag = TRUE)

# View summary and warnings 
summary(imputed_yc)
imputed_yc$loggedEvents # issues with age of marriage for those never married -- should be NA, need to do this manually

# Extract wide-form imputed datasets
imputed_wide_yc <- complete(imputed_yc, action = "all")  # Should now work!

# Verify row count is correct
nrow(imputed_wide_yc$"20")  # 1025 girls - correct! 


## --- Compare observed vs imputed data -- 

# Save all convergence plots to a PDF file
pdf("imputed_yc_diagnostics.pdf", width = 14, height = 10)  
plot(imputed_yc)  
dev.off()

# Save all density plots to a PDF file 
pdf("density_plots_yc.pdf", width = 14, height = 10)  # Adjust size as needed
densityplot(imputed_yc, ~ cladder_5 + wi_4 + hq_4 + cd_4)  # Generate the plots for numerical variables only
dev.off()  # Close the PDF device

# Compare pre vs post imputation
densityplot(imputed_yc, ~ cladder_5)

summary(data_yc_wide$cladder_5)  # Observed values
summary(complete(imputed_yc, action = 1)$cladder_5)  # Imputed values

# Everything looks in order!


## --- Reshape to long data -- 

names(imputed_wide_yc$`20`)

imputed_long_data_yc <- lapply(imputed_wide_yc, function(data) {
  data %>%
    pivot_longer(
      cols = starts_with(c("equivalent_round_", "typesite_", "hhsize_", "chhrel_", "chhealth_", "marstatcov_", "preg1cov_", "preg3cov_",
                           "cladder_", "wi_", "hq_", "cd_", "enrol_", 
                           "caredu_", "careladder_", "gad1_", "gad2_", 
                           "gad3_", "gad4_", "gad5_", "gad6_", "gad7_", 
                           "phq1_", "phq2_", "phq3_", "phq4_", "phq5_", 
                           "phq6_", "phq7_", "phq8_", "debt_", "parentasp_", "paid_", "aspirations_")),  # Include only multi-round variables
      names_to = c(".value", "round"),  # `.value` creates a single row per round
      names_pattern = "(.+)_(\\d+)"     # Extract variable name and round number
    ) 
})

imputed_long_data_yc <- lapply(imputed_long_data_yc, function(data) {
  data %>% relocate(equivalent_round, .after = childid) })
imputed_long_data_yc$"20" %>% head() # Long data, correct! 


## --- Save imputation data -- 

saveRDS(imputed_long_data_yc, "imputed_long_data_yc.rds")

imputed_long_combined_yc <- bind_rows(
  lapply(seq_along(imputed_long_data_yc), function(i) {
    imputed_long_data_yc[[i]] %>% mutate(.imp = i) # Add imputation number as sa column 
  })
)
saveRDS(imputed_long_combined_yc, "imputed_long_combined_yc")
