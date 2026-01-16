## This is the first coding sample for a current project on the impacts of child marriage on wellbeing across Peru, India, Ethiopia, and Vietnam.

## This is the data pre-processing file for PERU. 

## Here are the key steps:

# Data cleaning
# 1. Adding marital status, wellbeing, mental health data and covariates to the dataset
# 2. Transforming variables if necessary for standardisation across cohorts and rounds
# 3. Creating a child marriage variable (CM) and age of marriage (age_marriage_combined)
# 4. Stacking the cohorts according to ages (e.g. age 8 is round 1 for 1994-cohort but round 3 for 2001-cohort)

# Descriptive statistics and plots 
# 1. Pre-imputation descriptive statistics - word file
# 2. Visualisation plots for wellbeing score (grouping by age, cohort and child marriage status) 

# Save dataset for multiple imputation


# ----------------- LOAD LIBRARIES AND DATA ---------------------------

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
options(scipen = 999)

# NOTE: raw data files are not included due to data access restrictions.
# Code is provided to demonstrate workflow, structure, and methods.

# Load Datasets 
data <- read.csv("peru_constructed.csv")
gad7_oc <- read.csv("pe_oc_covid2_gad7_arch.csv")
phq8_oc <- read.csv("pe_oc_covid2_phq8_arch.csv")
gad7_yc <- read.csv("pe_yc_covid2_gad7_arch.csv")
phq8_yc <- read.csv("pe_yc_covid2_phq8_arch.csv")
cladder_oc <- read.csv("pe_oc_covid2_arch.csv")
cladder_yc <- read.csv("pe_yc_covid2_arch.csv")
marr_oc <- read.csv("pe_oc_covid4_arch.csv")
marr_yc <- read.csv("pe_yc_covid4_arch.csv")
marrcohab_oc_r4 <- read.csv("pe_r4_occh_olderchild.csv") # to separate marital status vs cohabitation
marrcohab_oc_r5 <- read.csv("pe_r5_occh_olderchild.csv") # round 5
birth_yc <- read.csv("pechildlevel5yrold.csv") %>% select(childid, birthorder = bornbef)
birth_oc <- read.csv("pechildlevel8yrold.csv") %>% select(childid, birthorder = order)
debt_yc <- read.csv("pe_r4_ychh_youngerhousehold.csv") 
debt_oc <- read.csv("pechildlevel12yrold.csv")
parentasp_yc <- read.csv("pe_r4_ychh_youngerhousehold.csv")
parentasp_oc <- read.csv("pechildlevel12yrold.csv") 
paidasp_yc <- read.csv("pe_r4_ycch_youngerchild.csv")
paidasp_oc <- read.csv("pechildquest12yrold.csv")
r3_sdq <- read.csv("r3_sdq.csv") # pe_oc_childlevel
r4_sdq <- read.csv("r4_sdq.csv") # pe_oc_saq_olderchild
r5_sdq <- read.csv("r5_sdq.csv") # pe_r5_ocsaq_olderchildsaq



# ----------------- DATA TRANSFORMATION ----------------------------------------

## --- Format Child ID ---
data %>% colnames()

data$childid %>% n_distinct() # Number of unique individuals: 2766
data <- data %>% filter(grepl("^PE\\d+", childid)) # Make sure format of childid is correct
data$childid %>% n_distinct() # There are 2766 individuals in total 

## Assign clustid based on childid

# The numeric part of the childid that corresponds to the clustid (if childid starts with IN01, clustid is 1)
data <- data %>%
  mutate(clustid = as.numeric(sub("^PE(\\d{2}).*", "\\1", childid)))

# Check if clustid assignment worked
sum(is.na(data$clustid)) # 0 NAs
table(data$clustid)      
n_distinct(data$childid) # 2766, correct! 

## Convert Age from Months to Years
data$agemon %>% summary() # range: 5-307
data$age <- data$agemon / 12
data$age %>% summary() # range in years: 0.42 - 25.6 years old

## Check overall data structure and class
str(data)

## Count individuals by cohort and round 
cohort_count <- data %>%
  count(yc, round, name = "count")
print(cohort_count)
# Older cohort (0): 714; younger cohort (1): 2052


# --- Convert to wide data ---
# Before adding more variables from different rounds, need to convert to wide data
data <- subset(data, select = c(childid, yc, round, inround, dint, deceased, clustid, placeid, region, typesite, childloc, chsex, chlang, chethnic, chldrel, hhsize, agemon, age, marrcohab, marrcohab_age, birth, birth_age, chhrel, chhealth, cladder, wi, hq, sv, cd, enrol, engrade, entype, caredu, dadedu, momedu, careladder))

data_wide <- data %>%
  pivot_wider(
    id_cols = c(childid, yc, chsex, chethnic), # Keep these as ID columns
    names_from = round,
    values_from = -c(childid, yc, chsex, chethnic) # Exclude ID columns from widening
  )

# Marital status data not collected in rounds 1-3; remove empty variables
data_wide <- data_wide %>% select(-c(marrcohab_1, marrcohab_2, marrcohab_3, marrcohab_age_1, marrcohab_age_2, marrcohab_age_3, birth_1, birth_2, birth_3, birth_age_1, birth_age_2, birth_age_3))

data_wide %>% colnames()


## ---- Add exposures - child marriage/cohabitation data ---

# In the data_wide dataset we have marrcohab - a combined child marriage/cohabitation variable constructed across datasets. This is a combined variable, so we cannot distinguish between child marriage and cohabitation.

# To examine rates separately, we need the original marriage and cohabitation data.  

# We also want to add Covid data for extra information on marriage, in case individuals didn't report in previous rounds but only in the Covid phone calls.


## --- Round 4 original data (marriage / cohabitation data separated) ---

## Change format of childid (add PE0) - not the same format across rounds
adjust_childid <- function(id) {
  if (nchar(id) == 6) {
    return(paste0("PE", id))
  } else if (nchar(id) == 5) {
    return(paste0("PE0", id))
  } else {
    return(id)  # Return original ID if not 5 or 6 digits
  }
}
marrcohab_oc_r4 <- marrcohab_oc_r4 %>% 
  mutate(cohort = "OC")
marrcohab_oc_r4 <- marrcohab_oc_r4 %>% mutate(CHILDCODE = sapply(CHILDCODE, adjust_childid))
marrcohab_oc_r4 <- marrcohab_oc_r4 %>% select(CHILDCODE, MRTSTSR4, EVRMARR4, EVRCOHR4, COH1MTHR4, COH1YRR4, MAR1MTHR4, MAR1YRR4)

## Rename relevant variables - specify this is round 4 (wide data)
marrcohab_oc_r4 <- marrcohab_oc_r4 %>%
  rename(
    marital_4 = MRTSTSR4,
    ever_married_4   = EVRMARR4,
    ever_cohab_4     = EVRCOHR4,
    firstcohab_mth_4 = COH1MTHR4,
    firstcohab_yr_4  = COH1YRR4,
    firstmarr_mth_4  = MAR1MTHR4,
    firstmarr_yr_4   = MAR1YRR4
  ) %>%
  mutate(
    marital_4 = case_when(
      marital_4 %in% c("1") ~ "Cohabited",
      marital_4 %in% c("2") ~ "Separated",
      marital_4 %in% c("3", "4", "5") ~ "Married",
      marital_4 == "6" ~ "Single",
      marital_4 %in% c("77", "79") | is.na(marital_4) ~ NA_character_,
      TRUE ~ NA_character_),
    ever_married_4 = case_when(
      ever_married_4 == 0 ~ "No",
      TRUE ~ as.character(ever_married_4)
    ),
    ever_cohab_4 = case_when(
      ever_cohab_4 == 0 ~ "No",
      ever_cohab_4 == 1 ~ "Yes",
      TRUE ~ NA_character_
    ),
    firstcohab_mth_4 = na_if(firstcohab_mth_4, 79),
    firstcohab_mth_4 = na_if(firstcohab_mth_4, 99),
    firstcohab_yr_4  = if_else(firstcohab_yr_4 %in% c(2, 79, 7777, 9999), NA_real_, firstcohab_yr_4) # Remove NA and erroneous values
  ) 

## Merge datasets back to data_wide
data_wide <- left_join(data_wide, marrcohab_oc_r4, by = join_by(childid == CHILDCODE))
data_wide %>% colnames() # Check the new variables are included


## --- Round 5 original data (marriage / cohabitation data separated) ---

## Adjust format of childid
marrcohab_oc_r5 <- marrcohab_oc_r5 %>% 
  mutate(cohort = "OC")
marrcohab_oc_r5 <- marrcohab_oc_r5 %>% mutate(CHILDCODE = sapply(CHILDCODE, adjust_childid))
marrcohab_oc_r5 <- marrcohab_oc_r5 %>% select(CHILDCODE, MRTSTSR5, EVRMARR5, EVRCOHR5, COH1MTHR5, COH1YRR5, MAR1MTHR5, MAR1YRR5)

marrcohab_oc_r5 <- marrcohab_oc_r5 %>%
  rename(
    marital_5 = MRTSTSR5,
    ever_married_5   = EVRMARR5,
    ever_cohab_5     = EVRCOHR5,
    firstcohab_mth_5 = COH1MTHR5,
    firstcohab_yr_5  = COH1YRR5,
    firstmarr_mth_5  = MAR1MTHR5,
    firstmarr_yr_5   = MAR1YRR5
  ) %>%
  mutate(
    marital_5 = case_when(
      marital_5 %in% c("1", "9", "10") ~ "Cohabited",
      marital_5 %in% c("2", "3", "4", "5", "7", "8") ~ "Married",
      marital_5 == "6" ~ "Single",
      marital_5 %in% c("77", "79", "88") | is.na(marital_5) ~ NA_character_,
      TRUE ~ NA_character_),
    ever_married_5 = case_when(
      ever_married_5 == 0 ~ "No",
      ever_married_5 == 1 ~ "Yes",
      TRUE ~ NA_character_
    ),
    ever_cohab_5 = case_when(
      ever_cohab_5 == 0 ~ "No",
      ever_cohab_5 == 1 ~ "Yes",
      TRUE ~ NA_character_
    ),
    firstcohab_mth_5 = na_if(firstcohab_mth_5, 79),
    firstcohab_mth_5 = na_if(firstcohab_mth_5, 99),
    firstcohab_yr_5  = if_else(firstcohab_yr_5 %in% c(3, 13, 7777, 9999), NA_real_, firstcohab_yr_5) # Remove NA and erroneous values 
  )  

## Merge datasets back to data_wide
data_wide <- left_join(data_wide, marrcohab_oc_r5, by = join_by(childid == CHILDCODE))
data_wide %>% colnames() # Check the new variables are included


## --- Covid child marriage data ---
# Marital status collected in Covid phone call 4 (2021)

## Add cohort identifier columns to marriage datasets
marr_yc <- marr_yc %>%
  mutate(cohort = "YC")
marr_oc <- marr_oc %>%
  mutate(cohort = "OC")

## Combine datasets by rows
marr_covid <- bind_rows(marr_yc, marr_oc)
marr_covid <- marr_covid %>% select(-cohort, -INFCVNOCOV4, -INFCVYSECOV4, -INFCVYMECOV4, -INFCVNKCOV4, -INFCVRTACOV4, -testinfcov4, -vacoffercov4, -cmptrcov4, -intcmptrcov4)
marr_covid %>% colnames()
marr_covid <- marr_covid %>% rename(dint_6 = DINTCOV4, clustid_6 = CLUSTIDCOV4, region_6 = REGIONCOV4, typesite_6 = typesitecov4)

## Rename region_6 variables in line with 'data' dataset
marr_covid <- marr_covid %>% mutate(region_6 = case_when(
  region_6 == "Coast" ~ 31,
  region_6 == "Mountain"  ~ 32,
  region_6 == "Jungle" ~ 33, 
  region_6 %in% c("1", "99") | is.na(region_6) ~ NA_real_,
  TRUE ~ as.integer(region_6)
))
marr_covid$region_6 %>% table()

## Adjust childid - format not the same across rounds (use previously defined function)
marr_covid <- marr_covid %>% mutate(CHILDCODE = sapply(CHILDCODE, adjust_childid))

## Merge datasets back to data_wide 
data_wide <- left_join(data_wide, marr_covid, by = join_by(childid == CHILDCODE))
data_wide$childid %>% head() # Correct format for childid
data_wide %>% colnames() 

## Rename such that all variables from Covid round 6 end with '_ [roundno]' -- round 6
data_wide <- data_wide %>% rename(marstatcov_6 = marstatcov4, mthmarcov_6 = mthmarcov4, yrmarcov_6 = yrmarcov4, newhhmemcov_6 = newhhmemcov4, chawaycov_6 = chawaycov4, numchawaycov_6 = numchawaycov4, preg1cov_6 = preg1cov4, preg2cov_6 = preg2cov4, preg3cov_6 = preg3cov4)

## --- Add outcomes - wellbeing data ---

### --- Covid wellbeing data ---

# Wellbeing - cladder 

## Replace missing values with NA 
cladder_oc <- cladder_oc %>%
  select(CHILDCODE, cladder_6 = LADDRCOV2) %>%
  mutate(
    cladder_6 = case_when(
      cladder_6 %in% c(77, 79) ~ NA_integer_,
      TRUE ~ cladder_6
    ))
cladder_yc <- cladder_yc %>%
  select(CHILDCODE, cladder_6 = LADDRCOV2) %>%
  mutate(
    cladder_6 = case_when(
      cladder_6 %in% c(77, 79) ~ NA_integer_,
      TRUE ~ cladder_6
    ))


## --- Covid mental health data ---
# Add Covid GAD7 and PHQ8 data as extra information for imputation 

## Convert GAD-7 and PHQ-8 Data to Wide Format
gad7_oc <- gad7_oc %>% pivot_wider(id_cols = CHILDCODE, names_from = GAD7ID, values_from = c(ANXTYCOV2, OFTANXCOV2))
phq8_oc <- phq8_oc %>% pivot_wider(id_cols = CHILDCODE, names_from = PHQ8ID, values_from = c(PHQ8COV2, OFTNPHQCOV2))
gad7_yc <- gad7_yc %>% pivot_wider(id_cols = CHILDCODE, names_from = GAD7ID, values_from = c(ANXTYCOV2, OFTANXCOV2))
phq8_yc <- phq8_yc %>% pivot_wider(id_cols = CHILDCODE, names_from = PHQ8ID, values_from = c(PHQ8COV2, OFTNPHQCOV2))

## Merge datasets 
mh_oc <- merge(gad7_oc, phq8_oc, by = "CHILDCODE")
mh_oc <- merge(mh_oc, cladder_oc, by = "CHILDCODE")
mh_yc <- merge(gad7_yc, phq8_yc, by = "CHILDCODE")
mh_yc <- merge(mh_yc, cladder_yc, by = "CHILDCODE")
mh_covid <- rbind(mh_yc, mh_oc)
mh_covid <- mh_covid %>% mutate(CHILDCODE = sapply(CHILDCODE, adjust_childid))
data_wide <- left_join(data_wide, mh_covid, by = join_by(childid == CHILDCODE)) # Merge back to data_wide

## Combine gad1 to gad7 variables
# Create a function to combine occurrence and frequency
combine_vars <- function(occur, freq) {
  case_when(
    occur == 0 ~ 0,      # No occurrence
    occur == 1 ~ freq,   # Frequency when occurred
    TRUE ~ NA_real_      # Handle missing values (if any)
  )
}
# Apply the function to the GAD and PHQ variables
data_wide <- data_wide %>%
  mutate(
    gad1_6 = combine_vars(ANXTYCOV2_1, OFTANXCOV2_1),
    gad2_6 = combine_vars(ANXTYCOV2_2, OFTANXCOV2_2),
    gad3_6 = combine_vars(ANXTYCOV2_3, OFTANXCOV2_3),
    gad4_6 = combine_vars(ANXTYCOV2_4, OFTANXCOV2_4),
    gad5_6 = combine_vars(ANXTYCOV2_5, OFTANXCOV2_5),
    gad6_6 = combine_vars(ANXTYCOV2_6, OFTANXCOV2_6),
    gad7_6 = combine_vars(ANXTYCOV2_7, OFTANXCOV2_7),
    phq1_6 = combine_vars(PHQ8COV2_1, OFTNPHQCOV2_1),
    phq2_6 = combine_vars(PHQ8COV2_2, OFTNPHQCOV2_2),
    phq3_6 = combine_vars(PHQ8COV2_3, OFTNPHQCOV2_3),
    phq4_6 = combine_vars(PHQ8COV2_4, OFTNPHQCOV2_4),
    phq5_6 = combine_vars(PHQ8COV2_5, OFTNPHQCOV2_5),
    phq6_6 = combine_vars(PHQ8COV2_6, OFTNPHQCOV2_6),
    phq7_6 = combine_vars(PHQ8COV2_7, OFTNPHQCOV2_7),
    phq8_6 = combine_vars(PHQ8COV2_8, OFTNPHQCOV2_8)
  )

data_wide <- data_wide %>%
  select(-starts_with("ANXTYCOV2"), -starts_with("OFTANXCOV2"), 
         -starts_with("PHQ8COV2"), -starts_with("OFTNPHQCOV2"))
data_wide %>% colnames() # Check the variables are named correctly


## --- Emotional SDQ data ---
# Add rounds 3-5 SDQ data as extra information for imputation 

## Filter older cohort - data only available in OC
data_oc <- data_wide %>% filter(yc == 0)
data_oc %>% head()
data_oc %>% colnames()

## Select emotional SDQ items
r3_sdq <- subset(r3_sdq, select = c(childid, headacr3, wrylotr3, unhppyr3, nrvsitr3, mnyferr3))
r4_sdq <- subset(r4_sdq, select = c(CHILDCODE, HEADACR4, WRYLOTR4, UNHPPYR4, NRVSITR4, MNYFERR4))
r4_sdq$CHILDCODE <- r4_sdq$CHILDCODE %>% as.character()
r5_sdq <- subset(r5_sdq, select = c(CHILDCODE, HEADACR5, WRYLOTR5, UNHPPYR5, NRVSITR5, MNYFERR5))
r5_sdq$CHILDCODE <- r5_sdq$CHILDCODE %>% as.character()

## Adjust to correct chlidid format 
r4_sdq <- r4_sdq %>% mutate(CHILDCODE = sapply(CHILDCODE, adjust_childid))
r5_sdq <- r5_sdq %>% mutate(CHILDCODE = sapply(CHILDCODE, adjust_childid))

## Merge into one dataset
data_oc <- left_join(data_oc, r3_sdq, by = join_by(childid))
data_oc <- left_join(data_oc, r4_sdq, by = join_by(childid == CHILDCODE))
data_oc <- left_join(data_oc, r5_sdq, by = join_by(childid == CHILDCODE))
data_oc %>% colnames()
data_oc %>% nrow() # 714 rows, correct!

## Rename SDQ items
data_oc <- data_oc %>% rename(headache_r3 = headacr3, worry_r3 = wrylotr3, unhappy_r3 = unhppyr3, nervous_r3 = nrvsitr3, fears_r3 = mnyferr3)
data_oc <- data_oc %>% rename(headache_r4 = HEADACR4, worry_r4 = WRYLOTR4, unhappy_r4 = UNHPPYR4, nervous_r4 = NRVSITR4, fears_r4 = MNYFERR4)
data_oc <- data_oc %>% rename(headache_r5 = HEADACR5, worry_r5 = WRYLOTR5, unhappy_r5 = UNHPPYR5, nervous_r5 = NRVSITR5, fears_r5 = MNYFERR5)

# Round 3-5 labels 
# 1 - certainly true for you, 2 - a little true for you, 3 - not true for you, 77 - nk, 79 - refused to answer, 88 - n/a, 99 - missing
data_oc$headache_r3 %>% unique()
data_oc$headache_r4 %>% unique()
data_oc$headache_r5 %>% unique()

# SDQ sub-scale is normally 0 - not true, 1 - somewhat true, 2 - certainly true -> 10 full score. We need to flip and rescale all the rounds to this scale! 
# 3->0, 2->1, 1->2

## First, re-format NAs 
# Define NA replacement function
replace_na_values <- function(score) {
  case_when(score %in% c(77, 79, 88, 99) ~ NA_integer_, TRUE ~ score)
}

# Apply NA replacement for rounds
data_oc <- data_oc %>% 
  mutate(across(ends_with("_r3") | ends_with("_r4") | ends_with("_r5"), replace_na_values))

# Double check 
data_oc$headache_r3 %>% unique()
data_oc$headache_r4 %>% unique()
data_oc$headache_r5 %>% unique()

## Then, rescale for rounds 3-5 to the normal 0 - 2 scale 
# Define rescale functions
rescale <- function(score) {
  case_when(score == 3 ~ 0, score == 2 ~ 1, score == 1 ~ 2, TRUE ~ NA_real_)
}

# Apply rescaling functions
data_oc <- data_oc %>% 
  mutate(across(ends_with("_r3") | ends_with("_r4") | ends_with("_r5"), rescale))

# Check summaries
data_oc$headache_r3 %>% summary()
data_oc$worry_r4 %>% summary()
data_oc$unhappy_r5 %>% summary()

## Now, check missingness in emotion SDQ data for rounds 3-5 to determine method of imputation (use mean to impute)

# Round 3 
data_oc <- data_oc %>% mutate(
  nonmiss_r3 = rowSums(!is.na(data_oc[, c("headache_r3", "worry_r3", "unhappy_r3", "nervous_r3", "fears_r3")])))
data_oc$nonmiss_r3 %>% summary()

sum(data_oc$nonmiss_r3 == 3) # 6
sum(data_oc$nonmiss_r3 == 2) # 1
sum(data_oc$nonmiss_r3 == 0) # 80

# Round 4
data_oc <- data_oc %>% mutate(
  nonmiss_r4 = rowSums(!is.na(data_oc[, c("headache_r4", "worry_r4", "unhappy_r4", "nervous_r4", "fears_r4")])))
data_oc$nonmiss_r4 %>% summary()

sum(data_oc$nonmiss_r4 == 3) # 5
sum(data_oc$nonmiss_r4 == 2) # 1
sum(data_oc$nonmiss_r4 == 0) # 119

# Round 5
data_oc <- data_oc %>% mutate(
  nonmiss_r5 = rowSums(!is.na(data_oc[, c("headache_r5", "worry_r5", "unhappy_r5", "nervous_r5", "fears_r5")])))
data_oc$nonmiss_r5 %>% summary()

sum(data_oc$nonmiss_r5 == 3) # 4
sum(data_oc$nonmiss_r5 == 2) # 1
sum(data_oc$nonmiss_r5 == 0) # 161

# If we choose 3 as the cutoff, as long as there are 3 or more answers we will use the average to impute the other 2. If fewer than 3 answers we will drop the participant.

## Function to impute missing data using mean of nonmissing values - if there are >2 nonmissing
impute_emo <- function(data, round) {
  data %>%
    rowwise() %>%
    mutate(!!sym(paste0("emo_r", round)) := ifelse(
      !!sym(paste0("nonmiss_r", round)) > 2,
      mean(c_across(all_of(paste0(c("headache", "worry", "unhappy", "nervous", "fears"), "_r", round))), na.rm = TRUE) * 5,
      NA
    ) %>% round()) %>%
    ungroup()
}

# Apply function for rounds 3, 4, and 5
for (r in 3:5) {
  data_oc <- impute_emo(data_oc, r)
}

## Check values of emotional SDQ scores
data_oc$emo_r3 %>% summary()
data_oc$emo_r4 %>% summary()
data_oc$emo_r5 %>% summary()

data_oc$emo_r3 %>% hist()
data_oc$emo_r4 %>% hist()
data_oc$emo_r5 %>% hist()

data_oc %>% colnames()

# Now all the emotional SDQ subscale has scores 0-10! (Each item is scored 0-2, with 5 items within emotional subscale)

## Merge dataset back to main data_wide
data_wide <- dplyr::left_join(data_wide, data_oc %>% dplyr::select(childid, headache_r3, worry_r3, unhappy_r3, nervous_r3, fears_r3, headache_r4, worry_r4, unhappy_r4, nervous_r4, fears_r4, headache_r5, worry_r5, unhappy_r5, nervous_r5, fears_r5, emo_r3, emo_r4, emo_r5), by = "childid")

## Double check new mental health variables are present
data_wide %>% colnames() 

# Change the format to align with other variables i.e. variablename_roundno, e.g. emo_4 
data_wide <- data_wide %>% 
  rename_with(.cols = starts_with("headache_r"), ~ gsub("_r", "_", .)) %>%
  rename_with(.cols = starts_with("worry_r"), ~ gsub("_r", "_", .)) %>%
  rename_with(.cols = starts_with("unhappy_r"), ~ gsub("_r", "_", .)) %>%
  rename_with(.cols = starts_with("nervous_r"), ~ gsub("_r", "_", .)) %>%
  rename_with(.cols = starts_with("fears_r"), ~ gsub("_r", "_", .)) %>%
  rename_with(.cols = starts_with("emo_r"), ~ gsub("_r", "_", .))
data_wide %>% colnames() # done!



## --- Stack cohorts according to age ---
# oc round 1 = yc round 3 (age 8)
# oc round 2 = yc round 4 (age 12)
# oc round 3 = yc round 5 (age 15)
# oc round 4 = yc round 6 (age 19)
# oc round 5 = yc round 7 (age 22)

## Convert back to long data
data_long <- data_wide %>%
  pivot_longer(
    cols = -c(childid, yc, chsex, chethnic),  # Exclude identifier columns
    names_to = c(".value", "rounds"),
    names_pattern = "(.+)_(\\d+)"
  )

data_long %>% colnames()
data_long <- data_long %>% select(-round) # Remove original
data_long <- data_long %>% rename(round = rounds) # Rename

data_long <- data_long %>% 
  mutate(equivalent_round = case_when(
    yc == 1 & round == 1 ~ "Age 1",
    yc == 1 & round == 2 ~ "Age 5",
    yc == 0 & round == 1 ~ "Age 8", 
    yc == 1 & round == 3 ~ "Age 8", 
    yc == 0 & round == 2 ~ "Age 12",
    yc == 1 & round == 4 ~ "Age 12",
    yc == 0 & round == 3 ~ "Age 15", 
    yc == 1 & round == 5 ~ "Age 15",
    yc == 0 & round == 4 ~ "Age 19",
    yc == 1 & round == 6 ~ "Age 19",
    yc == 0 & round == 5 ~ "Age 22",
    yc == 0 & round == 6 ~ "Age 26",
    TRUE ~ NA_character_ # if not in defined rounds
  )) %>% 
  filter(!is.na(equivalent_round))

data_long$equivalent_round %>% unique()
data_long$equivalent_round <- as.factor(data_long$equivalent_round)
data_long$equivalent_round <- factor(data_long$equivalent_round, levels = c("Age 1", "Age 5", "Age 8", "Age 12", "Age 15", "Age 19", "Age 22", "Age 26"))

## Check stacked data (long form)
data_long$childid %>% n_distinct() # 2766 


## --- Add confounders (age 12) ---

# Birth order 
# Debt
# Parental aspirations 
# Child aspirations 
# Paid work 

## Place in birth order: YC - bornbef; OC - order
birth_combined <- bind_rows(birth_yc, birth_oc)
data_long <- dplyr::left_join(data_long, birth_combined, by = "childid")
data_long$birthorder %>% unique()

## Debt burden (at age 12): YC - SRSDBTR4; OC - debt
debt_yc <- debt_yc %>% 
  select(childid = CHILDCODE, debt = SRSDBTR4) %>% 
  mutate(childid = sapply(childid, adjust_childid))
debt_oc <- debt_oc %>% select(childid, debt)
debt_combined <- bind_rows(debt_yc, debt_oc)

# Merge debt only for age 12 
data_long <- data_long %>% 
  dplyr::left_join(debt_combined, by = "childid") %>% 
  mutate(debt = ifelse(equivalent_round == "Age 12", debt, NA))

data_long$debt %>% unique()
data_long <- data_long %>% 
  mutate(debt = factor(debt, levels = c(0,1), labels = c("No", "Yes")))


## Parental aspirations for daughter (when aged 12): YC - GRDLKER4; OC - GRADLIKE
parentasp_yc <- parentasp_yc %>% 
  select(childid = CHILDCODE, parentasp = GRDLKER4) %>% 
  mutate(childid = sapply(childid, adjust_childid))

parentasp_yc <- parentasp_yc %>% 
  mutate(parentasp = case_when(
    parentasp == 0 ~ "None",
    parentasp %in% 1:6 ~ "Primary",
    parentasp %in% 7:11 ~ "Secondary", 
    parentasp == 14 ~ "Incomplete university",
    parentasp == 16 ~ "Complete university",
    parentasp == 17 ~ "Adult literacy", 
    parentasp == 18 ~ "Others",
    parentasp == 19 ~ "Postgraduate",
    parentasp == 22 ~ "Complete technical",
    parentasp %in% c(77, 79, 88, 99) ~ NA_character_,
    TRUE ~ NA_character_
  )) %>% 
  mutate(parentasp = factor(parentasp, levels = c("None", "Primary", "Secondary", "Complete technical", "Incomplete university", "Complete university", "Adult literacy", "Others", "Postgraduate")))

parentasp_oc <- parentasp_oc %>% 
  select(childid, parentasp = gradlike)

parentasp_oc <- parentasp_oc %>% 
  mutate(parentasp = case_when(
    parentasp == 0 ~ "None",
    parentasp == 13 ~ "Incomplete technical",
    parentasp == 14 ~ "Complete technical",
    parentasp == 15 ~ "Incomplete university",
    parentasp == 16 ~ "Complete university",
    parentasp == 17 ~ "Adult literacy",
    parentasp == 18 ~ "Others",
    parentasp %in% c(77, 79, 88, 99) ~ NA_character_,
    TRUE ~ NA_character_
  )) %>% 
  mutate(parentasp = factor(parentasp, levels = c("None", "Incomplete technical", "Complete technical", "Incomplete university", "Complete university", "Adult literacy", "Others")))

parentasp_combined <- bind_rows(parentasp_yc, parentasp_oc)

# Merge parental aspirations only for age 12 
data_long <- data_long %>% 
  left_join(parentasp_combined, by = "childid") %>% 
  mutate(parentasp = if_else(equivalent_round == "Age 12", parentasp, NA_character_))

data_long %>% colnames()


## Paid work at age 12 - YC: ACTPAYR4; OC - chldwork
## Child's aspirations for their education at age 12 - YC: CLDSTDR4; OC - cgrdlike
paidasp_yc <- paidasp_yc %>% 
  select(childid = CHILDCODE, paid = ACTPAYR4, aspirations = CLDSTDR4) %>% 
  mutate(childid = sapply(childid, adjust_childid))
# 0 - None, 1-6 - Pri1-6, 7-11 - Sec1-5, 14 - Sup. Univ not completed, 16 - Univ completed (inc escuela de oficiales), 17 - Adult literacy programme, 18 - Others, 19 - Postgraduate, 22 - Cent. T, 77, 79, 88

# Recode as cohorts not coded the same 
paidasp_yc <- paidasp_yc %>% 
  mutate(aspirations = case_when(
    aspirations == 0 ~ "None",
    aspirations %in% 1:6 ~ "Primary",
    aspirations %in% 7:11 ~ "Secondary", 
    aspirations == 14 ~ "Incomplete university",
    aspirations == 16 ~ "Complete university",
    aspirations == 17 ~ "Adult literacy", 
    aspirations == 18 ~ "Others",
    aspirations == 19 ~ "Postgraduate",
    aspirations == 22 ~ "Complete technical",
    aspirations %in% c(77, 79, 88, 99) ~ NA_character_,
    TRUE ~ NA_character_
  )) %>% 
  mutate(aspirations = factor(aspirations, levels = c("None", "Primary", "Secondary", "Complete technical", "Incomplete university", "Complete university", "Adult literacy", "Others", "Postgraduate")))

paidasp_oc <- paidasp_oc %>% 
  select(childid = childid, paid = chldwork, aspirations = cgrdlike) # remove 77, 79, 88, 99
# 0 - None, 13 - Incomplete technical college, 14 - Complete technical college, 15 - Incomplete university, 16 - Complete university, 17 - Adult literacy programme, 18 - Other, 77, 79, 88, 99

paidasp_oc <- paidasp_oc %>% 
  mutate(aspirations = case_when(
    aspirations == 0 ~ "None",
    aspirations == 13 ~ "Incomplete technical",
    aspirations == 14 ~ "Complete technical",
    aspirations == 15 ~ "Incomplete university",
    aspirations == 16 ~ "Complete university",
    aspirations == 17 ~ "Adult literacy",
    aspirations == 18 ~ "Others",
    aspirations %in% c(77, 79, 88, 99) ~ NA_character_,
    TRUE ~ NA_character_
  )) %>% 
  mutate(aspirations = factor(aspirations, levels = c("None", "Incomplete technical", "Complete technical", "Incomplete university", "Complete university", "Adult literacy", "Others")))

paidasp_combined <- bind_rows(paidasp_yc, paidasp_oc)

# Merge paid work and aspirations only for age 12 
data_long <- data_long %>% 
  left_join(paidasp_combined, by = "childid") %>% 
  mutate(
    paid = ifelse(equivalent_round == "Age 12", paid, NA), 
    aspirations = if_else(equivalent_round == "Age 12", aspirations, NA_character_))

## Finalise and save data_wide 
data_long %>% colnames()
# data_long %>% view()

data_long$paid %>% unique()
data_long <- data_long %>% 
  mutate(paid = case_when(
    paid == 1 ~ "Yes",
    paid == 0 ~ "No",
    paid %in% c(77, -77) ~ NA_character_
  )) %>% 
  mutate(paid = factor(paid, levels = c("Yes", "No")))
data_long$paid %>% table() # 202 individuals have had paid work at age 12 (inc boys) [paid work only present for age 12]

data_long %>% colnames() # now added 4 new confounders: birth order, debt when child is 12, parent aspirations when child is 12 and child's own aspirations, and paid work when child is 12



# ----------------- CREATE MARRIAGE VARIABLES (EXPOSURES) ---------------------------

## --- Create Covid marital status variables ---

## Check and replace NAs
data_long <- data_long %>% replace_with_na(replace = list(marstatcov = 79))
data_long <- data_long %>% replace_with_na(replace = list(preg1cov = 77))
data_long <- data_long %>% replace_with_na(replace = list(preg2cov = c(77, 88)))
data_long <- data_long %>% replace_with_na(replace = list(preg3cov = c(77, 79, 80)))
data_long <- data_long %>% replace_with_na(replace = list(entype = 88))

## With Covid round 6 data, create variables ever_married_covid and ever_cohabited_covid

# Check values
data_long$marstatcov %>% unique() # NA  0  2  1  5  6  8  3  7
data_long$marstatcov %>% unique() # NA  0  2  1  5  6  8  3  7

# Group together values - ever married: 1, 3, 4, 6, 7; ever cohabited: 2, 5, 8
data_long <- data_long %>% 
  mutate(ever_married_covid = case_when (
    marstatcov %in% c(1, 3, 4, 6, 7) ~ "Yes",
    TRUE ~ "No"
  ))
data_long$ever_married_covid %>% table()

data_long <- data_long %>% 
  mutate(ever_cohabited_covid = case_when (
    marstatcov %in% c(2, 5, 8) ~ "Yes",
    TRUE ~ "No"
  ))
data_long$ever_cohabited_covid %>% table()


## --- Create age of marriage/cohabitation variables ---

### --- Rounds 4&5 data - age of marriage/ cohabitation variables ---
# Current variables: month/date of first marriage & first cohabitation 

# To calculate age of marriage, need to first calculate date of birth
data_long$dint %>% class()
data_long$dint <- as.Date(data_long$dint, "%m/%d/%Y")
data_long$dint %>% head()

# Calculate DOB from available rounds
dob_data <- data_long %>%
  group_by(childid) %>%
  arrange(round) %>%  # Ensure processing in order
  summarise(DOB = first(na.omit(dint - months(agemon)))) %>%
  ungroup()

data_long <- data_long %>%
  left_join(dob_data, by = "childid")

sum(is.na(dob_data$DOB)) # 0 NAs 

# Make sure month of marriage only has valid months
unique(data_long$mthmarcov)

# Create age of marriage/cohabitation variables
data_long <- data_long %>%
  mutate(
    ## ---- Marriage date 
    firstmarr_date = case_when(
      !is.na(firstmarr_mth) & firstmarr_mth %in% 1:12 & !is.na(firstmarr_yr) ~
        make_date(firstmarr_yr, firstmarr_mth, 1),
      !is.na(firstmarr_yr) ~
        make_date(firstmarr_yr, 1, 1),
      TRUE ~ NA_Date_
    ),
    
    ## ---- Cohabitation date 
    firstcohab_date = case_when(
      !is.na(firstcohab_mth) & firstcohab_mth %in% 1:12 & !is.na(firstcohab_yr) ~
        make_date(firstcohab_yr, firstcohab_mth, 1),
      !is.na(firstcohab_yr) ~
        make_date(firstcohab_yr, 1, 1),
      TRUE ~ NA_Date_
    ),
    
    ## ---- Marriage age 
    firstmarr_age = case_when(
      !is.na(firstmarr_date) & !is.na(DOB) ~
        interval(DOB, firstmarr_date) / years(1),
      TRUE ~ NA_real_
    ),
    
    ## ---- Cohabitation age 
    firstcohab_age = case_when(
      !is.na(firstcohab_date) & !is.na(DOB) ~
        interval(DOB, firstcohab_date) / years(1),
      TRUE ~ NA_real_
    ),
    
    ## ---- Integer versions 
    firstmarr_age_int   = if_else(is.na(firstmarr_age), NA_real_, floor(firstmarr_age)),
    firstcohab_age_int  = if_else(is.na(firstcohab_age), NA_real_, floor(firstcohab_age))
  )

# Check 
data_long$firstmarr_age_int %>% table() # 15-21
data_long$firstcohab_age_int %>% table() # 11-22
data_long$marital %>% table() # 19 were separated - not sure whether they were cohabited/married before



### --- Covid phone call data - age of marriage and cohabitation variables ---

## Create age of marriage variable
data_long <- data_long %>%
  mutate(
    # Create a valid date_of_marriage using month and year when possible
    date_of_marriage = case_when(
      !is.na(mthmarcov) & mthmarcov %in% 1:12 & !is.na(yrmarcov) ~ make_date(yrmarcov, mthmarcov, 1),
      !is.na(yrmarcov) ~ make_date(yrmarcov, 1, 1),  # Default to January 1st of the year if month is invalid
      TRUE ~ NA_Date_  # Set to NA if year is missing
    ),
    # Calculate age_at_marriage with full date or only using year if month is missing
    age_at_marriage = case_when(
      !is.na(mthmarcov) & mthmarcov %in% 1:12 & !is.na(yrmarcov) & !is.na(DOB) ~ interval(DOB, date_of_marriage) / years(1),
      is.na(mthmarcov) & !is.na(yrmarcov) & !is.na(DOB) ~ yrmarcov - year(DOB),  # Calculate using only the year
      TRUE ~ NA_real_  # Set to NA if any required values are missing
    ),
    # Round down to get the integer part of the age
    age_at_marriage_int = ifelse(is.na(age_at_marriage), NA, floor(age_at_marriage))
  )

# Check data structure and variables
# data_long %>% view()
# Very few people actually have age of marriage 



## --- Create child marriage status variables ---


## Create variables for ever_marriedcohab_combined (ever married/cohabited) and ever_child_marriage (ever child married)
data_long <- data_long %>%
  mutate(
    ever_marriedcohab_combined = case_when(
      ever_married == "Yes" | ever_cohab == "Yes" ~ "Yes",
      ever_married == "No" & ever_cohab == "No"   ~ "No",
      ever_married == "No" & is.na(ever_cohab)    ~ "No",
      is.na(ever_married) & ever_cohab == "No"    ~ "No",
      TRUE ~ NA_character_
    )
  )      

data_long <- data_long %>% 
  mutate(ever_child_marriage = case_when(
    ever_marriedcohab_combined == "Yes" & age_at_marriage_int < 18 ~ "Child Marriage", 
    ever_marriedcohab_combined == "Yes" & age_at_marriage_int >= 18 ~ "Non-Child Marriage",
    ever_marriedcohab_combined == "No" ~ "Not Married",
    TRUE ~ NA_character_ # for other cases e.g. married but missing age of marriage
  ))

## Check missingness
sum(is.na(data_long$ever_child_marriage)) # 343
na_cases <- data_long %>% filter(is.na(ever_child_marriage))
na_cases <- na_cases %>% summarise(num_children = n_distinct(childid))
na_cases # 273 children are married but no age reported


## Create new variable CM for binary Child marriage vs non-Child marriage group (combining non-child marriage and not married groups into non-CM) -- this will be the variable to be used in all analyses!!
# For Peru analyses, marriage and cohabitation will be combined
data_long <- data_long %>%
  group_by(childid) %>%
  mutate(
    CM = case_when(
      any(is.na(ever_child_marriage)) ~ NA_character_,  # Preserve NA if any row has NA in `ever_child_marriage`
      any(ever_child_marriage == "Child Marriage") ~ "Child bride",  # Prioritise "Child bride" if any row has "Child Marriage"
      TRUE ~ "Non-child-bride"  # Otherwise, label as "Non-child-bride"
    )
  ) %>%
  ungroup()

# Convert CM to a factor 
data_long <- data_long %>%
  mutate(CM = factor(CM, levels = c("Non-child-bride", "Child bride")))

# Check unique values of equivalent_round
data_long$equivalent_round %>% unique()
data_long %>% colnames()

data_long$CM %>% table()
data_long %>% filter(CM == "Child bride") %>% summarise(n_child_bride = n_distinct(childid)) # 65 are child brides (inc boys)




# ----------------- DESCRIPTIVES STATISTICS ---------------------------

# Create document showing rates of of child marriage (for boys and girls)

descriptive_table <- data_wide %>% 
  group_by(yc, chsex) %>% 
  summarise(
    # Total n individuals in each group 
    Total_Individuals = n(),
    # Counts for CM vs non CM 
    Child_Married_Count = sum(CM == "Child bride", na.rm = TRUE),
    Non_Child_Married_Count = sum(CM == "Non-child-bride", na.rm = TRUE),
    # Percentages
    Child_Married_Percentage = round(100 * Child_Married_Count / Total_Individuals, 1),
    Non_Child_Married_Percentage = round(100 * Child_Married_Count / Total_Individuals, 1),
    # Average & SD of age of marriage for CM individuals
    Avg_Age_Marriage = round(mean(age_married_combined[CM == "Child bride"], na.rm = TRUE), 2),
    SD_Age_Marriage = round(sd(age_married_combined[CM=="Child bride"], na.rm = TRUE), 2),
    .groups = "drop"
  ) %>% 
  mutate(
    Cohort = ifelse(yc == 0, "1994-Cohort", "2001-Cohort"),
    Sex = ifelse(chsex == 2, "Girls", "Boys")
  ) %>% 
  select(Cohort, Sex, Total_Individuals, Child_Married_Count, Child_Married_Percentage, Non_Child_Married_Count, Non_Child_Married_Percentage, Avg_Age_Marriage, SD_Age_Marriage)
descriptive_table

# Subtotal for each cohort
cohort_totals <- descriptive_table %>%
  group_by(Cohort) %>%
  summarise(
    Sex = "TOTAL",
    Total_Individuals = sum(Total_Individuals, na.rm = TRUE),
    Child_Married_Count = sum(Child_Married_Count, na.rm = TRUE),
    Child_Married_Percentage = round((sum(Child_Married_Count, na.rm = TRUE) / sum(Total_Individuals, na.rm = TRUE)) * 100, 1),
    Non_Child_Married_Count = sum(Non_Child_Married_Count, na.rm = TRUE),
    Non_Child_Married_Percentage = round((sum(Non_Child_Married_Count, na.rm = TRUE) / sum(Total_Individuals, na.rm = TRUE)) * 100, 1),
    Avg_Age_Marriage = NA,  # Not meaningful to calculate for totals
    SD_Age_Marriage = NA    # Not meaningful to calculate for totals
  )

# Subtotal for each sex
sex_totals <- descriptive_table %>%
  group_by(Sex) %>%
  summarise(
    Cohort = "TOTAL",
    Total_Individuals = sum(Total_Individuals, na.rm = TRUE),
    Child_Married_Count = sum(Child_Married_Count, na.rm = TRUE),
    Child_Married_Percentage = round((sum(Child_Married_Count, na.rm = TRUE) / sum(Total_Individuals, na.rm = TRUE)) * 100, 1),
    Non_Child_Married_Count = sum(Non_Child_Married_Count, na.rm = TRUE),
    Non_Child_Married_Percentage = round((sum(Non_Child_Married_Count, na.rm = TRUE) / sum(Total_Individuals, na.rm = TRUE)) * 100, 1),
    Avg_Age_Marriage = NA,
    SD_Age_Marriage = NA
  )

# Combine all parts of the table
final_table <- bind_rows(
  descriptive_table,  # Original descriptive table with grouping
  cohort_totals,      # Cohort-wise totals
  sex_totals          # Sex-wise totals
)


# Create and adjust the flextable
descriptive_flextable <- final_table %>%
  flextable() %>%
  set_caption("Descriptive Statistics: Child Marriage by Cohort and Sex") %>%
  width(j = 1:ncol(final_table), width = 1) %>%  # Reduce column widths
  fontsize(size = 8, part = "all") %>%  # Reduce font size
  bold(i = (nrow(final_table) - 1):nrow(final_table), bold = TRUE, part = "body") %>%  # Bold last two rows
  align(align = "center", part = "all") %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#4F81BD") %>%
  align(align = "center", part = "header") %>%
  set_table_properties(width = 0.9)  # Scale table width to fit page

# Create a Word document in landscape mode with minimal margins
doc <- read_docx() %>%
  body_add_par("Descriptive Statistics: Child Marriage by Cohort and Sex", style = "heading 1") %>%
  body_add_flextable(descriptive_flextable) %>%
  body_end_section_landscape()  # Ensure the document is in landscape orientation

# Export the document
print(doc, target = "Descriptive_Statistics_Pre_Imputation.docx")

# Most child brides are girls. There are very few boys married as children in this sample.
# Our sample is thus a girls only sample.
# There are more child brides in the 1994-cohort than 2001-cohort. 

# For Peru, there are fewer child brides but in general those who are married are married younger - average age of marriage for 1994-cohort is 16. 



# ----------------- DESCRIPTIVES PLOTS GIRLS ONLY -------------------------------

## For wellbeing (outcome) - girls only

data_long$cladder %>% summary()


## Plot wellbeing scores across ages 
average_cladder_age <- data_long %>% 
  filter(chsex == 2) %>% 
  group_by(age) %>%
  summarise(mean_cladder = mean(cladder, na.rm = TRUE))

ggplot(average_cladder_age, aes(x = age, y = mean_cladder, group = 1)) +
  geom_smooth(method = "loess", linewidth = 1) +
  geom_point() +
  labs(title = "Average Wellbeing Across Ages (Girls)",
       x = "Age",
       y = "Average Wellbeing Score") +
  scale_x_continuous(breaks = c(8, 12, 15, 19, 22), limits = c(8,22)) +
  theme_minimal()


## Compare 2001- and 1994-cohorts

# Prepare data by calculating mean cladder score for each cohort and age
cohort_cladder <- data_long %>%
  filter(chsex == 2) %>% 
  group_by(equivalent_round, yc) %>%
  summarise(mean_cladder = mean(cladder, na.rm = TRUE),
            se_cladder = sd(cladder, na.rm = TRUE) / sqrt(n())) %>%
  mutate(Cohort = ifelse(yc == 0, "1994-Cohort", "2001-Cohort"))

# Plot cladder score separated by cohort 
ggplot(cohort_cladder, aes(x = equivalent_round, y = mean_cladder, color = Cohort, group = Cohort)) +
  geom_line(size = 1.2) +                      # Line connecting means
  geom_point(size = 3) +                       # Points at each mean
  geom_errorbar(aes(ymin = mean_cladder - se_cladder, ymax = mean_cladder + se_cladder), width = 0.1) +  # Error bars
  labs(title = "Average Cladder Score Across Ages by Cohort (Girls)",
       x = "Age (Equivalent Round)",
       y = "Average Cladder Score") +
  theme_minimal() +
  scale_y_continuous(limits = c(5.5, 7.5)) +
  scale_color_manual(values = c("#00BFFF", "#FF7F50")) +
  theme(legend.title = element_blank())        # Remove legend title


## Child marriage vs non-child marriage (both cohorts)
data_aggregated_CM <- data_long %>%
  group_by(age, CM) %>%
  filter(chsex == 2) %>% 
  filter(!is.na(CM)) %>% 
  summarise(mean_cladder = mean(cladder, na.rm = TRUE), .groups = 'drop')
head(data_aggregated_CM)
# dropped NA from plotting

ggplot(data_aggregated_CM, aes(x = age, y = mean_cladder, color = CM, group = CM)) +
  geom_smooth(method = "loess", size = 1) +
  geom_point(size = 1) +  
  scale_x_continuous(breaks = c(8, 12, 15, 19, 22), limits = c(8,22)) +
  labs(x = "Age", y = "Average Wellbeing Score", 
       title = "Average Wellbeing Score Across Ages by Child Marriage Status (Girls Combined Cohorts)") +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +  
  theme_minimal()




# ----------------- SAVE DATASET FOR MULTIPLE IMPUTATION -----------------------

# Final check: 
# data_long %>% view()

saveRDS(data_long, "data_long.rds")

# Save to CSV too
data_long_csv <- data_long %>%
  mutate(across(where(is.factor), as.character)) # convert factors to characters to preserve labels

write.csv(data_long_csv, "data_long.csv", row.names = FALSE, na = "")

#---------------------------------- Session information ---------------
sessionInfo()
