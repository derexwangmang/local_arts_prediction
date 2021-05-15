# Load Packages -----------------------------------------------------------
library(tidyverse)
library(tidymodels)

# Set Seed ----------------------------------------------------------------
set.seed(3206)

# Data Loading ---------------------------------------------------------------
load(file = "data/processed/local_arts_data.Rda")


# Remove NA Values of Income ----------------------------------------------
local_arts_data <- local_arts_data %>% 
  filter(!is.na(income))


# Removing Unwanted Columns -----------------------------------------------
local_arts_data <- local_arts_data %>% 
  # removing variables with over 20% missingness
  select(-bar5, -bar4, -mags, -more8, -bar3, -more7, -radio, -more6, 
         -tvtype, -bar2, -more5, -more4, -more3, -newsp) %>% 
  # remove variables with no variance
  select(-caseid, -site, -abtid, -exchange, -source2, -source3, -source4, -source5)

# Data Splitting ----------------------------------------------------------
# conduct initial split
local_arts_split <- initial_split(data = local_arts_data, prop = 0.7, strata = income)

# collect training data
local_arts_train <- training(local_arts_split)

# collect testing data
local_arts_test <- testing(local_arts_split)

# Data Folding (v-fold cross-validation) ----------------------------------
local_arts_fold <- vfold_cv(data = local_arts_train, v = 5, repeats = 3, strata = income)

# Write out Split and Folds -----------------------------------------------
# save training data
save(local_arts_train, file = "data/processed/local_arts_train.rda")

# save testing data
save(local_arts_test, file = "data/processed/local_arts_test.rda")

# save folds
save(local_arts_fold, file = "data/processed/local_arts_fold.rda")


