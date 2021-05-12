# Load Packages -----------------------------------------------------------
library(tidyverse)
library(tidymodels)

# Set Seed ----------------------------------------------------------------
set.seed(3206)

# Data Loading ---------------------------------------------------------------
load(file = "data/processed/local_arts_data.Rda")

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


