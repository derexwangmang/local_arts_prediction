# Load Packages -----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(glmnet)

# Set Seed ----------------------------------------------------------------
set.seed(399399)

# Load Necessary Files ----------------------------------------------------
load(file = "data/processed/local_arts_fold.rda")
load(file  = "data/processed/local_arts_train.rda")

# Recipe ------------------------------------------------------------------
en_recipe <-
  # set outcome variable (income) and predictors (all other variables)
  recipe(income ~ ., data = local_arts_train) %>% 
  # impute missing data
  step_impute_bag(all_predictors()) %>%
  # Yeo-Johnson transform numeric variables to deal with skewness
  step_YeoJohnson(all_numeric()) %>%
  # create an other category for infrequently occuring levels
  # of educ and race
  step_other(educ, threshold = 0.01) %>% 
  step_other(race, threshold = 0.01) %>% 
  # dummy encode categorical predictors
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  # normalize all predictors
  step_normalize(all_predictors()) %>% 
  # remove zero-variance predictors
  step_zv(all_predictors())

# Model -------------------------------------------------------------------
en_model <-
  # specify model type and parameters
  # to optimize
  multinom_reg(penalty = tune(), mixture = tune()) %>% 
  # set underlying engine
  set_engine("glmnet")

# Workflow ----------------------------------------------------------------
en_workflow <-
  # establish workflow
  workflow() %>% 
  # add model
  add_model(en_model) %>% 
  # add recipe
  add_recipe(en_recipe)

# Parameter Object --------------------------------------------------------
en_params <- parameters(en_workflow)

# Create Regular Grid -----------------------------------------------------
en_grid <- grid_regular(en_params, levels = 7)

# Tune --------------------------------------------------------------------
en_tuned <- en_workflow %>% 
  # indicate folds object, and grid object
  tune_grid(local_arts_fold, grid = en_grid)

# Write Out ---------------------------------------------------------------
save(en_tuned, en_recipe, en_model, en_workflow, file = "output/model_set_up/en_tuned.rda")




