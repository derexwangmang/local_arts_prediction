# Load Packages -----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(ranger)
library(vip)

# Set Seed ----------------------------------------------------------------
set.seed(3206)

# Load Necessary Files ----------------------------------------------------
load(file = "data/processed/local_arts_fold.rda")
load(file  = "data/processed/local_arts_train.rda")

# Recipe ------------------------------------------------------------------
rf_recipe <-
  # set outcome variable (income) and predictors (all other variables)
  recipe(income ~ ., data = local_arts_train) %>% 
  # impute missing data
  step_impute_bag(all_predictors()) %>%
  # create an other category for infrequently occuring levels
  # of educ and race
  step_other(educ, threshold = 0.01) %>% 
  step_other(race, threshold = 0.01) %>% 
  # normalize all numeric predictors
  step_normalize(all_numeric()) %>% 
  # remove zero-variance predictors
  step_zv(all_predictors())

# Random Forest Model -----------------------------------------------------
rf_model <-
  # specify the model
  rand_forest(mtry = tune(),
              min_n = tune(),
              trees = 500) %>%
  # set the underlying engine
  set_engine("ranger", importance = "impurity") %>%
  # set the mode (model outcome)
  set_mode("classification")

# Random Forest Workflow --------------------------------------------------
rf_workflow <-
  # establish the workflow
  workflow() %>%
  # add the model
  add_model(rf_model) %>%
  # add the recipe
  add_recipe(rf_recipe)

# Parameters --------------------------------------------------------------
# check parameters for random forest workflow
parameters(rf_workflow)

# use update to specify the range of mtry
# update range of mtry
rf_params <- parameters(rf_workflow) %>%
  update(mtry = mtry(range = c(10, 56)))
# view parameter object
rf_params

# Grid --------------------------------------------------------------------
# create a regular grid
rf_grid <- grid_regular(rf_params, levels = 5)

# view grid
rf_grid

# Tune --------------------------------------------------------------------
# Use parallel processing
# Indicate to control function that we want to run in parallel over everything
parallel_everything <- control_grid(parallel_over = "everything")

rf_tuned <- rf_workflow %>% 
  # indicate folds object, and grid object
  tune_grid(local_arts_fold, grid = rf_grid, control = parallel_everything)

# Write Out ---------------------------------------------------------------
save(rf_model, rf_workflow, rf_recipe, rf_tuned, file = "output/models/rf_tuned.rda")