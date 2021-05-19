# Load Packages -----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(xgboost)

# Set Seed ----------------------------------------------------------------
set.seed(399399)

# Load Necessary Files ----------------------------------------------------
load(file = "data/processed/local_arts_fold.rda")
load(file  = "data/processed/local_arts_train.rda")

# Recipe ------------------------------------------------------------------
bt_recipe <-
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
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  # normalize all predictors
  step_normalize(all_predictors()) %>%
  # remove zero-variance predictors
  step_zv(all_predictors())

# # Model -------------------------------------------------------------------
bt_model <-
  # specify model type and parameters
  # to optimize
  boost_tree(mode = "classification",
             mtry = tune(),
             min_n = tune(),
             learn_rate = tune()) %>%
  # set underlying engine
  set_engine("xgboost")

# Workflow ----------------------------------------------------------------
bt_workflow <-
  # establish workflow
  workflow() %>%
  # add model
  add_model(bt_model) %>%
  # add recipe
  add_recipe(bt_recipe)

# Parameter Object --------------------------------------------------------
bt_params <- parameters(bt_workflow) %>%
  update(mtry = mtry(range = c(2, 120)),
         learn_rate = learn_rate(range = c(-1, 0.01)))

# Create Regular Grid -----------------------------------------------------
bt_grid <- grid_regular(bt_params, levels = 5)

# Tune --------------------------------------------------------------------
bt_tuned <- bt_workflow %>% 
  # indicate folds object, and grid object
  tune_grid(local_arts_fold, grid = bt_grid)

# Write Out ---------------------------------------------------------------
save(bt_tuned, bt_model, bt_workflow, file = "output/model_set_up/bt_tuned_non_parallel.rda")
