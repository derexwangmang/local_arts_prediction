# Load Packages -----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(stacks)
library(earth)

# handle common conflicts
tidymodels_prefer()

# Set Seed ----------------------------------------------------------------
set.seed(1717)

# Load Necessary Files --------------------------------------------------
load(file = "data/processed/local_arts_fold.rda")
load(file  = "data/processed/local_arts_train.rda")

# Recipe ------------------------------------------------------------------
svm_recipe <- 
  # set outcome variable (income) and predictors (all other variables)
  recipe(income ~ ., data = local_arts_train) %>% 
  # impute missing data
  step_impute_knn(all_predictors()) %>%
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
svm_model <-
  # specify model type and parameters to optimize
  svm_rbf(mode = "classification",
          cost = tune(),
          rbf_sigma = tune()) %>%
  # set underlying engine/package
  set_engine("kernlab") 

# Workflow ----------------------------------------------------------------
svm_workflow <-
  # set up workflow
  workflow() %>% 
  # add model
  add_model(svm_model) %>% 
  # add recipe
  add_recipe(svm_recipe)

# Establish Parameter Object ----------------------------------------------
svm_params <- parameters(svm_model)

# Establish Grid ----------------------------------------------------------
svm_grid <- grid_regular(svm_params, levels = 10)

# Tune --------------------------------------------------------------------
# create control object using stack function
# control_stack_grid() >> tells the tuning grid to keep 
# predictions and workflow

control_grid <- control_stack_grid()

# tune
svm_tuned <- svm_workflow %>%
  tune_grid(
    resamples = local_arts_fold,
    grid = svm_grid,
    control = control_grid
  )

# Write out--------------------------
save(svm_tuned, svm_recipe, svm_model, svm_workflow, file = "output/models/svm_tuned.rda")
