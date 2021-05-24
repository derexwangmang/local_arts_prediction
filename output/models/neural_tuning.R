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
nn_recipe <- 
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
nn_model <-
  # specify model type and parameters to optimize
  mlp(mode = "classification",
      hidden_units = tune(),
      penalty = tune()) %>%
  # set underlying engine/package
  set_engine("keras") 

# Workflow ----------------------------------------------------------------
nn_workflow <-
  # set up workflow
  workflow() %>% 
  # add model
  add_model(nn_model) %>% 
  # add recipe
  add_recipe(nn_recipe)

# Establish Parameter Object ----------------------------------------------
nn_params <- parameters(nn_model)

# Establish Grid ----------------------------------------------------------
nn_grid <- grid_regular(nn_params, levels = 5)

# Tune --------------------------------------------------------------------
# create control object using stack function
# control_stack_grid() >> tells the tuning grid to keep 
# predictions and workflow

control_grid <- control_stack_grid()

# tune
nn_tuned <- nn_workflow %>%
  tune_grid(
    resamples = local_arts_fold,
    grid = nn_grid,
    control = control_grid
  )

# Write out--------------------------
save(nn_tuned, nn_recipe, nn_model, nn_workflow, file = "output/models/neural_tuned.rda")
