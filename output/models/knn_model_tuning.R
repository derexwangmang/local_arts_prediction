# Load Packages -----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(stacks)
library(kknn)

# Set Seed ----------------------------------------------------------------
set.seed(5252)

# Load Necessary Files --------------------------------------------------
load(file = "data/processed/local_arts_fold.rda")
load(file  = "data/processed/local_arts_train.rda")

# Recipe ------------------------------------------------------------------
knn_recipe <- 
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
knn_model <-
  # establish model and set parameters to tune
  nearest_neighbor(neighbors = tune()) %>% 
  # set underlying engine
  set_engine("kknn") %>% 
  # set mode of the model
  set_mode("classification")

# Workflow ----------------------------------------------------------------
knn_workflow <- 
  # establish workflow
  workflow() %>% 
  # add recipe
  add_recipe(knn_recipe) %>% 
  # add model
  add_model(knn_model)


# Establish Parameter Object ----------------------------------------------
knn_params <- parameters(knn_workflow) %>% 
  update(neighbors = neighbors(range = c(0, 30)))

# Establish Grid ----------------------------------------------------------
knn_grid <- grid_regular(knn_params, levels = 20)

# Tune --------------------------------------------------------------------
# create control object using stack function
# control_stack_grid() >> tells the tuning grid to keep 
# predictions and workflow

# using this because this model will be used in an ensemble

control_grid <- control_stack_grid()

# tune
knn_tuned <- knn_workflow %>%
  tune_grid(
    resamples = local_arts_fold,
    grid = knn_grid,
    control = control_grid
  )

# Write out--------------------------
save(knn_tuned, knn_recipe, knn_model, knn_workflow, file = "output/models/knn_res.rda")





