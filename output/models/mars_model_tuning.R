# Load Packages -----------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(stacks)
library(earth)

# Set Seed ----------------------------------------------------------------
set.seed(1717)

# Load Necessary Files --------------------------------------------------
load(file = "data/processed/local_arts_fold.rda")
load(file  = "data/processed/local_arts_train.rda")

# Recipe ------------------------------------------------------------------
mars_recipe <- 
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
mars_model <-
  # specify model type and parameters to optimize
  mars(num_terms = tune(),
       prod_degree = tune()) %>% 
  # set underlying engine/package
  set_engine("earth") %>% 
  # set mode
  set_mode("classification")

# Workflow ----------------------------------------------------------------
mars_workflow <-
  # set up workflow
  workflow() %>% 
  # add model
  add_model(mars_model) %>% 
  # add recipe
  add_recipe(mars_recipe)


# Establish Parameter Object ----------------------------------------------
mars_params <- parameters(mars_workflow) %>% 
  # update the range of num_terms and prod_degree
  update(num_terms = num_terms(range = c(2, 10)),
         prod_degree = prod_degree(range = c(1, 3)))

# Establish Grid ----------------------------------------------------------
mars_grid <- grid_regular(mars_params, levels = 10)

# Tune --------------------------------------------------------------------
# create control object using stack function
# control_stack_grid() >> tells the tuning grid to keep 
# predictions and workflow

# using this because this model will be used in an ensemble

control_grid <- control_stack_grid()

# tune
mars_tuned <- mars_workflow %>%
  tune_grid(
    resamples = local_arts_fold,
    grid = mars_grid,
    control = control_grid
  )

# Write out--------------------------
save(mars_tuned, mars_recipe, mars_model, mars_workflow, file = "output/models/mars_tuned.rda")