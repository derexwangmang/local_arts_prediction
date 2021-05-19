# Load Necessary Packages -------------------------------------------------
library(tidyverse)
library(tidymodels)

# Load Elastic Net Tuned Object -------------------------------------------
load(file = "output/model_set_up/en_tuned.rda")

# Evaluate Elastic Net Model ----------------------------------------------
en_tuned %>% 
  show_best(metric = "roc_auc")

# Load Random Forest Tuned Object -----------------------------------------
load(file = "output/model_set_up/rf_tuned.rda")

# Evaluated Random Forest Model -------------------------------------------
rf_tuned %>% 
  show_best(metric = "roc_auc")

