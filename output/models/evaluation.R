# Load Necessary Packages -------------------------------------------------
library(tidyverse)
library(tidymodels)

# Load Elastic Net Tuned Object -------------------------------------------
load(file = "output/models/en_tuned.rda")

# Evaluate Elastic Net Model ----------------------------------------------
en_tuned %>% 
  show_best(metric = "roc_auc")

# Load Random Forest Tuned Object -----------------------------------------
load(file = "output/models/rf_tuned.rda")

# Evaluated Random Forest Model -------------------------------------------
rf_tuned %>% 
  show_best(metric = "roc_auc")

# Load Boosted Tree Tuned Object ------------------------------------------
load(file = "output/models/bt_tuned.rda")

# Evaluated Boosted Tree Model --------------------------------------------
bt_tuned %>% 
  show_best(metric = "roc_auc")

# Load Support Vector Machine (Radial Basis Function Kernel) Tuned Object --------------------------------------
load(file = "output/models/svm_tuned.rda")

# Evaluate Support Vector Machine (Radial Basis Function Kernel) ----------
svm_tuned %>% 
  show_best(metric = "roc_auc")

# Load Neural Network Tuned Object -----------------------------------------------------
load(file = "output/models/neural_tuned.rda")

# Evaluate Neural Network -------------------------------------------------
nn_tuned %>% 
  show_best(metric = "roc_auc")

# Load K-Nearest Neighbors Model --------------------------------------
load(file = "output/models/knn_tuned.rda")


# Evaluate K-Nearest Neighbors Model --------------------------------------
knn_tuned %>% 
  show_best(metric = "roc_auc")


