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


# Finalizing Best Model (Random Forest) -----------------------------------
rf_final_workflow <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tuned, metric = "roc_auc"))


# Fitting Best Model (Random Forest) To Entire Training Data --------------
# set seed
set.seed(1234)

# load training and testing data
load(file = "data/processed/local_arts_train.rda")

load(file = "data/processed/local_arts_test.rda")

# fit
rf_final_results <- fit(rf_final_workflow, local_arts_train)

# Apply Fitted Model (rf_final_results) to Test Data ------------------------------------------------
# use predict to create table of predicted classes
rf_final_pred <- predict(rf_final_results, new_data = local_arts_test %>% select(-income), type = "prob") %>%
  # add the id column from test data
  bind_cols(local_arts_test %>% select(income))


# Evaluate Predicted Values -----------------------------------------------
# apply predicted values to roc_auc to see how our model performed
roc_auc(rf_final_pred, income, `.pred_Under $10,000`, `.pred_$10,000 to $14,999`, `.pred_$15,000 to $19,999`,
        `.pred_$20,000 to $29,999`, `.pred_$30,000 to $39,999`, `.pred_$40,000 to $49,999`,
        `.pred_$50,000 to $74,999`, `.pred_$75,000 to $99,999`, `.pred_$100,000 or more`)

