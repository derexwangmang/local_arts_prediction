# Load Necessary Packages -------------------------------------------------
library(tidyverse)
library(tidymodels)
# install.packages("kableExtra")
library(kableExtra)

# Load Elastic Net Tuned Object -------------------------------------------
load(file = "output/models/en_tuned.rda")

# Evaluate Elastic Net Model ----------------------------------------------
en_tuned %>% 
  show_best(metric = "roc_auc")

# Load Random Forest Tuned Object -----------------------------------------
load(file = "output/models/rf_tuned.rda")

# Evaluated Random Forest Model -------------------------------------------
# show best
rf_tuned %>% 
  show_best(metric = "accuracy")

# autoplot
# specify roc_auc as my performance measure
autoplot(rf_tuned, metric = "roc_auc")

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


# Creating table of roc-auc values and standard error of best model --------
# create tibble
best_models <- tribble(
  ~`Model Type`, ~Roc-auc, ~`Standard error`,
  #----------|---------|----------------|
  "Random Forest", 0.712, 0.00441,
  "Elastic Net", 0.684, 0.00264,
  "Boosted Tree", 0.688, 0.00265,
  "Support Vector Machine", 0.687, 0.00306,
  "Neural Network", 0.591, 0.0128,
  "K-Nearest Neighbors", 0.596, 0.00385
)

# arrange in descending order of roc-auc
best_models %>% 
  arrange(desc(roc_auc))


# Finalizing Best Model (Random Forest) -----------------------------------
rf_final_workflow <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tuned, metric = "roc_auc"))


# Fitting Best Model (Random Forest) To Entire Training Data --------------
# set seed
set.seed(1234)

# load training and testing data
load(file = "data/processed/local_arts_train.rda")

load(file = "data/processed/local_arts_test.rda")

# Fitting to all training data
rf_final_results <- fit(rf_final_workflow, local_arts_train)

# Apply Fitted Model (rf_final_results) to Test Data ------------------------------------------------
# use predict to create table of predicted classes
rf_final_pred <- predict(rf_final_results, new_data = local_arts_test %>% select(-income), type = "prob") %>%
  # add the id column from test data
  bind_cols(local_arts_test %>% select(income)) %>%
  bind_cols(predict(rf_final_results, new_data = local_arts_test %>% select(-income)))


# Evaluate Predicted Values -----------------------------------------------
# apply predicted values to roc_auc to see how our model performed
rf_final_pred %>%
  roc_auc(truth = income, `.pred_Under $10,000`, `.pred_$10,000 to $14,999`, `.pred_$15,000 to $19,999`,
                     `.pred_$20,000 to $29,999`, `.pred_$30,000 to $39,999`, `.pred_$40,000 to $49,999`,
                     `.pred_$50,000 to $74,999`, `.pred_$75,000 to $99,999`, `.pred_$100,000 or more`)

# save out predicted values of random forest model on test data
save(rf_final_pred, "output/models/random_forest_predictions.rda")

# read in predicted values
load(file = "output/models/random_forest_predictions.rda")
