# Load Necessary Packages ------------------------------------------------
library(tidyverse)
library(janitor)


# Load Data ---------------------------------------------------------------
load(file = "data/unprocessed/local_arts_data.rda")


# Make Data a Tibble ------------------------------------------------------
local_arts_data <- as_tibble(da35586.0001)


# Use a Loop To Make Appropriate Missing Values Zero ----------------------
all_col_names <- colnames(local_arts_data)

for (i in 1:(length(all_col_names) - 1)) {
  if (all_col_names[i] == substring(all_col_names[i+1], 2)) {
    for (j in 1:(nrow(local_arts_data[i]))) {
      if (local_arts_data[j,i] == "(2) No") {
        local_arts_data[j, i+1] = 0
      }
    }
  }
}

# Clean Variable Names ----------------------------------------------------
local_arts_data <- local_arts_data %>% 
  clean_names()

# Write Out Processed Data Set --------------------------------------------
save(local_arts_data, file = "data/processed/local_arts_data.Rda")
