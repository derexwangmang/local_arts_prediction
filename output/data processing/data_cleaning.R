# Load Necessary Packages ------------------------------------------------
library(tidyverse)


# Load Data ---------------------------------------------------------------
load(file = "data/unprocessed/local_arts_data.rda")


# Make Data a Tibble ------------------------------------------------------
local_arts_data <- as_tibble(da35586.0001)
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

save(local_arts_data, file = "data/processed/local_arts_data.Rda")
