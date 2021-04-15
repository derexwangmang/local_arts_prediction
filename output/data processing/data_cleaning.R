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


# Renaming Levels of Outcome ----------------------------------------------
local_arts_data <- local_arts_data %>% 
  mutate(
    income = fct_recode(income,
       "Refused" = "(11) Refused",
       "Don't know" = "(10) Don't know", 
       "$100,000 or more" = "(09) $100,000 OR MORE", 
       "$75,000 to $99,999" = "(08) $75,000 TO $99,999",
       "$50,000 to $74,000" = "(07) $50,000 TO $74,999",
       "$40,000 to $49,999" = "(06) $40,000 TO $49,999",
       "$30,000 to $39,999" = "(05) $30,000 TO $39,999",
       "$20,000 to $29,999" = "(04) $20,000 TO $29,999",
       "$15,000 to $19,999" = "(03) $15,000 TO $19,999",
       "$10,000 to $14,999" = "(02) $10,000 TO $14,999",
       "Under $10,000" = "(01) Under $10,000"
       )
  )


# Write Out Processed Data Set --------------------------------------------
save(local_arts_data, file = "data/processed/local_arts_data.Rda")
