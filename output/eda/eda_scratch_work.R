# Load Necessary Packages -------------------------------------------------
library(tidyverse)
library(naniar)
library(tidymodels)

# Load in Data ------------------------------------------------------------
load(file = "data/processed/local_arts_data.Rda")

# Create a Graph of the Outcome Variable ----------------------------------
local_arts_data %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  ggplot(aes(x = income)) +
  geom_bar(fill = "#3b4b21") +
  coord_flip() +
  labs(
    title = "Distribution of Respondent's Household Income"
  ) +
  theme_minimal()

# Split Data --------------------------------------------------------------
# conduct initial split
local_arts_split <- initial_split(data = local_arts_data, prop = 0.7, strata = income)

# collect training data
local_arts_train <- training(local_arts_split)

# collect testing data
local_arts_test <- testing(local_arts_split)

# More EDA ---------------------------------------------------------------


