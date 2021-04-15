# Load Necessary Packages -------------------------------------------------
library(tidyverse)
library(naniar)

# Load in Data ------------------------------------------------------------
load(file = "data/processed/local_arts_data.Rda")

# Create a Graph of the Outcome Variable ----------------------------------
local_arts_data %>% 
  filter(!is.na(income) & income != "(11) Refused" & income != "(10) Don't know") %>% 
  ggplot(aes(x = income)) +
  geom_bar() +
  coord_flip()

