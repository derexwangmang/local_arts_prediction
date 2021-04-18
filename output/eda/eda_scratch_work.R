# Load Necessary Packages -------------------------------------------------
library(tidyverse)
library(naniar)
library(tidymodels)
library(skimr)


# Set Seed ----------------------------------------------------------------
set.seed(3206)

# Load in Data ------------------------------------------------------------
load(file = "data/processed/local_arts_data.Rda")

# Create a Graph of the Outcome Variable ----------------------------------
local_arts_data %>%
  ggplot(aes(x = income)) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Distribution of Respondent's Household Income"
  ) +
  theme_minimal()

local_arts_data %>% 
  count(income)

# Only keep observations with values for income
local_arts_data <- local_arts_data %>% 
  filter(!is.na(income))


# Missingness -------------------------------------------------------------
# create function to identify columns with missing values
has_na <- function(x){
  any(is.na(x))
}

# graph missingness
local_arts_data %>%
  # select only columns that have missing values
  select_if(has_na) %>%
  # graph using naniar
  gg_miss_var()

# table of missingness
local_arts_data %>%
  # select only columns that have missing values
  select_if(has_na) %>%
  # summary of missing variables
  miss_var_summary() %>% 
  print(n = Inf)

# take out variables with a significant amount of missingness
local_arts_data <- local_arts_data %>% 
  select(-bar5, -bar4, -mags, -more8, -bar3, -more7, -radio, -more6, 
         -tvtype, -bar2, -more5, -more4, -more3, -newsp)

# Split Data --------------------------------------------------------------
# conduct initial split
local_arts_split <- initial_split(data = local_arts_data, prop = 0.7, strata = income)

# collect training data
local_arts_train <- training(local_arts_split)

# collect testing data
local_arts_test <- testing(local_arts_split)

# More EDA ---------------------------------------------------------------
# income by jazz
local_arts_train %>% 
  filter(jazz != "(3) Don't know") %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  ggplot(aes(income, fill = jazz)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Attendance at live jazz concerts by income",
       fill = "Did you attend\na live jazz concert\nin last 12 months?",
       y = "prop")

# income by classic
local_arts_train %>% 
  filter(classic != "(3) Don't know") %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  ggplot(aes(income, fill = classic)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Attendance at live classical concerts by income",
       fill = "Did you attend\na classical performance\nin last 12 months?",
       y = "prop")

# income by nclassic
local_arts_train %>% 
  ggplot(aes(income, nclassic)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 20)) +
  coord_flip() +
  labs(
    y = "number of times attended live classical concert in last 12 months"
  )

# income by ballet
local_arts_data %>% 
  filter(ballet != "(3) Don't know") %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  ggplot(aes(income, fill = ballet)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Attendance at ballet performances by income",
       fill = "Did you attend\na ballet performance\nin last 12 months?",
       y = "prop")

# income by ballet by gender
local_arts_data %>% 
  filter(ballet != "(3) Don't know") %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  ggplot(aes(income, fill = ballet)) +
  geom_bar(position = "fill") +
  coord_flip() +
  facet_wrap(~ gender) +
  theme_minimal() +
  labs(title = "Attendance at ballet performances by income and gender",
       fill = "Did you attend\na ballet performance\nin last 12 months?",
       y = "prop")

# income by museum
local_arts_data %>% 
  filter(museum != "(3) Don't know") %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  ggplot(aes(income, fill = museum)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Attendance at museums by income",
       fill = "Did you visit\n a museum\nin last 12 months?",
       y = "prop")

# income by books
local_arts_data %>% 
  filter(books != "(3) Don't know" & books != "(4) Refused") %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  ggplot(aes(income, fill = books)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Reading and income: books",
       fill = "Did you read a book\nin last 12 months?",
       y = "prop")

# income by poetry
local_arts_data %>% 
  filter(readpoet != "(3) Don't know") %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  ggplot(aes(income, fill = readpoet)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Reading and income: poetry",
       fill = "Did you read poetry\nin last 12 months?",
       y = "prop")

# income by tvopera
local_arts_data %>% 
  filter(tvopera != "(3) Don't know") %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  group_by(income, tvopera) %>% 
  count() %>% 
  ggplot(aes(tvopera, income, fill = n)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Watched opera on tv by income",
       x = "Have you watched opera on tv in last 12 months?")

# income by tvplay
local_arts_data %>% 
  filter(tvplay != "(3) Don't know") %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  ggplot(aes(income, fill = tvplay)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Watched plays on television by income",
       fill = "Have you watched a stage play\nin last 12 months?",
       y = "prop")

# income by lisclass
local_arts_data %>% 
  filter(lisclass != "(3) Don't know") %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  ggplot(aes(income, fill = lisclass)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Listening to classical music on radio by income",
       fill = "Have you listened to classical\nmusic on radio\nin last 12 months?",
       y = "prop")

# income by cinema
local_arts_data %>% 
  filter(cinema != "(3) Don't know" & cinema != "(4) Refused") %>% 
  filter(!is.na(income) & income != "Refused" & income != "Don't know") %>% 
  ggplot(aes(income, fill = cinema)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Going to the movies by income",
       fill = "Have you gone to movie theater\nto see a movie\nin last 12 months?",
       y = "prop")

# combined graph of categorical arts participation variables
local_arts_train %>% 
  select(income, jazz, classic, opera, musical, play, ballet, museum, fair) %>%
  gather(-income, key = "var", value = "value") %>%
  ggplot(aes(x = income, fill = value)) +
  geom_bar(position = "fill") +
  coord_flip() +
  facet_wrap(~ var, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(
    title = "Participation in Arts Activities by Income",
    fill = "have you\nparticipated\nin this\nactivity\nin last 12 months?"
  )

# combined graph of numeric arts participation variables
local_arts_train %>%
  select(income, starts_with("n")) %>%
  filter(!is.na(income)) %>%
  select(., 1:10) %>%
  gather(-income, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = income)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free") +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 10)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(
    title = "Frequency of Attendance at Arts Activities by Income"
  )

local_arts_train %>% 
  ggplot(aes(nclassic)) +
  geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(0, 15)) +
  facet_wrap(~ income) +
  labs(
    x = "number of times attended live classical concert in last 12 months"
  )

local_arts_train %>%
  select(income, starts_with("n")) %>%
  filter(!is.na(income)) %>%
  select(., 1, 11:19) %>%
  gather(-income, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = income)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free") +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 20)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(
    title = "Frequency of Engaging in/Watching Arts Activities by Income"
  )

# distribution of bar1
local_arts_train %>% 
  mutate(
    bar1 = bar1 %>% fct_infreq() %>% fct_rev()
  ) %>% 
  ggplot(aes(bar1)) +
  geom_bar() +
  coord_flip() +
  labs(
    x = "reason for not attending"
  )

# distribution of bar1 by income
local_arts_train %>% 
  mutate(
    bar1 = bar1 %>% fct_infreq() %>% fct_rev()
  ) %>% 
  ggplot(aes(bar1)) +
  geom_bar() +
  coord_flip() +
  facet_wrap(~ income) +
  labs(
    x = "reason for not attending"
  )
