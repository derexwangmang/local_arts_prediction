# Load Necessary Packages ------------------------------------------------
library(tidyverse)
library(janitor)
library(labelled)
library(haven)

# Load Data ---------------------------------------------------------------
load(file = "data/unprocessed/local_arts_data.rda")


# Make Data a Tibble ------------------------------------------------------
local_arts_data <- as_tibble(da35586.0001)

attr(local_arts_data, 'variable.labels') <- NULL

# Dropping Variables ------------------------------------------------------

# Use a loop to make appropriate missing values zero and track "w" variables 
# (variables indicating where participant saw performance)
all_col_names <- colnames(local_arts_data)
dropped_cols <- c()

for (i in 1:(length(all_col_names) - 1)) {
  if (all_col_names[i] == substring(all_col_names[i+1], 2)) {
    for (j in 1:(nrow(local_arts_data[i]))) {
      if (local_arts_data[j,i] == "(2) No") {
        local_arts_data[j,i+1] = 0
      } else if (all_col_names[i] == substring(all_col_names[i+2], 2)) {
        dropped_cols <- c(dropped_cols, all_col_names[i+2])
      }
    }
  }
}

# dropping questions about where/what facility participant saw performance
local_arts_data <- local_arts_data %>%
  select(-dropped_cols)

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
       "$50,000 to $74,999" = "(07) $50,000 TO $74,999",
       "$40,000 to $49,999" = "(06) $40,000 TO $49,999",
       "$30,000 to $39,999" = "(05) $30,000 TO $39,999",
       "$20,000 to $29,999" = "(04) $20,000 TO $29,999",
       "$15,000 to $19,999" = "(03) $15,000 TO $19,999",
       "$10,000 to $14,999" = "(02) $10,000 TO $14,999",
       "Under $10,000" = "(01) Under $10,000"
       )
  )

# Categorizing Variables as NA --------------------------------------------

# Converts categorical values with "Don't know" or "Refused" to NA
local_arts_data <- local_arts_data %>%
  mutate_all(~ replace(., grepl("Don't know$|Refused$", .), NA))

# Converts numerical values with "Don't know" or "Refused" to NA
local_arts_data <- local_arts_data %>%
  mutate_all(~ replace(., . == 98 | . == 99, NA))


# Re-level Categorical Variables Whose Values of Don't Know and Refused were Changed to NA --------
# Create function for variables with only yes/no levels
yes_no_levels <- function(x){
  factor(x, levels = c("(1) Yes", "(2) No"))
}

# Mutate Yes/No Categorical Variables
local_arts_data <- local_arts_data %>% 
  mutate(
    jazz = yes_no_levels(jazz),
    classic = yes_no_levels(classic),
    opera = yes_no_levels(opera),
    musical = yes_no_levels(musical),
    play = yes_no_levels(play),
    ballet = yes_no_levels(ballet),
    odance = yes_no_levels(odance),
    museum = yes_no_levels(museum),
    fair = yes_no_levels(fair),
    park = yes_no_levels(park),
    books = yes_no_levels(books),
    readplay = yes_no_levels(readplay),
    readpoet = yes_no_levels(readpoet),
    readnov = yes_no_levels(readnov),
    hearpoet = yes_no_levels(hearpoet),
    hearnov = yes_no_levels(hearnov),
    tvjazz = yes_no_levels(tvjazz),
    tvclass = yes_no_levels(tvclass),
    tvopera = yes_no_levels(tvopera),
    tvmus = yes_no_levels(tvmus),
    tvplay = yes_no_levels(tvplay),
    tvdance = yes_no_levels(tvdance),
    tvart = yes_no_levels(tvart),
    lisjazz = yes_no_levels(lisjazz),
    lisclass = yes_no_levels(lisclass),
    lisopera = yes_no_levels(lisopera),
    lismus = yes_no_levels(lismus),
    lisplay = yes_no_levels(lisplay),
    cinema = yes_no_levels(cinema)
  )

# Dealing with re-leveling and fixing missing values in other categorical variables
# more1
local_arts_data <- local_arts_data %>% 
  mutate(
    more1 = na_if(more1, "(98) None/don't know"),
    more1 = factor(more1, levels = c("(01) Jazz music", "(02) Classical music", "(03) Operas",
                                     "(04) Musicals", "(05) Plays", "(06) Ballet", "(07) Other dance",
                                     "(08) Art museums/galleries"))
  )

# moremost
local_arts_data <- local_arts_data %>% 
  mutate(
    more1 = na_if(moremost, "(98) None/don't know"),
    more1 = factor(moremost, levels = c("(01) Jazz music", "(02) Classical music", "(03) Operas",
                                     "(04) Musicals", "(05) Plays", "(06) Ballet", "(07) Other dance",
                                     "(08) Art museums/galleries"))
  )

# gomore
local_arts_data <- local_arts_data %>% 
  mutate(
    gomore = yes_no_levels(gomore)
  )

# bar1
local_arts_data <- local_arts_data %>% 
  mutate(
    bar1 = factor(bar1, levels = c("(01) Lack of interest", "(02) Don't have time", "(03) Cost of tickets",
                                  "(04) Overall cost", "(05) Tickets sold out", "(06) Transportation/traffic/parking",
                                  "(07) Distance/travel time", "(08) Crime", "(09) Lack of child care", 
                                  "(10) Poor quality of performance", "(11) Not available/no variety", "(12) No one to go with",
                                  "(13) Handicap problem", "(14) Age problem", "(15) Prefer to watch TV",
                                  "(16) Just don't get around to it", "(95) Other"))
  )

# mostimp
local_arts_data <- local_arts_data %>% 
  mutate(
    mostimp = factor(mostimp, levels = c("(02) Lack of interest", "(03) Don't have time", "(04) Cost of tickets",
                                         "(05) Overall cost", "(06) tickets sold out", "(07) Transportation/traffic/parking",
                                         "(08) Distance/travel time", "(09) Crime", "(10) Lack of child care",
                                         "(11) Poor quality of performance", "(12) Not available/no variety", "(13) No one to go with",
                                         "(14) Handicap problem", "(15) Age problem", "(16) Prefer to watch TV",
                                         "(17) Just don't get around to it", "(18) Not enough info on events", "(95) Other"))
  )

# howimp
local_arts_data <- local_arts_data %>% 
  mutate(
    howimp = factor(howimp, levels = c("(1) Very important", "(2) Somewhat important", "(3) Not at all important"))
  )

# schools
local_arts_data <- local_arts_data %>% 
  mutate(
    schools = factor(schools, levels = c("(1) Very important", "(2) Somewhat important", "(3) Not at all important"))
  )

# source1
local_arts_data <- local_arts_data %>% 
  mutate(
    source1 = na_if(source1, "(96) None/noinfo"),
    source1 = factor(source1, levels = c("(01) Newspapers", "(02) Word of mouth", "(03) Friends/relatives/others", "(04) Direct mail",
                                         "(05) Posters/notices", "(06) Television", "(07) Radio", "(08) Magazines",
                                         "(09) School notices", "(10) Telemarketing", "(95) Other" ))
  )

# rateinfo
local_arts_data <- local_arts_data %>% 
  mutate(
    rateinfo = factor(rateinfo, levels = c("(1) Excellent", "(2) Good", "(3) Fair", "(4) Poor"))
  )

# marital
local_arts_data <- local_arts_data %>% 
  mutate(
   marital = factor(marital, levels = c("(1) Married", "(2) Widowed", "(3) Separated", "(4) Divorced",
                                        "(5) Never married")) 
  )

# educ
local_arts_data <- local_arts_data %>% 
  mutate(
    educ = factor(educ, levels = c("(01) No school", "(02) Grades k-8", "(03) Grades 9-11", "(04) High school", "(05) Vocational school",
                                   "(06) Some college", "(07) Bachelors degree", "(08) Some grad. school", "(09) Grad. degree", "(95) Other"))
  )

# race
local_arts_data <- local_arts_data %>% 
  mutate(
    race = factor(race, levels = c("(01) White, not Hispanic",  "(02) Black", "(03) Hispanic/Latino", "(04) Carribean",
                                   "(05) Central American", "(06) South American", "(07) Native American", "(08) Alaskan Native",
                                   "(09) Asian/Pacific Islander/Filipino", "(95) Other" ))
  )

# income
local_arts_data <- local_arts_data %>% 
  mutate(
    income = factor(income, levels = c("Under $10,000", "$10,000 to $14,999", "$15,000 to $19,999", "$20,000 to $29,999",
                                       "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $74,999",
                                       "$75,000 to $99,999", "$100,000 or more"))
  )


# Write Out Processed Data Set --------------------------------------------
save(local_arts_data, file = "data/processed/local_arts_data.Rda")
