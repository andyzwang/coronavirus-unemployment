# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing
# NOTE: range is manually set

raw <- read_excel("CO_data.xlsx", range = "A2:BO16", na = "***") 

# import FIPS codes

data(county.fips)

output <- raw %>%
  
  # pivot long
  
  pivot_longer(
    cols = Adams:Yuma, names_to = "county",
    values_to = "claims"
  ) %>%
  
  #remove total
  
  select(-'Colorado Total') %>%
  clean_names(case = "snake") %>%
  
  mutate(
    
    # set general info
    state = "Colorado",
    state_fips = 8,
    state_short = "CO",
    
    # modify dates
    date = excel_numeric_to_date(as.numeric(week_ending_date)),
    week = week(date),
    month = month(date),
    year = year(date),
    
    # County FIPS code and name
    
    polyname = paste("colorado,", tolower(county), sep = "")
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%
  
  # relevant columns
  
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  
  filter(!is.na(date)) %>%
  
  arrange(week)

# output
write.csv(output, file = "CO_compiled.csv", row.names = FALSE)
