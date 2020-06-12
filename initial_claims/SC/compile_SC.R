# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# processing

raw <- read_csv("SC_gathered.csv") 

# import FIPS codes

data(county.fips)

output <- raw %>%

  # pivot long

  pivot_longer(
    cols = ends_with("20"), names_to = "Date",
    values_to = "claims"
  ) %>%

  mutate(

    # set general info
    state = "South Carolina",
    state_fips = 45,
    state_short = "SC",

    # modify dates
    date = mdy(Date),
    month = month(date),
    year = year(date),
    week = week(date),
    
    # create polyname 
    
    polyname = paste("south carolina,", tolower(county), sep = "")
  ) %>%
  
  # Join with FIPS codes
  
  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(month)

# output
write.csv(output, file = "SC_compiled.csv", row.names = FALSE)
