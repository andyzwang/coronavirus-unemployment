# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(tabulizer)

# processing
# NOTE: columns manually set

raw <- read_csv("OH_gathered.csv") %>%
  remove_empty("rows")

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
    state = "Ohio",
    state_fips = 39,
    state_short = "OH",

    # modify dates
    date = mdy(Date),
    month = month(date),
    year = year(date),
    week = week(date),
    
    # create polyname 
    
    polyname = paste("ohio,", tolower(county), sep = "")
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
write.csv(output, file = "OH_compiled.csv", row.names = FALSE)
