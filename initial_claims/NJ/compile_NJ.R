# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)


raw <- read_csv("NJ_gathered.csv") 

# import FIPS codes

data(county.fips)

output <- raw %>%
  pivot_longer(cols = !starts_with("county"), names_to = "date", values_to = "claims") %>%
  mutate(
    
    # date info
    
    date = mdy(date),
    week = week(date),
    month = month(date),
    year = year(date),
    
    # set general info
    state = "New Jersey",
    state_fips = 34,
    state_short = "NJ",
    
    polyname = case_when(
      TRUE ~ paste("new jersey,", tolower(county), sep = "")
    )
  ) %>%
  
  # Join with FIPS
  
  left_join(county.fips, by = "polyname") %>%
  rename(
    county_fips = fips
  ) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "NJ_compiled.csv", row.names = FALSE)
