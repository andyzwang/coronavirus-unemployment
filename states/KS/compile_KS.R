# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import
raw <- read_csv("KS_gathered.csv")

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
    state = "Kansas",
    state_fips = 20,
    state_short = "KS",
    
    polyname = case_when(
      TRUE ~ paste("kansas,", tolower(county), sep = "")
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
write.csv(output, file = "KS_compiled.csv", row.names = FALSE)
