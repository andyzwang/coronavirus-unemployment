# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)


raw <- read_csv("NV_gathered.csv") 

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
    state = "Nevada",
    state_fips = 32,
    state_short = "NV",
    
    polyname = case_when(
      TRUE ~ paste("nevada,", tolower(county), sep = "")
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
write.csv(output, file = "NV_compiled.csv", row.names = FALSE)
