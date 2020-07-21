# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(tabulizer)

# processing

raw <- read_csv("NY_gathered.csv")  %>%
  remove_empty("cols")


# import FIPS codes

data(county.fips)

output <- raw %>%
  pivot_longer(
    cols = !starts_with("county"), names_to = "date", values_to = "claims"
  ) %>%
  mutate(
    
    # info
    state = "New York",
    state_short = "NY",
    state_fips = 36,
    
    # dates
    date = mdy(date),
    month = month(date),
    week = week(date),
    year = year(date),
    
    # Get FIPS
    
    polyname = case_when(
      county == "St. Lawrence" ~ "new york,st lawrence",
      TRUE ~ paste("new york,", tolower(county), sep = "")
    )
  ) %>%
  
  # Join with FIPS
  
  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "NY_compiled.csv", row.names = FALSE)
