# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# processing

raw <- read_csv("KY_gathered.csv")

# import FIPS codes

output <- raw %>%
  pivot_longer(cols = !starts_with("county"), names_to = "date", values_to = "claims") %>%
  mutate(
    
    # date info
    
    date = ceiling_date(dmy(paste("1-", date)), "month") -1,
    week = NA,
    month = month(date),
    year = year(date),
    
    # set general info
    state = "Kentucky",
    state_fips = 22,
    state_short = "KY",
    
    polyname = case_when(
      TRUE ~ paste("kentucky,", tolower(county), sep = "")
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
write.csv(output, file = "KY_compiled.csv", row.names = FALSE)
