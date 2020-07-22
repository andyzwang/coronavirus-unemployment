# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# processing

raw <- read_csv("TN_data.csv") %>%
  remove_empty("cols")


output <- raw %>%

  clean_names("snake") %>%
  mutate(

    # set general info
    state = "Tennessee",
    state_fips = 47,
    state_short = "TN",

    # modify dates
    date = mdy(claims_week_ending),
    month = month(date),
    year = year(date),
    week = week(date),
    county= str_remove(county, "\\?"),
    
    claims = total_initial_claims,
    polyname = case_when(
      county == "DeKalb" ~ "tennessee,de kalb",
      TRUE ~ paste("tennessee,", str_to_lower(county), sep = ""))
  ) %>%
  
  # Join with FIPS codes
  
  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

write.csv(output, file = "TN_compiled.csv", row.names = FALSE)
