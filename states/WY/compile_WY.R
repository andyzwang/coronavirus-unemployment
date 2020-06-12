# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing

raw <- read_excel("WY_data.xlsx", sheet = "County", skip = 7)

# import FIPS codes

data(county.fips)

output <- raw %>%

  # housekeeping first

  filter(County != "Total") %>%
  remove_empty("cols") %>%
  clean_names("snake") %>%
  mutate(

    # set general info
    state = "Wyoming",
    state_fips = 56,
    state_short = "WY",

    # modify dates

    date = ymd(week_ending),
    week = week(date),
    month = month(date),
    year = year(date),

    # County FIPS code and name

    polyname = case_when(
      TRUE ~ paste("wyoming,", tolower(county), sep = "")
    )
  ) %>%
  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    county_fips = fips,
    claims = initial_claims
  ) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "WY_compiled.csv", row.names = FALSE)
