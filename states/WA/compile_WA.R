# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing

raw <- read_excel("WA_data.xlsx", sheet = "Weekly county initial claims")

# import FIPS codes

data(county.fips)

output <- raw %>%

  clean_names("snake") %>%
  mutate(
    date = mdy(str_remove(week_ending, ".*- ")),
    claims = initial_claims,
    county = str_to_title(county)
  ) %>%
  select(-county_fips) %>%
  mutate(

    # set general info
    state = "Washington",
    state_fips = 53,
    state_short = "WA",

    # modify dates
    week = week(date),
    month = month(date),
    year = year(date),

    # County FIPS code and name

    polyname = case_when(
      county == "Pierce" ~"washington,pierce:main",
      county == "San Juan" ~"washington,san juan:san juan island",
      TRUE ~ paste("washington,", tolower(county), sep = "")
    )
  ) %>%
  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "WA_compiled.csv", row.names = FALSE)
