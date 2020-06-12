# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# processing

raw <- read_csv("IA_data.csv") %>%
  clean_names(case = "snake")


output <- raw %>%
  select(fips, county, week_ending, initial_claims) %>%
  rename(
    county_fips = fips,
    date = week_ending,
    claims = initial_claims
  ) %>%
  mutate(

    # dates
    date = mdy(date),
    week = week(date),
    month = month(date),
    year = year(date),

    # set general info
    state = "Iowa",
    state_fips = 19,
    state_short = "IA"
  ) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week) %>%
  filter(year >= 2019)

# output
write.csv(output, file = "IA_compiled.csv", row.names = FALSE)

