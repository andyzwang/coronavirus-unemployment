# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)


raw <- read_csv("NV_gathered.csv") %>%
  remove_empty("cols")

output <- raw %>%
  rename(
    state = state_name,
    state_short = state_abbrev,
    county = county_name,
    claims = initial_claims
  ) %>%
  mutate(
    date = mdy(date),
    year = year(date),
    week = week(date),
    month = month(date)
  ) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "NV_compiled.csv", row.names = FALSE)
