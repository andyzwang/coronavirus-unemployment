# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing

raw <- read_excel("ID_data.xlsx", sheet = "County Claims", na = "", skip = 1)

# import FIPS codes

data(county.fips)

output <- raw %>%

  # select relevant

  select(-"Grand Total", -Unclassified) %>%

  # pivot long

  pivot_longer(
    cols = Ada:Washington, names_to = "county",
    values_to = "claims"
  ) %>%
  clean_names(case = "snake") %>%
  mutate(
    # set general info
    state = "Idaho",
    state_fips = 16,
    state_short = "ID",

    # modify dates

    date = ymd(week_end_date),
    week = week(date),
    month = month(date),
    year = year(date),
    polyname = paste("idaho,", tolower(county), sep = "")
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  filter(year == 2020) %>%
  arrange(week)


# output
write.csv(output, file = "ID_compiled.csv", row.names = FALSE)
