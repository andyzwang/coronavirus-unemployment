# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing
# NOTE: range is manually set

raw <- read_excel("AZ_data.xlsx", range = "A4:S21")

# import FIPS codes

data(county.fips)

output <- raw %>%

  # remove unnecessary columns

  select(-"Not Coded", -"Arizona", -"Unknown") %>%

  # pivot long

  pivot_longer(
    cols = ends_with("County"), names_to = "county",
    values_to = "claims"
  ) %>%
  mutate(

    # set general info
    state = "Arizona",
    state_fips = 4,
    state_short = "AZ",
    county = str_remove(county, " County"),

    # modify dates
    date = ymd(Date),
    week = week(date),
    month = month(date),
    year = year(date),

    # County FIPS code and name

    polyname = case_when(
      TRUE ~ paste("arizona,", tolower(county), sep = "")
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
write.csv(output, file = "AZ_compiled.csv", row.names = FALSE)
