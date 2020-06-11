# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing
# NOTE: range is manually set

raw <- read_excel("PA_data.xlsx", sheet = "IC by County and Week", skip = 1) %>%
  select(County, starts_with("WE"))

# import FIPS codes

data(county.fips)

output <- raw %>%
  clean_names(case = "snake") %>%

  # pivot long

  pivot_longer(
    cols = !starts_with("County"), names_to = "date",
    values_to = "claims"
  ) %>%
  mutate(

    # set general info
    state = "Pennsylvania",
    state_fips = 42,
    state_short = "PA",
    county = str_remove(county, " County"),
    date = str_remove(date, "we_"),

    # County FIPS code and name

    polyname = case_when(
      TRUE ~ paste("pennsylvania,", tolower(county), sep = "")
    ),

    # modify dates
    date = mdy(date),
    week = week(date),
    month = month(date),
    year = year(date)
  ) %>%


  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week) %>%
  filter(!is.na(claims)) %>%
  filter(county != "Total")

# output
write.csv(output, file = "PA_compiled.csv", row.names = FALSE)
