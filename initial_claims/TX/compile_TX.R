# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing
# NOTE: range is manually set

raw <- read_excel("TX_data.xlsx", range = "A3:T257")

# import FIPS codes

data(county.fips)

output <- raw %>%

  # pivot long

  pivot_longer(
    cols = !starts_with("County"), names_to = "date",
    values_to = "claims"
  ) %>%
  mutate(

    # set general info
    state = "Texas",
    state_fips = 48,
    state_short = "TX",
    county = County,

    # modify dates
    date = mdy(date),
    week = week(date),
    month = month(date),
    year = year(date),

    # County FIPS code and name

    polyname = case_when(
      county == "Galveston" ~ "texas,galveston:main",
      TRUE ~ paste("texas,", tolower(county), sep = "")
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
write.csv(output, file = "TX_compiled.csv", row.names = FALSE)
