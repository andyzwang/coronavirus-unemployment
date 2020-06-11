# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing
# NOTE: range is manually set

raw <- read_excel("OR_data.xlsx", skip = 6, na = "-c-") %>%
  clean_names(case = "snake")

# import FIPS codes

data(county.fips)

output <- raw %>%
  select(week_ending, area, total) %>%
  rename(
    date = week_ending,
    county = area,
    claims = total
  ) %>%
  filter(county != "Oregon") %>%
  mutate(

    # set general info
    state = "Oregon",
    state_fips = 41,
    state_short = "OR",
    county = str_remove(county, " County"),

    # modify dates
    date = ymd(date),
    week = week(date),
    month = month(date),
    year = year(date),

    # County FIPS code and name

    polyname = case_when(
      TRUE ~ paste("oregon,", tolower(county), sep = "")
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
write.csv(output, file = "OR_compiled.csv", row.names = FALSE)
