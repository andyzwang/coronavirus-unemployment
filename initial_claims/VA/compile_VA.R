# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing

raw <- read_excel("VA_data.xlsx", sheet = "Initial by County and City", skip = 1)

# import FIPS codes

data(county.fips)

output <- raw %>%

  # remove cities

  # pivot long

  pivot_longer(
    cols = ends_with("Claims"), names_to = "date",
    values_to = "claims"
  ) %>%
  rename(county = COUNTY) %>%
  select(county, date, claims, FIPS) %>%
  mutate(

    # set general info
    state = "Virginia",
    state_fips = 51,
    state_short = "VA",
    county = str_remove(county, " County"),

    # modify dates
    date = mdy(paste(str_remove(date, " Claims"), "2020", sep = " ")),
    week = week(date),
    month = month(date),
    year = year(date),

    county_fips = paste("51", str_pad(FIPS, 3, pad = "0"), sep = "")
  ) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "VA_compiled.csv", row.names = FALSE)
