# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing

raw <- read_excel("WI_data.xlsx") %>%
  slice(1:73) %>%
  select(-"Grand Total")

# import FIPS codes

data(county.fips)

output <- raw %>%
  pivot_longer(
    cols = starts_with("UI"), names_to = "week",
    names_prefix = "UI Week ", values_to = "claims"
  ) %>%
  clean_names("snake") %>%
  mutate(

    # set general info
    state = "Wisconsin",
    state_fips = 55,
    state_short = "WI",
    county = str_to_title(county),

    # modify dates

    date = ceiling_date(
      ymd("2020-01-01") + weeks(
        as.numeric(week) - 1
      ), "week"
    ) - 1,
    week = week(date),
    month = month(date),
    year = year(date),

    # County FIPS code and name

    polyname = case_when(
      TRUE ~ paste("wisconsin,", tolower(county), sep = "")
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
write.csv(output, file = "WI_compiled.csv", row.names = FALSE)
