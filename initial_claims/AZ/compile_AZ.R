# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing
# NOTE: range is manually set

raw <- read_excel("AZ_data.xlsx", range = "A4:S23")

# import FIPS codes

data(county.fips)

output <- raw %>%

  # pivot long

  pivot_longer(
    cols = !starts_with("date"), names_to = "county",
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
  mutate(fips = as.character(fips),
    county_fips = case_when(
      str_count(fips, "4.*") == 1 ~ paste("0", fips, sep = ""),
      TRUE ~ "NA"
    ))%>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "AZ_compiled.csv", row.names = FALSE)

