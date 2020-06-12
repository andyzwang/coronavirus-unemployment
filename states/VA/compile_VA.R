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

  filter(FIPS <= 200) %>%

  # pivot long

  pivot_longer(
    cols = ends_with("Claims"), names_to = "date",
    values_to = "claims"
  ) %>%
  rename(county = COUNTY) %>%
  select(county, date, claims) %>%
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

    # County FIPS code and name

    polyname = case_when(
      county == "Accomack" ~ "virginia,accomack:main",
      county == "Albermarle" ~ "virginia,albemarle",
      county == "King & Queen" ~ "virginia,king and queen",
      TRUE ~ paste("virginia,", tolower(county), sep = "")
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
write.csv(output, file = "VA_compiled.csv", row.names = FALSE)
