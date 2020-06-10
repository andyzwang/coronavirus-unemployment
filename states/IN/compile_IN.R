# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# processing

raw <- read_csv("IN_data.csv", skip = 4) %>%
  clean_names(case = "snake") %>%
  filter(pct_change_from_same != "Week Last Year")

# import FIPS codes

data(county.fips)


output <- raw %>%
  select(geography, state_fips, year, month_code, week, initial_claims) %>%

  # rename columns
  rename(
    month = month_code,
    claims = initial_claims,
    county = geography
  ) %>%
  mutate(
    # remove "County, IN"
    county = str_remove_all(county, " County, IN"),

    # get date

    date = ceiling_date(ymd("2020-01-01") + weeks(week - 1), "week") - 1,

    # set general info
    state = "Indiana",
    state_fips = 18,
    state_short = "IN",

    polyname = case_when(
      county == "DeKalb" ~ "indiana,de kalb",
      county == "LaPorte" ~ "indiana,la porte",
      county == "St. Joseph" ~ "indiana,st joseph",
      TRUE ~ paste("indiana,", tolower(county), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    county_fips = fips
  ) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "IN_compiled.csv", row.names = FALSE)
