# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)

# processing

raw <- read_csv("AL_data.csv") %>%
  clean_names(case = "snake")

# import FIPS codes

data(county.fips)

# clean data

output <- raw %>%

  # grab relevant portions

  select(wed_title, county, ui) %>%
  rename(claims = ui) %>%
  mutate(

    # modify dates
    date = mdy(wed_title),
    week = week(date),
    month = month(date),
    year = year(date),

    # state FIPS code and name

    state_fips = "1",
    state_short = "AL",
    state = "Alabama",

    # County FIPS code and name

    polyname = case_when(
      county == "DeKalb" ~ "alabama,de kalb",
      county == "St. Clair" ~ "alabama,st clair",
      TRUE ~ paste("alabama,", tolower(county), sep = "")
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
  arrange(week) %>%
  filter(county != "Total")

write.csv(output, file="AL_compiled.csv", row.names = FALSE)

