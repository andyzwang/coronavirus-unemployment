# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(tabulizer)


raw <- read_csv("ND_data.csv")


output <- raw %>%
  pivot_longer(
    cols = ends_with("County"), names_to = "county",
    values_to = "claims"
  ) %>%
  clean_names(case = "snake") %>%
  mutate(
    county = str_remove(county, " County"),

    # info
    state = "North Dakota",
    state_short = "ND",
    state_fips = 38,

    # dates
    date = dmy(end_date),
    month = month(date),
    week = week(date),
    year = year(date),

    # Get FIPS

    polyname = case_when(
      county == "LaMoure" ~ "north dakota,la moure",
      TRUE ~ paste("north dakota,", tolower(county), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "ND_compiled.csv", row.names = FALSE)
