# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw_city_county <- read_csv("ME_data.csv") %>%
  clean_names("snake")

raw_state <- read_csv("ME_state_data.csv") %>%
  clean_names("snake") %>%
  select(-type, -employment_population_ratio_sa_only, -participation_rate_sa_only)

raw <- rbind(raw_city_county, raw_state)

output <- raw %>%
  filter(year >= 2019) %>%
  filter(geography == "State" | geography == "County" | geography == "City/Town") %>%
  mutate(
    area_type = case_when(
      geography == "City/Town" ~ "city",
      geography == "State" ~ "state",
      geography == "County" ~ "county"
    ),
    relevant = case_when(
      civilian_labor_force >= 10000 ~ "YES",
      geography == "state" ~ "YES",
      geography == "county" ~ "Yes"
    ),
    state_fips = "23",
    state_short = "ME",
    state = "Maine",
    period = month,
    polyname = case_when(
      area_type == "county" ~ paste("maine,", tolower(str_remove(str_remove(area_name, "[[:punct:]]"), " Cty")), sep = "")
    ),
    area = str_replace(area_name, "Cty", "County")
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    labor_force = civilian_labor_force
  ) %>%

  # note: optional to filter out irrelevant small cities
  filter(relevant == "YES") %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "ME_compiled.csv", row.names = FALSE)
