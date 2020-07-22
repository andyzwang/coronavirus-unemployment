# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("IN_data.csv", skip = 4) 

data(county.fips)

output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "18",
    state_short = "IN",
    state = "Indiana",

    period = month,
    area = str_remove(geography, ", IN"),
    area_type = case_when(
      str_detect(area, "County") ~ "county",
      area == "Indiana" ~ "state",
      TRUE ~ "city"
    ),
    fips = case_when(
      area_type == "county" ~ paste("18", str_pad(county_fips, 3, pad = "0"), sep = "")
    ),
    area = str_to_title(area),
    area = case_when(
      area == "Michigan City City" ~ "Michigan City",
      str_detect(area, "(Core)") ~ "Indianapolis",
      TRUE ~ str_remove_all(area, " (City|Town|Village)"))
  ) %>%
  filter(!str_detect(area, "(Cons)")) %>%
  rename(
    employment = employed,
    unemployment = unemployed,
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    labor_force, employment, unemployment
  )

write.csv(output, file = "IN_compiled.csv", row.names = FALSE)
