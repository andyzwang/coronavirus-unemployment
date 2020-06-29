# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("LA_data.csv") %>%
  clean_names("snake")


output <- raw %>%
  mutate(
    state_fips = "22",
    state_short = "LA",
    state = "Louisiana",
    year = 2020,
    area_type = case_when(
      str_detect(area, "Parish") ~ "county",
      TRUE ~ "state"
    ),
    polyname = case_when(
      area =="St. Martin Parish" ~ "louisiana,st martin:north",
      area_type == "county" ~ paste("louisiana,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " Parish")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    employment = employed,
    unemployment = unemployed
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "LA_compiled.csv", row.names = FALSE)
