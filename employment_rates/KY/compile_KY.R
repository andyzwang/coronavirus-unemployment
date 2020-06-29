# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# import

raw <- read_excel("KY_data.xlsx", sheet="County & MSA Level (NSA)") %>%
  clean_names("snake")


output <- raw %>%
  filter(!is.na(employment)) %>%
  filter(!str_detect(area_title, "MSA")) %>%
  mutate(
    state_fips = "21",
    state_short = "KY",
    state = "Kentucky",
    area = area_title,
    year = 2020,
    period = month,
    area_type = case_when(
      str_detect(area, "County") ~ "county",
      TRUE ~ "state"
    ),
    polyname = case_when(
      area_type == "county" ~ paste("kentucky,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    labor_force = civilian_labor_force
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "KY_compiled.csv", row.names = FALSE)

