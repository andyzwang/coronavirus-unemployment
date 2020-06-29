# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
# import

raw <- read_csv("SC_data.csv")


output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "45",
    state_short = "SC",
    state = "South Carolina",
    year = 2020,
    area_type = case_when(
      area == "South Carolina" ~ "state",
      TRUE ~ "county"
    ),
    polyname = case_when(
      area_type == "county" ~ paste("south carolina,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
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
    employment, labor_force, unemployment
  )

write.csv(output, file = "SC_compiled.csv", row.names = FALSE)
