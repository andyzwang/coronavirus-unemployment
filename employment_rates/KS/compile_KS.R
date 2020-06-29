# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("KS_data.csv") %>%
  clean_names("snake")


output <- raw %>%
  mutate(
    state_fips = "20",
    state_short = "KS",
    state = "Kansas",
    area = str_to_title(area),
    year = 2020,
    polyname = case_when(
      area_type == "county" ~ paste("kansas,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    employment = employed,
    unemployment = unemployed,
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "KS_compiled.csv", row.names = FALSE)
