# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("MI_data.csv")

output <- raw %>%
  clean_names("snake") %>%
  select(-area) %>%
  mutate(
    state_fips = stfips,
    state_short = "MI",
    state = "Michigan",
    area_type = str_to_lower(area_type),
    area = str_remove(area_2, ", MI"),
    polyname = case_when(
      area_type == "county" ~ paste("michigan,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
    rename(unemployment = unemployed,
           employment = employed) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  )

write.csv(output, file = "MI_compiled.csv", row.names = FALSE)
