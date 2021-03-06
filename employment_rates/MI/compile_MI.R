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
  mutate(
    state_fips = 26,
    state_short = "MI",
    state = "Michigan",
    area_type = case_when(
      str_detect(area, "County") ~ "county",
      area == "Michigan" ~ "state",
      TRUE ~ "city"
    ),
    area = str_remove(area, ", MI"),
    polyname = case_when(
      area_type == "county" ~ paste("michigan,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    ),
    area = case_when(
      str_detect(area,"Wyoming") ~ "Wyoming (MI)",
      TRUE ~ area
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
    rename(unemployment = unemployed,
           employment = employed) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  ) %>%
  filter(!str_detect(area, ",.*part"))

write.csv(output, file = "MI_compiled.csv", row.names = FALSE)
