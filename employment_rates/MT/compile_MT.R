# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("MT_data.csv")

output <- raw %>%
  clean_names("snake") %>%
  select(-area) %>%
  mutate(
    state_fips = stfips,
    state_short = "MT",
    state = "Montana",
    area_type = str_to_lower(area_type),
    area_type = case_when(
      str_detect(area_type, "cities") ~ "city",
      TRUE ~ area_type
    ),
    area = area_2, 
    polyname = case_when(
      area_type == "county" ~ paste("montana,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
    rename(unemployment = unemployed,
           employment = employed) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "MT_compiled.csv", row.names = FALSE)
