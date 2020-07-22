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
  mutate(
    state_fips = 30,
    state_short = "MT",
    state = "Montana",
    area_type = case_when(
      area == "Montana" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    polyname = case_when(
      area_type == "county" ~ paste("montana,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    ),
    area = case_when(
      area_type =="county" ~ paste(area, "County", sep=" "),
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
  )

write.csv(output, file = "MT_compiled.csv", row.names = FALSE)
