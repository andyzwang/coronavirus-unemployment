# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("MO_data.csv") 

data(county.fips)

output <- raw %>%
  clean_names("snake") %>%
  filter(!str_detect(area, "County Part")) %>%
  filter(!str_detect(area, "County part")) %>%
  mutate(
    state_fips = "29",
    state_short = "MO",
    state = "Missouri",
    area_type = case_when(
      area == "Missouri" ~ "state",
      area == "St. Louis City" ~ "county",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    period = month,
    polyname = case_when(
      area == "Dekalb County" ~ "missouri,de kalb",
      area_type == "county" ~ paste("missouri,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname")  %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  )

write.csv(output, file = "MO_compiled.csv", row.names = FALSE)
