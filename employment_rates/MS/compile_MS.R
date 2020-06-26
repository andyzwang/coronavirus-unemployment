# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("MS_data_gathered.csv") 

data(county.fips)

output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "28",
    state_short = "MS",
    state = "Mississippi",
    area_type = case_when(
      area == "Mississippi" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    year = 2020,
    polyname = case_when(
      area == "DeSoto County" ~ "mississippi,de soto",
      area_type == "county" ~ paste("mississippi,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname")  %>%
  rename(
    employment = employed,
    unemployment = unemployed
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  )

write.csv(output, file = "MN_compiled.csv", row.names = FALSE)
