# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("IA_data.csv") %>%
  clean_names("snake")


output <- raw %>%
  filter(areatyname == "Cities" | areatyname == "County" | areatyname == "State") %>%
  filter(adjusted == 0) %>%
  filter(year >= 2019) %>%
  mutate(
    state_fips = "19",
    state_short = "IA",
    state = "Iowa",
    area = areaname,

    period = month,
    area_type = case_when(
      areatyname == "Cities" ~ "city",
      areatyname == "County" ~ "county",
      areatyname == "State" ~ "state"
    ),
    polyname = case_when(
      area_type == "county" ~ paste("iowa,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    employment = emp,
    unemployment = unemp,
    labor_force = laborforce
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  )

write.csv(output, file = "IA_compiled.csv", row.names = FALSE)
