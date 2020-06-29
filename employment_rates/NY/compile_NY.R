# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

state <- read_csv("NY_state_data.txt") %>%
  clean_names("snake") %>%
  remove_empty("cols") %>%
  select(-starts_with("unemprate"))

counties <- read_csv("NY_county_data.txt") %>%
  clean_names("snake") %>%
  remove_empty("cols") %>%
  select(-starts_with("unemprate"))

cities <- read_csv("NY_city_data.txt") %>%
  clean_names("snake") %>%
  remove_empty("cols") %>%
  select(-starts_with("unemprate"))

raw <- rbind(state, counties, cities)


output <- raw %>%
  mutate(
    state_fips = "36",
    state_short = "NY",
    state = "New York",
    area_type = case_when(
      area == "BALANCE OF STATE" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    period = month(as.numeric(month), label = T, abbr = F),
    polyname = case_when(
      area_type == "county" ~ paste("new york,",
        tolower(str_remove(str_remove(
          area,
          "[[:punct:]]"
        ), " County")),
        sep = ""
      )
    ),
    area = case_when(
      area == "BALANCE OF STATE" ~ "New York",
      TRUE ~ area
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    employment = emp,
    labor_force = laborforce,
    unemployment = unemp
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  ) %>%
  filter(year >= 2019) %>%
  filter(!is.na(period))

write.csv(output, file = "NY_compiled.csv", row.names = FALSE)
