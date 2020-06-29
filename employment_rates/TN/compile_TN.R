# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
# import

raw <- read_csv("TN_data.csv")


output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "47",
    state_short = "TN",
    state = "Tennessee",
    area_type = case_when(
      area == "Tennessee" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    period = month(dmy(paste("01", time_period, sep = "")), label = T, abbr = F),
    year = year(dmy(paste("01", time_period, sep = ""))),
    polyname = case_when(
      area_type == "county" ~ paste("tennessee,",
        tolower(str_remove(str_remove(
          area,
          "[[:punct:]]"
        ), " County")),
        sep = ""
      )
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
    labor_force, employment, unemployment
  )  %>%
  filter(!str_detect(area, "-.* Co"))

write.csv(output, file = "TN_compiled.csv", row.names = FALSE)
