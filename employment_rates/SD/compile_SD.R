# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
# import

raw <- read_csv("SD_data.csv")


output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "46",
    state_short = "SD",
    state = "South Dakota",
    area_type = case_when(
      area == "South Dakota" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    period = month(dmy(paste("01", time_period, sep = "")), label = T, abbr = F),
    year = year(dmy(paste("01", time_period, sep = ""))),
    polyname = case_when(
      area_type == "county" ~ paste("south dakota,",
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
    employment, labor_force, unemployment
  )  %>%
  filter(!str_detect(area, "-.* Co"))

write.csv(output, file = "SD_compiled.csv", row.names = FALSE)
