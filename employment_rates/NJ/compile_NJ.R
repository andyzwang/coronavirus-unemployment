# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# import

raw <- read_excel("NJ_data.xlsx", skip = 11)

output <- raw %>%
  filter(!is.na(area)) %>%
  pivot_longer(cols = !area:metric, names_to = "date", values_to = "value") %>%
  clean_names("snake") %>%
  filter(!is.na(area)) %>%
  filter(!is.na(value)) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "34",
    state_short = "NJ",
    state = "New Jersey",
    area_type = case_when(
      area == "New Jersey" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    area = str_remove(area, ", NJ"),
    period = month(mdy(paste(date, "-1-2020", sep = "")), label = T, abbr = F),
    year = 2020,
    polyname = case_when(
      area_type == "county" ~ paste("new jersey,",
        tolower(str_remove(str_remove(
          area,
          "[[:punct:]]"
        ), " County")),
        sep = ""
      )
    ),
    area =str_remove(area, " borough"),
    area = str_remove(area, "ship"),
    area = case_when(
      area == "Washington town" ~ "Washington (NJ)",
      TRUE ~ area
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "NJ_compiled.csv", row.names = FALSE)
