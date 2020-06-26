# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# import

raw_county <- read_excel("NJ_counties_data.xlsx", skip = 11)

output_county <- raw_county %>%
  filter(!is.na(Area)) %>%
  pivot_longer(cols = !Area:Metric, names_to = "date", values_to = "value") %>%
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
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  )

write.csv(output, file = "NJ_compiled.csv", row.names = FALSE)
