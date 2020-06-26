# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# import

raw <- read_excel("NH_data.xlsx", skip = 2)

output <- raw %>%
  pivot_longer(cols = !Area:Metric, names_to = "date", values_to = "value") %>%
  clean_names("snake") %>%
  filter(!is.na(area)) %>%
  filter(!is.na(value)) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "33",
    state_short = "NH",
    state = "New Hampshire",
    area_type = case_when(
      area == "New Hampshire" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    period = month(dmy(paste("1-", date, sep="")), label = T, abbr = F),
    year = year(dmy(paste("1-", date, sep=""))),
    polyname = case_when(
      area_type == "county" ~ paste("new hampshire,", 
                                    tolower(str_remove(str_remove(area, 
                                                                  "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  )

write.csv(output, file = "NH_compiled.csv", row.names = FALSE)
