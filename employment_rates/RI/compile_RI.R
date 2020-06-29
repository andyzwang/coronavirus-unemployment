# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)
# import

raw <- read_excel("RI_data.xlsx")


output <- raw %>%
  filter(!is.na(AREA)) %>%
  pivot_longer(cols = starts_with("43"), names_to = "date", values_to = "value") %>%
  clean_names("snake") %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  clean_names("snake") %>%
  mutate(
    area = str_to_title(area),
    date = excel_numeric_to_date(as.numeric(date)),
    state_fips = "44",
    state_short = "RI",
    state = "Rhode Island",
    year = 2020,
    period = month(date, label = T, abbr = F),
    area_type = case_when(
      str_detect(area, "County") ~ "county",
      area == "Rhode Island" ~ "state",
      TRUE ~ "city"
    ),
    polyname = case_when(
      area_type == "county" ~ paste("rhode island,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%
  
  # Join with FIPS
  
  left_join(county.fips, by = "polyname") %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )
write.csv(output, file = "RI_compiled.csv", row.names = FALSE)
