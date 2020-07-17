# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)
# import

raw <- read_excel("TX_data.xlsx")


output <- raw %>%
  clean_names("snake") %>%
  filter(adjustment == "Not Adjusted") %>%
  mutate(
    state_fips = "48",
    state_short = "TX",
    state = "Texas",
    area_type = case_when(
      area_type == "Texas" ~ "state",
      TRUE ~ str_to_lower(area_type)
    ),
    polyname = case_when(
      area =="DeWitt" ~ "texas,de witt",
      area=="Galveston"~"texas,galveston:main",
      area_type == "county" ~ paste("texas,",
        tolower(str_remove(str_remove(
          area,
          "[[:punct:]]"
        ), " County")),
        sep = ""
      )
    ),
    area = case_when(
      area_type == "county" ~ paste(trimws(area), "County", sep = " "),
      TRUE ~ as.character(area)
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    labor_force = civilian_labor_force
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "TX_compiled.csv", row.names = FALSE)
