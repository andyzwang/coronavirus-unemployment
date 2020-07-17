# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)
# import

raw <- read_excel("NC_data.xlsx")


output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "37",
    state_short = "NC",
    state = "North Carolina",
    area = area_name,
    area_type = case_when(
      area == "North Carolina" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    period = month(as.numeric(month), label = T, abbr = F),
    polyname = case_when(
      area_type == "county" ~ paste("north carolina,",
        tolower(str_remove(str_remove(
          area,
          "[[:punct:]]"
        ), " County")),
        sep = ""
      )
    ),
    area = str_remove(area, " City"),
    area = str_remove(area, " Town")
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
  ) %>%
  filter(!str_detect(area, "County Part")) %>%
  filter(!str_detect(area, "-.*County")) %>%
  filter(!str_detect(area, "-.*Cty")) %>%
  filter(!is.na(period))

write.csv(output, file = "NC_compiled.csv", row.names = FALSE)
