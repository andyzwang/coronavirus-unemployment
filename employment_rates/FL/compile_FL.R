# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# import

raw_counties <- read_excel("FL_LAUS_data.xlsx", skip = 5)

data(county.fips)

output_counties <- raw_counties %>%
  clean_names("snake") %>%
  filter(!str_detect(area, "MSA") & !str_detect(area, "MD")) %>%
  mutate(
    # set basics
    area_type = case_when(
      str_detect(area, "County") ~ "county",
      TRUE ~ "state"
    ),

    state_fips = "12",
    state_short = "FL",
    state = "Florida",

    year = yr,
    period = month(mo, label = TRUE, abbr = FALSE),

    # work on FIPS
    polyname = case_when(
      area == "DeSoto County" ~ "florida,de soto",
      area == "Okaloosa County" ~ "florida,okaloosa:main",
      area == "St. Johns County" ~ "florida,st johns",
      area == "St. Lucie County" ~ "florida,st lucie",
      TRUE ~ paste("florida,", tolower(str_remove(area, " County")), sep = "")
    ),
  ) %>% # Join with FIPS

  left_join(county.fips, by = "polyname") %>%

  # pad zeros

  mutate(
    fips = str_pad(fips, 5, pad = "0")
  ) %>%
  rename(
    labor_force = lf,
    employment = emp,
    unemployment = unemp
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

raw_cities <- read_excel("FL_cities_data.xlsx")

output_cities <- raw_cities %>%
  filter(!is.na(City)) %>%
  remove_empty("cols") %>%
  pivot_longer(
    cols = JAN:MAY, names_to = "month_label",
    values_to = "value"
  ) %>%
  clean_names("snake") %>%
  pivot_wider(names_from = "year_2020", values_from = "value") %>%
  clean_names("snake") %>%
  mutate(
    # set basics
    area_type = "city",
    state_fips = "12",
    state_short = "FL",
    state = "Florida",

    year = 2020,
    period = month(mdy(paste0(month_label, "-1-20", sep = "")),
      label = TRUE, abbr = FALSE
    ),

    # work on FIPS
    fips = NA,
    area = str_to_title(city),
    area = str_remove(area, " City")
    
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

output <- rbind(output_counties, output_cities)
write.csv(output, file = "FL_compiled.csv", row.names = FALSE)
