# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# import

raw <- read_excel("CT_data.xlsx", range = "A1:I913")



output <- raw %>%
  pivot_longer(cols = contains("2020"), names_to = "period", values_to = "value") %>%
  clean_names("snake") %>%
  filter(str_detect(area_title, "County") | str_detect(area_title, "city") | str_detect(area_title, "town") | str_detect(area_title, "Connecticut")) %>%
  filter(!is.na(data_type)) %>%
  mutate(
    # remove prelim or standardized
    period = str_remove(period, " \\(.\\)")
  ) %>%
  # pivot wider
  pivot_wider(names_from = data_type, values_from = value) %>%
  clean_names("snake") %>%
  mutate(
    # set basics
    
    area_type = case_when(
      str_detect(area_title, "County") ~ "county",
      str_detect(area_title, "city") ~ "city",
      str_detect(area_title, "town") ~ "city",
      str_detect(area_title, "Connecticut") ~ "state"
    ),
    
    state_fips = "09",
    state_short = "CT",
    state = "Connecticut",
    
    year = str_match(period, "\\d{4}"),
    period = str_remove(period, " \\d{4}"),
    area = str_remove(area_title, ", CT"),
    
    # work on FIPS
    polyname = paste("connecticut,", tolower(str_remove(area, " County")), sep = ""),
  ) %>% # Join with FIPS
  
  left_join(county.fips, by = "polyname") %>%
  
  # pad zeros
  
  mutate(
    fips = str_pad(fips, 5, pad = "0")
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )


write.csv(output, file="CT_compiled.csv", row.names = FALSE)
