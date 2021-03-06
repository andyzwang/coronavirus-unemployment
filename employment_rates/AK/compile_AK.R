# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

raw <- read_csv("AK_data.csv", skip = 2) %>%
  clean_names(case = "snake") 

output <- raw %>%
  
  filter(year >= 2019) %>%
  filter(area_type != 60) %>%
  filter(period != 0) %>%
  mutate(
    # set basics
    
    area = area_name,
    area_type = case_when(
      area_type == 1 ~ "state",
      area_type == 4 ~ "county",
      area_type == 2 ~ "city",
    ),
    
    state_fips = "02",
    state_short = "AK",
    state = "Alaska",
    
    # work on FIPS
    fips = case_when(
      area_type == "county" ~ 
        paste("02", str_pad(area_code, 3, pad = "0"), sep = "")
    ),
    
    # dates
    period = month
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file="AK_compiled.csv", row.names = FALSE)
