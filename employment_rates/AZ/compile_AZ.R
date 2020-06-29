# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("AZ_data.csv") %>%
  clean_names(case = "snake") 


output <- raw %>%
  filter(year >= 2019) %>%
  filter(areatype != 0) %>%
  filter(areatype == 1 | areatype == 4 | areatype == 20) %>%
  filter(period!="Annual Average") %>%
  mutate(
    # set basics
    
    area_type = case_when(
      areatype == 1 ~ "state",
      areatype == 4 ~ "county",
      areatype == 20 ~ "city",
    ),
    
    state_fips = "04",
    state_short = "AZ",
    state = "Arizona",
    
    # work on FIPS
    fips = case_when(
      area_type == "county" ~ 
        paste("04", str_pad(area_fips, 3, pad = "0"), sep = "")
    )
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  ) %>%
  filter(!str_detect(area, ",.*part"))

write.csv(output, file="AZ_compiled.csv", row.names = FALSE)

