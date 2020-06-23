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
      areatype == 1 ~ "State",
      areatype == 4 ~ "County",
      areatype == 20 ~ "City",
    ),
    
    state_fips = "05",
    state_short = "AZ",
    state = "Arizona",
    
    # work on FIPS
    fips = case_when(
      area_type == "County" ~ 
        paste("05", str_pad(area_fips, 3, pad = "0"), sep = "")
    ),
    
    # dates
    date = ceiling_date(mdy(paste(period, "1", year, sep = "/")), "months") - 1,
    month = month(date),
    year = year(date),
    week = NA
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, date,
    employment, labor_force, unemployment
  )

write.csv(output, file="AZ_compiled.csv", row.names = FALSE)
