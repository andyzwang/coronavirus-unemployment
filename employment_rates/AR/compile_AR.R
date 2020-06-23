# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("AR_data.csv") %>%
  clean_names(case = "snake") 


output <- raw %>%
  # pivot wide
  pivot_wider(names_from = measure_names, values_from = measure_values) %>%
  clean_names("snake") %>%
  mutate(
    # set basics
    
    area_type = case_when(
      str_detect(area, "County") == TRUE ~ "County",
      area == "Arkansas" ~ "State",
      TRUE ~ "City"
    ))
    
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
