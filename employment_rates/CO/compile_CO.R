# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw_counties <- read_csv("CO_counties_data.csv") %>%
  clean_names(case = "snake") 


output_counties <- raw_counties %>%
  mutate(
    # set basics
    
    area_type = "county",
    
    state_fips = "08",
    state_short = "CO",
    state = "Colorado",
    
    # work on FIPS
    polyname = paste("colorado,", tolower(str_remove(area, " County")), sep = "")
  ) %>% # Join with FIPS
  
  left_join(county.fips, by = "polyname") %>%
  
  # pad zeros
  
  mutate(
    fips = str_pad(fips, 5, pad = "0")
  ) %>%
  rename(
    unemployment = unemployed,
    employment = employed,
    period = month
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    employment, labor_force, unemployment
  )

raw_state <- read_csv("CO_state_data.csv") %>%
  clean_names(case = "snake") 

output_state <- raw_state %>%
  mutate(
    # set basics
    
    area_type = "state",
    area = "Colorado",
    state_fips = "08",
    state_short = "CO",
    state = "Colorado",
    fips = NA,
    period = str_remove(time_period, ", 2020"),
    year = 2020
  ) %>% 
  rename(
    unemployment = unemployed,
    employment = employed,
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    employment, labor_force, unemployment
  )

output <- rbind(output_counties, output_state)

write.csv(output, file="CO_compiled.csv", row.names = FALSE)
