# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw_county <- read_csv("CA_county_data.csv") %>%
  clean_names(case = "snake") 

data(county.fips)

output_county <- raw_county %>%
  mutate(
    
    state_fips = "06",
    state_short = "CA",
    state = "California",
    
    # work on FIPS
    polyname = case_when(
      area_type == "county" ~ paste("california,", tolower(str_remove(area_name, " County")), sep = "")
    )
  ) %>% # Join with FIPS
  
  filter(seasonally_adjusted_y_n == "N") %>%
  
  left_join(county.fips, by = "polyname") %>%
  
  # pad zeros
  
  mutate(
    fips = str_pad(fips, 5, pad = "0")
  ) %>%
  rename(
    area = area_name,
    period = month_2
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    employment, labor_force, unemployment
  ) %>%
  filter(year >= 2019)

raw_city <- read_csv("CA_city_data.csv") %>%
  clean_names(case = "snake") 

output_city <- raw_city %>%
  mutate(
    
    state_fips = "06",
    state_short = "CA",
    state = "California",
    area_type = "city",
    fips = NA
  )  %>%
  filter(str_detect(area_name, "city")) %>%
  filter(seasonally_adjusted_y_n == "N") %>%
  rename(
    area = area_name,
    period = month
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    employment, labor_force, unemployment
  ) %>%
  filter(year >= 2019)

raw_state <- read_csv("CA_state_data.csv") %>%
  clean_names(case = "snake") 

output_state <- raw_state %>%
  mutate(
    state_fips = "06",
    state_short = "CA",
    state = "California",
    area_type = "state",
    fips = NA
  )  %>% 
  filter(seasonally_adjusted_y_n == "N") %>%
  rename(
    area = area_name,
    period = month_2
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    employment, labor_force, unemployment
  ) %>%
  filter(year >= 2019)

output <- rbind(output_city, output_county, output_state)

write.csv(output, file="CA_compiled.csv", row.names = FALSE)
