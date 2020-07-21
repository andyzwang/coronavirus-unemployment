# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing
# NOTE: range is manually set

raw <- read_excel("CT_town_data.xlsx")

# import FIPS codes

data(county.fips)

# city county mapping

town_county <- read_csv("CT_town_county.csv")

output <- raw %>%
  # pivot long
  
  pivot_longer(cols = !starts_with("Town"), 
               values_to = "claims", names_to = "date") %>%
  rename(town = 'Town Name') %>%
  left_join(town_county, by = "town") %>%
  mutate(
    county = case_when(
      town == 'Out of state' ~ "Out of State",
      town == 'unknown' ~ 'unknown',
      TRUE ~ county
    )
  )  %>%
  group_by(county, date) %>%
  summarize(claims = sum(claims)) %>%
  ungroup() %>%
  mutate(
    
    # set general info
    state = "Connecticut",
    state_fips = "09",
    state_short = "CT",
    
    # modify dates
    date = mdy(date),
    week = week(date),
    month = month(date),
    year = year(date),
  ) %>%
  filter(year >= 2019, !is.na(county)) %>%
  mutate(
    
    # County FIPS code and name
    
    polyname = case_when(
      TRUE ~ paste("connecticut,", tolower(county), sep = "")
    )
  ) %>%
  # Join with FIPS
  left_join(county.fips, by = "polyname") %>%
  mutate(fips = as.character(fips),
         county_fips = case_when(
           str_count(fips, "4.*") == 1 ~ paste("0", fips, sep = ""),
           TRUE ~ "NA"
         ))%>%
  
  # relevant columns
  
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "CT_compiled.csv", row.names = FALSE)

