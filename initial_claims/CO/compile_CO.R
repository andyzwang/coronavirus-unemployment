# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing
# NOTE: range is manually set

raw_regular <- read_excel("CO_regular_UI_data.xlsx", range = "A2:BO20", na = "***") 

# import FIPS codes

data(county.fips)

output_regular <- raw_regular %>%
  
  # pivot long
  
  pivot_longer(
    cols = Adams:Yuma, names_to = "county",
    values_to = "claims"
  ) %>%
  
  #remove total
  
  select(-'Colorado Total') %>%
  clean_names(case = "snake") %>%
  filter(week_number!="N/A") %>%
  
  mutate(
    
    # set general info
    state = "Colorado",
    state_fips = 8,
    state_short = "CO",
    
    # modify dates
    date = excel_numeric_to_date(as.numeric(week_ending_date)),
    week = week(date),
    month = month(date),
    year = year(date),
    
    # County FIPS code and name
    
    polyname = paste("colorado,", tolower(county), sep = "")
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%
  
  # relevant columns
  
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  
  arrange(week) %>%
  mutate(
    metric = "regular"
  )

raw_pua <- read_excel("CO_pua_ui_data.xlsx", range = "A2:BO13", na = "***") 

output_pua <- raw_pua %>%
  
  # pivot long
  
  pivot_longer(
    cols = Adams:Yuma, names_to = "county",
    values_to = "claims"
  ) %>%
  
  #remove total
  
  select(-'Colorado Total') %>%
  clean_names(case = "snake") %>%
  
  mutate(
    
    # set general info
    state = "Colorado",
    state_fips = 8,
    state_short = "CO",
    
    # modify dates
    date = ymd(week_ending_date),
    week = week(date),
    month = month(date),
    year = year(date),
    
    # County FIPS code and name
    
    polyname = paste("colorado,", tolower(county), sep = "")
  ) %>%
  
  # Join with FIPS
  
  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%
  
  # relevant columns
  
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  
  arrange(week) %>%
  mutate(
    metric = "pua"
  )

raw <- rbind(output_pua, output_regular)

output <- raw %>%
  pivot_wider(names_from = "metric", values_from = "claims") %>%
  mutate(
    pua = case_when(
      is.na(pua) ~ 0,
      TRUE ~ pua
    ),
    regular = case_when(
      is.na(regular) ~ 0,
      TRUE ~ regular
    ),
  ) %>%
  mutate(claims = pua + regular) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  
  arrange(week)

# output
write.csv(output, file = "CO_compiled.csv", row.names = FALSE)
