# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing
# NOTE: range is manually set

raw <- read_excel("NE_data.xlsx", sheet = "County", skip = 2, na = "*")

# import FIPS codes

data(county.fips)

output <- raw %>%
  
  # pivot long
  
  pivot_longer(
    cols = !starts_with("County"), names_to = "Date",
    values_to = "claims"
  ) %>%
  
  clean_names(case = "snake") %>%
  
  mutate(
    
    county = county_name,
    # set general info
    state = "Nebraska",
    state_fips = 31,
    state_short = "NE",
    
    # modify dates
    
    date = excel_numeric_to_date(as.numeric(date)),
    week = week(date),
    month = month(date),
    year = year(date),
    
    # County FIPS code and name
    
    polyname = paste("nebraska,", tolower(county), sep = "")
  )  %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%
  
  # relevant columns
  
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "NE_compiled.csv", row.names = FALSE)
