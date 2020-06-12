# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(tabulizer)

# processing

raw <- extract_tables("HI_data.pdf", output = "data.frame")[[1]] %>%
  slice(2:n()) %>%
  row_to_names(row_number = 1, remove_row = T)


# create FIPS codes

hawaii.fips <- tibble(
  county = c("Oahu","Hawaii","Maui","Kauai", "Unknown"),
  county_fips = c(15003, 15001, 15009, 15007, NA)
)

output <- raw %>%
  
  rename(Unknown = "Agent *") %>%
  
  # take only top-level counties
  
  select(date, OAHU, HAWAII, MAUI, KAUAI, Unknown) %>%
  
  # pivot long
  
  pivot_longer(
    cols = OAHU:Unknown, names_to = "county",
    values_to = "claims"
  )  %>%
  
  mutate(
    
    # make claims numeric
    
    claims = as.integer(claims),
    
    # set general info
    state = "Hawaii",
    state_fips = 15,
    state_short = "HI",
    county = str_to_title(county),
    
    # modify dates
    date = ymd(date),
    month = month(date),
    year = year(date),
    week = week(date)
  ) %>%
  
  # combine with FIPS
  
  left_join(hawaii.fips, by = "county") %>%
  
  # relevant columns
  
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "HI_compiled.csv", row.names = FALSE)
