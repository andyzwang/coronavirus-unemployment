# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# processing

raw <- read_csv("MA_data.csv") %>%
  remove_empty("cols") 


# import FIPS codes

data(county.fips)

# import regions

towns <- read_csv("MA_towns.csv")

# Converting the individual town claims to county claims.

output <- raw %>%
  clean_names("snake") %>%
  mutate(town = str_remove(area_name, "( City| Town|, County/Town)$")) %>%
  left_join(towns) %>%
  mutate(
    
    # make claims numeric
    
    claims = as.integer(claims),
    
    # set general info
    state = "Massachusetts",
    state_fips = 25,
    state_short = "MA",
    
    # modify dates
    date = mdy(week_ending_date),
    month = month(date),
    year = year(date),
    week = week(date),
    
    polyname = case_when(
      TRUE ~ paste("massachusetts,", tolower(county), sep = "")
    )
  ) %>%
  
  # combine with FIPS
  
  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%
  
  # relevant columns
  
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

write.csv(output, file = "TN_compiled.csv", row.names = FALSE)
