# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import
raw <- read_csv("MD_gathered.csv")

# import FIPS codes

data(county.fips)

output <- raw %>%
  pivot_longer(cols = !starts_with("county"), names_to = "date", values_to = "claims") %>%
  mutate(
    
    # date info
    
    date = mdy(date),
    week = week(date),
    month = month(date),
    year = year(date),
    
    # set general info
    state = "Maryland",
    state_fips = 24,
    state_short = "MD",
    
    polyname = case_when(
      county == "Baltimore County" ~ "maryland,baltimore",
      county == "Prince George's" ~ "maryland,prince georges",
      county == "Queen Anne's" ~ "maryland,queen annes",
      county == "St. Mary's" ~ "maryland,st marys",
      TRUE ~ paste("maryland,", tolower(county), sep = "")
    )
  ) %>%
  
  # Join with FIPS
  
  left_join(county.fips, by = "polyname") %>%
  rename(
    county_fips = fips
  ) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

# output
write.csv(output, file = "MD_compiled.csv", row.names = FALSE)
