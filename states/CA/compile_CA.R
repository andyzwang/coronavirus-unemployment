# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(tabulizer)

# processing
# NOTE: columns manually set

raw <- extract_tables("CA_data.pdf", output = "data.frame")[[1]] %>%
  remove_empty("cols") %>%
  select(County, January.2020, February.2020, March.2020, April.2020)

# import FIPS codes

data(county.fips)

output <- raw %>%

  # pivot long

  pivot_longer(
    cols = ends_with("2020"), names_to = "Month",
    values_to = "claims"
  ) %>%

  # clean names

  clean_names(case = "snake") %>%

  # remove total

  filter(county != "Total All Counties") %>%

  # make claims numeric

  mutate(
    claims = str_remove(claims, ","),
    claims = as.integer(claims)
  ) %>%

  mutate(

    # set general info
    state = "California",
    state_fips = 6,
    state_short = "CA",
    county = str_to_title(county),

    # modify dates
    date = ceiling_date(
      myd(paste0(month, ".01")),
      unit = "month"
    ) - 1,
    month = month(date),
    year = year(date),
    week = NA,
    
    # create polyname 
    
    polyname = paste("california,", tolower(county), sep = "")
  ) %>%
  
  # Join with FIPS codes
  
  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(month)

# output
write.csv(output, file = "CA_compiled.csv", row.names = FALSE)
