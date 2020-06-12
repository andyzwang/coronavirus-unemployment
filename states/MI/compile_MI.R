# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import CSV

raw <- read_csv("MI_data.csv")

# import FIPS codes

data(county.fips)

output <- raw %>%

  pivot_longer(cols = !starts_with("county"), names_to = "date", 
               values_to = "claims") %>%
  mutate(

    # set general info
    state = "Michigan",
    state_fips = 26,
    state_short = "MI",

    # modify dates
    date = mdy(date),
    month = month(date),
    year = year(date),
    week = week(date),

    # create polyname

    polyname = case_when(
      TRUE ~ paste("michigan,", 
                   tolower(str_remove_all(county, "[[:punct:]]")), 
                   sep = "")
    )
  )  %>%
  
  # Join with FIPS codes

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%

  arrange(week)

# output
write.csv(output, file = "MI_compiled.csv", row.names = FALSE)

