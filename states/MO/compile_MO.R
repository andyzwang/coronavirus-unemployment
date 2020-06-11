# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import CSV

raw <- read_csv("MO_data.csv")

# import FIPS codes

data(county.fips)

output <- raw %>%

  mutate(

    # set general info
    state = "Missouri",
    state_fips = 29,
    state_short = "MO",

    # modify dates
    date = mdy(weekEnding),
    month = month(date),
    year = year(date),
    week = week(date),

    # create polyname

    polyname = case_when(
      county == "DeKalb" ~ "missouri,de kalb",
      TRUE ~ paste("missouri,", 
                   tolower(str_remove_all(county, "[[:punct:]]")), 
                   sep = "")
    )
  )  %>%
  
  # only recent entries
  
  filter(year == 2020) %>%
  
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
write.csv(output, file = "MO_compiled.csv", row.names = FALSE)
