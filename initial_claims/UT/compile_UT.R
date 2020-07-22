# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# processing
# note: the file is pretty large, it might take a moment

raw <- read_csv("UT_data.csv")

# import FIPS codes

data(county.fips)

output <- raw %>%
  
  clean_names("snake") %>%
  
  filter(year >= 2019) %>%
  
  # clean up datetime format
  
  mutate(
    date = ceiling_date(mdy(str_remove(week_date, " 0:00")), "week") -1
  ) %>%
  
  # group by date and county
  
  group_by(area, date) %>%
  summarize(claims = sum(claim)) %>%
  ungroup() %>%
  mutate(

    county = str_remove(area, " County"),
    # set general info
    state = "Utah",
    state_fips = 49,
    state_short = "UT",

    # modify dates
    week = week(date),
    month = month(date),
    year = year(date),

    # County FIPS code and name

    polyname = case_when(
      TRUE ~ paste("utah,", tolower(county), sep = "")
    )
  ) %>%
  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  filter(county != "State of Utah") %>%
  arrange(week)

# output
write.csv(output, file = "UT_compiled.csv", row.names = FALSE)
