# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing

raw <- read_csv("LA_parish_data.csv")

# import FIPS codes

data(county.fips)

output <- raw %>%

  clean_names(case = "snake")  %>%
  filter(claim_type1 == "Initial") %>%
  mutate(

    # change dates out of Excel format
    date = mdy(week_ending),

    # set general info
    state = "Louisiana",
    state_fips = 22,
    state_short = "LA",

    # modify dates

    week = week(date),
    month = month(date),
    year = year(date),

    # fix some incorrect names
    county = str_remove(parish1, " Parish"),

    # create polyname

    polyname = case_when(
      county == "St. Martin" ~ "louisiana,st martin:north",
      TRUE ~ paste("louisiana,", tolower(str_remove(county, "\\.")), sep = "")
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
  arrange(month)

# output
write.csv(output, file = "LA_compiled.csv", row.names = FALSE)

