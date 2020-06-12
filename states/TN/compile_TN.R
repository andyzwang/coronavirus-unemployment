# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# processing

raw <- read_csv("TN_gathered.csv") %>%
  remove_empty("cols")

# import regions
regions <- read_csv("TN_regions.csv")

# TN uses workforce development areas, so much of the work is just condensing it
# down to county data so we can FIPs it.

output <- raw %>%
  mutate(

    # get average of each county in the region
    claims = as.numeric(str_remove(count, ",\\s*")),
    claims = ceiling(case_when(
      area == "East Tennessee" ~ claims / 16,
      area == "Greater Memphis" ~ claims / 4,
      area == "Northeast Tennessee" ~ claims / 8,
      area == "Northern Middle Tennessee" ~ claims / 13,
      area == "Northwest Tennessee" ~ claims / 9,
      area == "Southeast Tennessee" ~ claims / 10,
      area == "Southern Middle Tennessee" ~ claims / 13,
      area == "Southwest Tennessee" ~ claims / 8,
      area == "Upper Cumberland" ~ claims / 14
    ))
  ) %>%
  left_join(regions, by = "area") %>%

  # Join with FIPS codes

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%
  mutate(

    # set general info
    state = "Tennessee",
    state_fips = 47,
    state_short = "TN",

    # modify dates
    date = mdy(date),
    month = month(date),
    year = year(date),
    week = week(date),

    county = str_to_title(str_remove(polyname, "tennessee,"))
  ) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week)

write.csv(output, file = "TN_compiled.csv", row.names = FALSE)
