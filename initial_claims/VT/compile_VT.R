# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
# processing

raw <- read_csv("VT_gathered.csv")

# import FIPS codes

data(county.fips)

# create counties

vt_counties <- tibble(
  center = c(
    "Barre", "Bennington", "Brattleboro", "Burlington", "Middlebury",
    "Morrisville", "Newport", "Rutland", "St. Albans", "St. Johnsbury",
    "Springfield", "White River", "Agent State"
  ),
  county = c(
    "Washington", "Bennington", "Windham", "Chittenden", "Addison",
    "Lamoille", "Orleans", "Rutland", "Franklin", "Caledonia", "Windsor",
    "Windsor", "Unknown"
  )
)

output <- raw %>%
  pivot_longer(cols = !starts_with("center"), names_to = "date", 
                                   values_to = "claims") %>%
  left_join(vt_counties, by = "center") %>%
  group_by(county, date) %>%
  summarize(claims = sum(claims)) %>%
  mutate(

    # make claims numeric
    claims = as.integer(claims),

    # set general info
    state = "Vermont",
    state_fips = 50,
    state_short = "VT",

    # modify dates
    date = myd(date),
    month = month(date),
    year = year(date),
    week = week(date),
    # create polyname 
    
    polyname = paste("vermont,", tolower(county), sep = "")
  ) %>%

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
write.csv(output, file = "VT_compiled.csv", row.names = FALSE)
