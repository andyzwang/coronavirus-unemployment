# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(tabulizer)


raw <- read_csv("NM_gathered.csv") %>%
  remove_empty("cols") %>%
  remove_empty("rows")


output <- raw %>%
  mutate(

    # info
    state = "New Mexico",
    state_short = "NM",
    state_fips = 35,

    # dates
    date = mdy(date),
    date = ceiling_date(date, "month") - 1,
    month = month(date),
    week = NA,
    year = year(date),
    
    # Get FIPS

    polyname = case_when(
      TRUE ~ paste("new mexico,", tolower(county), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(month)

# output
write.csv(output, file = "NM_compiled.csv", row.names = FALSE)
