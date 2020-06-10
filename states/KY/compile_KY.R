# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# processing

raw <- read_csv("KY_gathered.csv")

output <- raw %>%
  rename(state = state_name,
         state_short = state_abbrev,
         county = county_name,
         claims = initial_claims) %>%
  mutate(
    date = ceiling_date(ymd(paste(year, month, "01", sep = "-")), "month") -1,
    week = NA
  ) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  filter(year == 2020) %>%
  arrange(month)

# output
write.csv(output, file = "KY_compiled.csv", row.names = FALSE)
