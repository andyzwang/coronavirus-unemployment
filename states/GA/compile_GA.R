# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(tabulizer)


raw <- read_csv("GA_gathered.csv")

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
  arrange(month)

# output
write.csv(output, file = "GA_compiled.csv", row.names = FALSE)
