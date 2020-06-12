# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# read in

raw <- read_csv("GA_gathered.csv")

# import FIPS codes

data(county.fips)

output <- raw %>%
  pivot_longer(cols = !starts_with("county"), names_to = "month", values_to = "claims") %>%
  mutate(

    # basic_info
    state = "Georgia",
    state_fips = 13,
    state_short = "GA",

    # dates
    date = ceiling_date(ymd(
      paste(2020, month, "01", sep = "-")
    ), "month") - 1,
    month = month(date),
    week = week(date),
    year = year(date),
    # create polyname

    polyname = case_when(
      county == "DeKalb" ~ "georgia,de kalb",
      TRUE ~ paste("georgia,", tolower(county), sep = "")
    )
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
write.csv(output, file = "GA_compiled.csv", row.names = FALSE)
