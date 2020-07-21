# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)


raw <- read_csv("MS_data.csv")

# import FIPS codes

data(county.fips)

output <- raw %>%
  pivot_longer(cols = !starts_with("county"), names_to = "month", values_to = "claims") %>%

  # make claims numeric

  mutate(
    claims = as.integer(claims),

    # set general info
    state = "Mississippi",
    state_fips = 28,
    state_short = "MS",

    # modify dates
    date = ceiling_date(
      ymd(paste0("2020-", month, "-01")),
      unit = "month"
    ) - 1,
    month = month(date),
    year = year(date),
    week = NA,

    # create polyname

    polyname = case_when(
      county == "DeSoto" ~ "mississippi,de soto",
      TRUE ~ paste("mississippi,", tolower(county), sep = "")
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
  arrange(month)

# output
write.csv(output, file = "MS_compiled.csv", row.names = FALSE)
