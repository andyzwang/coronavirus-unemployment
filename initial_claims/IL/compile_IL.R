# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing

raw <- read_excel("IL_data.xlsx", sheet = "County Initial Claims", na = "", skip = 3) %>%
  slice(1:102) %>%

  # select only things from this decade (otherwise overload on memory)

  select("County name", starts_with("43"))

# import FIPS codes

data(county.fips)


output <- raw %>%

  # pivot long

  pivot_longer(
    cols = "43009":"43922", names_to = "date", values_to = "claims"
  ) %>%
  clean_names(case = "snake") %>%
  mutate(

    # change dates out of Excel format
    date = as.numeric(date),
    date = excel_numeric_to_date(date),

    # set general info
    state = "Illinois",
    state_fips = 17,
    state_short = "IL",

    # modify dates

    date = ceiling_date(date, "month") - 1,
    week = NA,
    month = month(date),
    year = year(date),

    # fix some incorrect names

    county_name = case_when(
      county_name == "Caroll" ~ "Carroll",
      county_name == "Mc Donough" ~ "McDonough",
      TRUE ~ county_name
    ),

    # create polyname

    polyname = case_when(
      county_name == "DeKalb" ~ "illinois,de kalb",
      county_name == "St. Clair" ~ "illinois,st clair",
      county_name == "DeWitt" ~ "illinois,de witt",
      county_name == "DuPage" ~ "illinois,du page",
      county_name == "JoDaviess" ~ "illinois,jo daviess",
      county_name == "LaSalle" ~ "illinois,la salle",
      TRUE ~ paste("illinois,", tolower(county_name), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    county_fips = fips,
    county = county_name
  ) %>%
  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  filter(year >= 2019) %>%
  arrange(month)

# output
write.csv(output, file = "IL_compiled.csv", row.names = FALSE)

