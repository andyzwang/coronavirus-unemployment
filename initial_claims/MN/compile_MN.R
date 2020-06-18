# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(tabulizer)


raw <- read_csv("MN_gathered.csv")%>%
  remove_empty("cols")


output <- raw %>%
  pivot_longer(cols = !starts_with("county"), names_to = "date", values_to = "claims") %>%
  mutate(

    # info
    state = "Minnesota",
    state_short = "MN",
    state_fips = 27,

    # dates

    date = ceiling_date(mdy(date), "month") - 1,
    month = month(date),
    week = NA,
    year = year(date),

    # get FIPS

    county = str_remove(county, " County"),

    polyname = case_when(
      county == "Saint Louis" ~ "minnesota,st louis",
      TRUE ~ paste("minnesota,", tolower(county), sep = "")
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
write.csv(output, file = "MN_compiled.csv", row.names = FALSE)
