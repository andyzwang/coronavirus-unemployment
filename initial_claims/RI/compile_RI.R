# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# processing
# NOTE: range is manually set

raw <- read_excel("RI_data.xlsx", skip = 2) %>%
  filter(...1 == "UI Weekly Report Initial Claims")

regions <- tibble(state_short = "RI", county = c("Bristol", "Kent","Newport","Providence","Washington"))

output <- raw %>%

  pivot_longer(
    cols = starts_with("Week"), names_to = "date",
    values_to = "claims"
  )  %>%
  
  select(date, claims) %>%

  mutate(

    # set general info
    state = "Rhode Island",
    state_fips = 44,
    state_short = "RI",
    date = str_remove(date, "Week Ending "),

    # modify dates
    date = mdy(paste(date, "2020", sep = " ")),
    week = week(date),
    month = month(date),
    year = year(date),
    
    # weird bc RI
    claims = claims / 5
    
  ) %>%
  
  left_join(regions, by = "state_short") %>%
  mutate(
    polyname = case_when(
      TRUE ~ paste("rhode island,", tolower(county), sep = "")
    )
  ) %>%
  # Join with FIPS
  
  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(week) 

# output
write.csv(output, file = "RI_compiled.csv", row.names = FALSE)
