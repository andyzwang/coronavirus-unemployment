# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("UT_data.csv")

output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "49",
    state_short = "UT",
    state = "Utah",
    area_type = case_when(
      county == "State of Utah" ~ "state",
      TRUE ~ "county"
    ),
    period = month(month, abbr= F, label = T),
    area = county,
    polyname = case_when(
      area_type == "county" ~ paste("utah,",
        tolower(str_remove(str_remove(
          area,
          "[[:punct:]]"
        ), " County")),
        sep = ""
      )
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    employment = employed,
    unemployment = unemployed
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  )  %>%
  filter(year >= 2019) %>%
  filter(area != "United States")

write.csv(output, file = "UT_compiled.csv", row.names = FALSE)
