# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("NE_data.csv")

output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "31",
    state_short = "NE",
    state = "Nebraska",
    year = 2020,
    area_type = case_when(
      str_detect(area, "County") ~ "county",
      area == "Nebraska" ~ "state",
      TRUE ~ "city"
    ),
    polyname = case_when(
      area_type == "county" ~ paste("nebraska,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
    rename(unemployment = unemployed,
           employment = employed) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  )

write.csv(output, file = "NE_compiled.csv", row.names = FALSE)
