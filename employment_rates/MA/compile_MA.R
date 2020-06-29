# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("MA_data.csv")

output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "25",
    state_short = "MA",
    state = "Massachusetts",
    period = month,
    area_type = str_to_lower(area_type),
    area = str_to_title(area),
    polyname = case_when(
      area_type == "county" ~ paste("massachusetts,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
    rename(unemployment = unemployed,
           employment = employed) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "MA_compiled.csv", row.names = FALSE)
