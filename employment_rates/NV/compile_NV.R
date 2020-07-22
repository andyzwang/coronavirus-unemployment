# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("NV_data.csv")

output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "32",
    state_short = "NV",
    state = "Nevada",
    area_type = case_when(
      area == "Montana" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    area = str_remove(area, ", Nevada"),
    polyname = case_when(
      area_type == "county" ~ paste("nevada,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
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

write.csv(output, file = "NV_compiled.csv", row.names = FALSE)
