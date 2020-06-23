# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("AL_data.csv") %>%
  clean_names(case = "snake")

# import FIPS
data(county.fips)

output <- raw %>%
  filter(areadesc == "County" | areadesc == "Major City" | areadesc == "State") %>%
  mutate(
    # dates
    date = ceiling_date(mdy(datefield), "months") - 1,
    month = month(date),
    week = NA,
    year = year(date),

    area_type = case_when(
      areadesc == "Major City" ~ "City",
      TRUE ~ areadesc
    ),

    # set general info

    state_fips = "1",
    state_short = "AL",
    state = "Alabama",

    # work on FIPS

    area = str_remove(area_name, "( County| City)"),
    polyname = case_when(
      area == "DeKalb" ~ "alabama,de kalb",
      area == "St. Clair" ~ "alabama,st clair",
      areadesc == "County" ~ paste("alabama,", tolower(area), sep = "")
    )
  ) %>%
  filter(year(date) >= 2019) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%

  # pad zeros

  mutate(
    fips = str_pad(fips, 5, pad = "0")
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, date, week, month, 
    year,employment, labor_force, unemployment
  )

write.csv(output, file="AL_compiled.csv", row.names = FALSE)

