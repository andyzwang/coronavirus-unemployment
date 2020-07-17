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
  filter(adjusted_tooltip == "not adjusted") %>%
  filter(period != 0) %>%
  mutate(
    # dates
    period = month(mdy(datefield), label = T, abbr = F),

    area_type = case_when(
      areadesc == "Major City" ~ "city",
      TRUE ~ str_to_lower(areadesc)
    ),

    # set general info

    state_fips = "01",
    state_short = "AL",
    state = "Alabama",

    # work on FIPS

    area = area_name,
    area = case_when(
      area_type == "city" ~ str_replace(area, "City", "city"),
      TRUE ~ area
    ),
    polyname = case_when(
      area == "DeKalb County" ~ "alabama,de kalb",
      area == "St. Clair County" ~ "alabama,st clair",
      area_type == "county" ~ paste("alabama,", tolower(str_remove(area, " County")), sep = "")
    )
  ) %>%
  filter(year >= 2019) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%

  # pad zeros

  mutate(
    fips = str_pad(fips, 5, pad = "0")
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file="AL_compiled.csv", row.names = FALSE)
