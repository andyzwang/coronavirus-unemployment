# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# import

raw_counties <- read_csv("GA_counties_data.csv")

data(county.fips)

output_counties <- raw_counties %>%
  clean_names("snake") %>%
  mutate(
    # set basics
    area_type = case_when(
      str_detect(area, "County") ~ "county",
      TRUE ~ "state"
    ),

    state_fips = "13",
    state_short = "GA",
    state = "Georgia",

    year = 2020,
    period = month(mdy(paste(month,"-1-2020", sep = "")), label = TRUE, abbr = FALSE),

    # work on FIPS
    polyname = case_when(
      area ==  "DeKalb County" ~ "georgia,de kalb",
      TRUE ~ paste("georgia,", tolower(str_remove(area, " County")), sep = "")
    ),
  ) %>% # Join with FIPS

  left_join(county.fips, by = "polyname") %>%

  # pad zeros

  mutate(
    fips = str_pad(fips, 5, pad = "0")
  )  %>%
  rename(
    employment = employed,
    unemployment = unemployed
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output_counties, file = "GA_compiled.csv", row.names = FALSE)
