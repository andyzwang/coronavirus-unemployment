# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# import

raw <- read_excel("VT_data.xlsx", sheet = "Full Dataset", skip = 7)

output <- raw %>%
  clean_names("snake") %>%
  filter(year >= 2019) %>%
  filter(seasonally_adjusted != "Yes") %>%
  filter(time_period != 0) %>%
  filter(!str_detect(area, "Labor Market Area")) %>%
  mutate(
    state_fips = "50",
    state_short = "VT",
    state = "Vermont",
    area_type = case_when(
      area == "Vermont" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    period = month(as.numeric(time_period), abbr= F, label = T),
    polyname = case_when(
      area_type == "county" ~ paste("vermont,",
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
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "VT_compiled.csv", row.names = FALSE)
