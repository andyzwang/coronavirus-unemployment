# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("WI_data.csv")

output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "55",
    state_short = "WI",
    state = "Wisconsin",
    area_type = case_when(
      area == "Wisconsin" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
      ),
    polyname = case_when(
      area_type == "county" ~ paste("wisconsin,",
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
  filter(!str_detect(area, "-.*County")) %>%
  mutate(
    area = case_when(
      str_detect(area, "-.*Counties") ~ str_remove(area, "-.*Counties"),
      TRUE ~ area
    )
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "WI_compiled.csv", row.names = FALSE)
