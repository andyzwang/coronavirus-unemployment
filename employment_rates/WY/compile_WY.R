# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("WY_compiled_data.csv")

output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "56",
    state_short = "WY",
    state = "Wyoming",
    year = 2020,
    area_type = str_to_lower(area_type),
    polyname = case_when(
      area_type == "county" ~ paste("wyoming,",
                                    tolower(str_remove(str_remove(
                                      area,
                                      "[[:punct:]]"
                                    ), " County")),
                                    sep = ""
      )
    ),
    area = case_when(
      area_type == "city" ~ str_remove(area, " village"),
      TRUE ~ as.character(area)
    ),
    area = case_when(
      area_type == "county" ~ paste(trimws(area),"County"),
      TRUE ~ area
    )
  ) %>%
  
  # Join with FIPS
  
  left_join(county.fips, by = "polyname")  %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "WY_compiled.csv", row.names = FALSE)
