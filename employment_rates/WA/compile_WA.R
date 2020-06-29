# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# import

raw <- read_excel("WA_data.xlsx", sheet = "NSA_Columns")

output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "53",
    state_short = "WA",
    state = "Washington",
    area = str_remove(area_title, ", WA"),
    area_type = case_when(
      area_type == "State" ~ "state",
      area_type == "County" ~ "county",
      area_type == "City"   ~ "city"
      ),
    period = month(as.numeric(month), abbr= F, label = T),
    polyname = case_when(
      area =="San Juan County" ~ "washington,san juan:lopez island",
      area_type == "county" ~ paste("washington,",
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
    employment, labor_force, unemployment
  )  %>%
  filter(year >= 2019) %>%
  filter(!is.na(area_type)) %>%
  filter(!is.na(period))

write.csv(output, file = "WA_compiled.csv", row.names = FALSE)
