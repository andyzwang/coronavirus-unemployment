# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# import

raw_counties <- read_excel("IL_county_data.xlsx", skip = 6) %>%
  mutate(area_type = case_when(
    AREA == "ILLINOIS" ~ "state",
    TRUE ~ "county"
  ))
raw_cities <- read_excel("IL_city_data.xlsx", skip = 6) %>%
  mutate(FIPS = NA) %>%
  select(FIPS, everything())%>%
  mutate(area_type = "city")

raw <- rbind(raw_counties, raw_cities)
data(county.fips)

output <- raw %>%
  clean_names("snake")%>%
  filter(year != "NA") %>%
  filter(!str_detect(area, "COUNTY PART")) %>%
  mutate(
    state_fips = "17",
    state_short = "IL",
    state = "Illinois",
    
    period = month(mo_number, label = T, abbr = F),
    fips = case_when(
      area_type == "county" ~ paste("17",fips, sep="")
  ),
  area = str_to_title(area)
  ) %>%
  rename(
    employment = employed,
    unemployment = number, 
    labor_force = force
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  )

write.csv(output, file = "IL_compiled.csv", row.names = FALSE)
