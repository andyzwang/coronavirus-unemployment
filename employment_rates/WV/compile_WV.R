# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("WV_data.csv")

output <- raw %>%
  clean_names("snake") %>%
  distinct(areaname, period_period, periodyear, .keep_all = T) %>%
  filter(pertypdesc == "Monthly") %>%
  select(areaname, period_period, periodyear, civilian_labor_force, emp, 
         unemployment) %>%
  filter(periodyear >= 2019) %>%
  filter(civilian_labor_force != 0) %>%
  mutate(
    state_fips = "54",
    state_short = "WV",
    state = "West Virginia",
    area = str_to_title(areaname),
    area_type = case_when(
      area == "West Virginia" ~ "state",
      str_detect(area, "County") ~ "county"
      ),
    period = month(mdy(paste(period_period, "-01-2020", sep = "")), abbr= F, label = T),
    year = periodyear,
    polyname = case_when(
      area_type == "county" ~ paste("west virginia,",
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
  rename(
    employment = emp,
    labor_force = civilian_labor_force
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )  %>%
  filter(!is.na(area_type))

write.csv(output, file = "WV_compiled.csv", row.names = FALSE)
