# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("AR_data.csv") %>%
  clean_names(case = "snake") 


output <- raw %>%
  # pivot wide
  clean_names("snake") %>%
  mutate(
    # set basics
    
    area_type = case_when(
      str_detect(area, "County") == TRUE ~ "county",
      area == "Arkansas" ~ "state",
      TRUE ~ "city"
    ),
    
    state_fips = "05",
    state_short = "AR",
    state = "Arkansas",
    
    # work on FIPS
    polyname = case_when(
      area == "St. Francis County" ~ "arkansas,st francis",
      area_type == "county" ~ paste("arkansas,", tolower(str_remove(area, " County")), sep = "")
    )
  ) %>% # Join with FIPS
  
  left_join(county.fips, by = "polyname") %>%
  
  # pad zeros
  
  mutate(
    fips = str_pad(fips, 5, pad = "0")
  ) %>%
  rename(
    unemployment = unemployed,
    employment = employed
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file="AR_compiled.csv", row.names = FALSE)
