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
  pivot_wider(names_from = measure_names, values_from = measure_values) %>%
  clean_names("snake") %>%
  mutate(
    # set basics
    
    area_type = case_when(
      str_detect(area, "County") == TRUE ~ "County",
      area == "Arkansas" ~ "State",
      TRUE ~ "City"
    ),
    
    state_fips = "05",
    state_short = "AR",
    state = "Arkansas",
    
    # work on FIPS
    polyname = case_when(
      area == "St. Francis County" ~ "arkansas,st francis",
      area_type == "County" ~ paste("arkansas,", tolower(str_remove(area, " County")), sep = "")
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
    employment, labor_force, unemployment
  )

write.csv(output, file="AR_compiled.csv", row.names = FALSE)
