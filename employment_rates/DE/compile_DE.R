# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("DE_data.csv")

data(county.fips)

output <- raw %>%
  pivot_longer(cols = contains("/"), names_to = "period", values_to = "measure_value") %>%
  clean_names("snake")%>%
  mutate(
    value = str_remove(value, ",.*$")
  ) %>%
  # pivot wider
  pivot_wider(names_from = value, values_from = measure_value) %>%
  clean_names("snake") %>%
  mutate(
    # set basics
    
    state_fips = "10",
    state_short = "DE",
    state = "Delaware",
    
    year = year(mdy(period)),
    period = month(mdy(period), label = T, abbr = F),
    
    # work on FIPS
    polyname = case_when(
      area_type == "county" ~ paste("delaware,", str_to_lower(area), sep = "")
    )
  ) %>% # Join with FIPS
  
  left_join(county.fips, by = "polyname") %>%
  
  # pad zeros
  
  mutate(
    fips = str_pad(fips, 5, pad = "0")
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    employment, labor_force, unemployment
  )


write.csv(output, file="DE_compiled.csv", row.names = FALSE)
