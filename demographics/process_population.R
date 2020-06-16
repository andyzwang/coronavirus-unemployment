# includes

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# read in 

raw <- read_csv("population_raw.csv")

output <- raw %>%
  clean_names("snake") %>%
  select(state, county, ctyname, popestimate2019) %>%
  mutate(state_fips = formatC(state, width = 2, format = "d", flag = "0"),
         county_fips = formatC(county, width = 3, format = "d", flag = "0"),
         county_fips = paste(state_fips, county_fips, sep = "")) %>%
  select(county_fips, popestimate2019)
         

write.csv(output, file = "population_processed.csv", row.names = F)
