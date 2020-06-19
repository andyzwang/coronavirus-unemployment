# includes
library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(janitor)

# import
raw <- read_excel("LUAS_data.xlsx") %>%
  filter(!is.na(Period))

output <- raw %>%
  clean_names("snake") %>%
  filter(period == "Apr-20 p") %>%
  select(state_fips, whole_fips, labor_force, employed, unemployed, unemployment_rate)

write.csv(output, file = "LUAS_Apr_20_compiled.csv", row.names = FALSE)