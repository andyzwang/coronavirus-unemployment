library(tidyverse)
library(stringr)
library(janitor)

# import
raw <- read_csv("acs_data.csv")

output <- raw %>%
  select(S1901_C01_012E, GEO_ID) %>%
  slice(2:n()) %>%
  mutate(
    county_fips = str_remove(GEO_ID, ".*US"),
    median_household_income = as.numeric(S1901_C01_012E)
  ) %>%
  select(county_fips, median_household_income)

write.csv(output, file = "acs_wages_compiled.csv", row.names = F)
