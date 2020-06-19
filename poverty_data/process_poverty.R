# includes
library(tidyverse)
library(janitor)
library(stringr)

# import
raw <- read_csv("census_poverty_data.csv")

output <- raw %>%
  select(S1701_C03_001E, GEO_ID) %>%
  slice(2:n()) %>%
  mutate(
    county_fips = str_remove(GEO_ID, ".*US"),
    percent_poverty = as.numeric(S1701_C03_001E)
  ) %>%
  select(county_fips, percent_poverty)

write.csv(output, file = "povery_compiled.csv", row.names = F)