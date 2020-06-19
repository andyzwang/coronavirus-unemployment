library(tidyverse)
library(stringr)
library(janitor)

# import
raw <- read_csv("bea_data.csv")

output <- raw %>%
  clean_names("snake") %>%
  filter(line_code == 3) %>%
  rename(per_capita_income = x2018) %>%
  select(geo_fips, geo_name, per_capita_income) %>%
  mutate(geo_fips = str_pad(geo_fips, 5, pad = "0"))

write.csv(output, file = "bea_2018_compiled.csv", row.names = F)
