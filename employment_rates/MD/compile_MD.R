# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("MD_data.csv")

output <- raw %>%
  pivot_longer(cols = contains("/"), names_to = "date", values_to = "values") %>%
  pivot_wider(names_from = "Metric", values_from = "values") %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "24",
    state_short = "MD",
    state = "Maryland",
    period = month(mdy(date), abbr = F, label = T),
    area_type = str_to_lower(area_type),
    year = year(mdy(date)),
    area = series_title,
    polyname = case_when(
      area == "St. Mary's County" ~ "maryland,st marys",
      area_type == "county" ~ paste("maryland,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  )

write.csv(output, file = "MD_compiled.csv", row.names = FALSE)
