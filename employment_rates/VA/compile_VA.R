# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("VA_data.csv")

output <- raw %>%
  clean_names("snake")  %>%
  mutate(
    state_fips = "51",
    state_short = "VA",
    state = "Virginia",
    area = area_name,
    area_type = case_when(
      area == "Virginia" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    period = period_name,
    polyname = case_when(
      area == "	Accomack County" ~"virginia,accomack:main",
      area_type == "county" ~ paste("virginia,",
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
      unemployment = unemployed,
      employment = employed
    ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "VA_compiled.csv", row.names = FALSE)
