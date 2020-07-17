# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
# import

raw <- read_csv("OH_data.csv")


output <- raw %>%
  clean_names("snake") %>%
  mutate(
    state_fips = "39",
    state_short = "OH",
    state = "Ohio",
    area = str_to_title(area),
    area_type = str_to_lower(area_type),
    period = month(mdy(paste(period, "-01-2020", sep = "")), label = T, abbr = F),
    polyname = case_when(
      area_type == "county" ~ paste("ohio,",
        tolower(str_remove(str_remove(
          area,
          "[[:punct:]]"
        ), " County")),
        sep = ""
      )
    ),
    area = str_remove(area, " City")
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    employment = employed,
    unemployment = unemployed
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  ) %>%
  mutate(
    area = case_when(
      area == "Delaware" ~ "Delaware (OH)",
      TRUE ~ area
    )
  )

write.csv(output, file = "OH_compiled.csv", row.names = FALSE)
