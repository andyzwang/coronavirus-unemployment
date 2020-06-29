# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("NM_data.csv") %>%
  clean_names("snake")

output <- raw %>%
  mutate(
    state_fips = "35",
    state_short = "NM",
    state = "New Mexico",
    area_type = str_to_lower(type),
    area = geo_name,
    date= ymd(paste("20", date, "-1", sep = "")),
    period = month(date, label = T, abbr = F),
    year = year(date),
    polyname = case_when(
      area_type == "county" ~ paste("new mexico,",
        tolower(str_remove(str_remove(
          area,
          "[[:punct:]]"
        ), " County")),
        sep = ""
      )
    ),
    keep = case_when(
      area_type == "county" ~ TRUE,
      area_type == "state" & area == "New Mexico" ~ TRUE
    )
  ) %>%
  
  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%
  rename(
    employment = e,
    labor_force = lf,
    unemployment = u
  )%>%
  filter(keep == TRUE) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "NM_compiled.csv", row.names = FALSE)
