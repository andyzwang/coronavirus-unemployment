# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

raw <- read_csv("ID_data.csv") %>%
  clean_names(case = "snake") %>%
  remove_empty("cols")

# import FIPS
data(county.fips)

output <- raw %>%
  mutate(
    # dates
    period = month(dym(paste("1-",date,sep="")), label = T, abbr = F),
    year = 2020,
    
    # set general info
    
    state_fips = "16",
    state_short = "ID",
    state = "Idaho",
    
    # work on FIPS
    
    area = str_to_title(str_remove(area, "\\(.*\\)")),
    polyname = case_when(
      area_type == "county" ~ paste("idaho,", tolower(trimws(area)), sep = "")
    ),
    area = case_when(
      str_detect(area, "Washington") ~ "Washington (ID)",
      TRUE ~ area),
    area = case_when(
      area_type == "county" ~ paste(trimws(area), "County", sep = " "),
      TRUE ~ as.character(area)
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname") %>%

  # pad zeros

  mutate(
    fips = str_pad(fips, 5, pad = "0")
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file="ID_compiled.csv", row.names = FALSE)

