# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)

# import

lf <- read_csv("MN_laborforce_data.csv") %>%
  pivot_longer(cols = !contains("Year-Month"), names_to = "area", values_to = "value") %>%
  mutate(
    metric = "labor_force"
  )

unemployment <- read_csv("MN_unemployment_data.csv") %>%
  pivot_longer(cols = !contains("Year-Month"), names_to = "area", values_to = "value") %>%
  mutate(
    metric = "unemployment"
  )

employment <- read_csv("MN_employment_data.csv") %>%
  pivot_longer(cols = !contains("Year-Month"), names_to = "area", values_to = "value") %>%
  mutate(
    metric = "employment"
  )

raw <- rbind(lf, unemployment, employment)

output <- raw %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  clean_names("snake")%>%
  mutate(
    state_fips = "27",
    state_short = "MN",
    state = "Minnesota",
    area_type = case_when(
      area == "Minnesota" ~ "state",
      str_detect(area, "County") ~ "county",
      TRUE ~ "city"
    ),
    year = str_remove(year_month, "/.*$"),
    period = month(ymd(paste(year_month,"/1", sep = "")), label = T, abbr = F),
    polyname = case_when(
      area == "Saint Louis County" ~ "minnesota,st louis",
      area_type == "county" ~ paste("minnesota,", tolower(str_remove(str_remove(area, "[[:punct:]]"), " County")), sep = "")
    ),
    area = case_when(
      area_type =="city" ~ str_remove(area, ",.*$"),
      TRUE ~ area
    )
  ) %>%

  # Join with FIPS

  left_join(county.fips, by = "polyname")  %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )

write.csv(output, file = "MN_compiled.csv", row.names = FALSE)
