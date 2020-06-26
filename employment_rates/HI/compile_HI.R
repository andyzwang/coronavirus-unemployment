# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(readxl)

# import

raw_state <- read_excel("HI_data.xlsx", sheet="State", range = "B7:F587") %>%
  mutate(area = "Hawaii", area_type = "state", fips = NA)

raw_hawaii <- read_excel("HI_data.xlsx", sheet="Hawaii Cty", range = "B7:F405")%>%
  mutate(area = "Hawaii County", area_type = "county", fips = "15001")

raw_honolulu <- read_excel("HI_data.xlsx", sheet="Honolulu", range = "B7:F405")%>%
  mutate(area = "Honolulu County", area_type = "county", fips = "15003")

raw_kauai <- read_excel("HI_data.xlsx", sheet="Kauai Cty", range = "B7:F405")%>%
  mutate(area = "Kauai County", area_type = "county", fips = "15007")

raw_maui <- read_excel("HI_data.xlsx", sheet="Maui Cty", range = "B7:F405")%>%
  mutate(area = "Maui County", area_type = "county", fips = "15009")

raw <- rbind(raw_state, raw_hawaii, raw_honolulu, raw_kauai, raw_maui) %>%
  clean_names

output <- raw %>%
  mutate(date = dmy(paste("1-", annual, sep =""))) %>%
  filter(!is.na(date)) %>%
  filter(year(date) >= 2019) %>%
  mutate(
    state_fips = "15",
    state_short = "HI",
    state = "Hawaii",
    year = year(date),
    period = month(date)
  ) %>%
  
  rename(
    employment = employed,
    unemployment = unemployed,
    rate = labor_force,
    labor_force = total
  ) %>%
  select(
    state_fips, state_short, state, area, area_type, fips, period, year,
    employment, labor_force, unemployment
  )

write.csv(output, file = "HI_compiled.csv", row.names = FALSE)
