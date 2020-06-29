# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
# import

em <- read_csv("OR_em_data.csv", na= c("", NA, "-")) %>%
  mutate(mark = "employment")
unem <- read_csv("OR_unem_data.csv", na= c("", NA, "-"), skip = 4)%>%
  mutate(mark = "unemployment")
lf <- read_csv("OR_lf_data.csv", na= c("", NA, "-")) %>%
  mutate(mark = "labor_force")

raw <- rbind(em, unem, lf)

output <- raw %>%
  remove_empty("cols") %>%
  pivot_longer(cols = contains("-"), names_to = "date", values_to = "value") %>%
  clean_names("snake") %>%
  pivot_wider(names_from = "mark", values_from = "value") %>%
  mutate(
    state_fips = "41",
    state_short = "OR",
    state = "Oregon",
    area = str_remove(area," \\(Not Seasonally Adjusted\\)"),
    area_type = case_when(
      str_detect(area, "County") ~ "county",
      area == "Oregon" ~ "state"
    ),
    period = month(ymd(paste("20", date, "-01", sep = "")), label = T, abbr = F),
    year = year(ymd(paste(date, "-01", sep = ""))),
    polyname = case_when(
      area_type == "county" ~ paste("oregon,",
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
  select(
    state_fips, state_short, state, area, area_type, fips, period, year, 
    labor_force, employment, unemployment
  )  %>%
  filter(!is.na(area_type))

write.csv(output, file = "OR_compiled.csv", row.names = FALSE)
