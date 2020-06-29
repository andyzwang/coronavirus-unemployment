# quick script to combine all individual state CSV's: run this after updating
# any states

library(tidyverse)
library(stringr)
library(lubridate)

# list all files, including in our subfolders

filenames <- list.files(".", recursive = T, pattern = "_compiled.csv")

# put all in one table

output <- Reduce(rbind, lapply(filenames, read.csv)) %>%
  filter(!is.na(employment)) %>%
  filter(employment != 0) %>%
  filter(period != "Annual Averages") %>%
  filter(period != "Annual") %>%
  filter(period != "-") %>%
  filter(period != "0") %>%
  mutate(
    fips = as.character(fips),
    fips = case_when(
      str_count(fips, "\\d") == 4~ paste("0", fips, sep = ""),
      TRUE ~ fips
    ),
    area_type = str_to_lower(area_type),
    period = month(mdy(paste(period, "-1-2020", sep = "")), abbr = F, label = T),
    unemployment_rate = as.numeric(unemployment) / as.numeric(labor_force),
    area = case_when(
      area_type == "city" ~ str_remove(area, " city"),
      TRUE ~ as.character(area)
    ),
    area = case_when(
      area_type == "city" ~ str_remove(area, " town"),
      TRUE ~ as.character(area)
    ),
    area = case_when(
      area_type == "city" ~ str_remove(area, " village"),
      TRUE ~ as.character(area)
    )
  )

# save


 write.csv(output, file = "unemployment_rates.csv", row.names = F)


# voila ;)
