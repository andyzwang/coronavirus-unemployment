# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)
library(stringr)
library(tabulizer)

# processing in the data
# NOTE: pages manually set

apr_1 <- extract_tables("MS_April_data.pdf", output = "data.frame", pages = 22)[[1]]
apr_2 <- extract_tables("MS_April_data.pdf", output = "data.frame", pages = 23)[[1]]

# clean up first half

apr_1 <- apr_1 %>%
  mutate(
    April.2020 = str_extract(April.2020, "^.* "),
    March.2020 = str_extract(March.2020, "^.* "),
  ) %>%
  slice(3:n()) %>%
  rename(county = X) %>%
  select(-April.2019)

# clean up second half

apr_2 <- apr_2 %>%
  rename(
    April.2020 = Initial.Claims,
    March.2020 = Initial.Claims.1,
    county = X
  ) %>%
  slice(2:n()) %>%
  select(county, April.2020, March.2020)

# combine

apr_data <- rbind(apr_1, apr_2)

# import FIPS codes

data(county.fips)

output <- apr_data %>%
  pivot_longer(
    cols = ends_with("2020"), names_to = "month",
    values_to = "claims"
  ) %>%

  # make claims numeric

  mutate(
    claims = str_remove(claims, ","),
    claims = as.integer(claims),

    # set general info
    state = "Mississippi",
    state_fips = 28,
    state_short = "MS",

    # modify dates
    date = ceiling_date(
      myd(paste0(month, ".01")),
      unit = "month"
    ) - 1,
    month = month(date),
    year = year(date),
    week = NA,

    # create polyname

    polyname = case_when(
      county == "DeSoto" ~ "mississippi,de soto",
      TRUE ~ paste("mississippi,", tolower(county), sep = "")
    )
  ) %>%

  # Join with FIPS codes

  left_join(county.fips, by = "polyname") %>%
  rename(county_fips = fips) %>%

  # relevant columns

  select(
    state, state_fips, state_short, county, county_fips,
    date, week, month, year, claims
  ) %>%
  arrange(month)

# output
write.csv(output, file = "MS_compiled.csv", row.names = FALSE)
