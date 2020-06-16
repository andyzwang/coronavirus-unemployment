# quick script to combine all individual state CSV's: run this after updating
# any states

library(tidyverse)

# list all files, including in our subfolders

filenames <- list.files(".", recursive = T, pattern = "_compiled.csv")

# put all in one table

output <- Reduce(rbind, lapply(filenames, read.csv)) %>%
  filter(!is.na(claims)) %>%
  filter(claims != 0) %>%
  mutate(
    county_fips = as.character(county_fips),
    county_fips = case_when(
      str_count(county_fips, "\\d") == 4~ paste("0", county_fips, sep = ""),
      TRUE ~ county_fips
    )
  )

# save


write.csv(output, file = "UI_initial.csv", row.names = F)

# voila ;)
