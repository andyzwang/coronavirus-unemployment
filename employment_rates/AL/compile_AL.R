# imports

library(tidyverse)
library(janitor)
library(lubridate)
library(maps)

# import

raw <- read_csv("AL_data.csv") %>%
  clean_names(case = "snake")

