# quick script to combine all individual state CSV's: run this after updating
# any states

library(tidyverse)

# list all files, including in our subfolders

filenames <- list.files(".", recursive = T, pattern = "_compiled.csv")

# put all in one table

output <- Reduce(rbind, lapply(filenames, read.csv))

# save

write.csv(output, file = "UI_initial_compiled.csv", row.names = F)

# voila ;)



output %>% group_by(state) %>% summarize(cases = sum(claims))
