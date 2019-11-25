## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(dplyr)

mkdir("data")

# load control file and write out summary table:

data_overview <- read.taf("bootstrap/data/control_file/control_file.csv")

# summarise the surveys used
survey_summary <-
  data_overview %>%
  group_by(Division, Survey.name) %>%
  mutate(
    Quarter = paste(Quarter, collapse = ", "),
    Start.year = paste(unique(Start.year), collapse = ", ")) %>%
  unique() %>%
  arrange(Division, Quarter)

write.taf(survey_summary, dir = "data", quote = TRUE)
