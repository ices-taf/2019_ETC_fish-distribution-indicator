## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(dplyr)

mkdir("data")
source("utilities.R")

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
  arrange(Division, Quarter) %>%
  ungroup()

write.taf(survey_summary, dir = "data", quote = TRUE)

# read in and summarise hh data
hh_files <-
  data.frame(fname = dir("bootstrap/data/datras", pattern = "hh_*")) %>%
  mutate(
    Survey.name = sapply(strsplit(fname, "_"), "[[", 2),
    Year = as.numeric(sapply(strsplit(fname, "_"), "[[", 3)),
    Quarter = as.numeric(sapply(strsplit(fname, "_|[.]"), "[[", 4))
  ) %>%
  right_join(data_overview)

hh_data <-
  do.call(
    rbind,
    lapply(
      file.path("bootstrap/data/datras", hh_files$fname),
      function(x) {
        read.taf(x) %>%
          select(Survey, Quarter, Gear, Year, StatRec, DayNight) %>%
          filter(DayNight == "D") %>%
          select(-DayNight)
      })
    ) %>%
  rename(Survey.name = Survey) %>%
  right_join(hh_files, by = c("Survey.name", "Quarter", "Gear", "Year")) %>%
  filter(Year >= Start.year)

table(hh_data$Gear, hh_data$Survey.name)

# summarise stat rectangles fished
