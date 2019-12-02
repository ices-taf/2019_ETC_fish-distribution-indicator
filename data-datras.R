## Preprocess data, write TAF data tables

## Before:
## After:

source("utilities.R")
detachAllPackages()

library(icesTAF)
library(dplyr)
library(tidyr)
library(sf)

mkdir("data")
source("utilities.R")

# load control file and write out summary table:
data_overview <- read.taf("bootstrap/data/control_file/control_file.csv")

# read in statsq table
statrecs <- sf::read_sf("bootstrap/data/ICES-stat-rec/StatRec_map_Areas_Full_20170124.shp")

# format statsquare table
statrecs <-
  statrecs %>%
  as.data.frame() %>%
  select(
    ICESNAME, Area_27, stat_x, stat_y
  ) %>%
  rename(
    StatRec = ICESNAME,
    F_CODE = Area_27,
    Lat = stat_x,
    Lon = stat_y
  ) %>%
  filter(!is.na(F_CODE)) %>%
  mutate(
    F_CODE = F_CODE %>%
                 strsplit("[.]") %>%
                 lapply("[", 1:2) %>%
                 sapply(paste0, collapse = ".")
  ) %>%
  mutate(
    F_CODE = ifelse(F_CODE %in% c("3.b", "3.c"), "3.b, c", F_CODE)
  )


# prepare files to read and join to control file
files <-
  data.frame(hh = dir("bootstrap/data/datras", pattern = "hh_*")) %>%
  mutate(
    Survey.name = sapply(strsplit(hh, "_"), "[[", 2),
    Year = as.numeric(sapply(strsplit(hh, "_"), "[[", 3)),
    Quarter = as.numeric(sapply(strsplit(hh, "_|[.]"), "[[", 4))
  ) %>%
  mutate(
    hl = datras.fname("hl", Survey.name, Year, Quarter)
  ) %>%
  select(hh, hl, Survey.name, Year, Quarter) %>%
  left_join(data_overview, by = c("Survey.name", "Quarter"))

if (FALSE) { # here for debugging
  # subset out only north sea
  files <-
    files %>%
    filter(Division == "4.b" & Year == 2011)
}

# read in and summarise hh and hl data
hh_data <-
  lapply(
    1:nrow(files),
    function(i) {
      if (i %% 50 == 0) msg("processing: ", i, " of ", nrow(files), ": ", files$hh[i])
      process_datras(files$hh[i], files$hl[i], files$Gear[i])
    }
  ) %>%
  do.call(what = rbind) %>%
  left_join(statrecs, by = "StatRec") %>%
  left_join(files %>% rename(Survey = Survey.name), by = c("Survey", "Quarter", "Year")) %>%
  filter(
    F_CODE == Division
  ) %>%
  select(
    -Start.year, -hh, -hl, -Gear, -Division
  )

write.taf("hh_data", dir = "data", quote = TRUE)
