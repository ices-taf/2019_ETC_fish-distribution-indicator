## Run analysis, write model results

## Before:
## After:

source("utilities.R")
detachAllPackages()

library(icesTAF)
library(dplyr)
library(tidyr)

mkdir("model")

# read in data
hh_data <- read.taf("data/hh_data.csv")
sst <- read.taf("data/sst.csv")

# read species aphia code table
worms <- read.taf("bootstrap/data/ecotaxanomic.csv")

# read in species groupings
biogeog <- read.taf("bootstrap/data/Reclaim_species_grouping.csv")


# read in species info
if (FALSE) {
  worms <- icesVocab::getCodeList("SpecWoRMS")
  worms <-
    worms %>%
    rename(
      Valid_Aphia = Key,
      Species = Description
    ) %>%
    select(
      Valid_Aphia, Species
    ) %>%
    mutate(
      Species = ifelse(Species == "Salmo trutta trutta", "Salmo trutta", Species)
    )
}

# join affinity info to aphia code
biogeog <-
  biogeog %>%
  select(
     Family,
     Species,
     Common.name,
     Biogeographical.affinity
  ) %>%
  right_join(worms, by = "Species")

head(biogeog)

biogeog %>%
  filter(is.na(Valid_Aphia))

biogeog[grep("Sebastes", biogeog$Species),]

# join affinity to hh data

# ...


if (FALSE) {
  # investigate unknown species
  # move to model script

  hh_data %>%
    filter(`Biogeographical affinity` == "Unknown") %>%
    select(Valid_Aphia, `Biogeographical affinity`, Species) %>%
    unique()

  srch <- "lolig" # "Nephrops"
  grep(srch, tolower(biogeog$Species), value = TRUE)
  grep(srch, tolower(worms$Species), value = TRUE)
  worms[grep(srch, tolower(worms$Species)),]
  biogeog[grep(srch, tolower(biogeog$Species)),]

}

# Compute stat square and annual summaries

# check missing species count in 40E8 in 2011

stsq_data <-
  hh_data %>%
  select(
    Year, StatRec, Survey, Quarter, F_CODE, Lat, Lon, `Biogeographical affinity`, Valid_Aphia
  ) %>%
  unique() %>%
  group_by(
    Year, StatRec, Survey, Quarter, F_CODE, Lat, Lon, `Biogeographical affinity`
  ) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = `Biogeographical affinity`, values_from = count
  ) %>%
  mutate(
    ratio = Lusitanian / Boreal
  )

stsq_data %>% filter(StatRec == "41E8" & Year == 2011)

write.taf(stsq_data, dir = "model", quote = TRUE)

# compute annual summaries
yy_data <-
  hh_data %>%
  select(
    Year, Survey, F_CODE, Valid_Aphia, `Biogeographical affinity`
  ) %>%
  unique() %>%
  group_by(
    Year, Survey, F_CODE, `Biogeographical affinity`
  ) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = `Biogeographical affinity`, values_from = count
  ) %>%
  mutate(
    ratio = Lusitanian / Boreal
  ) %>%
  left_join(sst %>% rename(Year = year, F_CODE = area), by = c("Year", "F_CODE"))

yy_data %>% filter(grepl("4[.]", F_CODE) & Year == 2011)

write.taf(yy_data, dir = "model", quote = TRUE)

