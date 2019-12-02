## Extract results of interest, write TAF output tables

## Before:
## After:

source("utilities.R")
detachAllPackages()

library(icesTAF)
library(dplyr)
library(tidyr)

mkdir("output")

# read in model output
stsq_data <- read.taf("model/stsq_data.csv")
yy_data <- read.taf("model/yy_data.csv")


# Fig 1 requires

# per stat sq -
# * StatRec - 28F1
# * Survey - NS_IBTS
# * F_CODE - i.e 27.4.c
# * Quarter
# * year
# * Lusitanian  - count
# * Boreal - count
# * ratio - Lusitanian / Boreal
# * Lat - latitude
# * Lon - longitude

fig1_data <-
  stsq_data %>%
  mutate(
    F_CODE = paste0(27, ".", F_CODE),
  ) %>%
  select(
    StatRec, Survey, F_CODE, Quarter, Year,
    Lusitanian, Boreal, ratio, Lat, Lon
  )

head(fig1_data)

write.taf(fig1_data, dir = "output", quote = TRUE)

# Figure 2 requires

# * Year
# * Atlantic species
# * Boreal species
# * Lusitanian species
# * Unknown Biological affinity
# * Area

fig2_data <-
  yy_data %>%
  mutate(
    F_CODE = paste0(27, ".", F_CODE),
  ) %>%
  select(
    Year, Atlantic, Boreal, Lusitanian, Unknown, F_CODE
  ) %>%
  arrange(F_CODE)

write.taf(fig2_data, dir = "output", quote = TRUE)


# Figure 3 requires

# * Year
# * L/B Ratio
# * Temp - ?
# * Temperature - this is whats plotted
# * Temp2 - ?
# * Area

fig3_data <-
  yy_data %>%
  mutate(
    F_CODE = paste0(27, ".", F_CODE),
  ) %>%
  select(
    Year, ratio, sst, F_CODE
  ) %>%
  group_by(
    F_CODE
  )  %>%
  mutate(
    sst_lag1 = c(NA, sst[-length(sst)]),
    sst_lag2 = if (length(sst) > 2) c(NA, NA, sst[-length(sst) + 1:0]) else rep(NA, length(sst))
  ) %>%
  ungroup() %>%
  arrange(F_CODE)

write.taf(fig3_data, dir = "output", quote = TRUE)

# previous files from JOrgen had:
# Species Division  Year  Caught  Fished  Prop
