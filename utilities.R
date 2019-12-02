require(dplyr)

# datras_fname utility:
datras.fname <- function(what, survey, year, quarter) {
  paste0(what, "_", survey, "_", year, "_", quarter, ".csv")
}

process_hh <-  function(hh) {
  file.path("bootstrap/data/datras", hh) %>%
    read.taf(colClasses = c(StatRec = "character")) %>%
    select(Survey, Quarter, Country, Ship, Gear, SweepLngt, GearExp, DoorType, StNo, HaulNo, Year, StatRec, DayNight) %>%
    filter(DayNight == "D") %>%
    select(-DayNight)
}

process_hl <- function(hl) {
  file.path("bootstrap/data/datras", hl) %>%
    read.taf() %>%
    select(Survey, Quarter, Country, Ship, Gear, SweepLngt, GearExp, DoorType, StNo, HaulNo, Year, HLNoAtLngt, Valid_Aphia) %>%
    filter(HLNoAtLngt > 0) %>%
    select(-HLNoAtLngt) %>%
    unique()
}

process_datras <- function(hh, hl, Gear) {
  hl %>%
    process_hl() %>%
    filter(Gear == Gear) %>%
    left_join(
      process_hh(hh) %>% filter(Gear == Gear),
      by = c("Survey", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearExp", "DoorType", "StNo", "HaulNo", "Year")
    ) %>%
    select(
      Survey, Quarter, Year, StatRec, Valid_Aphia
    ) %>%
    unique()
}

detachAllPackages <- function() {

  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")

  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]

  package.list <- setdiff(package.list,basic.packages)

  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)

}
