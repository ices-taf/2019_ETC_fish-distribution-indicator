
# -------------------------------------
#
# download datras data files
#
# -------------------------------------

library(icesDatras)
library(icesTAF)

# read design table and look download all required surveys
ctab <- read.taf("../control_file/control_file.csv")
#ctab <- read.taf("bootstrap/data/control_file/control_file.csv")

# surveys to get are:
toget <- unique(ctab[c("Survey.name", "Quarter", "Start.year")])
tab <- with(toget, tapply(Start.year, list(survey = Survey.name, quarter = Quarter), min))
toget <- expand.grid(Survey.name = gsub("[[:space:]]*$", "",rownames(tab)),
                     Quarter = as.integer(colnames(tab)),
                     stringsAsFactors = FALSE,
                     KEEP.OUT.ATTRS = FALSE)
toget$Start.year <- c(tab)
toget <- toget[!is.na(toget$Start.year),]
row.names(toget) <- NULL

# datras_fname utility:
datras.fname <- function(what, survey, year, quarter) {
  paste0(what, "_", survey, "_", year, "_", quarter, ".csv")
}

# loop over surveys and download (NOTE final year fixed at 2018)
for (i in seq_len(nrow(toget))) {
  # which survey etc.
  survey <- toget[i, "Survey.name"]
  start.year <- toget[i, "Start.year"]
  end.year <- 2018
  quarter <- toget[i, "Quarter"]

  for (year in start.year:end.year) {
    # within loop short-cut for file name
    fname <- function(what) {
      datras.fname(what, survey, year, quarter)
    }

    # download - make it re-entrant
    if (!file.exists(fname("hh"))) {

      message("---- downloading HH ----")
      hh <- getDATRAS(record = "HH", survey = survey, year = year, quarter = quarter)
      write.taf(hh, file = fname("hh"))
      rm(hh); gc()
    }

    if (!file.exists(fname("hl"))) {
      message("---- downloading HL ----")
      hl <- getDATRAS(record = "HL", survey = survey, year = year, quarter = quarter)
      write.taf(hl, file = fname("hl"))
      rm(hl); gc()
    }

    if (!file.exists(fname("ca"))) {
      message("---- downloading CA ----")
      ca <- getDATRAS(record = "CA", survey = survey, year = year, quarter = quarter)
      write.taf(ca, file = fname("ca"))
      rm(ca); gc()
    }
  }
}
