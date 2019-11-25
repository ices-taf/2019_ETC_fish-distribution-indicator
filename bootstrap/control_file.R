# -------------------------------------
#
# process supplied control file
#
# -------------------------------------

library(icesTAF)

tab <- read.taf("../Overview_of_surveys_to_be_used.csv", check.names = TRUE)
#tab <- read.taf("bootstrap/data/Overview_of_surveys_to_be_used.csv", check.names = TRUE)

# expand quarter and start year
qtr <- strsplit(tab$Quarter, ",")
syr <- strsplit(tab$Start.year, ",")
orig.row <- rep(1:nrow(tab), sapply(qtr, length))

tab <- cbind(tab[orig.row,c("Division", "Survey.name", "Gear")],
             data.frame(Quarter = as.integer(unlist(qtr)),
                        Start.year = as.integer(unlist(syr))))

tab$Survey.name <- trimws(tab$Survey.name)
tab$Gear <- trimws(tab$Gear)
tab$Division <- trimws(tab$Division)

row.names(tab) <- NULL

write.taf(control_file)
