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

control_file <- cbind(tab[orig.row,c("Division", "Survey.name", "Gear")],
             data.frame(Quarter = as.integer(unlist(qtr)),
                        Start.year = as.integer(unlist(syr))))

control_file$Survey.name <- trimws(control_file$Survey.name)
control_file$Gear <- trimws(control_file$Gear)
control_file$Division <- trimws(control_file$Division)

row.names(control_file) <- NULL

write.taf(control_file, quote = TRUE)
