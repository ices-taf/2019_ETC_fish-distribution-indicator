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

tab <- cbind(tab[orig.row,c("Species", "Division", "Survey.name", "Gear")],
             data.frame(Quarter = as.integer(unlist(qtr)),
                        Start.year = as.integer(unlist(syr))))

# expand division
div <- strsplit(tab $ Division, ",")
orig.row <- rep(1:nrow(tab), sapply(div, length))

tab <- cbind(tab[orig.row,c("Species", "Survey.name", "Gear", "Quarter", "Start.year")],
             data.frame(Division = unlist(div)) )

# expand species
sp <- strsplit(tab $ Species, ",")
sp <- lapply(sp, function(x) trimws(gsub("\n|(and)", "", x)))
orig.row <- rep(1:nrow(tab), sapply(sp, length))

tab <- cbind(data.frame(Species = unlist(sp)),
             tab[orig.row,c("Division", "Survey.name", "Gear", "Quarter", "Start.year")])

tab$Survey.name <- trimws(tab$Survey.name)
tab$Division <- trimws(tab$Division)

row.names(tab) <- NULL

control_file <- tab[complete.cases(tab),]

write.taf(control_file)
