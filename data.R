## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

mkdir("data")

sourceTAF("data-sst.R")
sourceTAF("data-datras.R")
