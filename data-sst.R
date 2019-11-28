
library(icesTAF)
library(ncdf4)
library(raster)
library(chron)
library(dplyr)
library(sf)
library(tidyverse)

# read control file
ctrl <- read.taf("bootstrap/data/control_file/control_file.csv")

# read in ICES areas shapes
areas <- sf::read_sf("bootstrap/data/ICES-areas/ICES_Areas_20160601_cut_dense_3857.shp")

# subset to areas of interest
areas$subarea.div <- paste(areas$SubArea, areas$Division, sep = ".")
areas <- areas[which(areas$subarea.div %in% ctrl$Division),]

# read sst nc as raster
b <- brick("bootstrap/data/hadley-sst/HadSST.4.0.0.0_median.nc")

areas <- st_transform(areas, crs(b))

b <- crop(b, extent(areas), snap = "out")
b[b > 100] <- NA

# simplify anc combine
areas <-
  areas %>%
  st_simplify(FALSE, 0.05) %>%
  group_by(subarea.div) %>%
  summarise(geog = st_union(geometry)) %>%
  ungroup()

areas <-
  areas[1:nrow(areas),] %>% # weird...
  st_simplify(FALSE, 0.2)


# filter for years of interest
b <- b[[which(years(b@z$time) %in% 1965:2018)]]
yrs <- years(b@z$time)

# group accross years
b <- stackApply(b, yrs, fun = mean)
yrs <- as.numeric(levels(yrs))
names(b) <- paste(yrs)

image(b[["X2018"]])
plot(areas, add = TRUE, col = "transparent")

# calculate number of cells and means
means <- raster::extract(b, areas, mean)

sst <-
  data.frame(
    area = rep(areas$subarea.div, ncol(means)),
    year = rep(yrs, each = nrow(means)),
    sst = c(means)
  )

head(sst)

library(ggplot2)
ggplot(sst, aes(x = year, y = sst, col = area)) +
  geom_line()

