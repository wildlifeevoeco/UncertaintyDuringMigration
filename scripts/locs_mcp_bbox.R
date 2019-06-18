### Generate bounding box of caribou locs ====


### Packages ----
pkgs <- c('data.table', 'sp', 'adehabitatHR')
p <- lapply(pkgs, library, character.only = TRUE)

### Data ----
locs <- readRDS('output/caribouclean.Rds')
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

### Processing ----
# Convert to a SpatialPointsDataFrame
spdf <- SpatialPointsDataFrame(
  locs[, .(X, Y)],
  proj4string = utm21N
)
