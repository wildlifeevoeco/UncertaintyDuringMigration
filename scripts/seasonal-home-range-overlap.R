

### Packages ---
libs <- c('data.table','lubridate','rgeos',
          'tidyr','igraph','asnipe','sp','rgdal', 'raster', 
          'ggplot2', 'spatsoc', 'ggraph', 'adehabitatHR', 'sp')
lapply(libs, require, character.only = TRUE)


### Input data ----

### READ IN CLEANED GPS DATA 
### NOTE: MAKE SURE YOU HAVE DEFINED YOUR SEASONS 
locs <- readRDS("output/cleaned-locs.Rds")

locs <- setDT(locs)[HERD == "MIDRIDGE"]

locs$IDseason <- as.character(paste(locs$ANIMAL_ID, locs$season, sep = "_"))

locs_test <- readRDS("/Users/quinnwebber/Desktop/locs_test.RDS")

## test with 3 individuals
locs_test <- locs[ANIMAL_ID == "mr2009a03" | 
                  ANIMAL_ID == "mr2009a09" |
                  ANIMAL_ID == "mr2009a27"]


## look at tracks for the test individuals
ggplot(locs_test, aes(EASTING, NORTHING)) +
  geom_point(aes(color = season)) +
  geom_path(aes(fill = season)) +
  facet_wrap(~Year*ANIMAL_ID)

# UTM zone
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

#### RUN HOME RANGE OVERLAP WITH GROUP_POLYS
areaDT <- group_polys(locs_test, area = TRUE, 'mcp', list(percent = 95),
                      projection = utm21N,
                      id = 'IDseason', 
                      coords = c('EASTING', 'NORTHING'), splitBy = c("Year"))

## split the IDs and seasons apart
areaDT <- areaDT %>% 
  separate(IDseason, into = c("ID1", "season1"), sep="_") %>% 
  separate(IDseason2, into = c("ID2", "season2"), sep="_")

## remove comparisons of the same season (i.e. calving/calving and winter/winter)
areaDT <- areaDT[season1 != "calving" | season2 != "calving"][season1 != "winter" | season2 != "winter"]

## remove pairs of individuals (only want to compare IDs to themselves)
names <- data.table(ID = unique(areaDT$ID1))
out = c()
for(i in 1:length(names$ID)){
  id <- as.character(names[i])
  out[[i]] <- areaDT[ID1 == id & ID2 == id]
}

## Note, only 4/13 IDyears show up in the final output.
out2 <- rbindlist(out)

saveRDS(out2, "/Users/quinnwebber/Desktop/seasonal-home-range-overlap.csv")