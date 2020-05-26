####Clean data and import them into Migration mapper App
library(raster)
library(sf)
##Table CaribouDataClean (Git)

head(CaribouDataClean)

##Subset MIDRIDGE
unique(CaribouDataClean$HERD)

Midridge_herd <- subset (CaribouDataClean, HERD == "MIDRIDGE")

##Subset one indiv to check first if data works in MM App
unique(Midridge_herd$ANIMAL_ID)

mr2009a21 <- subset(Midridge_herd, ANIMAL_ID == "mr2009a21")

## Prepare data into shp to import in MM App
##add column time and year in text
mr2009a21$datetime_txt <- as.character(mr2009a21$datetime)
mr2009a21$year_txt <- as.character(mr2009a21$year)

p.sf.all <- st_as_sf(mr2009a21, coords = c("EASTING", "NORTHING"), crs = "+proj=utm +zone=21 ellps=WGS84") 
p.sf.all
st_write(p.sf.all, "E:/Emilie_project/Git/emilie_nlcaribou/input", "mr2009a21_MM_App.shp", driver="ESRI Shapefile")

