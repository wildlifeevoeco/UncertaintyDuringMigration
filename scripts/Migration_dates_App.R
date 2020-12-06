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


###Extract and convert all individuals from Midridge in shp
Midridge_herd$datetime_txt <- as.character(Midridge_herd$datetime)
Midridge_herd$year_txt <- as.character(Midridge_herd$year)


p.sf.all <- st_as_sf(Midridge_herd, coords = c("EASTING", "NORTHING"), crs = "+proj=utm +zone=21 ellps=WGS84") 
p.sf.all
st_write(p.sf.all, "~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/input/Migration_mapper_App_Input", "Midridge_herd_MM_App.shp", driver="ESRI Shapefile")


### Original migration dates
library(data.table)
library(dplyr)
MigrationDataNSD <- MigrationDataNSD[with(MigrationDataNSD, order(Time)),]
MigrationDataNSD_Midridge <- subset (MigrationDataNSD, Herd == "MIDRIDGE")
New_dates <- MigrationDataNSD_Midridge %>%
  group_by(Animal_ID, MigStartDay, MigEndDay) %>%
  summarise(Firstdate = min(Time),
            Lastdate = max(Time))
