#########  Extracting spatial data from rasters to points  ##############

library(raster)
library(data.table)

### Simple example for landcover data (or any other data that is temporally static):
Filesraster<-list.files('~/Documents/Emilie_project/Git/landcover')
setwd('~/Documents/Emilie_project/Git/landcover')

### Read in the landcover data using "raster"
Conifer<-raster("Conifer100.tif")
MixedWood<-raster("MixedWood100.tif")
Anthro<-raster("Anthro100.tif")
Broadleaf<-raster("Broadleaf100.tif")
Lichen<-raster("Lichen100.tif")
Rocky<-raster("Rocky100.tif")
Water<-raster("Water100.tif")
Wetland<-raster("Wetland100.tif")

MixedWood_Bdl <- Broadleaf + MixedWood
Forest <- Broadleaf + MixedWood + Conifer

### Then use the extract function to get the value of the raster at the points
##example : ConiferData<-extract(x=Conifer,y=data.frame(caribouData$UTMX,caribouData$UTMY))

#WetlandData<-extract(x=Wetland, y=data.frame(cariboumodel$EASTING,cariboumodel$NORTHING))
WaterData<-extract(x=Water, y=data.frame(allweather$Easting,allweather$Northing))

### This generates a vector of the values at all your locations, 
### but it's easiest to just append it to your dataset:
##example : caribouData$Conifer<-extract(x=Conifer,y=data.frame(caribouData$UTMX,caribouData$UTMY))

allweather$Water<-extract(x=Water, y=data.frame(allweather$Easting,allweather$Northing))

### You can do this for each habitat class you're interested in,
### or combine categories:

Conifer<-raster("focalConiferForest.tif")
ConiferScrub<-raster("focalConiferScrub.tif")
MixedWood<-raster("MixedWood.tif")

Forest<-Conifer+Broadleaf+MixedWood

caribouData$Forest<-extract(x=Forest,y=data.frame(caribouData$UTMX,caribouData$UTMY))

###Delete a column 
cariboumodel<-cariboumodel[, ConiferScrub := NULL]

###Save data 
saveRDS(cariboumodel, '~/Git/emilie_nlcaribou/output/Data extraction/cariboumodel.Rds')
save(cariboumodel, file ='~/Git/emilie_nlcaribou/output/Data extraction/cariboumodel.csv')
saveRDS(allweather, '~/Documents/Emilie_project/Git/emilie_nlcaribou/output/allweather2.Rds')

