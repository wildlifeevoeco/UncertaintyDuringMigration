#########  Extracting spatial data from rasters to points  ##############

library(raster)
library(data.table)

### Simple example for landcover data (or any other data that is temporally static):
Filesraster<-list.files('C:/Users/emitn/OneDrive/Documents/Stage/Stage Canada/landcover/tifraster')

cariboumodel<-readRDS('~/Stage/Stage Canada/Caribou data/cariboumodel.Rds')

### Read in the landcover data using "raster"
Conifer<-raster("Conifer100.tif")
MixedWood<-raster("MixedWood100.tif")
Anthro<-raster("Anthro100.tif")
Broadleaf<-raster("Broadleaf100.tif")
Lichen<-raster("Lichen100.tif")
Rocky<-raster("Rocky100.tif")
Water<-raster("Water100.tif")
Wetland<-raster("Wetland100.tif")


### Then use the extract function to get the value of the raster at the points
##example : ConiferData<-extract(x=Conifer,y=data.frame(caribouData$UTMX,caribouData$UTMY))

Wetlanddata<-extract(x=Wetland, y=data.frame(cariboumodel$EASTING,cariboumodel$NORTHING))

### This generates a vector of the values at all your locations, 
### but it's easiest to just append it to your dataset:
##example : caribouData$Conifer<-extract(x=Conifer,y=data.frame(caribouData$UTMX,caribouData$UTMY))

cariboumodel$Wetland<-extract(x=Wetland, y=data.frame(cariboumodel$EASTING,cariboumodel$NORTHING))

### You can do this for each habitat class you're interested in,
### or combine categories:

Conifer<-raster("focalConiferForest.tif")
ConiferScrub<-raster("focalConiferScrub.tif")
MixedWood<-raster("MixedWood.tif")

Forest<-Conifer+Broadleaf+MixedWood

caribouData$Forest<-extract(x=Forest,y=data.frame(caribouData$UTMX,caribouData$UTMY))

###Delete a column 
library(data.table)
cariboumodel<-cariboumodel[, ConiferScrub := NULL]

###Save data 
saveRDS(cariboumodel, '~/Git/emilie_nlcaribou/output/Data extraction/cariboumodel.Rds')
write.csv(cariboumodel,'~/Git/emilie_nlcaribou/output/Data extraction/cariboumodel.csv')

