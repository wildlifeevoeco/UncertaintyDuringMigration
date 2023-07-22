##################
##################
#### Maj 06/11 - Extract habitat based on new buffer size = 200m ####


### Packages ---
library(data.table)
library(ggplot2)
library(rgdal)
library(spatsoc)
library(amt)
library(tidyverse)
library(lubridate)
library(raster)
library(sp)
library(adehabitatHR)

##Input raw data
allNDVI <- readRDS("output/allNDVI.Rds")

##Input landcover data
lcNewfoundland <- raster("../nl-landcover/input/FINAL_PRODUCT/FINAL_RC.tif")
Legend <- fread("../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv") 

#####Calculate MCP for MIDRIGDE
allNDVI$Animal_ID <- as.character(allNDVI$Animal_ID) ### if not, mcp doesn't work

allNDVI_sp <- allNDVI[,c("Animal_ID","Easting", "Northing")]

# Create a SpatialPointsDataFrame by defining the coordinates
coordinates(allNDVI_sp) <- c("Easting", "Northing")

#set UTM
proj4string(allNDVI_sp) <- CRS( "+proj=utm +zone=21 ellps=WGS84" )

# Calculate MCPs 
allNDVI_sp_mcp <- mcp(allNDVI_sp, percent = 95)
plot(allNDVI_sp_mcp)

### CROP MCP WITH lcNL
MR_lc <- crop(lcNewfoundland, allNDVI_sp_mcp)
plot(MR_lc)
plot(allNDVI_sp_mcp, add= TRUE)

##### Landcover MR
MR_lc[is.na(MR_lc)] <- 10
WetlandMR <- subs(MR_lc, data.frame(Legend$Value, ifelse(Legend$Landcover=="Wetland",1,0)))
BroadleafMR <- subs(MR_lc, data.frame(Legend$Value, ifelse(Legend$Landcover=="Broadleaf",1,0)))
ConiferMR <- subs(MR_lc, data.frame(Legend$Value, ifelse(Legend$Landcover=="Conifer Forest",1,0)))
ScrubMR <- subs(MR_lc, data.frame(Legend$Value, ifelse(Legend$Landcover=="Conifer Scrub",1,0)))
MixedWoodMR <- subs(MR_lc, data.frame(Legend$Value, ifelse(Legend$Landcover=="Mixed Wood",1,0)))
RockMR <- subs(MR_lc, data.frame(Legend$Value, ifelse(Legend$Landcover=="Rocky Barren",1,0)))
WaterMR <- subs(MR_lc, data.frame(Legend$Value, ifelse(Legend$Landcover=="Water",1,0)))
LichenMR <- subs(MR_lc, data.frame(Legend$Value, ifelse(Legend$Landcover=="Lichen and Heath",1,0)))
AnthroMR <- subs(MR_lc, data.frame(Legend$Value, ifelse(Legend$Landcover=="Anthropogenic and disturbance",1,0)))

## combine habitat types ### doestn't work ?
open_MR <- (WetlandMR + RockMR + WaterMR)
Forest_MR <- ConiferMR + MixedWoodMR + ScrubMR + BroadleafMR

###### Create focal of 200m
openBuffMR <- focalWeight(open_MR, d = 400, type='circle')
ForestBuffMR <- focalWeight(Forest_MR, d = 400, type='circle')
LichenBuffMR <- focalWeight(LichenMR, d = 400, type='circle')
AnthroBuffMR <- focalWeight(AnthroMR, d = 400, type='circle')

##FOCAL ON Raster
OpenBuff200MR <- focal(open_MR,openBuffMR,na.rm=TRUE,pad=TRUE,padValue=0)
ForestBuff200MR <- focal(Forest_MR,ForestBuffMR,na.rm=TRUE,pad=TRUE,padValue=0)
LichenBuff200MR <- focal(LichenMR,LichenBuffMR,na.rm=TRUE,pad=TRUE,padValue=0)
AnthroBuff200MR <- focal(AnthroMR,AnthroBuffMR,na.rm=TRUE,pad=TRUE,padValue=0)


###Time to export new buffered data on dataset 
allNDVI$Lichen_200<-extract(x=LichenBuff200MR,y=data.frame(allNDVI$Easting,allNDVI$Northing))
allNDVI$Open_200<-extract(x=OpenBuff200MR,y=data.frame(allNDVI$Easting,allNDVI$Northing))
allNDVI$Forest_200<-extract(x=ForestBuff200MR,y=data.frame(allNDVI$Easting,allNDVI$Northing))
allNDVI$Anthro_200<-extract(x=AnthroBuff200MR,y=data.frame(allNDVI$Easting,allNDVI$Northing))

summary(allNDVI$Lichen)
summary(allNDVI$Lichen_200)

summary(allNDVI$Forest)
summary(allNDVI$Forest_200)

summary(allNDVI$Anthro)
summary(allNDVI$Anthro_200)

summary(allNDVI$Wetland)
summary(allNDVI$Open_200)

boxplot(Lichen_200 ~ Randoms, data = allNDVI, "Lichen")
boxplot(Open_200 ~ Randoms, data = allNDVI, "Lichen")
boxplot(Forest_200 ~ Randoms, data = allNDVI, "Lichen")


#### Clean the table, save it as new 
colnames(allNDVI)
Final_MR_migration<-allNDVI[order(allNDVI$IDYear,allNDVI$JDateTime),]

saveRDS(Final_MR_migration, "output/Final_MR_migration.RDS")
