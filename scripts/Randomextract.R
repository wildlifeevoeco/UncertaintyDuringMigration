######### Extract randoms points for RSF ######## (Mike's code)
## Load packages

library(raster)
library(adehabitatHR)
library(spatstat)
library(maptools)
library(data.table)
library(rgdal)
#devtools::install_github('robitalec/toast')
library(toast)

#### Migration data

### Read in the data

Migr<-readRDS("~/Documents/Emilie_project/Git/emilie_nlcaribou/output/MoveHMM/outMR.RDS")

## Re-order the data so it's in the correct order
Migr<-Migr[order(Migr$IDYear,Migr$JDateTime),]

Migr = subset(Migr, select = -c(19))
## Add a point ID field
Migr$PtID<-c(1:nrow(Migr))
summary(Migr)

## Remove vestigial "X" field
Migr$X<-NULL

head(Migr)

### Migration models

## Rename columns
colnames(Migr)<-c("IDYear","ID","Herd","FixDate","FixTime","Year","Month","JDate","Easting","Northing",
                  "Time","JDateTime","Calved","Lost","CalvingDate","LossDate","NSD","Displace",
                  "MigStartDay","MigEndDay","MigDuration","PtID")

head(Migr)

### Making the random points by herd

### Middle Ridge ####

## Get rid of unused ID*Year levels
Migr$IDYear<-droplevels(Migr$IDYear)

## Set up spatial data for kernelUD function
idMidR<-data.frame(Migr$IDYear)
xyMidR<-cbind(Migr$Easting,Migr$Northing)
coordinates(idMidR)<-xyMidR

## Make the kernel
kernMidR<-kernelUD(idMidR, grid=800, extent=2, h=400)

## Get the 99% kernel HR
hr99kernMidR<-getverticeshr(kernMidR, percent = 99)

## Make a dataframe for the random points (25 for each point...will be reduced later)
rpMidR<-Migr[rep(seq_len(nrow(Migr)), each=15),]

## Set up the kernel HRs to use as windows for generating random points
yMidR <- as(hr99kernMidR, "SpatialPolygons")
pMidR <- slot(yMidR, "polygons")
vMidR <- lapply(pMidR, function(z) { SpatialPolygons(list(z)) })
winlistMidR <- lapply(vMidR, as.owin)

## Making list of bursts
burstsMidR<-unique(Migr$IDYear)

## Make the random points
t=1
for (i in burstsMidR){
  assign(paste("Rand",i,sep=""),runifpoint(n=nrow(subset(rpMidR, IDYear==i)), 
                                           win=winlistMidR[[t]]))
  t<-t+1
}

### Put them together - fairly long step
t <- 1    
for(i in burstsMidR){
  if (t==1){
    RandAllpppMidR <- eval(parse(text=paste("Rand",i,sep="")))
  } else {
    RandAllpppMidR <- superimpose(RandAllpppMidR, eval(parse(text=paste("Rand",i,sep=""))))
  }
  print(t/length(burstsMidR)*100)
  t <- t+1
}

## Make it a spatial points object
RandAllMidR<-as.SpatialPoints.ppp(RandAllpppMidR)
##Get the coordinates of the random locs
RandCoordsMidR<-coordinates(RandAllMidR)

##Put the dataframe together (random points)
RandDataMidR<-data.frame(rpMidR$Animal_ID, rpMidR$IDYear,rpMidR$Herd, rpMidR$FixDate,
                         rpMidR$FixTime, rpMidR$Year,rpMidR$Month,rpMidR$JDate,
                         RandCoordsMidR,rpMidR$PtID,rpMidR$Time, rpMidR$JDateTime, 
                         rpMidR$NSD,rpMidR$Displace,rpMidR$MigStartDay,rpMidR$MigEndDay,rpMidR$MigDuration, 
                         rpMidR$state,
                         rpMidR$step, rpMidR$angle, rpMidR$stepRes, rpMidR$angleRes)

##Set "presence" to 0
RandDataMidR$Presence<-0

##Put together matching used data
PresDataMidR<-data.frame(Migr$Animal_ID,Migr$IDYear,Migr$Herd,Migr$FixDate,
                         Migr$FixTime,Migr$Year,Migr$Month,Migr$JDate,
                         Migr$Easting,Migr$Northing,Migr$PtID,Migr$Time,
                         Migr$JDateTime,Migr$NSD,Migr$Displace,Migr$MigStartDay,
                         Migr$MigEndDay,Migr$MigDuration,Migr$state,Migr$step, 
                         Migr$angle, Migr$stepRes, Migr$angleRes)

##Set Presence to 1
PresDataMidR$Presence<-1

## Make column names
ColNames<-c("Animal_ID","IDYear","Herd","FixDate","FixTime","Year","Month","JDate","Easting",
            "Northing","PtID","Time","JDateTime","NSD","Displace","MigStartDay","MigEndDay",
            "MigDuration", "state", "step", "angle", "stepRes", "angleRes")

## Set column names for true and random data
colnames(RandDataMidR)<-ColNames
colnames(PresDataMidR)<-ColNames

### Dataset that includes all random locations...no spatial data yet though

## Put used and random points together
#RandDataMidR = subset(RandDataMidR, select = -c(24))
DataNewMidR<-rbind(PresDataMidR,RandDataMidR)

## New Point ID field (old pt ID field = "strata" field)
DataNewMidR$ptID<-c(1:nrow(DataNewMidR))
DataSortMidR<-DataNewMidR[order(DataNewMidR$JDate),]

summary(DataSortMidR)

saveRDS(DataSortMidR, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/RandomPoints/RSFMigrationMR.RDS")
DataSortMidR <- RSFMigrationMR
rm(list=ls())

###### Eliminate data that fall in water or that have NAs

## Read in water raster
Water<-raster("~/Documents/Emilie_project/Git/Landcover/Water.tif")

DataSortMidR$IsWater<-extract(Water,data.frame(DataSortMidR$Easting,DataSortMidR$Northing))
## Exclude points that fall in water
DataSortMidR<-subset(DataSortMidR,IsWater==0)

summary(DataSortMidR)
names(DataSortMidR)[24]<-"Randoms"
saveRDS(DataSortMidR, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/RandomPoints/RSFMigrationMRfinal.RDS")

#####subset data by random and obs points####
RSFMigrationMR$Randoms <- as.factor(RSFMigrationMR$Randoms)
Randoms<-subset(RSFMigrationMRfinal, Randoms != "1")
Observed<-subset(RSFMigrationMRfinal, Randoms == "1")
saveRDS(Randoms, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/RandomPoints/Randoms.RDS")
saveRDS(Observed, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/RandomPoints/Observed.RDS")


numloc <- Observed %>%
  group_by(Year)%>%
  summarise(total.fixes = n())

setDT(Randoms)
setDT(Observed)

nrdm <- nrow(Randoms)
Random1 <- Randoms[1:250000]
Random2 <- Randoms[250001: nrdm]
Randoms$FixDate <- as.Date( as.character(Randoms$FixDate), "%Y-%m-%d")
Random1 <- subset(Randoms, Year == '2010') 
Random2 <- subset(Randoms, Year == '2011' & FixDate >= '2011-02-12' & FixDate <= '2011-03-31')
Random3 <- subset(Randoms, Year == '2011' & FixDate >= '2011-04-01' & FixDate <= '2011-05-19')
Random4 <- subset(Randoms, Year > '2011' & Year <= '2013')

utm21N <- '+proj=utm +zone=21 ellps=WGS84'
Random2 <- as.data.table(Random2)
Random3 <- as.data.table(Random3)
Random4 <- as.data.table(Random4)

build_pt_asset(
  DT = Random1,
  out = 'output/Random1-emilie-nlcaribou',
  projection = utm21N,
  id = 'Animal_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Randoms), c('Animal_ID', 'Easting', 'Northing')))

build_pt_asset(
  DT = Random2,
  out = 'output/Random2-emilie-nlcaribou',
  projection = utm21N,
  id = 'Animal_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Randoms), c('Animal_ID', 'Easting', 'Northing')))

build_pt_asset(
  DT = Random3,
  out = 'output/Random3-emilie-nlcaribou',
  projection = utm21N,
  id = 'Animal_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Randoms), c('Animal_ID', 'Easting', 'Northing')))

build_pt_asset(
  DT = Random4,
  out = 'output/Random4-emilie-nlcaribou',
  projection = utm21N,
  id = 'Animal_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Randoms), c('Animal_ID', 'Easting', 'Northing')))

build_pt_asset(
  DT = Observed,
  out = 'output/Observed-emilie-nlcaribou',
  projection = utm21N,
  id = 'Animal_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Observed), c('Animal_ID', 'Easting', 'Northing')))

######Solve problem with disparition of 'angle' and 'angleRes' columns######
###create new dataframe with new column from Observed data with : time and ID
Obsall <- Observed[,TimeIDYear := paste(Time, IDYear, sep = '_')]
Obsall = subset(Obsall, select = c(9:10,21,23,27))
##create new column from Daymet with time and ID
observed_daymet_2<-as.data.table(observed_daymet_2)
obsDaymet <- observed_daymet_2[,TimeIDYear := paste(Time, IDYear, sep = '_')]
##Merge new dataset with obsDaymet by time/ID column
mergeObs <- merge(Obsall, obsDaymet, all.x = TRUE, by = colnames("TimeIDYear"))

###After EE
obsDaymet <- fread('output/Observed-emilie-nlcaribou/observed-daymet.csv')
rdm1Daymet <- fread('~/Documents/Emilie_project/Git/emilie_nlcaribou/output/Random1-emilie-nlcaribou/random1-daymet.csv')
rdm2Daymet <- fread('~/Documents/Emilie_project/Git/emilie_nlcaribou/output/Random2-emilie-nlcaribou/random2-daymet-2.csv')
rdm3Daymet <- fread('~/Documents/Emilie_project/Git/emilie_nlcaribou/output/Random3-emilie-nlcaribou/random3-daymet.csv')
rdm4Daymet <- fread('~/Documents/Emilie_project/Git/emilie_nlcaribou/output/Random4-emilie-nlcaribou/random4-daymet.csv')

rdm3Daymet = subset(rdm3Daymet, select = -c(20:21))
rdmDaymet <- rbindlist(list(rdm1Daymet, rdm2Daymet, rdm3Daymet, rdm4Daymet))

#Observed <- readRDS('the original Observed file')
#Randoms <- readRDS('the original Randoms file')

names(Randoms)[25]<-"ptID_1"
Rdm = subset(rdmDaymet, select = c(23:26, 30:35))
##Merge new dataset with obsDaymet by time/ID column
mergeRdm <- merge(Rdm, Randoms, all.x = TRUE, by = colnames("ptID_1"))


####When I will extract covariates
### Next steps are to remove points where IRG or other variables are NA
sum(is.na(mergeRdm$swe))
sum(is.na(mergeRdm$tmax))
sum(is.na(mergeRdm$tmin))     ### O NAs all covariates
sum(is.na(mergeRdm$vp))
sum(is.na(mergeRdm$prcp))

saveRDS(mergeObs, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/mergeObs.RDS")
saveRDS(mergeRdm, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/mergeRdm.RDS")

names(mergeObs)[7]<-"Animal_ID"
names(mergeObs)[15]<-"JDateTime"
names(mergeObs)[8]<-"Displace"
names(mergeObs)[17]<-"MigStartDay"
names(mergeObs)[16]<-"MigEndDay"
names(mergeObs)[18]<-"MigDuration"
setcolorder(mergeObs, c("Animal_ID","IDYear","Herd","FixDate","FixTime","Year","Month","JDate","Easting",
                        "Northing","PtID","Time","JDateTime","NSD","Displace","MigStartDay","MigEndDay",
                        "MigDuration", "state", "step", "angle", "stepRes", "angleRes", "Randoms", "IsWater", 
                        "ptID_1", "jul", "prcp", "srad", "swe", "tmax", "tmin", "vp", "yr", ".geo"))
mergeObs = subset(mergeObs, select = -c(36:40))
setcolorder(mergeRdm, c("Animal_ID","IDYear","Herd","FixDate","FixTime","Year","Month","JDate","Easting",
                        "Northing","PtID","Time","JDateTime","NSD","Displace","MigStartDay","MigEndDay",
                        "MigDuration", "state", "step", "angle", "stepRes", "angleRes", "Randoms", "IsWater", 
                        "ptID_1", "jul", "prcp", "srad", "swe", "tmax", "tmin", "vp", "yr", ".geo"))

mergeObs<-mergeObs[order(mergeObs$ptID_1),]
mergeRdm<-mergeRdm[order(mergeRdm$ptID_1),]

mergeObs$FixDate <-as.Date(mergeObs$FixDate, format="%Y-%m-%d")
mergeObs$FixTime <-as.ITime(mergeObs$FixTime, format="%H:%M:%S")
mergeRdm$FixDate <-as.Date(mergeRdm$FixDate, format="%Y-%m-%d")
mergeRdm$FixTime <-as.ITime(mergeRdm$FixTime, format="%H:%M:%S")
mergeRdm$Time <- as.POSIXct(mergeRdm$Time,format="%Y-%m-%d %H:%M:%S")
mergeObs$Time <- as.POSIXct(mergeObs$Time,format="%Y-%m-%d %H:%M:%S")

allweather <- rbind(mergeObs,mergeRdm)
saveRDS(allweather, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/allweather.Rds")

#Migration2<-Migration[!is.na(mergeObs$swe),]
#nrow(Migration2)
#summary(Migration2)
#Migration2<-Migration2[!is.na(Migration2$PeakIRG),]
#Migration2<-Migration2[!is.na(Migration2$SnowOffDate),]
#setDT(Migration2)
#NewD<-data.table(Migration2)

#### Find points/strata where only random points exist (mean == 0)
#NewD[, StrMean := mean(Pres),by = .(strata)]
#summary(NewD)
#nrow(NewD)

