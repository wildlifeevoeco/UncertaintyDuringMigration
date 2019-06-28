######### Extract randoms points for RSF ######## (Mike's code)
## Load packages

library(raster)
library(adehabitatHR)
library(spatstat)
library(maptools)
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


setDT(Randoms)
setDT(Observed)


build_pt_asset(
  DT = Randoms,
  out = 'output/Randoms-emilie-nlcaribou',
  projection = utm21N,
  id = 'ANIMAL_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Randoms), c('ANIMAL_ID', 'Easting', 'Northing')))

build_pt_asset(
  DT = Observed,
  out = 'output/Observed-emilie-nlcaribou',
  projection = utm21N,
  id = 'ANIMAL_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Observed), c('ANIMAL_ID', 'Easting', 'Northing')))



saveRDS(Randoms, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/RandomPoints/Randoms.RDS")
saveRDS(Observed, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/RandomPoints/Observed.RDS")

####When I will extract covariates
### Next steps are to remove points where IRG or other variables are NA

Migration2<-Migration[!is.na(Migration$IRG),]
nrow(Migration2)
summary(Migration2)
Migration2<-Migration2[!is.na(Migration2$PeakIRG),]
Migration2<-Migration2[!is.na(Migration2$SnowOffDate),]

setDT(Migration2)

NewD<-data.table(Migration2)

#### Find points/strata where only random points exist (mean == 0)
NewD[, StrMean := mean(Pres),by = .(strata)]
summary(NewD)
nrow(NewD)



