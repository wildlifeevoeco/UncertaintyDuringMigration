######### Extract randoms points for RSF ######## (Mike's code)

rm(list=ls())

### Packages ---
libs <- c('data.table', 'moveHMM',  'raster', 'adehabitatHR',
          'dplyr', 'spatstat', 'maptools', 'data.table', 'rgdal', 'toast')
lapply(libs, require, character.only = TRUE)

#### Migration data

Migr<-readRDS("~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/outMR.Rds")

## Re-order the data so it's in the correct order
Migr<-Migr[order(Migr$IDYear,Migr$JDateTime),]

## Add a point ID field
# Migr$PtID<-c(1:nrow(Migr))
summary(Migr)

## Remove vestigial "X" field
# Migr$X<-NULL

head(Migr)

### Migration models

## Rename columns
colnames(Migr)<-c("Easting","Northing", "IDYear","Animal_ID","Herd","FixDate","FixTime","Year","Month","JDate",
                  "Time","JDateTime","Calved", "Lost","CalvingDate", "LossDate", "NSD","Displace",
                  "MigStartDay","MigEndDay","MigDuration", "PtID","state", "step", "angle", "(Intercept)", "stepRes",
                  "angleRes")
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
colnames(RandDataMidR)
PresDataMidR<-data.frame(Migr$Easting,Migr$Northing,Migr$IDYear,
                         Migr$Animal_ID,Migr$Herd,Migr$FixDate,
                         Migr$FixTime,Migr$Year,Migr$Month,Migr$JDate,Migr$Time,
                         Migr$JDateTime, Migr$NSD,Migr$Displace,Migr$MigStartDay,
                         Migr$MigEndDay,Migr$MigDuration,Migr$PtID,
                         Migr$state,Migr$step, 
                         Migr$angle, Migr$`(Intercept)`, Migr$stepRes, Migr$angleRes)

##Set Presence to 1
PresDataMidR$Presence<-1

## Make column names
ColNames<-c("Easting","Northing", "IDYear","Animal_ID","Herd","FixDate","FixTime","Year","Month","JDate",
            "Time","JDateTime", "NSD","Displace",
            "MigStartDay","MigEndDay","MigDuration", "PtID","state", "step", "angle", "(Intercept)", "stepRes",
            "angleRes")

## Set column names for true and random data
colnames(RandDataMidR)<-ColNames
colnames(PresDataMidR)<-ColNames

### Dataset that includes all random locations...no spatial data yet though

## Put used and random points together
PresDataMidR = subset(PresDataMidR, select = -c(25))
DataNewMidR<-rbind(PresDataMidR,RandDataMidR)

## New Point ID field (old pt ID field = "strata" field)
DataNewMidR$ptID<-c(1:nrow(DataNewMidR))
DataSortMidR<-DataNewMidR[order(DataNewMidR$JDate),]


saveRDS(DataSortMidR, "~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/RSFdata_MigrationMR.RDS")



###### Eliminate data that fall in water or that have NAs#######
################################################################

## Read in water raster
Water<-raster("/Internship 2019 Mun/Git/emilie_nlcaribou/input/Landcover/Water.tif")

DataSortMidR$IsWater<-extract(Water,data.frame(DataSortMidR$Easting,DataSortMidR$Northing))
## Exclude points that fall in water
DataSortMidR<-subset(DataSortMidR,IsWater==0)

#######Save file with obs/randoms pts##########
# summary(DataSortMidR)
names(DataSortMidR)[24]<-"Randoms"
saveRDS(DataSortMidR, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/RandomPoints/RSFMigrationMRfinal.RDS")


RSFMigrationMRfinal <- readRDS("~/Internship 2019 Mun/Git/emilie_nlcaribou/output/RandomPoints/RSFMigrationMRfinal.RDS")

#####subset data by random and obs points separately ########3
RSFMigrationMRfinal$Randoms <- as.factor(RSFMigrationMRfinal$Randoms)
Randoms<-subset(RSFMigrationMRfinal, Randoms != "1")
Observed<-subset(RSFMigrationMRfinal, Randoms == "1")
saveRDS(Randoms, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/RandomPoints/Randoms.RDS")
saveRDS(Observed, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/RandomPoints/Observed.RDS")

write.csv2(Randoms, "~/Internship 2019 Mun/Git/emilie_nlcaribou/output/Randoms.csv")
write.csv2(Observed, "~/Internship 2019 Mun/Git/emilie_nlcaribou/output/Observed.csv")

data.table::fwrite(Observed, 'Observed.csv')
data.table::fwrite(Randoms, 'Randoms.csv')


####### Finally number of pts in the study with 30 individuals #####
numloc <- Observed %>%
  group_by(Year)%>%
  summarise(total.fixes = n())

############ PREPARE DATA FOR EARTH ENGINE EXTRACTION########
#############################################################
setDT(Randoms)
setDT(Observed)

nrdm <- nrow(Randoms)

####Subset randoms data in 4 files
Randoms$FixDate <- as.Date( as.character(Randoms$FixDate), "%Y-%m-%d")
Random1 <- subset(Randoms, Year == '2010') 
Random2 <- subset(Randoms, Year == '2011' & FixDate >= '2011-02-12' & FixDate <= '2011-03-31')
Random3 <- subset(Randoms, Year == '2011' & FixDate >= '2011-04-01' & FixDate <= '2011-05-19')
Random4 <- subset(Randoms, Year > '2011' & Year <= '2013')

utm21N <- '+proj=utm +zone=21 ellps=WGS84'
Random2 <- as.data.table(Random2)
Random3 <- as.data.table(Random3)
Random4 <- as.data.table(Random4)

#################Prepare folders with data.table/shp ######
setwd("~/Internship 2019 Mun/Git/emilie_nlcaribou")

build_pt_asset(
  DT = Random1,
  out = 'output/Random1-emilie-nlcaribou_2020',
  projection = utm21N,
  id = 'Animal_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Randoms), c('Animal_ID', 'Easting', 'Northing')))

build_pt_asset(
  DT = Random2,
  out = 'output/Random2-emilie-nlcaribou_2020',
  projection = utm21N,
  id = 'Animal_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Randoms), c('Animal_ID', 'Easting', 'Northing')))

build_pt_asset(
  DT = Random3,
  out = 'output/Random3-emilie-nlcaribou_2020',
  projection = utm21N,
  id = 'Animal_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Randoms), c('Animal_ID', 'Easting', 'Northing')))

build_pt_asset(
  DT = Random4,
  out = 'output/Random4-emilie-nlcaribou_2020',
  projection = utm21N,
  id = 'Animal_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Randoms), c('Animal_ID', 'Easting', 'Northing')))

build_pt_asset(
  DT = Observed,
  out = 'output/Observed-emilie-nlcaribou_2020',
  projection = utm21N,
  id = 'Animal_ID',
  coords = c('Easting', 'Northing'),
  extra = setdiff(colnames(Observed), c('Animal_ID', 'Easting', 'Northing')))

#########AFTER DOWNLOADED DATA FROM EARTH ENGINE###########
###After EE
obsDaymet <- fread('output/Observed-emilie-nlcaribou/observed-daymet.csv')
rdm1Daymet <- fread('~/Documents/Emilie_project/Git/emilie_nlcaribou/output/Random1-emilie-nlcaribou_2020/random1-daymet.csv')
rdm2Daymet <- fread('~/Documents/Emilie_project/Git/emilie_nlcaribou/output/Random2-emilie-nlcaribou_2020/random2-daymet-2.csv')
rdm3Daymet <- fread('~/Documents/Emilie_project/Git/emilie_nlcaribou/output/Random3-emilie-nlcaribou_2020/random3-daymet.csv')
rdm4Daymet <- fread('~/Documents/Emilie_project/Git/emilie_nlcaribou/output/Random4-emilie-nlcaribou_2020/random4-daymet.csv')

rdm3Daymet = subset(rdm3Daymet, select = -c(20:21))
rdmDaymet <- rbindlist(list(rdm1Daymet, rdm2Daymet, rdm3Daymet, rdm4Daymet))

obsNDSI <- fread("C:/Users/emitn/Documents/Internship 2019 Mun/Git/emilie_nlcaribou/output/ouput_gee/observed-ndsi.csv")
RanNDSI <- fread("C:/Users/emitn/Documents/Internship 2019 Mun/Git/emilie_nlcaribou/output/ouput_gee/random-ndsi.csv")

###Test if ndsi data are correct
colnames(obsNDSI)

hist(obsNDSI$NDSI_Snow_Cover)
summary(obsNDSI$NDSI_Snow_Cover)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00   58.00   46.01   77.00   91.00   27473
hist(obsNDSI$NDSI_Snow_Cover_Algorithm_Flags_QA, breaks = 60)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.0000  0.0000  0.0555  0.0000 14.0000      81
summary(obsNDSI$NDSI_Snow_Cover_Algorithm_Flags_QA)
hist(obsNDSI$NDSI_Snow_Cover_Basic_QA)
summary(obsNDSI$NDSI_Snow_Cover_Basic_QA)
hist(obsNDSI$NDSI_Snow_Cover_Class)
summary(obsNDSI$NDSI_Snow_Cover_Class)
hist(obsNDSI$Snow_Albedo_Daily_Tile)
summary(obsNDSI$Snow_Albedo_Daily_Tile)
summary(obsNDSI$NDSI)

obsNDSI$state  <- as.factor(obsNDSI$state)
ggplot(data = obsNDSI, aes (x = state, y = NDSI_Snow_Cover)) +
  geom_boxplot()

obsNDSI_subNA <- subset(obsNDSI, NDSI_Snow_Cover)
obsNDSI_subNA <- subset(obsNDSI, is.na(obsNDSI$NDSI_Snow_Cover))
summary(obsNDSI_subNA$imgdoy)
summary(obsNDSI_subNA$imgyear)

data.table::fwrite(obsNDSI_subNA, 'obsNDSI_subNA.csv')


###
obsNDSI[, .N, by = .(is.na(NDSI_Snow_Cover), NDSI_Snow_Cover_Class)]

###Same with random 
###Test if ndsi data are correct
colnames(RanNDSI)

hist(RanNDSI$NDSI_Snow_Cover)
summary(RanNDSI$NDSI_Snow_Cover)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00   58.00   46.01   77.00   91.00   27473
hist(RanNDSI$NDSI_Snow_Cover_Algorithm_Flags_QA, breaks = 60)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.0000  0.0000  0.0555  0.0000 14.0000      81
summary(RanNDSI$NDSI_Snow_Cover_Algorithm_Flags_QA)
hist(RanNDSI$NDSI_Snow_Cover_Basic_QA)
summary(RanNDSI$NDSI_Snow_Cover_Basic_QA)
hist(RanNDSI$NDSI_Snow_Cover_Class)
summary(RanNDSI$NDSI_Snow_Cover_Class)
hist(RanNDSI$Snow_Albedo_Daily_Tile)
summary(RanNDSI$Snow_Albedo_Daily_Tile)
summary(RanNDSI$NDSI)

RanNDSI$state  <- as.factor(RanNDSI$state)
ggplot(data = RanNDSI, aes (x = state, y = NDSI_Snow_Cover)) +
  geom_boxplot()

RanNDSI_subNA <- subset(RanNDSI, NDSI_Snow_Cover)
RanNDSI_subNA <- subset(RanNDSI, is.na(RanNDSI$NDSI_Snow_Cover))
summary(RanNDSI_subNA$imgdoy)
summary(RanNDSI_subNA$imgyear)

RanNDSI[, .N, by = .(is.na(NDSI_Snow_Cover), NDSI_Snow_Cover_Class)]

DT <- fread('obsNDSI_subNA.csv')
RanNDSI_subNA[, `.geo` := NULL]

RanNDSI_subNA[is.na(NDSI_Snow_Cover),
   .N,
   by = .(NDSI_Snow_Cover_Algorithm_Flags_QA,
          NDSI_Snow_Cover_Basic_QA,
          NDSI_Snow_Cover_Class,
          Snow_Albedo_Daily_Tile_Class)][order(-N)]

######Solve problem with disparition of 'angle' and 'angleRes' columns######

###create new dataframe with new column from Observed data with : time and ID
Obsall <- Observed[,TimeIDYear := paste(Time, IDYear, sep = '_')]
Obsall = subset(Obsall, select = c(9:10,21,23,27))
##create new column from Daymet with time and ID
observed_daymet_2<-as.data.table(observed_daymet_2)
obsDaymet <- observed_daymet_2[,TimeIDYear := paste(Time, IDYear, sep = '_')]
##Merge new dataset with obsDaymet by time/ID column
mergeObs <- merge(Obsall, obsDaymet, all.x = TRUE, by = colnames("TimeIDYear"))

#Observed <- readRDS('the original Observed file')
#Randoms <- readRDS('the original Randoms file')

###subset Rdm file by the columns that we need
names(Randoms)[25]<-"ptID_1"
Rdm = subset(rdmDaymet, select = c(23:26, 30:35))
##Merge new dataset with RdmDaymet by time/ID column
mergeRdm <- merge(Rdm, Randoms, all.x = TRUE, by = colnames("ptID_1"))


####When I will extract covariates
### Next steps are to remove points where variables are NA
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

####Prepare data and save for extraction NDVI/weather 
allweather <- rbind(mergeObs,mergeRdm)
saveRDS(allweather, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/allweather.Rds")

#Migration2<-Migration[!is.na(mergeObs$swe),]
#nrow(Migration2)
#summary(Migration2)
#Migration2<-Migration2[!is.na(Migration2$PeakIRG),]
#Migration2<-Migration2[!is.na(Migration2$SnowOffDate),]
#setDT(Migration2)
NewD<-data.table(allNDVI)

#### Find points/strata where only random points exist (mean == 0)

NewD$Randoms<-ifelse(NewD$Randoms=="1",1,0)
head(NewD)
summary(NewD)
NewD[, StrMean := mean(as.numeric(Randoms)),by = .(PtID)]
summary(NewD)
nrow(NewD)

NewD2<-subset(NewD,StrMean>0)

#RSFdata<-NewD2
#RSFdataR<-RSFdata[, .SD[1:11], by = PtID]
#RSFdataR[, .N, by = PtID]
#summary(RSFdataR)
####savenewD2

saveRDS(allNDVI, "~/Documents/Emilie_project/Git/emilie_nlcaribou/output/allNDVI.Rds")


