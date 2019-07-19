###~~Extraction of climate data~~###

######RclimatePackage
library(rclimateca)
library(dplyr)
library(tidyverse)

###Stephenville station
getClimateSites("Stephenville A")
dailyStephenville<-ec_climate_data(6740, timeframe = "daily", start = "2006-06-28", end = "2013-10-03", quiet = FALSE)
summary(dailyStephenville$mean_temp_c)   ##10 NA for mean t?
summary(dailyStephenville$total_rain_mm) ##11 NA for mean total rain
summary(dailyStephenville$total_precip_mm) ##11 NA for mean total precip
dailyStephenville$mean_temp_c<-as.numeric(dailyStephenville$mean_temp_c)
dailyStephenville$total_rain_mm<-as.numeric(dailyStephenville$total_rain_mm)
dailyStephenville$total_precip_mm<-as.numeric(dailyStephenville$total_precip_mm)

##Gander Int'La 6633 (Good station)
getClimateSites("Gander")
dailyGander<-ec_climate_data(6633, timeframe ="daily", start = "2006-06-28", end = "2012-03-19")
summary(dailyGander$mean_temp_c) 
summary(dailyGander$total_rain_mm) 
summary(dailyGander$total_precip_mm)
dailyGander$mean_temp_c<-as.numeric(dailyGander$mean_temp_c)
dailyGander$total_rain_mm<-as.numeric(dailyGander$total_rain_mm)
dailyGander$total_precip_mm<-as.numeric(dailyGander$total_precip_mm)

dailyGander <- dailyGander[, c(2, 3, 4, 5, 6, 12, 18, 22)]

##Gander Airport CS (Only years 2012-2013)
dailyGanderAirport<-ec_climate_data(30346, timeframe ="daily", start = "2012-03-20", end = "2013-10-03")
summary(dailyGanderAirport$mean_temp_c)
summary(dailyGanderAirport$total_rain_mm)
summary(dailyGanderAirport$total_precip_mm)
dailyGanderAirport$mean_temp_c<-as.numeric(dailyGanderAirport$mean_temp_c)
dailyGanderAirport$total_rain_mm<-as.numeric(dailyGanderAirport$total_rain_mm)
dailyGanderAirport$total_precip_mm<-as.numeric(dailyGanderAirport$total_precip_mm)
dailyGanderAirport <- dailyGanderAirport[, c(2, 3, 4, 5, 6, 12, 18, 22)]

##Gander Intl A 50088 (only 2012-2013)
dailyGanderINT<-ec_climate_data(50088, timeframe = "daily", start = "2012-03-20", end = "2013-10-03", quiet = FALSE)
dailyGanderINT$mean_temp_c<-as.numeric(dailyGanderINT$mean_temp_c)
dailyGanderINT$total_rain_mm<-as.numeric(dailyGanderINT$total_rain_mm)
dailyGanderINT$total_precip_mm<-as.numeric(dailyGanderINT$total_precip_mm)
summary(dailyGanderINT$mean_temp_c)
summary(dailyGanderINT$total_rain_mm)
summary(dailyGanderINT$total_precip_mm)


###~~Gander average~~###
###GanderAiport CS (20/03/12 --> 03/10/13)
GanderextractAirportCS<-dailyGanderAirport
GanderextractAirportCS<-dailyGanderAirport[,c(6,12,18,22)]
setnames(GanderextractAirportCS, c("mean_temp_c"), c("mean_temp_ganderairCS"))
setnames(GanderextractAirportCS, c("total_rain_mm"), c("total_rain_ganderairCS"), skip_absent = TRUE)
setnames(GanderextractAirportCS, c("total_precip_mm"), c("total_precip_ganderairCS"), skip_absent = TRUE)

###GanderIntlA 50088 (20/03/12 --> 03/10/13)
GanderextractAirportINT50088<-dailyGanderINT[,c(12,18,22)]
setnames(GanderextractAirportINT50088, c("mean_temp_c"), c("mean_temp_ganderINT088"))
setnames(GanderextractAirportINT50088, c("total_rain_mm"), c("total_rain_ganderINT088"))
setnames(GanderextractAirportINT50088, c("total_precip_mm"), c("total_precip_ganderINT088"))

###Average of both
Gander1213<-cbind(GanderextractAirportCS, GanderextractAirportINT50088)

Gander1213$mean_temp_c<-rowMeans(Gander1213[c('mean_temp_ganderairCS','mean_temp_ganderINT088')],na.rm=TRUE)
Gander1213$total_rain_mm<-rowMeans(Gander1213[c('total_rain_ganderairCS','total_rain_ganderINT088')],na.rm=TRUE)
Gander1213$total_precip_mm<-rowMeans(Gander1213[c('total_precip_ganderairCS','total_precip_ganderINT088')],na.rm=TRUE)

summary(Gander1213$mean_temp_c)
summary(Gander1213$total_rain_mm)
summary(Gander1213$total_precip_mm)

Gander1213GoodTab<-Gander1213[,c(1:5,12,13,14)]

###Combine 2 tabs (add rows)
TotalGander<-bind_rows(dailyGander,Gander1213GoodTab)


###CornerBrook station (ID : NL 6610, good station)
getClimateSites("Corner Brook")
dailyCornerBrook1<-ec_climate_data(6610, timeframe ="daily", start = "2006-06-28", end = "2013-10-03", quiet = FALSE, value_parser = readr::parse_character)
summary(dailyCornerBrook1$mean_temp_c)
summary(dailyCornerBrook1$total_rain_mm)
summary(dailyCornerBrook1$total_precip_mm)
dailyCornerBrook1$mean_temp_c<-as.numeric(dailyCornerBrook1$mean_temp_c)
dailyCornerBrook1$total_rain_mm<-as.numeric(dailyCornerBrook1$total_rain_mm)
dailyCornerBrook1$total_precip_mm<-as.numeric(dailyCornerBrook1$total_precip_mm)

##CornerBrook 2nd station (bad --> not use)
dailyCornerBrook2<-ec_climate_data(6933, timeframe ="daily", start = "2006-06-28", end = "2013-10-03", quiet = FALSE, value_parser = readr::parse_character)
summary(dailyCornerBrook2$mean_temp_c)
summary(dailyCornerBrook2$total_rain_mm)
summary(dailyCornerBrook2$total_precip_mm)
dailyCornerBrook2$mean_temp_c<-as.numeric(dailyCornerBrook2$mean_temp_c)
dailyCornerBrook2$total_rain_mm<-as.numeric(dailyCornerBrook2$total_rain_mm)
dailyCornerBrook2$total_precip_mm<-as.numeric(dailyCornerBrook2$total_precip_mm)


###Correlation test 
Gander0612<-subset(dailyGander, year =='2006' | year ==  '2007' | year ==  '2008' | year ==  '2009' | year ==  '2010' | year ==  '2011' | year ==  '2012')
Stephenville0612<-subset(dailyStephenville, year == '2006' | year ==  '2007' | year ==  '2008' | year ==  '2009' | year ==  '2010' | year ==  '2011' | year ==  '2012')
CornerBrook0612<-subset(dailyCornerBrook1, year =='2006' | year ==  '2007' | year ==  '2008' | year ==  '2009' | year ==  '2010' | year ==  '2011' | year ==  '2012')


library("ggpubr")
shapiro.test(Gander0612$mean_temp_c)
shapiro.test(Stephenville0612$mean_temp_c)
ggqqplot(Gander0612$mean_temp_c, ylab = "Mean Temperature")

cortest2 <- cor.test(CornerBrook0612$mean_temp_c, Stephenville0612$mean_temp_c,  method="kendall")
cortest2

cortest<-cor.test(dailyGander$mean_temp_c, dailyCornerBrook1$mean_temp_c, method = "kendall")

library(dplyr)
library(data.table)

###Extract column for CornerBrook and Stephenville and change names
extractCornerBrook<-dailyCornerBrook1[,c(12,18,22)]
extractStephen<-dailyStephenville[,c(12,18,22)]

setnames(extractCornerBrook, c("mean_temp_c"), c("mean_temp_CB"))
setnames(extractCornerBrook, c("total_rain_mm"), c("total_rain_CB"))
setnames(extractCornerBrook, c("total_precip_mm"), c("total_precip_CB"))

setnames(extractStephen, c("mean_temp_c"), c("mean_temp_S"))
setnames(extractStephen, c("total_rain_mm"), c("total_rain_S"))
setnames(extractStephen, c("total_precip_mm"), c("total_precip_S"))

###Create new column
Meanalltab<-cbind(TotalGander, extractStephen,extractCornerBrook)

### Calculate average for the 3 stations and add new column
Meanalltab$mean_temp_c<-as.numeric(Meanalltab$mean_temp_c)
Meanalltab$mean_temp_CB<-as.numeric(Meanalltab$mean_temp_CB)
Meanalltab$mean_temp_S<-as.numeric(Meanalltab$mean_temp_S)

Meanalltab$total_rain_mm<-as.numeric(Meanalltab$total_rain_mm)
Meanalltab$total_rain_CB<-as.numeric(Meanalltab$total_rain_CB)
Meanalltab$total_rain_S<-as.numeric(Meanalltab$total_rain_S)

Meanalltab$total_precip_mm<-as.numeric(Meanalltab$total_precip_mm)
Meanalltab$total_precip_CB<-as.numeric(Meanalltab$total_precip_CB)
Meanalltab$total_precip_S<-as.numeric(Meanalltab$total_precip_S)

Meanalltab$mean_temp_all<-rowMeans(Meanalltab[c('mean_temp_c','mean_temp_CB', 'mean_temp_S')],na.rm=TRUE)
Meanalltab$total_rain_all<-rowMeans(Meanalltab[c('total_rain_mm','total_rain_CB', 'total_rain_S')],na.rm=TRUE)
Meanalltab$total_precip_all<-rowMeans(Meanalltab[c('total_precip_mm','total_precip_CB', 'total_precip_S')],na.rm=TRUE)

Finalclimate<-Meanalltab[,c(2,3,4,5,15,16,17)]

summary(Meanalltab$mean_temp_all)
summary(Meanalltab$total_rain_all)
summary(Meanalltab$total_precip_all)


#####SAVE TAB!##
saveRDS(Finalclimate, '~/Stage/Stage Canada/Caribou data/Finalclimate.Rds')
write.csv(Finalclimate,'~/Stage/Stage Canada/Caribou data/Finalclimate.csv')

climate<-read.csv('~/Stage/Stage Canada/Caribou data/Finalclimate.csv')

cariboumodel<-readRDS('~/Stage/Stage Canada/Caribou data/cariboumodel.Rds')

