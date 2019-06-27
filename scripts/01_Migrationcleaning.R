###Exploration Migration dates ###
rm(list=ls())

##load packages
.libPaths("C:/R") # location of your R packages
library(dplyr)
library(data.table)

##open the csv
migration <- read.csv("~/Documents/Emilie_project/Git/emilie_nlcaribou/input/MigrationDataNSD.csv")
migration <- as.data.table(migration)

##
summary(migration)
uniqueN(migration$Animal_ID) ##118 indiv in total

#subset by indiv that had min 30km "migration" (min distance between first point and other pts)
mig <- migration[Displace >= 30]

###remove indiv that don't really migrate or not enough
mig <- mig[IDYear != "mr2009a022012" & IDYear != "mr2009a032013" & IDYear != "mr2009a062012" &
           IDYear != "mr2009a062013" & IDYear != "mr2009a082012" & IDYear != "mr2009a102012" &
           IDYear != "mr2009a142010" & IDYear != "mr2009a152010" & IDYear != "mr2009a172010" &
           IDYear != "mr2009a182010" & IDYear != "mr2009a182012" & IDYear != "mr2009a212012" &
           IDYear != "mr2009a242010" & IDYear != "mr2009a272010" & IDYear != "mr2009a292010" &
           IDYear != "mr2009a312010" & IDYear != "mr2009a312012" & IDYear != "mr2011a012013"]

hist(mig$Displace)
summary(mig)
uniqueN(mig$Animal_ID) ###107 indiv after removed < 30km and false MR migration

##tab of dates for all herds
#summary.all <- mig %>%
#  group_by(Animal_ID, Year)%>%
#  summarise(first = min(MigStartDay) , 
#            last = max(MigEndDay),
#            meanduration = mean(MigDuration),
#            total.fixes = n())

avgJDate <- mig %>%
  group_by(Herd, Year)%>%
  summarise(firststart = min(MigStartDay),
            laststart = max(MigStartDay),
            firstend = min(MigEndDay),
            lastend = max(MigEndDay),
            meanstart = mean(MigStartDay),
            meanend = mean(MigEndDay),
            meanduration = mean(MigDuration),
            total.fixes = n())

avgJDate$firststart <- ceiling(avgJDate$firststart)
avgJDate$laststart <- ceiling(avgJDate$laststart)
avgJDate$firstend <- trunc(avgJDate$firstend, digits = 0)
avgJDate$lastend <- trunc(avgJDate$lastend, digits = 0)
avgJDate$meanstart <- round(avgJDate$meanstart)
avgJDate$meanend <- round(avgJDate$meanend, digits = 1)
avgJDate$meanduration <- round(avgJDate$meanduration, digits = 1)

write.csv(avgJDate, '~/Documents/Emilie_project/Git/emilie_nlcaribou/output/avgJDate.csv')

##Convert JDate in Calendar
avg <- mig %>%
  group_by(Herd,Year)%>%
  summarise(firststart = min(MigStartDay),
            laststart = max(MigStartDay),
            firstend = min(MigEndDay),
            lastend = max(MigEndDay),
            meanstart = mean(MigStartDay),
            meanend = mean(MigEndDay),
            meanduration = mean(MigDuration),
            total.fixes = n())

#firststart
df <- data.table (Year = avg$Year,
                  JDate = avg$firststart)
df$JDate <- ceiling(df$JDate)
date_info <- with(df, paste(Year, JDate))
date <- strptime(date_info, "%Y %j")
avg$firststart<- as.Date(date, format= "%Y-%m-%d")

#laststart
df <- data.table (Year = avg$Year,
                  JDate = avg$laststart)
df$JDate <- ceiling(df$JDate)
date_info <- with(df, paste(Year, JDate))
date <- strptime(date_info, "%Y %j")
avg$laststart<- as.Date(date, format= "%Y-%m-%d")

#firstend
df <- data.table (Year = avg$Year,
                  JDate = avg$firstend)
df$JDate <- trunc(df$JDate, digits = 0)
date_info <- with(df, paste(Year, JDate))
date <- strptime(date_info, "%Y %j")
avg$firstend<- as.Date(date, format= "%Y-%m-%d")

#lastend
df <- data.table (Year = avg$Year,
                  JDate = avg$lastend)
df$JDate <- trunc(df$JDate, digits = 0)
date_info <- with(df, paste(Year, JDate))
date <- strptime(date_info, "%Y %j")
avg$lastend<- as.Date(date, format= "%Y-%m-%d")

#meanstart
df <- data.table (Year = avg$Year,
                  JDate = avg$meanstart)
df$JDate <- round(df$JDate, digits = 0)
date_info <- with(df, paste(Year, JDate))
date <- strptime(date_info, "%Y %j")
avg$meanstart<- as.Date(date, format= "%Y-%m-%d")

#meanend
df <- data.table (Year = avg$Year,
                  JDate = avg$meanend)
df$JDate <- round(df$JDate, digits = 0)
date_info <- with(df, paste(Year, JDate))
date <- strptime(date_info, "%Y %j")
avg$meanend<- as.Date(date, format= "%Y-%m-%d")

#meanduration
avg$meanduration <- round(avg$meanduration, digits = 1)

write.csv(avg,'~/Documents/Emilie_project/Git/emilie_nlcaribou/output/datesgmigration.csv')

saveRDS(mig, '~/Documents/Emilie_project/Git/emilie_nlcaribou/output/Allmigrationdates.Rds')

## Subset by Midridge herd
Allmigration<-subset(mig, Herd=="MIDRIDGE")
levels(Allmigration$Herd)
Allmigration$Herd <- levels(droplevels(Allmigration$Herd))
unique(Allmigration$Herd)

#num indiv for Midridge 
uniqueN(Allmigration$Animal_ID) ##30 indiv for 4 years
Allmigration$FixDate <- as.character(Allmigration$FixDate, format = "%Y-%m-%d")

## Save tab with 34 indiv from Midridge between 2010 and 2013
saveRDS(Allmigration, '~/Documents/Emilie_project/Git/emilie_nlcaribou/output/AllmigrationMR.Rds')
