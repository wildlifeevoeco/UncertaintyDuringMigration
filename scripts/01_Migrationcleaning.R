###Exploration Migration dates ###
rm(list=ls())

##load packages
.libPaths("C:/R") # location of your R packages
library(dplyr)
library(data.table)
lapply(libs, require, character.only = TRUE)

##open the csv
migration <- read.csv("MigrationDataNSD.CSV")
migration <- as.data.table(migration)

##select migration distance above 30km 
summary(migration)
mig <- migration[Displace >= 30]
hist(mig$Displace)
summary(mig)

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

write.csv(avgJDate, '~/Emilie_project/Git/emilie_nlcaribou/output/avgJDate.csv')

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

write.csv(avg,'~/Emilie_project/Git/emilie_nlcaribou/output/datesgmigration.csv')

saveRDS(mig, '~/Emilie_project/Git/emilie_nlcaribou/output/Allmigrationdates.Rds')

## Subset by Midridge herd
caribouclean <- mig[Herd == 'MIDRIDGE']
summary(caribouclean)
caribouclean$FixDate <- as.character(caribouclean$FixDate, format = "%Y-%m-%d")

saveRDS(caribouclean, '~/Emilie_project/Git/emilie_nlcaribou/output/caribouclean.Rds')
