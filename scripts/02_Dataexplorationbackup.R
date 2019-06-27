###Exploration Migration dates###
rm(list=ls())

##load packages
.libPaths("C:/R") # location of your R packages
library(dplyr)
library(tidyr)
library(data.table)
library(rgdal)
library(chron)

##open the csv
mig <- read.csv("MigrationDataNSD.CSV")

mig[, uniqueN(Animal_ID), by = .(Herd,Year)] # ind/year/herd
mig[, .N, by = .(HERD, year, ID)][order(N)]

summary.all <- mig %>%
  group_by(Animal_ID, Year)%>%
  summarise(first = min(MigStartDay) , 
            last = max(MigEndDay),
            summig = (MigDuration),
            total.fixes = n())

avg <- mig %>%
  group_by(Herd, Year)%>%
  summarise(first = min(MigStartDay) , 
            last = max(MigEndDay),
            meanstart = mean(MigStartDay),
            meanend = mean(MigEndDay))

##Convert JDate in Calendar
#firstdate
df <- data.table (Year = avg$Year,
                  JDate = avg$first)
df$JDate <- round(df$JDate, digits = 0)
date_info <- with(df, paste(Year, JDate))
date <- strptime(date_info, "%Y %j")
avg$firstdate<- as.Date(date, format= "%Y-%m-%d")

#lastdate
df <- data.table (Year = avg$Year,
                  JDate = avg$last)
df$JDate <- round(df$JDate, digits = 0)
date_info <- with(df, paste(Year, JDate))
date <- strptime(date_info, "%Y %j")

avg <- subset(avg, select = -c(datefirst, datelast))
#meanfirstdate
df <- data.table (Year = avg$Year,
                  JDate = avg$meanstart)
df$JDate <- round(df$JDate, digits = 0)
date_info <- with(df, paste(Year, JDate))
date <- strptime(date_info, "%Y %j")
avg$meanfirstdate<- as.Date(date, format= "%Y-%m-%d")

#meanlastdate
df <- data.table (Year = avg$Year,
                  JDate = avg$meanend)
df$JDate <- round(df$JDate, digits = 0)
date_info <- with(df, paste(Year, JDate))
date <- strptime(date_info, "%Y %j")
avg$meanlastdate<- as.Date(date, format= "%Y-%m-%d")

avg <- subset(avg, select = -c(first,last,datefirst, datelast))
