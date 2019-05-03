#### Data cleaning ### 
rm(list=ls())
setwd("~/Stage/Stage Canada/Caribou data")

# load packages 
.libPaths("C:/R") # location of your R packages
library(dplyr)
library(tidyr)
library(moveHMM)
library(data.table)
library(rgdal)
lapply(libs,require, character.only = TRUE)

### read database and csv file ####
Dt<-read.csv("~/Stage/Stage Canada/Caribou data/MoveHMM/1-CaribouDataSpringClean.csv")
DT<-fread('~/Stage/Stage Canada/Caribou data/MoveHMM/1-CaribouDataSpringClean.csv')

##summary of tab
summary(DT$HERD)
summary(DT)

##Transform to coord -> UTM
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
DT[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]
head(DT)

##Add columns with ID,year,Herd
DT[,ID := paste (ANIMAL_ID, year, HERD, sep = '_')]

##Add columns with Year/Herd
DT[, YearHerd := paste(year, HERD, sep = '_')]

## Calculate number of locations by year and by herd, should I remove years when I don't have enought of data?
DT[, Nunique := .N, by = ID]
DT <- DT[Nunique > 50]
DT[, .N, by = .(HERD,year)]

##count the number of elements by columns
sapply(DT, function(x) sum(!duplicated(x)))
length(unique(caribouclean$ANIMAL_ID))  ###1479
length(DT$year)

#Number of data per year/herd
DT[, uniqueN(ANIMAL_ID), by = .(HERD,year)] # ind/year/herd
DT[, .N, by = .(HERD, year, ID)][order(N)] ##nib fixes/nd/year/herd
                                         
##Number of fixes by ind by date
fixes<-DT[, uniqueN(FIX_TIME), by =.(FIX_DATE, ANIMAL_ID)]
fixes

#Number of ind / herd
DT[, uniqueN (ANIMAL_ID), by =.(HERD)]

###remove data that I don't need 
caribouclean<-subset(cariboudata, select = -c(3,5,11:12,14:23))
summary(caribouclean$HERD)

###save Data

saveRDS(DT[, .(V1, ANIMAL_ID, HERD, datetime, year, JDate,
               EASTING, NORTHING, ID, YearHerd)],
        '~/Stage/Stage Canada/Caribou data/cleaned-locs.Rds')

saveRDS(fixes[,.(FIX_DATE, ANIMAL_ID, V1)],
        '~/Stage/Stage Canada/Caribou data/fixes.Rds')




