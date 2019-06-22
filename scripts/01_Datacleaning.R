#### Data cleaning ### 
rm(list=ls())

# load packages 
.libPaths("C:/R") # location of your R packages
library(dplyr)
library(tidyr)
library(moveHMM)
library(data.table)
library(rgdal)
library(toast)
lapply(libs,require, character.only = TRUE)
install.packages("toast")

### read database and csv file ####
caribou <- read_csv("~/Emilie_project/Git/emilie_nlcaribou/input/CaribouDataClean.CSV")
caribou <- as.data.table(caribou)

###1st part : exploration 
caribou$HERD <- as.factor(caribou$HERD)

##subset HERD MIDRIDGE
summary(caribou$HERD)
caribouclean <- caribou[HERD == 'MIDRIDGE']

##Subset by Jdate (see Migration dates)


##Add columns with ID,year,Herd
caribouclean[,ID := paste (ANIMAL_ID, year, HERD, sep = '_')]


## Calculate number of locations by year and by herd, should I remove years when I don't have enought of data?
caribouclean[, Nunique := .N, by = ID]
caribouclean <- caribouclean[Nunique > 50]
numloc <- caribouclean[, .N, by = .(HERD,year)]
numloc
write.csv(numloc, '~/Emilie_project/Git/emilie_nlcaribou/output/numloc.csv')

##count the number of elements by columns
sapply(caribouclean, function(x) sum(!duplicated(x))) 
length(unique(caribouclean$ANIMAL_ID))  ###133

#Number of data per year/herd
numindiv <- caribouclean[, uniqueN(ANIMAL_ID), by = .(HERD,year)] # numind/year/herd Careful, we have more than one year for some indiv
write.csv(numindiv, '~/Emilie_project/Git/emilie_nlcaribou/output/numindiv.csv')

#caribouclean[, .N, by = .(HERD, year, ID)][order(N)] ##nib fixes/nd/year/herd
                                         
##Number of fixes by ind by date
fixes<-caribouclean[, uniqueN(FIX_TIME), by =.(FIX_DATE, ANIMAL_ID)]
fixes

##number of day/indiv
caribouclean[, uniqueN(FIX_DATE), by =.(ANIMAL_ID)]

#Number of dataindiv / herd and number year/indiv
caribouclean[, uniqueN (ANIMAL_ID), by =.(HERD)]
caribouclean[, uniqueN(year), by =.(ANIMAL_ID)]

###remove data that I don't need 
caribouclean<-subset(caribouclean, select = -c(1,6:8, 15:18, 21))
summary(caribouclean$HERD)

###2nd part : subset by the dates of migration and Midridge











###save Data
saveRDS(caribouclean, '~/Emilie_project/Git/emilie_nlcaribou/output/caribouclean.Rds')
