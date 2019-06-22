#### Data cleaning and exploration ### 
rm(list=ls())

# load packages 
.libPaths("C:/R") # location of your R packages
library(dplyr)
library(data.table)
lapply(libs,require, character.only = TRUE)

### read file ####
caribouclean <- readRDS("caribouclean.RDS")

## remove columns 
caribouclean<-subset(caribouclean, select = -c(1,13:17))

##Add columns with ID,year,Herd
#caribouclean[,ID := paste (ANIMAL_ID, year, HERD, sep = '_')]

## Calculate number of locations by year 
numloc <- caribouclean %>%
  group_by(Year)%>%
  summarise(total.fixes = n())
#Year
#total.fixes
#1	2010	2995
#2	2011	23059
#3	2012	7853
#4	2013	4121

indiv <- caribouclean %>%              ###some indiv move more than 30km but in 2 days for ex.
  group_by(Animal_ID, Year)%>%
  summarise(fixes = n(),
            numday = uniqueN(FixDate),
            Distancemig = mean (Displace))

##number of indiv in total
length(unique(caribouclean$Animal_ID))  ##30

## num indiv with data per year                
year <- caribouclean %>%              
  group_by(Year)%>%
  summarise(ID = uniqueN(Animal_ID))

#Year    ID
#1	2010	15
#2	2011	21
#3	2012	13
#4	2013	12

##count the number of elements by columns
#sapply(caribouclean, function(x) sum(!duplicated(x))) 
#length(unique(caribouclean$ANIMAL_ID))  ###133

#Number of dataindiv / herd and number year/indiv
#caribouclean[, uniqueN (ANIMAL_ID), by =.(HERD)]
#caribouclean[, uniqueN(year), by =.(ANIMAL_ID)]
