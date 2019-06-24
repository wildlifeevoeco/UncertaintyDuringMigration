#### Data cleaning and exploration ### 
rm(list=ls())

# load packages 
.libPaths("C:/R") # location of your R packages
library(dplyr)
library(data.table)
lapply(libs,require, character.only = TRUE)

### read file ####
caribouclean <- readRDS("Allmigration.RDS")

## remove columns 
caribouclean<-subset(caribouclean, select = -c(1,14:17))

##Add columns with ID,year,Herd
#caribouclean[,ID := paste (ANIMAL_ID, year, HERD, sep = '_')]

## Calculate number of locations by year 
numloc <- caribouclean %>%
  group_by(Year)%>%
  summarise(total.fixes = n())

#Year  total.fixes
#2010	  3179
#2011	  23059
#2012 	7853
#2013 	4121

## Tab summary by indiv (nb pts by year by number of day and distance of migration)
indiv <- caribouclean %>%              ###some indiv move more than 30km but in 2 days for ex.
  group_by(Animal_ID, Year)%>%
  summarise(fixes = n(),
            numday = uniqueN(FixDate),
            Distancemig = unique(Displace))

library(ggplot2)
ggplot(caribouclean[Animal_ID == "mr2009a02"],aes(Easting, Northing)) +
  geom_point(aes(color = Animal_ID)) +
  geom_path(aes(group = Animal_ID), alpha = 0.2) +
  facet_wrap(~Year, scale = "free")

##number of indiv in total
length(unique(caribouclean$Animal_ID))  ##34

## num indiv with data per year                
year <- caribouclean %>%              
  group_by(Year)%>%
  summarise(ID = uniqueN(Animal_ID))

#Year   ID
# 2010	19
# 2011	21
# 2012	13
# 2013	12

##count the number of elements by columns
#sapply(caribouclean, function(x) sum(!duplicated(x))) 
#length(unique(caribouclean$ANIMAL_ID))  ###133

#Number of dataindiv / herd and number year/indiv
#caribouclean[, uniqueN (ANIMAL_ID), by =.(HERD)]
#caribouclean[, uniqueN(year), by =.(ANIMAL_ID)]

saveRDS(caribouclean, '~/Emilie_project/Git/emilie_nlcaribou/output/caribouclean.Rds')
