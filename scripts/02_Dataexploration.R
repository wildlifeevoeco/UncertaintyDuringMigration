#### Data cleaning and exploration ### 

# load packages 
library(dplyr)
library(data.table)
library(ggplot2)

### read file ####
caribouclean<- readRDS("output/migration_MR.RDS")

## remove columns 
caribouclean<-subset(caribouclean, select = -c(14:17))

##Add columns with ID,year,Herd
#caribouclean[,ID := paste (ANIMAL_ID, year, HERD, sep = '_')]

## Calculate number of locations by year 
numloc <- caribouclean %>%
  group_by(Year)%>%
  summarise(total.fixes = n())

#Year  total.fixes
# 2010	2774
# 2011	23059
# 2012	6255
#	2013	3702

## Tab summary by indiv (nb pts by year by number of day and distance of migration)
indiv <- caribouclean %>%              ###some indiv move more than 30km but in 2 days for ex.
  group_by(Animal_ID, Year)%>%
  summarise(fixes = n(),
            numday = uniqueN(FixDate),
            Distancemig = unique(Displace))
write.csv(indiv, 'output/indiv.csv')

##plot indiv on map
ggplot(caribouclean[Animal_ID == "mr2009a06"],aes(Easting, Northing)) +
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
# 2010	11
# 2011	21
# 2012	6
# 2013	9

##count the number of elements by columns
#sapply(caribouclean, function(x) sum(!duplicated(x))) 
#length(unique(caribouclean$ANIMAL_ID))  ###133

#Number of dataindiv / herd and number year/indiv
#caribouclean[, uniqueN (ANIMAL_ID), by =.(HERD)]
#caribouclean[, uniqueN(year), by =.(ANIMAL_ID)]

saveRDS(caribouclean, 'output/cariboucleanMR.Rds')
