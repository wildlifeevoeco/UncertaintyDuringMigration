###Cleaning migration data ###
rm(list=ls())

##load packages
# .libPaths("C:/R") # location of your R packages
library(dplyr)
library(data.table)


# Time zone 
tz <- 'America/St_Johns'

### Projection ----
projCols <- c('EASTING', 'NORTHING')

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

##open the csv
migration <- read.csv("~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/input/MigrationDataNSD.csv")
migration <- as.data.table(migration)
migration <- migration[,-1]  

##

uniqueN(migration$Animal_ID) ##118 indiv in total


##Number of ids by herds, 34 for MR
migration %>% group_by(Herd) %>% 
  summarize(n_id = uniqueN(Animal_ID))

# Herd      n_id
# BUCHANS     14
# GREY        16
# LAPOILE     18
# MIDRIDGE    34
# POTHILL     14
# TOPSAILS    22

#subset by indiv that had min 30km "migration" (min distance between first point and other pts)
dplyr::count(migration,  migration$Displace < 30)
mig <- migration[Displace >= 30]
hist(mig$Displace)

###remove indiv that don't really migrate or not enough
mig <- mig[IDYear != "mr2009a022012" & IDYear != "mr2009a032013" & IDYear != "mr2009a062012" &
           IDYear != "mr2009a062013" & IDYear != "mr2009a082012" & IDYear != "mr2009a102012" &
           IDYear != "mr2009a142010" & IDYear != "mr2009a152010" & IDYear != "mr2009a172010" &
           IDYear != "mr2009a182010" & IDYear != "mr2009a182012" & IDYear != "mr2009a212012" &
           IDYear != "mr2009a242010" & IDYear != "mr2009a272010" & IDYear != "mr2009a292010" &
           IDYear != "mr2009a312010" & IDYear != "mr2009a312012" & IDYear != "mr2011a012013"]

 
uniqueN(mig$Animal_ID) ###107 indiv after removed < 30km and false MR migration


##Subsetting only Middle Ridge herd
migration_MR<-subset(mig, Herd=="MIDRIDGE")
migration_MR$Herd <- droplevels(migration_MR$Herd)
unique(migration_MR$Herd)
uniqueN(migration_MR$Animal_ID) ####30 ids

migration_MR$FixDate <- as.character(migration_MR$FixDate, format = "%Y-%m-%d")

###migration summary dates
mig_MR_dates <- migration_MR %>%
  group_by(Herd, Year)%>% 
  mutate_if(is.numeric, round, 0)  %>%
  summarise(firststart = min(MigStartDay),
            laststart = max(MigStartDay),
            firstend = min(MigEndDay),
            lastend = max(MigEndDay),
            meanstart = mean(MigStartDay),
            meanend = mean(MigEndDay),
            meanduration = mean(MigDuration),
            meandist = mean(Displace),
            total.fixes = n())

###migration summary distance and duration
mig_MR_infos <- migration_MR %>%
  group_by(Year)%>% 
  mutate_if(is.numeric, round, 0)  %>%
  summarise(min_dur = min(MigDuration),
            max_dur = max(MigDuration),
            mean_dur = mean(MigDuration),
            sd_dur = sd(MigDuration),
            min_dist = min(Displace),
            max_dist = max(Displace),
            mean_dist = mean(Displace),
            sd_dist = sd(Displace),
            total.fixes = n())


##nb of locations
numloc <- migration_MR %>%
  group_by(Year)%>%
  summarise(total.fixes = n())
# Year total.fixes
# 2010        2774
# 2011       23059
# 2012        6255
# 2013        3702
sum(numloc$total.fixes) ### 35790 locations 

nbdays_id <- migration_MR %>%              ###some indiv move more than 30km but in 2 days for ex.
  group_by(Animal_ID, Year)%>%
  mutate_if(is.numeric, round, 0) %>%
  summarise(fixes = n(),
            numday = uniqueN(FixDate))
min(nbdays_id$numday) ## 6 
max(nbdays_id$numday) ## 94

#Number of dataindiv / herd and number year/indiv
uniqueN(migration_MR$IDYear) ### 47 

##Save output
write.csv(mig_MR_dates, '~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/tables/mig_MR_dates.csv')
write.csv(mig_MR_infos, '~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/tables/mig_MR_infos.csv')


saveRDS(migration_MR, '~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/migration_MR.RDS')


message('=== PREP COMPLETE ===')


