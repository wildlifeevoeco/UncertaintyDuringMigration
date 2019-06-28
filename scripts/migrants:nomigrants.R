######### Code to differenciate migrants/non migrants #######

#### Data cleaning and exploration ### 
rm(list=ls())

# load packages 
library(dplyr)
library(data.table)
library(rgdal)
library(ggplot2)

########NSD extraction ### See Mike's code##########

#### Data exploration - plotting displacement data
dataDisp <- readRDS("~/Documents/Emilie_project/Git/NSDMigration/dataDisp.RDS")
Migration <- readRDS("~/Documents/Emilie_project/Git/NSDMigration/Migration.RDS")
NSDdata <- readRDS("~/Documents/Emilie_project/Git/NSDMigration/NSDdata.RDS")

par(mar=c(4,4,1,1))

#subset by pre/during/post
PreMig<-subset(Migration, JDate<MigDayNSD10)
MigData<-subset(Migration, JDate>MigDayNSD10 & JDate<MigDayNSD90)
PostMig<-subset(Migration, JDate>MigDayNSD90)

unique(dataDisp$burst.x)
#par(mfrow=c(2,1))

###Select indiv 
i<-"mr2009a062011"
Test1<-subset(dataDisp, burst.x==i)
Test2<-subset(Test1, Displace>max(Displace)/2)
min(Test2$JdateTime)
summary(Test1$Displace)

### Plot NSD by JDate
plot(Test1$Displace~Test1$JdateTime,typ='l',
     xlab="Julian Day",ylab="Total net displacement")
abline(h=max(Test1$Displace),col='grey')
abline(h=max(Test1$Displace)/2,col='grey')
abline(v=subset(NSDdata,burst.x==i)$MigDayNSD10,col='red')
abline(v=subset(NSDdata,burst.x==i)$MigDayNSD90,col='red')

##Plot mvt and select first and end point migration 
plot(Test1$NORTHING~Test1$EASTING,typ='l',
     xlab="x",ylab="y")
pts=data.table(x = 653174.4, y = 5319052)
pts2=data.table(x = 618565.2, y = 5351970)
points(y ~ x, data = pts, pch = 19, col = "blue", cex = 1.5, type = 'p')  ###first pts migration
points(y ~ x, data = pts2, pch = 19, col = "red", cex = 1.5, type = 'p')  ###last pts migration

##subset by one indiv/one year 
i<-"mr2009a252011"
indivPre<-subset(PreMig, burst==i)
indivMig<-subset(MigData, burst==i)
indivPost<-subset(PostMig, burst==i)
### plot animal mvt with 3 periods (pre/mig/post)
(plot2 <- ggplot(indivPre, aes(EASTING,NORTHING)) + 
    geom_point(data = indivPre, color = 'green', size = 1.5, alpha = 0.9) +
    geom_point(data = indivMig, color = 'blue', size = 1.5, alpha = 0.8) +
    geom_point(data = indivPost, color = 'grey', size = 1.5, alpha = 0.9)
)

###Plot spring migration with mvt and encamp behavior
ggplot(outMR[Animal_ID == "mr2009a25"],aes(Easting, Northing)) +
  geom_point(aes(color = state)) +
  geom_path(aes(group = IDYear), alpha = 0.2) +
  facet_wrap(~Year, scale = "free")

ggplot(outMR[IDYear == "mr2009a252011"],aes(Easting, Northing)) +
  geom_point(aes(color = state))+theme_light()

test <- outMR
test$state <- factor(test$state, levels=c("1", "2"), labels=c("Encamped", "Movement"))

ggplot(test[IDYear == "mr2009a252011"],aes(Easting, Northing, fill = state)) +
  geom_point(aes(color = state))+theme(legend.position=c(.6,0.8))+ theme_light()

#######MCP Analysis
### read file ####
Allindiv<- read.csv("~/Documents/Emilie_project/CaribouDataClean.CSV")
as.data.table(Allindiv)
## Summary and change format 
summary(Allindiv)
uniqueN(Allindiv$ANIMAL_ID) 
Allindiv$HERD <- as.factor(Allindiv$HERD)
Allindiv$ANIMAL_ID <- as.factor(Allindiv$ANIMAL_ID)

# Select one herd
midindiv<-subset(Allindiv, HERD=="MIDRIDGE")
midindiv<-as.data.table(midindiv)

# Select seasons
summary(midindiv)
midindiv[JDate < 63, season := 'winter']
midindiv[JDate > 140 & JDate < 230, season := 'summer']

##midindiv <- midindiv[!(is.na(season))]
