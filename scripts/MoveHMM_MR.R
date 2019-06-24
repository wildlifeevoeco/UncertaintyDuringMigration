#### MoveHMM

library(moveHMM)

Migr<-readRDS("~/Emilie_project/Git/emilie_nlcaribou/output/Caribouclean.RDS")

MigrMidR2010<-subset(caribouclean, Year==2010 & Herd=="MIDRIDGE")
MigrMidR2010<-subset(caribouclean, Year==2010)
unique(MigrMidR2010$Year)
levels(MigrMidR2010$Herd)
MigrMidR2010$Herd <- levels(droplevels(MigrMidR2010$Herd))
unique(MigrMidR2010$Herd)

nrow(MigrMidR2010)

data<-data.frame(MigrMidR2010$Easting,MigrMidR2010$Northing,MigrMidR2010$IDYear)
colnames(data)<-c("x","y","ID")

MRPrep<-prepData(data, type="UTM")

hist(MRPrep$step)
hist(MRPrep$angle)

head(MRPrep)

fitHMM<-fitHMM(MRPrep, nbStates=2, stepPar0=c(20,500,100,100), 
                anglePar0=c(pi,0,1,3), verbose=2)

fitHMM

###state 2 = mvt and state 1 = encamp

