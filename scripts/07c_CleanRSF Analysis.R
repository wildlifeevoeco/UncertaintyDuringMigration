####################################################
####################RSF MODELS######################
####################################################
library(mclogit)
library(lme4)
library(car)
library(Rmisc)
library(ggplot2)
library(jtools)
library(MuMIn)
library(data.table)
library(stargazer)

#####Arranging data and graphs exploration#######
####Changing name of available and used pts to easily interpret them after
allNDVI$Randoms<-ifelse(allNDVI$Randoms=="1",1,0) #### 0 == available and 1 == used 
head(allNDVI)
summary(allNDVI)

###add column to reorganise Randoms to see if there is some of
### randoms points who not have used pts (removed from water step)
allNDVI[, StrMean := mean(as.numeric(Randoms)),by = .(PtID)]
summary(allNDVI)
nrow(allNDVI)

###changing name of state variables for interpretation and subset
allNDVI$HMM <- ifelse(allNDVI$state == 2,0,1)
allNDVIRSF <- allNDVI

allNDVIRSF$prcpSc<-scale(allNDVIRSF$prcp)
allNDVIRSF$tmaxSc<-scale(allNDVIRSF$tmax)
allNDVIRSF$prcpSc<-scale(allNDVIRSF$prcp)
allNDVIRSF$sweSc<-scale(allNDVIRSF$swe)
allNDVIRSF$ForestSc<-scale(allNDVIRSF$Forest)
allNDVIRSF$LichenSc<-scale(allNDVIRSF$Lichen)
allNDVIRSF$WetlandSc<-scale(allNDVIRSF$Wetland)
allNDVIRSF$RockySc<-scale(allNDVIRSF$Rocky)
allNDVIRSF$WaterSc<-scale(allNDVIRSF$Water)
allNDVIRSF$NDVISc<-scale(allNDVIRSF$NDVI)

allNDVIRSF <- subset(allNDVIRSF, select = -c(2:5,7, 12:18, 22:23, 27, 29, 33:34))


allNDVI_stop<- subset(allNDVIRSF, HMM == 1)
allNDVI_mvt <- subset(allNDVIRSF, HMM == 0)

allNDVIRSF$Animal_ID<-as.factor(allNDVIRSF$Animal_ID)
allNDVIRSF$Year<-as.factor(allNDVIRSF$Year)
allNDVIRSF$Randoms<-as.factor(allNDVIRSF$Randoms)

###############################################
#######RSF MODEL WITH ENCAMPED#################
###Habitat model RSF without year and indiv
RSFhab1 <- glmer(Randoms ~ LichenSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab2 <- glmer(Randoms ~ WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab3 <- glmer(Randoms ~ RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab4 <- glmer(Randoms ~ ForestSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab5 <- glmer(Randoms ~ WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")

summary(RSFhab1)
summary(RSFhab2)
summary(RSFhab3)
summary(RSFhab4)
summary(RSFhab5)

stargazer(RSFhab1, RSFhab2, RSFhab3, RSFhab4, RSFhab5, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "CandidateRSFhab.htm")

RSFhab6<- glmer(Randoms ~ LichenSc + WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab7<- glmer(Randoms ~ LichenSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab8<- glmer(Randoms ~ LichenSc + ForestSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab9<- glmer(Randoms ~ LichenSc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab10<- glmer(Randoms ~ ForestSc + WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab11<- glmer(Randoms ~ ForestSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab12<- glmer(Randoms ~ ForestSc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")

summary(RSFhab6)
summary(RSFhab7)
summary(RSFhab8)
summary(RSFhab9)
summary(RSFhab10)
summary(RSFhab11)
summary(RSFhab12)

RSFhab13<- glmer(Randoms ~ LichenSc + WetlandSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab14<- glmer(Randoms ~ LichenSc + WetlandSc + ForestSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab15<- glmer(Randoms ~ LichenSc + WetlandSc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab16<- glmer(Randoms ~ LichenSc + ForestSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab17<- glmer(Randoms ~ LichenSc + ForestSc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
RSFhab18<- glmer(Randoms ~ WetlandSc + ForestSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")

summary(RSFhab13)
summary(RSFhab14)
summary(RSFhab15)
summary(RSFhab16)
summary(RSFhab17)
summary(RSFhab18)

globalRSFhab <- glmer(Randoms ~ LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
summary(globalRSFhab)

##Calculate AIC and delta
aics<-AIC(RSFhab6,RSFhab7,RSFhab8,RSFhab9,RSFhab10,RSFhab11,RSFhab12,RSFhab13,
          RSFhab14,RSFhab15,RSFhab16,RSFhab17,RSFhab18,globalRSFhab)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)
#### % of information explained
r.squaredGLMM(globalRSFhab)

#####5 tops models landcover
stargazer(globalRSFhab, RSFhab14, RSFhab13, RSFhab17, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "5RSFbestmodelshab.htm")

####boxplot covariates by states
boxplot((LichenSc) ~ factor(Randoms)  , data =allNDVI_stop, notch = TRUE)
boxplot((WetlandSc) ~ factor(Randoms)  , data = allNDVI_stop, notch = TRUE)
boxplot((ForestSc) ~ factor(Randoms)  , data = allNDVI_stop, notch = TRUE)
boxplot((RockySc) ~ factor(Randoms)  , data = allNDVI_stop, notch = FALSE)
boxplot((WaterSc) ~ factor(Randoms)  , data = allNDVI_stop, notch = FALSE)
boxplot((sweSc) ~ factor(Randoms)  , data = allNDVI_stop, notch = FALSE)
boxplot((prcpSc) ~ factor(Randoms)  , data = allNDVI_stop, notch = FALSE)

####Weather candidates models
RSFweather1<-mclogit(cbind(Randoms,PtID)~prcpSc, data = allNDVI_stop)
RSFweather2<-mclogit(cbind(Randoms,PtID)~tmaxSc, data = allNDVI_stop)
RSFglobalweather<-mclogit(cbind(Randoms,PtID)~prcpSc + tmaxSc , data = allNDVI_stop)
summary(RSFweather1)
summary(RSFweather2)
summary(RSFglobalweather)

##Calculate AIC and delta
aics<-AIC(RSFweather1,RSFweather2,RSFglobalweather)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFglobalweather, RSFweather2, RSFweather1, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "RSFBestmodelweather.htm")


###Environment candidates models
RSFenv1<-mclogit(cbind(Randoms,PtID)~NDVISc, data = allNDVI_stop)
RSFenv2<-mclogit(cbind(Randoms,PtID)~sweSc, data = allNDVI_stop)
RSFglobalenv <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc, data = allNDVI_stop)
summary(RSFenv1)
summary(RSFenv2)
summary(RSFglobalenv)

##Calculate AIC and delta
aics<-AIC(RSFenv1,RSFenv2,RSFglobalenv)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFglobalenv, RSFenv1, RSFenv2, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "Candidateenv.htm")

###Landcover + env 
RSFmodel1 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc, data = allNDVI_stop)
summary(RSFmodel1)

RSFmodel2 <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:NDVISc, data = allNDVI_stop)
RSFmodel3 <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + WetlandSc:NDVISc, data = allNDVI_stop)
RSFmodel4 <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + RockySc:NDVISc, data = allNDVI_stop)
summary(RSFmodel2)
summary(RSFmodel3)
summary(RSFmodel4)
RSFmodel5 <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:sweSc, data = allNDVI_stop)
RSFmodel6 <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + WetlandSc:sweSc, data = allNDVI_stop)
RSFmodel7 <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + ForestSc:sweSc, data = allNDVI_stop)
RSFmodel8 <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + RockySc:sweSc, data = allNDVI_stop)
summary(RSFmodel5)
summary(RSFmodel6)
summary(RSFmodel7)
summary(RSFmodel8)

RSFmodel9 <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:sweSc + WetlandSc:sweSc + ForestSc:sweSc + RockySc:sweSc, data = allNDVI_stop)
RSFmodel10<- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  LichenSc:sweSc + ForestSc:sweSc + RockySc:sweSc, data = allNDVI_stop)
RSFmodel11<- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  LichenSc:sweSc + ForestSc:sweSc + WetlandSc:sweSc, data = allNDVI_stop)
summary(RSFmodel9)
summary(RSFmodel10)
summary(RSFmodel11)

aics<-AIC(RSFmodel1, RSFmodel5, RSFmodel6, RSFmodel7, RSFmodel8,
          RSFmodel9, RSFmodel10, RSFmodel11)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFmodel9, RSFmodel11, RSFmodel10, RSFmodel5, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "RSFLandcover+Env.htm")

######Landcover + Weather
RSFmodel12 <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc, data = allNDVI_stop)
summary(RSFmodel12)

RSFmodel13 <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:ForestSc, data = allNDVI_stop)
RSFmodel14 <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:LichenSc, data = allNDVI_stop)
RSFmodel15 <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:WetlandSc, data = allNDVI_stop)
RSFmodel16 <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:RockySc, data = allNDVI_stop)

RSFmodel17 <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:ForestSc, data = allNDVI_stop)
RSFmodel18 <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:LichenSc, data = allNDVI_stop)
RSFmodel19 <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:WetlandSc, data = allNDVI_stop)
RSFmodel20 <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:RockySc, data = allNDVI_stop)
summary(RSFmodel13)
summary(RSFmodel14)
summary(RSFmodel15)
summary(RSFmodel16)
summary(RSFmodel17)
summary(RSFmodel18)
summary(RSFmodel19)
summary(RSFmodel20) ####no effect prcp*Rocky

aics<-AIC(RSFmodel12,RSFmodel13, RSFmodel14, RSFmodel15, RSFmodel16, RSFmodel17,
          RSFmodel18, RSFmodel19, RSFmodel20)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

RSFmodel21 <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  tmaxSc:ForestSc +  tmaxSc:LichenSc + tmaxSc:WetlandSc + tmaxSc:RockySc, data = allNDVI_stop)
RSFmodel22 <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  prcpSc:ForestSc +  prcpSc:LichenSc + prcpSc:WetlandSc, data = allNDVI_stop)

summary(RSFmodel21)
summary(RSFmodel22)
RSFmodel23 <- mclogit(cbind(Randoms,PtID) ~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  tmaxSc:LichenSc + tmaxSc:WetlandSc + tmaxSc:RockySc + prcpSc:ForestSc + prcpSc:WetlandSc, data = allNDVI_stop)
RSFmodel24 <- mclogit(cbind(Randoms,PtID) ~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:ForestSc +  tmaxSc:LichenSc + tmaxSc:WetlandSc + tmaxSc:RockySc + prcpSc:ForestSc +  prcpSc:LichenSc + prcpSc:WetlandSc, data = allNDVI_stop)

summary(RSFmodel23)
summary(RSFmodel24)

aics<-AIC(RSFmodel13, RSFmodel14, RSFmodel15, RSFmodel16, RSFmodel17,
          RSFmodel18, RSFmodel19, RSFmodel20, RSFmodel21, RSFmodel22, RSFmodel23, RSFmodel24)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFmodel24,RSFmodel23, RSFmodel21, RSFmodel14, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "RSFLandcover+Weather.htm")

###Weather + Env 
RSFmodel25 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc , data = allNDVI_stop)
RSFmodel26 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc +  NDVISc:tmaxSc , data = allNDVI_stop)
RSFmodel27 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc +  prcpSc:sweSc , data = allNDVI_stop)
##RSFmodel28 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc +  NDVISc:tmaxSc +  prcpSc:sweSc , data = allNDVI_stop)

summary(RSFmodel25)
summary(RSFmodel26)
summary(RSFmodel27)


###test models
aics<-AIC(RSFmodel25,RSFmodel26)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFmodel26,RSFmodel25, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "RSFEnv+Weather.htm")

######3 models together 
RSFmodel27 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  NDVISc:tmaxSc , data = allNDVI_stop)
RSFmodel28 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:ForestSc +  tmaxSc:LichenSc + tmaxSc:WetlandSc + tmaxSc:RockySc, data = allNDVI_stop)
RSFmodel29 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:ForestSc +  prcpSc:LichenSc + prcpSc:WetlandSc , data = allNDVI_stop)
RSFmodel30 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:sweSc + WetlandSc:sweSc + ForestSc:sweSc + RockySc:sweSc , data = allNDVI_stop)
RSFmodel31 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:ForestSc + prcpSc:WetlandSc + tmaxSc:LichenSc + tmaxSc:WetlandSc + tmaxSc:RockySc + NDVISc:tmaxSc , data = allNDVI_stop)
RSFmodel32 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:sweSc + WetlandSc:sweSc + RockySc:sweSc + tmaxSc:LichenSc + tmaxSc:WetlandSc + tmaxSc:RockySc + NDVISc:tmaxSc , data = allNDVI_stop)
RSFmodel33 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:sweSc + WetlandSc:sweSc + RockySc:sweSc + prcpSc:ForestSc  + prcpSc:WetlandSc + NDVISc:tmaxSc , data = allNDVI_stop)
RSFmodel34 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:sweSc +
                        WetlandSc:sweSc + RockySc:sweSc + prcpSc:ForestSc  + prcpSc:WetlandSc + tmaxSc:LichenSc +
                        tmaxSc:WetlandSc + tmaxSc:RockySc + NDVISc:tmaxSc , data = allNDVI_stop)


summary(RSFmodel27)
summary(RSFmodel28)
summary(RSFmodel29)
summary(RSFmodel30)
summary(RSFmodel31)
summary(RSFmodel32)
summary(RSFmodel33)
summary(RSFmodel34)

####
aics<-AIC(RSFmodel27,RSFmodel28,RSFmodel29,RSFmodel30,RSFmodel31,RSFmodel32,RSFmodel33,
          RSFmodel34)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)



aics<-AIC(RSFmodel1, RSFmodel5, RSFmodel6, RSFmodel7, RSFmodel8,
          RSFmodel9, RSFmodel10, RSFmodel11,RSFmodel13, RSFmodel14, RSFmodel15, RSFmodel16, RSFmodel17,
          RSFmodel18, RSFmodel19, RSFmodel20, RSFmodel21, RSFmodel22, RSFmodel23, RSFmodel24,RSFmodel25,RSFmodel26,
          RSFmodel27,RSFmodel28,RSFmodel29,RSFmodel30,RSFmodel31,RSFmodel32,RSFmodel33,
          RSFmodel34)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFmodel34, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = TRUE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "BestRSFstopover.htm")

###Calculate interval of confidence
carsprfixef<-coef(RSFmodel34)##extract betas
carsprSEs<-c(sqrt(diag(vcov(RSFmodel34)))) ## Extract SEs
## Make lower and upper 95% CIs
carsprLCI<-carsprfixef-(carsprSEs*1.96)
carsprUCI<-carsprfixef+(carsprSEs*1.96)
## Round the values
carsprfixefR<-round(carsprfixef,3)
## Make the CIs
carsprCI<-paste(carsprfixefR, " [",round(carsprLCI,3), ", ",round(carsprUCI,3),"]",sep="")
names<-names(carsprfixef)
carsprCInamed<-data.frame(names,carsprCI)

###############################################
###############################################
#######RSF MODEL WITH MOVEMENT#################
###Habitat model RSF without year and indiv
RSFhab1mvt <- glmer(Randoms ~ LichenSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab2mvt <- glmer(Randoms ~ WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab3mvt <- glmer(Randoms ~ RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab4mvt <- glmer(Randoms ~ ForestSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab5mvt <- glmer(Randoms ~ WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")

summary(RSFhab1mvt)
summary(RSFhab2mvt)
summary(RSFhab3mvt)
summary(RSFhab4mvt)
summary(RSFhab5mvt)

stargazer(RSFhab1mvt, RSFhab2mvt, RSFhab3mvt, RSFhab4mvt, RSFhab5mvt, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "CandidateRSFhab_mvt.htm")

RSFhab6mvt<- glmer(Randoms ~ LichenSc + WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab7mvt<- glmer(Randoms ~ LichenSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab8mvt<- glmer(Randoms ~ LichenSc + ForestSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab9mvt<- glmer(Randoms ~ LichenSc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab10mvt<- glmer(Randoms ~ ForestSc + WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab11mvt<- glmer(Randoms ~ ForestSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab12mvt<- glmer(Randoms ~ ForestSc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")

summary(RSFhab6mvt)
summary(RSFhab7mvt)
summary(RSFhab8mvt)
summary(RSFhab9mvt)
summary(RSFhab10mvt)
summary(RSFhab11mvt)
summary(RSFhab12mvt)

RSFhab13mvt<- glmer(Randoms ~ LichenSc + WetlandSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab14mvt<- glmer(Randoms ~ LichenSc + WetlandSc + ForestSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab15mvt<- glmer(Randoms ~ LichenSc + WetlandSc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab16mvt<- glmer(Randoms ~ LichenSc + ForestSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab17mvt<- glmer(Randoms ~ LichenSc + ForestSc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
RSFhab18mvt<- glmer(Randoms ~ WetlandSc + ForestSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")

summary(RSFhab13mvt)
summary(RSFhab14mvt)
summary(RSFhab15mvt)
summary(RSFhab16mvt)
summary(RSFhab17mvt)
summary(RSFhab18mvt)

globalRSFhabmvt <- glmer(Randoms ~ LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
summary(globalRSFhabmvt)

##Calculate AIC and delta
aics<-AIC(RSFhab6mvt,RSFhab7mvt,RSFhab8mvt,RSFhab9mvt,RSFhab10mvt,RSFhab11mvt,RSFhab12mvt,RSFhab13mvt,
          RSFhab14mvt,RSFhab15mvt,RSFhab16mvt,RSFhab17mvt,RSFhab18mvt,globalRSFhabmvt)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)
#### % of information explained
r.squaredGLMM(globalRSFhab)

#####5 tops models landcover
stargazer(globalRSFhabmvt, RSFhab14mvt, RSFhab17mvt, RSFhab16mvt, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "5RSFbestmodelshabmvt.htm")

####boxplot covariates by states
boxplot((LichenSc) ~ factor(Randoms)  , data =allNDVI_mvt, notch = TRUE)
boxplot((WetlandSc) ~ factor(Randoms)  , data = allNDVI_mvt, notch = TRUE)
boxplot((ForestSc) ~ factor(Randoms)  , data = allNDVI_mvt, notch = TRUE)
boxplot((RockySc) ~ factor(Randoms)  , data = allNDVI_mvt, notch = FALSE)
boxplot((WaterSc) ~ factor(Randoms)  , data = allNDVI_mvt, notch = FALSE)
boxplot((sweSc) ~ factor(Randoms)  , data = allNDVI_mvt, notch = FALSE)
boxplot((prcpSc) ~ factor(Randoms)  , data = allNDVI_mvt, notch = FALSE)


####Weather candidates models
RSFweather1mvt<-mclogit(cbind(Randoms,PtID)~prcpSc, data = allNDVI_mvt)
RSFweather2mvt<-mclogit(cbind(Randoms,PtID)~tmaxSc, data = allNDVI_mvt)
RSFglobalweathermvt<-mclogit(cbind(Randoms,PtID)~prcpSc + tmaxSc , data = allNDVI_mvt)
summary(RSFweather1mvt)
summary(RSFweather2mvt)
summary(RSFglobalweathermvt)

##Calculate AIC and delta
aics<-AIC(RSFweather1mvt,RSFweather2mvt,RSFglobalweathermvt)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFglobalweather, RSFweather2, RSFweather1, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "RSFBestmodelweather.htm")

###Environment candidates models
RSFenv1mvt<-mclogit(cbind(Randoms,PtID)~NDVISc, data = allNDVI_mvt)
RSFenv2mvt<-mclogit(cbind(Randoms,PtID)~sweSc, data = allNDVI_mvt)
RSFglobalenvmvt <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc, data = allNDVI_mvt)
summary(RSFenv1mvt)
summary(RSFenv2mvt)
summary(RSFglobalenvmvt)

##Calculate AIC and delta
aics<-AIC(RSFenv1mvt,RSFenv2mvt,RSFglobalenvmvt)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFglobalenvmvt, RSFenv1mvt, RSFenv2mvt, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "RSFCandidateenv_mvt.htm")

###Landcover + env 
RSFmodel1mvt <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc, data = allNDVI_mvt)
summary(RSFmodel1mvt)

#RSFmodel2mvt <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:NDVISc, data = allNDVI_mvt)
#RSFmodel3mvt <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + WetlandSc:NDVISc, data = allNDVI_mvt)
#RSFmodel4mvt <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + ForestSc:NDVISc, data = allNDVI_mvt)
summary(RSFmodel2mvt)
summary(RSFmodel3mvt)
#summary(RSFmodel4mvt)

RSFmodel5mvt <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:sweSc, data = allNDVI_mvt)
RSFmodel6mvt <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + WetlandSc:sweSc, data = allNDVI_mvt)
RSFmodel7mvt <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + ForestSc:sweSc, data = allNDVI_mvt)
RSFmodel8mvt <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + RockySc:sweSc, data = allNDVI_mvt)
summary(RSFmodel5mvt)
summary(RSFmodel6mvt)
summary(RSFmodel7mvt)
summary(RSFmodel8mvt)

RSFmodel9mvt <- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:sweSc + WetlandSc:sweSc + ForestSc:sweSc + RockySc:sweSc, data = allNDVI_mvt)
RSFmodel10mvt<- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  LichenSc:sweSc + ForestSc:sweSc + RockySc:sweSc, data = allNDVI_mvt)
RSFmodel11mvt<- mclogit(cbind(Randoms,PtID) ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  LichenSc:sweSc + ForestSc:sweSc + WetlandSc:sweSc, data = allNDVI_mvt)
summary(RSFmodel9mvt)
summary(RSFmodel10mvt)
summary(RSFmodel11mvt)

aics<-AIC(RSFmodel1mvt, RSFmodel5mvt, RSFmodel6mvt, RSFmodel7mvt, RSFmodel8mvt,
          RSFmodel9mvt, RSFmodel10mvt, RSFmodel11mvt)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFmodel6mvt, RSFmodel1mvt, RSFmodel8mvt, RSFmodel7mvt, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "RSFLandcover+Env_mvt.htm")

######Landcover + Weather
RSFmodel12mvt <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc, data = allNDVI_mvt)
summary(RSFmodel12mvt)

RSFmodel13mvt <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:ForestSc, data = allNDVI_mvt)
RSFmodel14mvt <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:LichenSc, data = allNDVI_mvt)
RSFmodel15mvt <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:WetlandSc, data = allNDVI_mvt)
RSFmodel16mvt <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:RockySc, data = allNDVI_mvt)

RSFmodel17mvt <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:ForestSc, data = allNDVI_mvt)
RSFmodel18mvt <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:LichenSc, data = allNDVI_mvt)
RSFmodel19mvt <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:WetlandSc, data = allNDVI_mvt)
RSFmodel20mvt <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:RockySc, data = allNDVI_mvt)
summary(RSFmodel13mvt)
summary(RSFmodel14mvt)
summary(RSFmodel15mvt)
summary(RSFmodel16mvt)
summary(RSFmodel17mvt)
summary(RSFmodel18mvt)
summary(RSFmodel19mvt)
summary(RSFmodel20mvt) ####no effect prcp*Rocky

aics<-AIC(RSFmodel12mvt,RSFmodel13mvt, RSFmodel14mvt, RSFmodel15mvt, RSFmodel16mvt, RSFmodel17mvt,
          RSFmodel18mvt, RSFmodel19mvt, RSFmodel20mvt)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

RSFmodel21mvt <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  tmaxSc:ForestSc +  tmaxSc:LichenSc + tmaxSc:WetlandSc + tmaxSc:RockySc, data = allNDVI_mvt)
RSFmodel22mvt <- mclogit(cbind(Randoms,PtID)~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  prcpSc:ForestSc +  prcpSc:LichenSc + prcpSc:WetlandSc, data = allNDVI_mvt)

summary(RSFmodel21mvt)
summary(RSFmodel22mvt)
RSFmodel23mvt <- mclogit(cbind(Randoms,PtID) ~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  tmaxSc:LichenSc + tmaxSc:WetlandSc + tmaxSc:RockySc + prcpSc:ForestSc + prcpSc:WetlandSc, data = allNDVI_mvt)
RSFmodel24mvt <- mclogit(cbind(Randoms,PtID) ~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:ForestSc +  tmaxSc:LichenSc + tmaxSc:WetlandSc + tmaxSc:RockySc + prcpSc:ForestSc +  prcpSc:LichenSc + prcpSc:WetlandSc, data = allNDVI_mvt)
RSFmodel25mvt <- mclogit(cbind(Randoms,PtID) ~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:ForestSc +  tmaxSc:LichenSc + tmaxSc:WetlandSc + prcpSc:RockySc, data = allNDVI_mvt)

summary(RSFmodel23mvt)
summary(RSFmodel24mvt)
summary(RSFmodel25mvt)

aics<-AIC(RSFmodel12mvt,RSFmodel13mvt, RSFmodel14mvt, RSFmodel15mvt, RSFmodel16mvt, RSFmodel17mvt,
          RSFmodel18mvt, RSFmodel19mvt, RSFmodel20mvt, RSFmodel21mvt, RSFmodel22mvt, 
          RSFmodel23mvt, RSFmodel24mvt,RSFmodel25mvt)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFmodel24,RSFmodel23, RSFmodel21, RSFmodel14, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "RSFLandcover+Weather.htm")

###Weather + Env 
RSFmodel26mvt <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc , data = allNDVI_mvt)
RSFmodel27mvt <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc +  NDVISc:tmaxSc , data = allNDVI_mvt)
RSFmodel28mvt <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc +  prcpSc:sweSc , data = allNDVI_mvt)
RSFmodel29mvt <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc +  NDVISc:tmaxSc +  prcpSc:sweSc , data = allNDVI_mvt)

summary(RSFmodel26mvt)
summary(RSFmodel27mvt)
summary(RSFmodel28mvt)
summary(RSFmodel29mvt)


###test models
aics<-AIC(RSFmodel26mvt,RSFmodel27mvt,RSFmodel28mvt,RSFmodel29mvt)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFmodel26,RSFmodel25, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "RSFEnv+Weather.htm")

######3 models together 
RSFmodel30mvt <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc +
                           WetlandSc + ForestSc + RockySc + WaterSc + sweSc:WetlandSc +
                           prcpSc:RockySc + tmaxSc:ForestSc + tmaxSc:LichenSc + tmaxSc:WetlandSc +
                           prcpSc:sweSc + NDVISc:tmaxSc , data = allNDVI_mvt)
RSFmodel31mvt <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc +
                           WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:ForestSc + tmaxSc:LichenSc +
                           prcpSc:sweSc + NDVISc:tmaxSc , data = allNDVI_mvt)
RSFmodel32mvt <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc +
                           WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:ForestSc + tmaxSc:LichenSc + 
                           tmaxSc:WetlandSc + prcpSc:sweSc + NDVISc:tmaxSc , data = allNDVI_mvt)
summary(RSFmodel30mvt)
summary(RSFmodel31mvt)
summary(RSFmodel32mvt)


####
aics<-AIC(RSFmodel30mvt,RSFmodel31mvt,RSFmodel32mvt)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)



aics<-AIC(RSFmodel1mvt, RSFmodel5mvt, RSFmodel6mvt, RSFmodel7mvt, RSFmodel8mvt,
          RSFmodel9mvt, RSFmodel10mvt, RSFmodel12mvt,RSFmodel13mvt, RSFmodel14mvt,
          RSFmodel15mvt, RSFmodel16mvt, RSFmodel17mvt,
          RSFmodel18mvt, RSFmodel19mvt, RSFmodel20mvt, RSFmodel21mvt, RSFmodel22mvt, 
          RSFmodel23mvt, RSFmodel24mvt,RSFmodel25mvt,RSFmodel26mvt,RSFmodel27mvt,RSFmodel28mvt,
          RSFmodel29mvt,RSFmodel30mvt,RSFmodel31mvt,RSFmodel32mvt)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(RSFmodel30mvt, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = TRUE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "BestRSFstopover_mvt.htm")

####save tab with two best models mvt and stopover
stargazer(RSFmodel34,RSFmodel30mvt, type = "html",title = "RSF models",
          column.labels = c("Stopover model", "Movement model"),
          align = TRUE, 
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.95,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSFmodels stop_mvt.htm")

###Calculate interval of confidence
carsprfixef<-coef(RSFmodel30mvt)##extract betas
carsprSEs<-c(sqrt(diag(vcov(RSFmodel30mvt)))) ## Extract SEs
## Make lower and upper 95% CIs
carsprLCI<-carsprfixef-(carsprSEs*1.96)
carsprUCI<-carsprfixef+(carsprSEs*1.96)
## Round the values
carsprfixefR<-round(carsprfixef,3)
## Make the CIs
carsprCI<-paste(carsprfixefR, " [",round(carsprLCI,3), ", ",round(carsprUCI,3),"]",sep="")
names<-names(carsprfixef)
carsprCInamed<-data.frame(names,carsprCI)
