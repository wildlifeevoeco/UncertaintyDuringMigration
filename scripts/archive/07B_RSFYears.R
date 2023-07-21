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

####scale all variables in advance 
allNDVI<-subset(allNDVI, select = -c(2:5,7:8, 12:18, 22:23, 25:27))
allNDVI$prcpSc<-scale(allNDVI$prcp)
allNDVI$tmaxSc<-scale(allNDVI$tmax)
allNDVI$prcpSc<-scale(allNDVI$prcp)
allNDVI$sweSc<-scale(allNDVI$swe)
allNDVI$ForestSc<-scale(allNDVI$Forest)
allNDVI$LichenSc<-scale(allNDVI$Lichen)
allNDVI$WetlandSc<-scale(allNDVI$Wetland)
allNDVI$RockySc<-scale(allNDVI$Rocky)
allNDVI$WaterSc<-scale(allNDVI$Water)
allNDVI$NDVISc<-scale(allNDVI$NDVI)
allNDVI<-subset(allNDVI, select = -c(10:26))

####subset data by years
allNDVI2010 <- subset(allNDVI, Year == 2010)
allNDVI2011 <- subset(allNDVI, Year == 2011)
allNDVI2012 <- subset(allNDVI, Year == 2012)
allNDVI2013 <- subset(allNDVI, Year == 2013)

####subset by state
allNDVI_stop2010<- subset(allNDVI2010, HMM == 1)
allNDVI_mvt2010 <- subset(allNDVI2010, HMM == 0)
allNDVI_stop2011<- subset(allNDVI2011, HMM == 1)
allNDVI_mvt2011 <- subset(allNDVI2011, HMM == 0)
allNDVI_stop2012<- subset(allNDVI2012, HMM == 1)
allNDVI_mvt2012 <- subset(allNDVI2012, HMM == 0)
allNDVI_stop2013<- subset(allNDVI2013, HMM == 1)
allNDVI_mvt2013 <- subset(allNDVI2013, HMM == 0)

###############################################
#######RSF MODEL WITH ENCAMPED Year 2010#################
###Habitat model RSF without year and indiv
modelhab1 <- glm(Randoms ~ WetlandSc + ForestSc + LichenSc + WaterSc + RockySc , data = allNDVI_stop2010, family = "binomial")

summ(modelhab1)

car::vif(modelhab1)

boxplot((LichenSc) ~ factor(Randoms)  , data = allNDVI_stop2010, notch = TRUE)

###excluding water 
modelhab2<-glmer(Randoms ~ WetlandSc + ForestSc + LichenSc + RockySc + (1|Animal_ID), data = allNDVI_stop2010, family = "binomial")

summ(modelhab2)
summary(modelhab2)

###Foraging habitat (Lichen and Wetland)
modelhab3 <- glmer (Randoms ~ WetlandSc + LichenSc + (1|Animal_ID), data = allNDVI_stop2010, family = "binomial")
summ(modelhab3)

####R squared 
r.squaredGLMM(modelhab1)
r.squaredGLMM(modelhab2)
r.squaredGLMM(modelhab3)

#######MODELS WITH TEMPORAL DATA (NDVI AND WEATHER) using mclogit package (!Not possible to have random effects with these models, that's why I run the models by years to see if there is a difference)
###NDVI model 
model4<-mclogit(cbind(Randoms,PtID)~ NDVI, data = allNDVI_stop2010)
summary(model4)


boxplot((NDVISc) ~ factor(Randoms)  , data = allNDVI_stop2010, notch = TRUE)
boxplot((NDVI) ~ factor(Randoms)  , data = test, notch = TRUE)

####NDVI:Wetland and Lichen and Forest
model5<-mclogit(cbind(Randoms,PtID)~NDVISc*WetlandSc + NDVISc*LichenSc + NDVISc*ForestSc + ForestSc + NDVISc + WetlandSc + LichenSc , data=allNDVI_stop2010)
summary(model5)

####NDVI:Weather ???
model6<-mclogit(cbind(Randoms,PtID)~ NDVISc*(sweSc + prcpSc + tmaxSc) + NDVISc + sweSc + prcpSc + tmaxSc, data=allNDVI_stop2010)
summary(model6)

###Weather:Habitat RSF with interaction between Wetland/Weather and Forest/Weather
### There are not selecting for snow, because lot of swe data were = 0 this year, no snow on the ground everywhere? or real avoidance ?
model7<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_stop2010)
summary(model7)
boxplot((sweSc) ~ factor(Randoms)  , data = allNDVI_stop2010) ###when I check the graph, not a lot of snow this year, 0 = available and 1 = used

###Weather:Habitat RSF with interaction between Lichen/Weather and Forest/Weather
model8<-mclogit(cbind(Randoms,PtID)~ ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_stop2010)
summary(model8)

#####Weather selection ??
model8<-mclogit(cbind(Randoms,PtID)~ tmaxSc + prcpSc + sweSc, data=allNDVI_stop2010)
summary(model8)

####Global models RSF stopover#####
##Global model with interaction Habitat(Forest and Wetland)/Weather
globalmodel1_stop<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2010)
summary(globalmodel1_stop)

###Global model with interaction Habitat(Forest and Lichen)/Weather
globalmodel2_stop<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2010)
summary(globalmodel2_stop)

###Global model with interaction Weather/NDVI, really big selection for NDVI
#globalmodel3_stop<-mclogit(cbind(Randoms,PtID)~NDVISc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + #LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2010)
#summary(globalmodel3_stop)

###Global model 4
#globalmodel4_stop<-mclogit(cbind(Randoms,PtID)~ WetlandSc*(prcpSc + sweSc + tmaxSc) + LichenSc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2010)
#summary(globalmodel4_stop)

##Calculate AIC and delta
aics<-AIC(modelhab1,modelhab2,modelhab3,model4,model5,model6,model7,model8,globalmodel1_stop,globalmodel2_stop,globalmodel4_stop)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

####save best model 
stargazer(globalmodel2_stop, type = "html", title = "RSF model stopover",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Lichen","Wetland","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Lichen:Precipitation","Lichen:SWE","Lichen:Temperature"),
          align= TRUE,
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global model_stop2010.htm")

###############################################
#######RSF MODEL WITH MOVEMENT Year 2010#################
###Habitat model RSF without year and indiv
modelhab1 <- glm(Randoms ~ WetlandSc + ForestSc + LichenSc + WaterSc + RockySc , data = allNDVI_mvt2010, family = "binomial")

summ(modelhab1)

car::vif(modelhab1)

boxplot((LichenSc) ~ factor(Randoms)  , data = allNDVI_mvt2010, notch = TRUE)

###excluding water 
modelhab2<-glmer(Randoms ~ WetlandSc + ForestSc + LichenSc + RockySc + (1|Animal_ID), data = allNDVI_mvt2010, family = "binomial")

summ(modelhab2)
summary(modelhab2)

###Foraging habitat (Lichen and Wetland)
modelhab3 <- glmer (Randoms ~ WetlandSc + LichenSc + (1|Animal_ID), data = allNDVI_mvt2010, family = "binomial")
summ(modelhab3)

####R squared 
r.squaredGLMM(modelhab1)
r.squaredGLMM(modelhab2)
r.squaredGLMM(modelhab3)

#######MODELS WITH TEMPORAL DATA (NDVI AND WEATHER) using mclogit package (!Not possible to have random effects with these models, that's why I run the models by years to see if there is a difference)
###NDVI model 
model4<-mclogit(cbind(Randoms,PtID)~ NDVISc, data = allNDVI_mvt2010)
summary(model4)


boxplot((NDVISc) ~ factor(Randoms)  , data = allNDVI_mvt2010, notch = TRUE)
boxplot((NDVI) ~ factor(Randoms)  , data = test, notch = TRUE)

####NDVI:Wetland and Lichen and Forest
model5<-mclogit(cbind(Randoms,PtID)~NDVISc*WetlandSc + NDVISc*LichenSc + NDVISc*ForestSc + ForestSc + NDVISc + WetlandSc + LichenSc , data=allNDVI_mvt2010)
summary(model5)

####NDVI:Weather ???
model6<-mclogit(cbind(Randoms,PtID)~ NDVISc*(sweSc + prcpSc + tmaxSc) + NDVISc + sweSc + prcpSc + tmaxSc, data=allNDVI_mvt2010)
summary(model6)

###Weather:Habitat RSF with interaction between Wetland/Weather and Forest/Weather
### There are not selecting for snow, because lot of swe data were = 0 this year, no snow on the ground everywhere? or real avoidance ?
model7<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_mvt2010)
summary(model7)
boxplot((sweSc) ~ factor(Randoms)  , data = allNDVI_mvt2010) ###when I check the graph, not a lot of snow this year, 0 = available and 1 = used

###Weather:Habitat RSF with interaction between Lichen/Weather and Forest/Weather
model8<-mclogit(cbind(Randoms,PtID)~ ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_mvt2010)
summary(model8)

#####Weather selection ??
model9<-mclogit(cbind(Randoms,PtID)~ tmaxSc + prcpSc + sweSc, data=allNDVI_mvt2010)
summary(model9)

####Global models RSF stopover#####
##Global model with interaction Habitat(Forest and Wetland)/Weather
globalmodel1_mvt<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2010)
summary(globalmodel1_mvt)

###Global model with interaction Habitat(Forest and Lichen)/Weather
globalmodel2_mvt<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2010)
summary(globalmodel2_mvt)

###Global model with interaction Weather/NDVI, really big selection for NDVI
#globalmodel3_stop<-mclogit(cbind(Randoms,PtID)~NDVISc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + #LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2010)
#summary(globalmodel3_stop)

###Global model 4
globalmodel4_mvt<-mclogit(cbind(Randoms,PtID)~ WetlandSc*(prcpSc + sweSc + tmaxSc) + LichenSc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2010)
summary(globalmodel4_mvt)

##Calculate AIC and delta
aics<-AIC(modelhab1,modelhab2,modelhab3,model4,model5,model6,model7,model8,model9,globalmodel1_mvt,globalmodel2_mvt,globalmodel4_mvt)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

####save best model 
stargazer(globalmodel2_mvt, type = "html", title = "RSF model stopover",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Lichen","Wetland","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Lichen:Precipitation","Lichen:SWE","Lichen:Temperature"),
          align= TRUE,
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global model_mvt2010.htm")

###############################################
#######RSF MODEL WITH ENCAMPED Year 2011#################
###Habitat model RSF without year and indiv
modelhab1b <- glm(Randoms ~ WetlandSc + ForestSc + LichenSc + WaterSc + RockySc , data = allNDVI_stop2011, family = "binomial")

summ(modelhab1b)

car::vif(modelhab1b)

boxplot((LichenSc) ~ factor(Randoms)  , data = allNDVI_stop2011, notch = TRUE)

###excluding water 
modelhab2b<-glmer(Randoms ~ WetlandSc + ForestSc + LichenSc + RockySc + (1|Animal_ID), data = allNDVI_stop2011, family = "binomial")

summ(modelhab2b)
summary(modelhab2b)

###Foraging habitat (Lichen and Wetland)
modelhab3b <- glmer (Randoms ~ WetlandSc + LichenSc + (1|Animal_ID), data = allNDVI_stop2011, family = "binomial")
summ(modelhab3b)

#######MODELS WITH TEMPORAL DATA (NDVI AND WEATHER) using mclogit package (!Not possible to have random effects with these models, that's why I run the models by years to see if there is a difference)
###NDVI model 
model4b<-mclogit(cbind(Randoms,PtID)~ NDVI,data = allNDVI_stop2011)
summary(model4b)
boxplot((NDVISc) ~ factor(Randoms)  , data = allNDVI_stop2010, notch = TRUE)
boxplot((NDVI) ~ factor(Randoms)  , data = test, notch = TRUE)

####R squared 
r.squaredGLMM(modelhab1b)
r.squaredGLMM(modelhab2b)
r.squaredGLMM(modelhab3b)
r.squaredGLMM(modelhab4b)

####NDVI:Wetland and Lichen and Forest
model5b<-mclogit(cbind(Randoms,PtID)~NDVISc*WetlandSc + NDVISc*LichenSc + NDVISc*ForestSc + ForestSc + NDVISc + WetlandSc + LichenSc , data=allNDVI_stop2011)
summary(model5b)

####NDVI:Weather ???
model6b<-mclogit(cbind(Randoms,PtID)~ NDVISc*(sweSc + prcpSc + tmaxSc) + NDVISc + sweSc + prcpSc + tmaxSc, data=allNDVI_stop2011)
summary(model6b)

###Weather:Habitat RSF with interaction between Wetland/Weather and Forest/Weather
### There are not selecting for snow, because lot of swe data were = 0 this year, no snow on the ground everywhere? or real avoidance ?
model7b<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_stop2011)
summary(model7b)
boxplot((sweSc) ~ factor(Randoms)  , data = allNDVI_stop2011) ###when I check the graph, not a lot of snow this year, 0 = available and 1 = used

###Weather:Habitat RSF with interaction between Lichen/Weather and Forest/Weather
model8b<-mclogit(cbind(Randoms,PtID)~ ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_stop2011)
summary(model8b)

#####Weather selection ??
model9b<-mclogit(cbind(Randoms,PtID)~ tmaxSc + prcpSc + sweSc, data=allNDVI_stop2011)
summary(model9b)

####Global models RSF stopover#####
##Global model with interaction Habitat(Forest and Wetland)/Weather
globalmodel1_stopb<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2011)
summary(globalmodel1_stopb)

###Global model with interaction Habitat(Forest and Lichen)/Weather
globalmodel2_stopb<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2011)
summary(globalmodel2_stopb)

###Global model with interaction Weather/NDVI, really big selection for NDVI
#globalmodel3_stopb<-mclogit(cbind(Randoms,PtID)~NDVISc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2011)
#summary(globalmodel3_stopb)

###Global model 4
#globalmodel4_stopb<-mclogit(cbind(Randoms,PtID)~ WetlandSc*(prcpSc + sweSc + tmaxSc) + LichenSc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2011)
#summary(globalmodel4_stopb)

##Calculate AIC and delta
aics<-AIC(modelhab1b,modelhab2b,modelhab3b,model4b,model5b,model6b,model7b,model8b,model9b,globalmodel1_stopb,globalmodel2_stopb,globalmodel3_stopb,globalmodel4_stopb)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

####save best model 
stargazer(globalmodel2_stopb, type = "html", title = "RSF model stopover 2010",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Lichen","Wetland","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Lichen:Precipitation","Lichen:SWE","Lichen:Temperature"),
          align= TRUE,
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global model_stop2011.htm")

###############################################
#######RSF MODEL WITH MOVEMENT Year 2011#################
###Habitat model RSF without year and indiv
modelhab1c <- glm(Randoms ~ WetlandSc + ForestSc + LichenSc + WaterSc + RockySc , data = allNDVI_mvt2011, family = "binomial")

summ(modelhab1c)

car::vif(modelhab1c)

boxplot((LichenSc) ~ factor(Randoms)  , data = allNDVI_mvt2011, notch = TRUE)

###excluding water 
modelhab2c<-glmer(Randoms ~ WetlandSc + ForestSc + LichenSc + RockySc + (1|Animal_ID), data = allNDVI_mvt2011, family = "binomial")

summ(modelhab2c)
summary(modelhab2c)

###Foraging habitat (Lichen and Wetland)
modelhab3c <- glmer (Randoms ~ WetlandSc + LichenSc + (1|Animal_ID), data = allNDVI_mvt2011, family = "binomial")
summ(modelhab3c)

#######MODELS WITH TEMPORAL DATA (NDVI AND WEATHER) using mclogit package (!Not possible to have random effects with these models, that's why I run the models by years to see if there is a difference)
###NDVI model 
model4c<-mclogit(cbind(Randoms,PtID)~ NDVISc, data = allNDVI_mvt2011)
summary(model4c)

####R squared 
r.squaredGLMM(modelhab1c)
r.squaredGLMM(modelhab2c)
r.squaredGLMM(modelhab3c)
r.squaredGLMM(modelhab4c)

boxplot((NDVISc) ~ factor(Randoms)  , data = allNDVI_mvt2011, notch = TRUE)
boxplot((NDVI) ~ factor(Randoms)  , data = test, notch = TRUE)

####NDVI:Wetland and Lichen and Forest
model5c<-mclogit(cbind(Randoms,PtID)~NDVISc*WetlandSc + NDVISc*LichenSc + NDVISc*ForestSc + ForestSc + NDVISc + WetlandSc + LichenSc , data=allNDVI_mvt2011)
summary(model5c)

####NDVI:Weather ???
model6c<-mclogit(cbind(Randoms,PtID)~ NDVISc*(sweSc + prcpSc + tmaxSc) + NDVISc + sweSc + prcpSc + tmaxSc, data=allNDVI_mvt2011)
summary(model6c)

###Weather:Habitat RSF with interaction between Wetland/Weather and Forest/Weather
### There are not selecting for snow, because lot of swe data were = 0 this year, no snow on the ground everywhere? or real avoidance ?
model7c<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_mvt2011)
summary(model7c)
boxplot((sweSc) ~ factor(Randoms)  , data = allNDVI_mvt2011) ###when I check the graph, not a lot of snow this year, 0 = available and 1 = used

###Weather:Habitat RSF with interaction between Lichen/Weather and Forest/Weather
model8c<-mclogit(cbind(Randoms,PtID)~ ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_mvt2011)
summary(model8c)

#####Weather selection ??
model9c<-mclogit(cbind(Randoms,PtID)~ tmaxSc + prcpSc + sweSc, data=allNDVI_mvt2011)
summary(model9c)

####Global models RSF stopover#####
##Global model with interaction Habitat(Forest and Wetland)/Weather
globalmodel1_mvtc<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2011)
summary(globalmodel1_mvtc)

###Global model with interaction Habitat(Forest and Lichen)/Weather
globalmodel2_mvtc<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2011)
summary(globalmodel2_mvtc)

###Global model with interaction Weather/NDVI, really big selection for NDVI
#globalmodel3_stop<-mclogit(cbind(Randoms,PtID)~NDVISc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + #LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2010)
#summary(globalmodel3_stop)

###Global model 4
#globalmodel4_mvtc<-mclogit(cbind(Randoms,PtID)~ WetlandSc*(prcpSc + sweSc + tmaxSc) + LichenSc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2011)
#summary(globalmodel4_mvtc)

##Calculate AIC and delta
aics<-AIC(modelhab1c,modelhab2c,modelhab3c,model4c,model5c,model6c,model7c,model8c,model9c,globalmodel1_mvtc,globalmodel2_mvtc,globalmodel4_mvtc)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

####save best model 
stargazer(globalmodel2_mvtc, type = "html", title = "RSF model mvt",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Lichen","Wetland","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Lichen:Precipitation","Lichen:SWE","Lichen:Temperature"),
          align= TRUE,
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global model_mvt2011.htm")

###############################################
#######RSF MODEL WITH ENCAMPED Year 2012#################
###Habitat model RSF without year and indiv
modelhab1d <- glm(Randoms ~ WetlandSc + ForestSc + LichenSc + WaterSc + RockySc , data = allNDVI_stop2012, family = "binomial")

summ(modelhab1d)

car::vif(modelhab1b)

boxplot((LichenSc) ~ factor(Randoms)  , data = allNDVI_stop2012, notch = TRUE)

###excluding water 
modelhab2d<-glmer(Randoms ~ WetlandSc + ForestSc + LichenSc + RockySc + (1|Animal_ID), data = allNDVI_stop2012, family = "binomial")

summ(modelhab2d)
summary(modelhab2d)

###Foraging habitat (Lichen and Wetland)
modelhab3d <- glmer (Randoms ~ WetlandSc + LichenSc + (1|Animal_ID), data = allNDVI_stop2012, family = "binomial")
summ(modelhab3d)

#######MODELS WITH TEMPORAL DATA (NDVI AND WEATHER) using mclogit package (!Not possible to have random effects with these models, that's why I run the models by years to see if there is a difference)
###NDVI model 
model4d<-mclogit(cbind(Randoms,PtID)~ NDVI,data = allNDVI_stop2012)
summary(model4d)
boxplot((NDVISc) ~ factor(Randoms)  , data = allNDVI_stop2012, notch = TRUE)
boxplot((NDVI) ~ factor(Randoms)  , data = test, notch = TRUE)

####NDVI:Wetland and Lichen and Forest
model5d<-mclogit(cbind(Randoms,PtID)~NDVISc*WetlandSc + NDVISc*LichenSc + NDVISc*ForestSc + ForestSc + NDVISc + WetlandSc + LichenSc , data=allNDVI_stop2012)
summary(model5d)

####R squared 
r.squaredGLMM(modelhab1d)
r.squaredGLMM(modelhab2d)
r.squaredGLMM(modelhab3d)
r.squaredGLMM(modelhab4d)
r.squaredGLMM(modelhab5d)

####NDVI:Weather ???
model6d<-mclogit(cbind(Randoms,PtID)~ NDVISc*(sweSc + prcpSc + tmaxSc) + NDVISc + sweSc + prcpSc + tmaxSc, data=allNDVI_stop2012)
summary(model6d)

###Weather:Habitat RSF with interaction between Wetland/Weather and Forest/Weather
### There are not selecting for snow, because lot of swe data were = 0 this year, no snow on the ground everywhere? or real avoidance ?
model7d<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_stop2012)
summary(model7d)
boxplot((sweSc) ~ factor(Randoms)  , data = allNDVI_stop2012) ###when I check the graph, not a lot of snow this year, 0 = available and 1 = used

###Weather:Habitat RSF with interaction between Lichen/Weather and Forest/Weather
model8d<-mclogit(cbind(Randoms,PtID)~ ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_stop2012)
summary(model8d)

#####Weather selection ??
model9d<-mclogit(cbind(Randoms,PtID)~ tmaxSc + prcpSc + sweSc, data=allNDVI_stop2012)
summary(model9d)

####Global models RSF stopover#####
##Global model with interaction Habitat(Forest and Wetland)/Weather
globalmodel1_stopd<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2012)
summary(globalmodel1_stopd)

###Global model with interaction Habitat(Forest and Lichen)/Weather
globalmodel2_stopd<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2012)
summary(globalmodel2_stopd)

###Global model with interaction Weather/NDVI, really big selection for NDVI
#globalmodel3_stopd<-mclogit(cbind(Randoms,PtID)~NDVISc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + #LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2012)
#summary(globalmodel3_stopd)

###Global model 4
#globalmodel4_stopd<-mclogit(cbind(Randoms,PtID)~ WetlandSc*(prcpSc + sweSc + tmaxSc) + LichenSc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2012)
#summary(globalmodel4_stopd)

##Calculate AIC and delta
aics<-AIC(modelhab1d,modelhab2d,modelhab3d,model4d,model5d,model6d,model7d,model8d,model9d,globalmodel1_stopd,globalmodel2_stopd,globalmodel4_stopd)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

####save best model 
stargazer(globalmodel2_stopd, type = "html", title = "RSF model stopover 2010",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Lichen","Wetland","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Lichen:Precipitation","Lichen:SWE","Lichen:Temperature"),
          align= TRUE,
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global model_stop2012.htm")

###############################################
#######RSF MODEL WITH MOVEMENT Year 2012#################
###Habitat model RSF without year and indiv
modelhab1e <- glm(Randoms ~ WetlandSc + ForestSc + LichenSc + WaterSc + RockySc , data = allNDVI_mvt2012, family = "binomial")

summ(modelhab1e)

car::vif(modelhab1c)

boxplot((LichenSc) ~ factor(Randoms)  , data = allNDVI_mvt2012, notch = TRUE)

###excluding water 
modelhab2e<-glmer(Randoms ~ WetlandSc + ForestSc + LichenSc + RockySc + (1|Animal_ID), data = allNDVI_mvt2012, family = "binomial")

summ(modelhab2e)
summary(modelhab2e)

###Foraging habitat (Lichen and Wetland)
modelhab3e <- glmer (Randoms ~ WetlandSc + LichenSc + (1|Animal_ID), data = allNDVI_mvt2012, family = "binomial")
summ(modelhab3e)

#######MODELS WITH TEMPORAL DATA (NDVI AND WEATHER) using mclogit package (!Not possible to have random effects with these models, that's why I run the models by years to see if there is a difference)
###NDVI model 
model4e<-mclogit(cbind(Randoms,PtID)~ NDVISc, data = allNDVI_mvt2012)
summary(model4e)

####NDVI:Wetland and Lichen and Forest
model5e<-mclogit(cbind(Randoms,PtID)~NDVISc*WetlandSc + NDVISc*LichenSc + NDVISc*ForestSc + ForestSc + NDVISc + WetlandSc + LichenSc , data=allNDVI_mvt2012)
summary(model5e)

####R squared 
r.squaredGLMM(modelhab1e)
r.squaredGLMM(modelhab2e)
r.squaredGLMM(modelhab3e)
r.squaredGLMM(modelhab4e)
r.squaredGLMM(modelhab5e)

boxplot((NDVISc) ~ factor(Randoms)  , data = allNDVI_mvt2012, notch = TRUE)
boxplot((NDVI) ~ factor(Randoms)  , data = test, notch = TRUE)


####NDVI:Weather ???
model6e<-mclogit(cbind(Randoms,PtID)~ NDVISc*(sweSc + prcpSc + tmaxSc) + NDVISc + sweSc + prcpSc + tmaxSc, data=allNDVI_mvt2012)
summary(model6e)

###Weather:Habitat RSF with interaction between Wetland/Weather and Forest/Weather
### There are not selecting for snow, because lot of swe data were = 0 this year, no snow on the ground everywhere? or real avoidance ?
model7e<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_mvt2012)
summary(model7e)
boxplot((sweSc) ~ factor(Randoms)  , data = allNDVI_mvt2012) ###when I check the graph, not a lot of snow this year, 0 = available and 1 = used

###Weather:Habitat RSF with interaction between Lichen/Weather and Forest/Weather
model8e<-mclogit(cbind(Randoms,PtID)~ ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_mvt2012)
summary(model8e)

#####Weather selection ??
model9e<-mclogit(cbind(Randoms,PtID)~ tmaxSc + prcpSc + sweSc, data=allNDVI_mvt2012)
summary(model9e)

####Global models RSF stopover#####
##Global model with interaction Habitat(Forest and Wetland)/Weather
globalmodel1_mvte<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2012)
summary(globalmodel1_mvte)

###Global model with interaction Habitat(Forest and Lichen)/Weather
globalmodel2_mvte<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2012)
summary(globalmodel2_mvte)

###Global model with interaction Weather/NDVI, really big selection for NDVI
#globalmodel3_stop<-mclogit(cbind(Randoms,PtID)~NDVISc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + #LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2010)
#summary(globalmodel3_stop)

###Global model 4
#globalmodel4_mvte<-mclogit(cbind(Randoms,PtID)~ WetlandSc*(prcpSc + sweSc + tmaxSc) + LichenSc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2012)
#summary(globalmodel4_mvte)

##Calculate AIC and delta
aics<-AIC(modelhab1e,modelhab2e,modelhab3e,model4e,model5e,model6e,model7e,model8e,model9e,globalmodel1_mvte,globalmodel2_mvte,globalmodel4_mvte)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

####save best model 
stargazer(globalmodel2_mvte, type = "html", title = "RSF model mvt",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Lichen","Wetland","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Lichen:Precipitation","Lichen:SWE","Lichen:Temperature"),
          align= TRUE,
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global model_mvt2012.htm")

###############################################
#######RSF MODEL WITH ENCAMPED Year 2013#################
###Habitat model RSF without year and indiv
modelhab1f <- glm(Randoms ~ WetlandSc + ForestSc + LichenSc + WaterSc + RockySc , data = allNDVI_stop2013, family = "binomial")

summ(modelhab1f)

car::vif(modelhab1f)

boxplot((LichenSc) ~ factor(Randoms)  , data = allNDVI_stop2013, notch = TRUE)
boxplot((WetlandSc) ~ factor(Randoms)  , data = allNDVI_stop2013, notch = TRUE)
boxplot((ForestSc) ~ factor(Randoms)  , data = allNDVI_stop2013, notch = TRUE)

###excluding water 
modelhab2f<-glmer(Randoms ~ WetlandSc + ForestSc + LichenSc + RockySc + (1|Animal_ID), data = allNDVI_stop2013, family = "binomial")

summ(modelhab2f)
summary(modelhab2f)

###Foraging habitat (Lichen and Wetland)
modelhab3f <- glmer (Randoms ~ WetlandSc + LichenSc + (1|Animal_ID), data = allNDVI_stop2013, family = "binomial")
summ(modelhab3f)

#######MODELS WITH TEMPORAL DATA (NDVI AND WEATHER) using mclogit package (!Not possible to have random effects with these models, that's why I run the models by years to see if there is a difference)
###NDVI model 
model4f<-mclogit(cbind(Randoms,PtID)~ NDVI,data = allNDVI_stop2013)
summary(model4f)
boxplot((NDVISc) ~ factor(Randoms)  , data = allNDVI_stop2013, notch = TRUE)
boxplot((NDVI) ~ factor(Randoms)  , data = test, notch = TRUE)

####NDVI:Wetland and Lichen and Forest
model5f<-mclogit(cbind(Randoms,PtID)~NDVISc*WetlandSc + NDVISc*LichenSc + NDVISc*ForestSc + ForestSc + NDVISc + WetlandSc + LichenSc , data=allNDVI_stop2013)
summary(model5f)

####R squared 
r.squaredGLMM(modelhab1f)
r.squaredGLMM(modelhab2f)
r.squaredGLMM(modelhab3f)
r.squaredGLMM(modelhab4f)
r.squaredGLMM(modelhab5f)

####NDVI:Weather ???
model6f<-mclogit(cbind(Randoms,PtID)~ NDVISc*(sweSc + prcpSc + tmaxSc) + NDVISc + sweSc + prcpSc + tmaxSc, data=allNDVI_stop2013)
summary(model6f)

###Weather:Habitat RSF with interaction between Wetland/Weather and Forest/Weather
### There are not selecting for snow, because lot of swe data were = 0 this year, no snow on the ground everywhere? or real avoidance ?
model7f<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_stop2013)
summary(model7f)
boxplot((sweSc) ~ factor(Randoms)  , data = allNDVI_stop2013)###when I check the graph, not a lot of snow this year, 0 = available and 1 = used
boxplot((prcpSc) ~ factor(Randoms)  , data = allNDVI_stop2013)
boxplot((tmaxSc) ~ factor(Randoms)  , data = allNDVI_stop2013)

###Weather:Habitat RSF with interaction between Lichen/Weather and Forest/Weather
model8f<-mclogit(cbind(Randoms,PtID)~ ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_stop2013)
summary(model8f)

#####Weather selection ??
model9f<-mclogit(cbind(Randoms,PtID)~ tmaxSc + prcpSc + sweSc, data=allNDVI_stop2013)
summary(model9df)

####Global models RSF stopover#####
##Global model with interaction Habitat(Forest and Wetland)/Weather
globalmodel1_stopf<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2013)
summary(globalmodel1_stopf)

###Global model with interaction Habitat(Forest and Lichen)/Weather
globalmodel2_stopf<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2013)
summary(globalmodel2_stopf)

###Global model with interaction Weather/NDVI, really big selection for NDVI
#globalmodel3_stopd<-mclogit(cbind(Randoms,PtID)~NDVISc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + #LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2012)
#summary(globalmodel3_stopd)

###Global model 4
globalmodel4_stopf<-mclogit(cbind(Randoms,PtID)~ WetlandSc*(prcpSc + sweSc + tmaxSc) + LichenSc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2013)
summary(globalmodel4_stopf)

##Calculate AIC and delta
aics<-AIC(modelhab1f,modelhab2f,modelhab3f,model4f,model5f,model6f,model7f,model8f,model9f,globalmodel1_stopf,globalmodel2_stopf)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

####save best model 
stargazer(globalmodel2_stopf, type = "html", title = "RSF model stopover 2013",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Lichen","Wetland","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Lichen:Precipitation","Lichen:SWE","Lichen:Temperature"),
          align= TRUE,
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global model_stop2013Lichen.htm")

stargazer(globalmodel1_stopf, type = "html", title = "RSF model stopover 2013",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Wetland","Lichen","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Wetland:Precipitation","Wetland:SWE","Wetland:Temperature"),
          align= TRUE,
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global model_stop2013Wetland.htm")

###############################################
#######RSF MODEL WITH MOVEMENT Year 2013#################
###Habitat model RSF without year and indiv
modelhab1g <- glm(Randoms ~ WetlandSc + ForestSc + LichenSc + WaterSc + RockySc , data = allNDVI_mvt2013, family = "binomial")

summ(modelhab1g)

car::vif(modelhab1g)

boxplot((LichenSc) ~ factor(Randoms)  , data = allNDVI_mvt2013, notch = TRUE)

###excluding water 
modelhab2g<-glmer(Randoms ~ WetlandSc + ForestSc + LichenSc + RockySc + (1|Animal_ID), data = allNDVI_mvt2013, family = "binomial")

summ(modelhab2g)
summary(modelhab2g)

###Foraging habitat (Lichen and Wetland)
modelhab3g <- glmer (Randoms ~ WetlandSc + LichenSc + (1|Animal_ID), data = allNDVI_mvt2013, family = "binomial")
summ(modelhab3g)

#######MODELS WITH TEMPORAL DATA (NDVI AND WEATHER) using mclogit package (!Not possible to have random effects with these models, that's why I run the models by years to see if there is a difference)
###NDVI model 
model4g<-mclogit(cbind(Randoms,PtID)~ NDVISc, data = allNDVI_mvt2013)
summary(model4g)

####NDVI:Wetland and Lichen and Forest
model5g<-mclogit(cbind(Randoms,PtID)~NDVISc*WetlandSc + NDVISc*LichenSc + NDVISc*ForestSc + ForestSc + NDVISc + WetlandSc + LichenSc , data=allNDVI_mvt2013)
summary(model5g)

####R squared 
r.squaredGLMM(modelhab1g)
r.squaredGLMM(modelhab2g)
r.squaredGLMM(modelhab3g)
r.squaredGLMM(modelhab4g)
r.squaredGLMM(modelhab5g)

boxplot((NDVISc) ~ factor(Randoms)  , data = allNDVI_mvt2013, notch = TRUE)
boxplot((NDVI) ~ factor(Randoms)  , data = test, notch = TRUE)


####NDVI:Weather ???
model6g<-mclogit(cbind(Randoms,PtID)~ NDVISc*(sweSc + prcpSc + tmaxSc) + NDVISc + sweSc + prcpSc + tmaxSc, data=allNDVI_mvt2013)
summary(model6g)

###Weather:Habitat RSF with interaction between Wetland/Weather and Forest/Weather
### There are not selecting for snow, because lot of swe data were = 0 this year, no snow on the ground everywhere? or real avoidance ?
model7g<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_mvt2013)
summary(model7g)
boxplot((sweSc) ~ factor(Randoms)  , data = allNDVI_mvt2013) ###when I check the graph, not a lot of snow this year, 0 = available and 1 = used

###Weather:Habitat RSF with interaction between Lichen/Weather and Forest/Weather
model8g<-mclogit(cbind(Randoms,PtID)~ ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + tmaxSc + prcpSc + sweSc, data = allNDVI_mvt2013)
summary(model8g)

#####Weather selection ??
model9g<-mclogit(cbind(Randoms,PtID)~ tmaxSc + prcpSc + sweSc, data=allNDVI_mvt2013)
summary(model9g)

####Global models RSF stopover#####
##Global model with interaction Habitat(Forest and Wetland)/Weather
globalmodel1_mvtg<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + WetlandSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2013)
summary(globalmodel1_mvtg)

###Global model with interaction Habitat(Forest and Lichen)/Weather
globalmodel2_mvtg<-mclogit(cbind(Randoms,PtID)~ForestSc*(prcpSc + sweSc + tmaxSc) + LichenSc * (prcpSc+ sweSc + tmaxSc)+ ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2013)
summary(globalmodel2_mvtg)

###Global model with interaction Weather/NDVI, really big selection for NDVI
#globalmodel3_stop<-mclogit(cbind(Randoms,PtID)~NDVISc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + #LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_stop2010)
#summary(globalmodel3_stop)

###Global model 4
#globalmodel4_mvte<-mclogit(cbind(Randoms,PtID)~ WetlandSc*(prcpSc + sweSc + tmaxSc) + LichenSc*(prcpSc + sweSc + tmaxSc) + ForestSc + WetlandSc + LichenSc + RockySc + WaterSc + prcpSc + sweSc + tmaxSc + NDVISc, data = allNDVI_mvt2012)
#summary(globalmodel4_mvte)

##Calculate AIC and delta
aics<-AIC(modelhab1g,modelhab2g,modelhab3g,model4g,model5g,model6g,model7g,model8g,model9g,globalmodel1_mvtg,globalmodel2_mvtg)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

####save best model 
stargazer(globalmodel2_mvtg, type = "html", title = "RSF model mvt 2013",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Lichen","Wetland","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Lichen:Precipitation","Lichen:SWE","Lichen:Temperature"),
          align= TRUE,
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global model_mvt2013Lichen.htm")

stargazer(globalmodel1_mvtg, type = "html", title = "RSF model mvt 2013",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Wetland","Lichen","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Wetland:Precipitation","Wetland:SWE","Wetland:Temperature"),
          align= TRUE,
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global model_mvt2013Wetland.htm")
