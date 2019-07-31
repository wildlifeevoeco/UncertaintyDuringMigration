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
allNDVI_stop<- subset(allNDVI, HMM == 1)
allNDVI_mvt <- subset(allNDVI, HMM == 0)


###Calculate Se within variables (here NDVI)
NDVIrsf <- summarySEwithin(allNDVI_stop, measurevar = "NDVI", idvar = "Animal_ID",
                        withinvars = "Randoms", "Year", na.rm = FALSE, conf.interval = .95)

##Plot SE of NDVI
png("Graphics/NDVIvariance.png", units = 'px', height=360, width=600)
NDVIrsfyear<-ggplot(NDVIrsf, aes(x=Year, y=NDVI, colour=Randoms, group = Randoms)) +
  geom_line() +
  geom_errorbar(width=0.2, aes(ymin=NDVI-sd, ymax=NDVI+sd))+
  geom_point(shape=20, size=3, fill="white") +
  ylim(-0.25,0.5)+
  theme_bw()
dev.off()

###Calculate mean and se within variables (here temperature) 
tmaxrsf <- summarySEwithin(allNDVI_stop, measurevar = "tmax", idvar = "Animal_ID",
                           withinvars = "Randoms", "Year", na.rm = FALSE, conf.interval = .95)
####Plot SE variation of temp between years
tmaxrsfyear<-ggplot(tmaxrsf, aes(x=Year, y=tmax, colour=Randoms, group = Randoms)) +
  geom_line() +
  geom_errorbar(width=0.2, aes(ymin=tmax-sd, ymax=tmax+sd)) +
  geom_point(shape=20, size=3, fill="white") +
  theme_bw()

###Calculate Se within variables (here swe)
swersf <- summarySEwithin(allNDVI_stop, measurevar = "swe", idvar = "Animal_ID",
                           withinvars = "Randoms", "Year", na.rm = FALSE, conf.interval = .95)
##Plot SE of snow water equivalent SWE
swersfyear<-ggplot(swersf, aes(x=Year, y=swe, colour=Randoms, group = Randoms)) +
  geom_line() +
  geom_errorbar(width=0.2, aes(ymin=swe-sd, ymax=swe+sd))+
  geom_point(shape=20, size=3, fill="white") +
  ylim(100,220)+
  theme_bw()

###############################################
#######RSF MODEL WITH ENCAMPED#################
###Habitat model RSF without year and indiv
modelhab1 <- glm(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + scale(Water) + scale(Rocky) , data = allNDVI_stop, family = "binomial")

summ(modelhab1)

car::vif(modelhab1)

#####Habitat model RSF with indiv and year as a random effects
modelhab2<-glmer(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + scale(Water) + scale(Rocky) + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
summ(modelhab2)

boxplot((Lichen) ~ factor(Randoms)  , data = allNDVI_stop, notch = TRUE)
boxplot((NDVI) ~ factor(Randoms)  , data = allNDVI_stop, notch = TRUE)


ggplot(allNDVI_stop, aes(x = Randoms, y = NDVI)) +
  geom_boxplot()+
  theme_classic()

###Let's try habitat model with Year as fixed effect
#PROBLEM


###excluding water 
modelhab3<-glmer(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + scale(Rocky) + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
summ(modelhab3)

###Foraging habitat (Lichen and Wetland)
modelhab4 <- glmer (Randoms ~ scale(Wetland) + scale(Lichen) + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
summ(modelhab4)

####R squared 
r.squaredGLMM(modelhab1)
r.squaredGLMM(modelhab2)
r.squaredGLMM(modelhab3)
r.squaredGLMM(modelhab4)

#######MODELS WITH TEMPORAL DATA (NDVI AND WEATHER) using mclogit package (!Not possible to have random effects with these models, what is the difference ? How it's influence my data/results ?)
###NDVI model 
RSFmodelNDVI_stop<-mclogit(cbind(Randoms,PtID)~scale(NDVI), data = allNDVI_stop)
summary(RSFmodelNDVI_stop)

boxplot((NDVI) ~ factor(Randoms)  , data = allNDVI_stop, notch = TRUE)

####NDVI:Habitat
#RSFmodelNDVIhab_stop<-mclogit(cbind(Randoms,PtID)~scale(NDVI)*scale(Forest) + scale(NDVI)*scale(Wetland) + scale(NDVI) + 
#scale(Wetland) + scale(Lichen) + scale(Rocky) + scale(Water), data=allNDVI_stop)
#summary(RSFmodelNDVIhab_stop)

###NDVI:Habitat 
#RSFmodelNDVIhab_stop2<-mclogit(cbind(Randoms,PtID)~scale(NDVI)*scale(Forest) + scale(NDVI)*scale(Lichen) + scale(NDVI) + scale(Wetland) + scale(Lichen) + scale(Rocky) + scale(Water), data=allNDVI_stop)
#summary(RSFmodelNDVIhab_stop2)

####NDVI:Weather ???
RSFmodelNDVIWeather_stop<-mclogit(cbind(Randoms,PtID)~scale(NDVI)*(scale(swe) + scale(prcp) + scale(tmax)) + scale(NDVI) + scale(swe) + scale(prcp) + scale(tmax), data=allNDVI_stop)
summary(RSFmodelNDVIWeather_stop)

###Weather:Habitat RSF with interaction between Wetland/Weather and Forest/Weather
RSFWeatherHab_stop<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                      (scale(Wetland)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + 
                      scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe), data = allNDVI_stop)

summary(RSFWeatherHab_stop)

RSFWeatherHab_stop2<-mclogit(cbind(Randoms,PtID)~ scale(Forest) + scale(Wetland) + 
                              scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe) + scale(Lichen)*scale(prcp)*scale(swe), data = allNDVI_stop)

summary(RSFWeatherHab_stop2)
###Weather:Habitat RSF with interaction between Lichen/Weather and Forest/Weather
RSFWeatherHab_stop2<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                      (scale(Lichen)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + 
                      scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe), data = allNDVI_stop)


summary(RSFWeatherHab_stop2)

#####Weather selection ??
RSFWeather_stop<-mclogit(cbind(Randoms,PtID)~scale(tmax) + scale(prcp) + scale(swe), data=allNDVI_stop)
summary(RSFWeather_stop)

####Global models RSF stopover#####
##Global model with interaction Habitat(Forest and Wetland)/Weather
globalmodel1_stop<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                               (scale(Wetland)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + 
                               scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe) + scale(NDVI), data = allNDVI_stop)
summary(globalmodel1_stop)

###Global model with interaction Habitat(Forest and Lichen)/Weather
globalmodel2_stop<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                             (scale(Lichen)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe) + scale(NDVI), data = allNDVI_stop)
summary(globalmodel2_stop)


###Global model with interaction Weather/NDVI
globalmodel3_stop<-mclogit(cbind(Randoms,PtID)~(scale(NDVI)*(scale(prcp) + scale(swe) + scale(tmax))) + + scale(Forest) + scale(Wetland) + scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe) + scale(NDVI), data = allNDVI_stop)
summary(globalmodel3_stop)

##Calculate AIC and delta
aics<-AIC(modelhab1,modelhab2,modelhab3,modelhab4,RSFmodelNDVI_stop,RSFWeather_stop,RSFWeatherHab_stop,RSFWeatherHab_stop2, globalmodel1_stop, globalmodel2_stop, globalmodel3_stop)
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
          out = "RSF global model_stop.htm")

###############################################
#######RSF MODEL WITH MOVEMENT#################
###Habitat model RSF without year and indiv
modelhab1_mvt <- glm(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + scale(Water) + scale(Rocky) , data = allNDVI_mvt, family = "binomial")

summ(modelhab1_mvt)

car::vif(modelhab1)

#####Habitat model RSF with indiv and year as a random effects
modelhab2_mvt<-glmer(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + scale(Water) + scale(Rocky) + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
summ(modelhab2_mvt)

boxplot((Lichen) ~ factor(Randoms)  , data = allNDVI_stop, notch = TRUE)

###Let's try habitat model with Year as fixed effect
#PROBLEM


###excluding water 
modelhab3_mvt<-glmer(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + scale(Rocky) + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
summ(modelhab3_mvt)

###Foraging habitat (Lichen and Wetland)
modelhab4_mvt <- glmer (Randoms ~ scale(Wetland) + scale(Lichen) + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
summ(modelhab4_mvt)

####R squared 
r.squaredGLMM(modelhab1_mvt)
r.squaredGLMM(modelhab2_mvt)
r.squaredGLMM(modelhab3_mvt)
r.squaredGLMM(modelhab4_mvt)

#######MODELS WITH TEMPORAL DATA (NDVI AND WEATHER) using mclogit package (!Not possible to have random effects with these models, what is the difference ? How it's influence my data/results ?)
###NDVI model 
model5_mvt<-mclogit(cbind(Randoms,PtID)~scale(NDVI), data = allNDVI_mvt)
summary(model5_mvt)

boxplot((NDVI) ~ factor(Randoms)  , data = allNDVI_mvt, notch = TRUE)

####NDVI:Habitat
#RSFmodelNDVIhab_stop<-mclogit(cbind(Randoms,PtID)~scale(NDVI)*scale(Forest) + scale(NDVI)*scale(Wetland) + scale(NDVI) + 
#scale(Wetland) + scale(Lichen) + scale(Rocky) + scale(Water), data=allNDVI_stop)
#summary(RSFmodelNDVIhab_stop)

###NDVI:Habitat 
#RSFmodelNDVIhab_stop2<-mclogit(cbind(Randoms,PtID)~scale(NDVI)*scale(Forest) + scale(NDVI)*scale(Lichen) + scale(NDVI) + scale(Wetland) + scale(Lichen) + scale(Rocky) + scale(Water), data=allNDVI_stop)
#summary(RSFmodelNDVIhab_stop2)

####NDVI:Weather ???
Model6_mvt<-mclogit(cbind(Randoms,PtID)~scale(NDVI)*(scale(swe) + scale(prcp) + scale(tmax)) + scale(NDVI) + scale(swe) + scale(prcp) + scale(tmax), data=allNDVI_mvt)
summary(Model6_mvt)

###Weather:Habitat RSF with interaction between Wetland/Weather and Forest/Weather
Model7_mvt<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                              (scale(Wetland)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + 
                              scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe), data = allNDVI_mvt)

summary(Model7_mvt)

###Weather:Habitat RSF with interaction between Lichen/Weather and Forest/Weather
Model8_mvt<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                               (scale(Lichen)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + 
                               scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe), data = allNDVI_mvt)

summary(Model8_mvt)

#####Weather selection ??
Model9_mvt<-mclogit(cbind(Randoms,PtID)~scale(tmax) + scale(prcp) + scale(swe), data=allNDVI_mvt)
summary(Model9_mvt)

####Global models RSF stopover#####
##Global model with interaction Habitat(Forest and Wetland)/Weather
globalmodel1_mvt<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                             (scale(Wetland)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + 
                             scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe) + scale(NDVI), data = allNDVI_mvt)
summary(globalmodel1_mvt)

###Global model with interaction Habitat(Forest and Lichen)/Weather
globalmodel2_mvt<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                             (scale(Lichen)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe) + scale(NDVI), data = allNDVI_mvt)
summary(globalmodel2_mvt)


###Global model with interaction Weather/NDVI
globalmodel3_mvt<-mclogit(cbind(Randoms,PtID)~(scale(NDVI)*(scale(prcp) + scale(swe) + scale(tmax))) + + scale(Forest) + scale(Wetland) + scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe) + scale(NDVI), data = allNDVI_mvt)
summary(globalmodel3_mvt)

##Calculate AIC and delta
aics<-AIC(modelhab1_mvt,modelhab2_mvt,modelhab3_mvt,modelhab4_mvt,model5_mvt,Model6_mvt,Model7_mvt,Model8_mvt,Model9_mvt,globalmodel1_mvt,globalmodel2_mvt,globalmodel3_mvt)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

####save best model 
stargazer(globalmodel2_mvt, type = "html", title = "RSF model movement",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Lichen","Wetland","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Lichen:Precipitation","Lichen:SWE","Lichen:Temperature"),
          align= TRUE,
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global model_mvt.htm")


####save tab with two best models mvt and stopover
stargazer(globalmodel2_stop,globalmodel2_mvt, type = "html",title = "RSF models",
          covariate.labels=c("Forest","Precipitation","SWE","Temperature","Lichen","Wetland","Rocky","Water","NDVI","Forest:Precipitation","Forest:SWE","Forest:Temperature","Lichen:Precipitation","Lichen:SWE","Lichen:Temperature"),
          align = TRUE, 
          intercept.bottom =  FALSE,
          ci = TRUE, ci.level = 0.90,
          omit.stat = c("n"),
          single.row = TRUE,
          out = "RSF global mvt & stop.htm")



