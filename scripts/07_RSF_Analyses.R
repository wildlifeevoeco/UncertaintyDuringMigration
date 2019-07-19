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
NDVIrsfyear<-ggplot(NDVIrsf, aes(x=Year, y=NDVI, colour=Randoms, group = Randoms)) +
  geom_line() +
  geom_errorbar(width=0.2, aes(ymin=NDVI-sd, ymax=NDVI+sd))+
  geom_point(shape=20, size=3, fill="white") +
  ylim(-0.25,0.5)+
  theme_bw()

###Calculate mean and se within variables (here temperature) 
tmaxrsf <- summarySEwithin(allNDVI_stop, measurevar = "tmax", idvar = "Animal_ID",
                           withinvars = "Randoms", "Year", na.rm = FALSE, conf.interval = .95)
####Plot SE variation of temp between years
tmaxrsfyear<-ggplot(tmaxrsf, aes(x=Year, y=tmax, colour=Randoms, group = Randoms)) +
  geom_line() +
  geom_errorbar(width=0.2, aes(ymin=tmax-sd, ymax=tmax+sd)) +
  geom_point(shape=20, size=3, fill="white") +
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

###Let's try habitat model with Year as fixed effect
PROBLEM


###excluding water 
modelhab3<-glmer(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + scale(Rocky) + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
summ(modelhab3)

###Foraging habitat
modelhab4 <- glmer (Randoms ~ scale(Wetland) + scale(Lichen) + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
summ(modelhab4)

####R squared 
r.squaredGLMM(modelhab1)
r.squaredGLMM(modelhab2)
r.squaredGLMM(modelhab3)
r.squaredGLMM(modelhab4)


###NDVI model with mclogit
RSFmodelNDVI<-mclogit(cbind(Randoms,PtID)~scale(NDVI), data = allNDVI_stop)
summary(RSFmodelNDVI)
boxplot((NDVI) ~ factor(Randoms)  , data = allNDVI_stop, notch = TRUE)
###Weather:Habitat RSF
RSFWeather<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                      (scale(Wetland)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + 
                      scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe), data = allNDVI_stop)

summary(RSFWeather)

RSFWeather_stop2<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                      (scale(Lichen)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + 
                      scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe), data = allNDVI_stop)

summary(RSFWeather_stop2)

RSFtmax<-mclogit(cbind(Randoms,PtID)~scale(tmax), data=allNDVI_stop)
summary(RSFtmax)

stargazer(RSFWeather,RSFtmax, type = "html",
          column.labels = c("Weather","Temperature"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          header = FALSE,
          out = "Weather_RSF.htm")
#####RSF MODEL WITH MOVEMENT 
###HABITAT model
model2hab<-glmer(Randoms ~ scale(Wetland) +  scale(Forest) + scale(Lichen) + scale(Water) + scale(Rocky) + (1|Animal_ID) + (1|Year), data = allNDVI_mvt, family = "binomial")
summary(model2hab)

###Weather model
RSFWeather_mvt<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                      (scale(Wetland)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + 
                      scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe), data = allNDVI_mvt)

summary(RSFWeather_mvt)

RSFWeather_mvt2<-mclogit(cbind(Randoms,PtID)~(scale(Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + 
                          (scale(Lichen)*(scale(prcp)+scale(swe)+scale(tmax)))+ scale(Forest) + scale(Wetland) + 
                          scale(Lichen) + scale(Rocky) + scale(Water) + scale(tmax) + scale(prcp) + scale(swe), data = allNDVI_mvt)
summary(RSFWeather_mvt2)
AIC(RSFWeather_mvt2)
stargazer(model1hab,model2hab, type = "html",
          column.labels = c("Stop","Movement"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          header = FALSE,
          out = "Habitat_RSF.htm")

stargazer(RSFWeather,RSFWeather_mvt, type = "html",
          column.labels = c("Stop","Movement"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          header = FALSE,
          out = "Weather_RSF_comparaison.htm")

stargazer(RSFWeather_stop2,RSFWeather_mvt2, type = "html",
          column.labels = c("Stop","Movement"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          header = FALSE,
          out = "Weather_RSF_2.htm")
