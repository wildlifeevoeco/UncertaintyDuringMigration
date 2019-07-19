####################################################
####################RSF MODELS######################
####################################################
library(mclogit)
library(lme4)

allNDVI$Randoms<-ifelse(allNDVI$Randoms=="1",1,0) #### 0 == available and 1 == used 
head(allNDVI)
summary(allNDVI)
allNDVI[, StrMean := mean(as.numeric(Randoms)),by = .(PtID)]
summary(allNDVI)
nrow(allNDVI)
###subset by state
allNDVI$HMM <- ifelse(allNDVI$state == 2,0,1)
allNDVI_stop<- subset(allNDVI, HMM == 1)
allNDVI_mvt <- subset(allNDVI, HMM == 0)

NDVIrsf <- summarySEwithin(allNDVI_stop, measurevar = "NDVI", idvar = "Animal_ID",
                        withinvars = "Randoms", "Year", na.rm = FALSE, conf.interval = .95)

NDVIrsfyear<-ggplot(NDVIrsf, aes(x=Year, y=NDVI, colour=Randoms, group = Randoms)) +
  geom_line() +
  geom_errorbar(width=0.2, aes(ymin=NDVI-sd, ymax=NDVI+sd))+
  geom_point(shape=20, size=3, fill="white") +
  ylim(-0.25,0.5)+
  theme_bw()

###summary temperature 
tmaxrsf <- summarySEwithin(allNDVI_stop, measurevar = "tmax", idvar = "Animal_ID",
                           withinvars = "Randoms", "Year", na.rm = FALSE, conf.interval = .95)
####SE variation between years
tmaxrsfyear<-ggplot(tmaxrsf, aes(x=Year, y=tmax, colour=Randoms, group = Randoms)) +
  geom_line() +
  geom_errorbar(width=0.2, aes(ymin=tmax-sd, ymax=tmax+sd)) +
  geom_point(shape=20, size=3, fill="white") +
  theme_bw()


#######RSF MODEL WITH ENCAMPED
###Habitat model RSF
model1hab<-glmer(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + scale(Water) + scale(Rocky) + (1|Animal_ID) + (1|Year), data = allNDVI_stop, family = "binomial")
summary(model1hab)
boxplot((Lichen) ~ factor(Randoms)  , data = allNDVI_stop, notch = TRUE)

summ(model1hab)
###NDVI model
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
