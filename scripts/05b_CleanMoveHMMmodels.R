#############################################
############ MoveHMM model ##################

##load package
library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(stargazer)

## see format of variables
glimpse(allNDVI)

##plot density of variables 
ggplot(continuous, aes(x = tmin)) +
  geom_density(alpha = .2, fill = "#FF6666")

###plot categorical variables
ggplot(allNDVI, aes(x = state, y = swe)) +
  geom_boxplot(aes(group = state)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

ggplot(allNDVI, aes(x = NDVI)) +
  geom_density(aes(color = state), alpha = 0.5) +
  theme_classic()

#####correlation between variables
library(GGally)
testcorr = subset(allNDVI, select = c(19:21, 28:33, 36:43))
# Convert data to numeric
corr <- data.frame(lapply(testcorr, as.integer))
# Plot the graph
ggcorr(corr,method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50")

cor.test(allNDVI$prcp, allNDVI$swe, method=c("spearman"))
cor.test(allNDVI$NDVI, allNDVI$tmax, method=c("pearson")) ##0.66
cor.test(allNDVI$NDVI, allNDVI$prcp, method=c("spearman"))
cor.test(allNDVI$NDVI, allNDVI$Forest, method=c("spearman"))
cor.test(allNDVI$NDVI, allNDVI$Wetland, method=c("spearman"))
cor.test(allNDVI$NDVI, allNDVI$Lichen, method=c("spearman"))
cor.test(allNDVI$NDVI, allNDVI$Forest, method=c("spearman"))
cor.test(allNDVI$Wetland, allNDVI$Forest, method=c("spearman"))
cor.test(allNDVI$Lichen, allNDVI$Forest, method=c("spearman"))
cor.test(allNDVI$Water, allNDVI$Forest, method=c("spearman"))
cor.test(allNDVI$Water, allNDVI$Wetland, method=c("spearman"))


library(popbio)
logi.hist.plot(allNDVIobs$tmax,allNDVIobs$HMM,boxp=FALSE,type="hist",col="gray")
logi.hist.plot(allNDVIobs$prcp,allNDVIobs$HMM,boxp=FALSE,type="hist",col="gray")
logi.hist.plot(allNDVIobs$swe,allNDVIobs$HMM,boxp=FALSE,type="hist",col="gray")
logi.hist.plot(allNDVIobs$NDVI,allNDVIobs$HMM,boxp=FALSE,type="hist",col="gray")

##############MOVEHMM MODEL ANALYSIS##################
#####################################################

###################  NEGATIVE VALUES= Less likely to be encamped = more likely to be 0
allNDVIobs<-subset(allNDVI, Randoms == 1) ##subset

allNDVIobs$HMM <- ifelse(allNDVIobs$state == 2,0,1)  #### 0 == movement and 1 == encamping !!!!!!!!!!!

##to not re-write always scale
allNDVIobs$prcpSc<-scale(allNDVIobs$prcp)
allNDVIobs$tmaxSc<-scale(allNDVIobs$tmax)
allNDVIobs$prcpSc<-scale(allNDVIobs$prcp)
allNDVIobs$sweSc<-scale(allNDVIobs$swe)
allNDVIobs$ForestSc<-scale(allNDVIobs$Forest)
allNDVIobs$LichenSc<-scale(allNDVIobs$Lichen)
allNDVIobs$WetlandSc<-scale(allNDVIobs$Wetland)
allNDVIobs$RockySc<-scale(allNDVIobs$Rocky)
allNDVIobs$WaterSc<-scale(allNDVIobs$Water)
allNDVIobs$NDVISc<-scale(allNDVIobs$NDVI)

allNDVIobs <- subset(allNDVIobs, select = -c(2:5,7, 12:18, 22:23, 27, 29, 33:34))

######Landcover candidates models####
nohab <- glmer(HMM ~  (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial") 
hab1<- glmer(HMM ~ LichenSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab2<- glmer(HMM ~ WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab3<- glmer(HMM ~ RockySc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab4<- glmer(HMM ~ ForestSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab5<- glmer(HMM ~ WaterSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")


hab1_bis<- glm(HMM ~ LichenSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")

summary(nohab)
summary(hab1)
summary(hab2)
summary(hab3)
summary(hab4)
summary(hab5)

stargazer(hab1, hab2, hab3, hab4, hab5, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "Candidatehab.htm")

hab6<- glmer(HMM ~ LichenSc + WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab7<- glmer(HMM ~ LichenSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab8<- glmer(HMM ~ LichenSc + ForestSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab9<- glmer(HMM ~ LichenSc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")

summary(hab6)
summary(hab7)
summary(hab8)
summary(hab9)

hab10<- glmer(HMM ~ LichenSc + WetlandSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab11<- glmer(HMM ~ LichenSc + WetlandSc + ForestSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab12<- glmer(HMM ~ LichenSc + WetlandSc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab13<- glmer(HMM ~ LichenSc + ForestSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab14<- glmer(HMM ~ LichenSc + ForestSc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
hab15<- glmer(HMM ~ WetlandSc + ForestSc + RockySc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")

summary(hab10)
summary(hab11)
summary(hab12)
summary(hab13)
summary(hab14)
summary(hab15)

globalhab <- glmer(HMM ~ LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")

##Calculate AIC and delta
aics<-AIC(nohab,hab1,hab2,hab3,hab4,hab5,hab6,hab7,hab8,hab9,hab10,hab11,hab12,hab13,hab14,hab15,globalhab)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics

#### % of information explained
r.squaredGLMM(globalhab)

#####5 tops models landcover
stargazer(globalhab, hab14, hab11, hab12, hab8, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "5Bestmodelshab.htm")

####boxplot covariates by states
boxplot((LichenSc) ~ factor(HMM)  , data = allNDVIobs, notch = TRUE)
boxplot((WetlandSc) ~ factor(HMM)  , data = allNDVIobs, notch = TRUE)
boxplot((ForestSc) ~ factor(HMM)  , data = allNDVIobs, notch = TRUE)
boxplot((RockySc) ~ factor(HMM)  , data = allNDVIobs, notch = FALSE)
boxplot((WaterSc) ~ factor(HMM)  , data = allNDVIobs, notch = FALSE)
boxplot((sweSc) ~ factor(HMM)  , data = allNDVIobs, notch = FALSE)

####Weather candidates models
weather1 <- glmer(HMM ~ prcpSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
weather2 <- glmer(HMM ~ tmaxSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(weather1)
summary(weather2)

stargazer(weather2, globalweather,weather1, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "Bestmodelweather.htm")

globalweather <- glmer(HMM ~ prcpSc + tmaxSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(globalweather)

##Calculate AIC and delta
aics<-AIC(weather1,weather2,globalweather)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics

stargazer(globalweather, weather2, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "Candidateweather.htm")


###Environment candidates models
env1 <- glmer(HMM ~ NDVISc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
env2 <- glmer(HMM ~ sweSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
globalenv <- glmer(HMM ~ NDVISc + sweSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(env1)
summary(env2)
summary(globalenv)

##Calculate AIC and delta
aics<-AIC(env1,env2,globalenv)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics

stargazer(env1, env2, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "Candidateenv.htm")

stargazer(env2, env1, globalenv, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "Bestmodelenv.htm")

###Landcover + env (test with 1st and 2nd best model env)
model1 <- glmer(HMM ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model2 <- glmer(HMM ~ sweSc +  LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model3 <- glmer(HMM ~ NDVISc +  LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")

summary(model1)
summary(model2)
summary(model3)

aics<-AIC(model1,model2,model3)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics ##best model is the one with all variables

modelNDVI1 <- glmer(HMM ~ NDVISc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:NDVISc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
modelNDVI2 <- glmer(HMM ~ NDVISc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + WetlandSc:NDVISc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
modelNDVI3 <- glmer(HMM ~ NDVISc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + ForestSc:NDVISc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
modelNDVI4 <- glmer(HMM ~ NDVISc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + RockySc:NDVISc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
modelNDVI5 <- glmer(HMM ~ NDVISc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:sweSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
modelNDVI6 <- glmer(HMM ~ NDVISc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + WetlandSc:sweSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
modelNDVI7 <- glmer(HMM ~ NDVISc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + ForestSc:sweSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
modelNDVI8 <- glmer(HMM ~ NDVISc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + RockySc:sweSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(modelNDVI1)
summary(modelNDVI2)
summary(modelNDVI3)
summary(modelNDVI4)
summary(modelNDVI5)
summary(modelNDVI6)
summary(modelNDVI7)
summary(modelNDVI8)


modelNDVI9 <- glmer(HMM ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:NDVISc + WetlandSc:NDVISc  +RockySc:NDVISc + WetlandSc:sweSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
modelNDVI10 <- glmer(HMM ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:NDVISc + ForestSc:NDVISc + RockySc:NDVISc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
modelNDVI11<- glmer(HMM ~ NDVISc  + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc +  LichenSc:NDVISc + WetlandSc:NDVISc + RockySc:NDVISc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
modelNDVI12 <- glmer(HMM ~ NDVISc + sweSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:NDVISc +RockySc:NDVISc + WetlandSc:sweSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")

summary(modelNDVI9) ##
summary(modelNDVI10) ## 
summary(modelNDVI11) ##
summary(modelNDVI12)


aics<-AIC(model1,model2,model3,modelNDVI1,modelNDVI2,modelNDVI3,modelNDVI4,modelNDVI5,modelNDVI6,modelNDVI7,modelNDVI8,modelNDVI9,
          modelNDVI10,modelNDVI11,modelNDVI12)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)


stargazer(modelNDVI12, modelNDVI9, modelNDVI11, modelNDVI10, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "Landcover+Env.htm")

######Landcover + Weather
model6 <- glmer(HMM ~ prcpSc + tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model7 <- glmer(HMM ~ tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(model6)
summary(model7)


model8 <- glmer(HMM ~ tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:ForestSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model9 <- glmer(HMM ~ tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:LichenSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model10 <- glmer(HMM ~ tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model11 <- glmer(HMM ~ tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:RockySc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model12 <- glmer(HMM ~ tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:WaterSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model13 <- glmer(HMM ~ tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:ForestSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model14 <- glmer(HMM ~ tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:LichenSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model15 <- glmer(HMM ~ tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model16 <- glmer(HMM ~ tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:RockySc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(model8)
summary(model9)
summary(model10)
summary(model11)
summary(model12)
summary(model13)
summary(model14)
summary(model15)
summary(model16)


model17 <- glmer(HMM ~ tmaxSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:RockySc + tmaxSc:LichenSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model18 <- glmer(HMM ~ tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + prcpSc:ForestSc + prcpSc:WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model19 <- glmer(HMM ~ tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:LichenSc + tmaxSc:RockySc + prcpSc:ForestSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model20 <- glmer(HMM ~ tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:LichenSc + tmaxSc:RockySc + prcpSc:WetlandSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model21 <- glmer(HMM ~ tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + tmaxSc:LichenSc + tmaxSc:RockySc + prcpSc:WetlandSc + prcpSc:ForestSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")


summary(model17)
summary(model18)
summary(model19)
summary(model20)
summary(model21)

aics<-AIC(model6, model7,model8,model9,model10,model11,model12,model13,model14,model15,model16,model17,model18,model19,model20,model21)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

stargazer(model21,model20, model19, model18, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "Landcover+Weather.htm")


###Weather + Env 
model22 <- glmer(HMM ~ NDVISc + sweSc + tmaxSc + prcpSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model23 <- glmer(HMM ~ NDVISc + sweSc + tmaxSc + prcpSc + NDVISc:tmaxSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
model24 <- glmer(HMM ~ NDVISc + sweSc + tmaxSc + prcpSc + prcpSc:sweSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")

summary(model22)
summary(model23)
summary(model24)

model25 <- glmer(HMM ~ NDVISc + sweSc + tmaxSc + prcpSc + NDVISc:tmaxSc + prcpSc:sweSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(model25)

###test models
aics<-AIC(model22,model23,model24,model25)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)


stargazer(model25,model24,model23, model22, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "Env+Weather.htm")

######3 models together 
model26 <- glmer(HMM ~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc + WetlandSc + 
                   ForestSc + RockySc + WaterSc + LichenSc*NDVISc + WetlandSc*NDVISc +
                   RockySc*NDVISc + NDVISc*tmaxSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")

model27 <- glmer(HMM ~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc + WetlandSc + 
                   ForestSc + RockySc + WaterSc + LichenSc*tmaxSc + RockySc*tmaxSc + WetlandSc*prcpSc + ForestSc*prcpSc + NDVISc*tmaxSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")

model28 <- glmer(HMM ~ NDVISc + tmaxSc + prcpSc + LichenSc + WetlandSc + 
                   ForestSc + RockySc + WaterSc + LichenSc*NDVISc + WetlandSc*NDVISc + RockySc*NDVISc +
                   LichenSc*tmaxSc + RockySc*tmaxSc + WetlandSc*prcpSc + ForestSc*prcpSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")

model29 <- glmer(HMM ~ NDVISc  + tmaxSc + prcpSc + LichenSc + WetlandSc + 
                   ForestSc + RockySc + WaterSc + LichenSc*NDVISc + WetlandSc*NDVISc + RockySc*NDVISc
                 + LichenSc*tmaxSc + RockySc*tmaxSc + WetlandSc*prcpSc + ForestSc*prcpSc + NDVISc*tmaxSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(model26)
summary(model27)
summary(model28)
summary(model29)

###test models
aics<-AIC(model26,model27,model28,model29)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
  Weights(aics)


aics<-AIC(model1,model2,model3,modelNDVI2,modelNDVI3,modelNDVI4,modelNDVI5,modelNDVI6,modelNDVI7,modelNDVI8,modelNDVI9,
          modelNDVI10,modelNDVI11,modelNDVI12,model6, model7,model8,model9,model10,model11,model12,model13,model14,model15,model16,model17,
          model18,model19,model20,model21,model22,model23,model24,model25,model26,model27,model28,model29)
aicMin<-min(aics$AIC)
aics$deltaaic<-aics$AIC-aicMin
aics
Weights(aics)

r.squaredGLMM(model29)

###Calculate interval of confidence
carsprfixef<-fixef(model29)##extract betas
carsprSEs<-c(sqrt(diag(vcov(model29)))) ## Extract SEs
## Make lower and upper 95% CIs
carsprLCI<-carsprfixef-(carsprSEs*1.96)
carsprUCI<-carsprfixef+(carsprSEs*1.96)
## Round the values
carsprfixefR<-round(carsprfixef,3)
## Make the CIs
carsprCI<-paste(carsprfixefR, " [",round(carsprLCI,3), ", ",round(carsprUCI,3),"]",sep="")
names<-names(carsprfixef)
carsprCInamed<-data.frame(names,carsprCI)

stargazer(model29, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = TRUE,
          notes.append = FALSE,
          ci = TRUE, ci.level = 0.95,
          header = FALSE,
          out = "AllmoveHMMmodel.htm")
