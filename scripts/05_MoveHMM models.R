#############################################
############ MoveHMM model ##################

##load package
library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)

## see format of variables
glimpse(allNDVI)

## select continuous variable 
continuous <- select_if(allNDVI, is.numeric)
summary(continuous)

##plot density of variables 
ggplot(continuous, aes(x = tmin)) +
  geom_density(alpha = .2, fill = "#FF6666")

# Select categorical column
allNDVI$state<- as.factor(allNDVI$state)
factor <- data.frame(select_if(allNDVI, is.factor))
ncol(factor)

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

anova <- aov(NDVI~state, allNDVI)
summary(anova)

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

##
#a1 <- lme4::lmer(NDVI ~ state + Year + (1|Animal_ID), data = allNDVI[Randoms == 1]) 

###Temperature/JDate
all2011 <- subset(allNDVI, Year == '2011')
ggplot(all2011,aes(JDate,tmax))+stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)



plot(allNDVIobs$tmax, allNDVIobs$HMM)
g=glm(HMM~tmaxSc*ForestSc, family = "binomial", data = allNDVIobs)
curve(predict(g,data.frame(tmaxSc=x),type="resp"),add=TRUE)

interplot(m=g, var1 = "ForestSc", var2 = "tmaxSc")+
  theme(axis.text.x  = element_text(angle=90))
allNDVIobs
ggplot(allNDVIobs, aes(tmax, HMM))+
  stat_smooth(method = 'glm', family = binomial, formula = y~x, alpha = 0.2, size = 2) +
  xlab('Temperature') +
  ylab('Allocare occurrence')

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

allNDVIobs <- subset(allNDVIobs, select = -c(2:5,7, 11:13))

###################  NEGATIVE VALUES= Less likely to be encamped = more likely to be 0
allNDVIobs<-subset(allNDVI, Randoms == 1) ##subset

allNDVIobs$HMM <- ifelse(allNDVIobs$state == 2,0,1)  #### 0 == movement and 1 == encamping !!!!!!!!!!!

##to not re-write always scale
allNDVIobs$prcpSc<-scale(allNDVIobs$prcp)
allNDVIobs$state<-as.factor(allNDVIobs$state)
test<-ggplot(allNDVIobs, aes(state, step)) +geom_violin()+geom_boxplot(width=.1)
test
allNDVIobs$Year<-as.factor(allNDVIobs$Year)
ggplot(allNDVIobs, aes(JDate, tmax, fill = state, colour = state))+geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+facet_wrap(~Year)
#######1st model = NDVI model (PREDICTION 1)
HMMNDVI<- glmer(HMM ~ scale(NDVI) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMMNDVI)  ####NDVI not influence mvt/stop behaviour
boxplot((NDVI) ~ factor(HMM)  , data = allNDVIobs, notch = TRUE)

NDVIYear<-glmer(HMM ~ (scale(NDVI)*Year) +  (1|Animal_ID), data = allNDVIobs, family = "binomial")
summary(NDVIYear)

NDVIYearRdm<-glmer(HMM ~ scale(NDVI) +  (1|Animal_ID) + (NDVI|Year), data = allNDVIobs, family = "binomial")
summary(NDVIYearRdm)

######2nd model = Weather (PREDICTION 2)
HMMWeather<- glmer(HMM ~ scale(prcp) + scale(swe)+ scale(tmax) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMMWeather) ######## more likely to move when it's warmer

WeatherYear<-glmer(HMM ~ (scale(swe)*Year) + (scale(tmax)*Year) + (scale(prcp)*Year) + (1|Animal_ID), data = allNDVIobs, family = "binomial")
summary(WeatherYear)

#####3rd model = Habitat (PREDICTION 3)
HMMHabitat <- glmer(HMM ~ scale(Wetland) + scale(Lichen) + scale(Rocky) + scale(Forest) + scale(Water) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMMHabitat)

HabitatYear <- glmer(HMM ~ (scale(Wetland)*Year) + scale(Lichen) + scale(Rocky) + (scale(Forest)*Year) + scale(Water) + (1|Animal_ID), data = allNDVIobs, family = "binomial")
summary(HabitatYear)
######4th model = Weather:Forest (PREDICTION 3)
HMMWeatherForest<- glmer(HMM ~ (scale(Forest) * (scale(prcp) + scale(tmax) + scale(swe))) + scale(Wetland) + scale(Lichen) + scale(Rocky) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMMWeatherForest)

######5th model = Weather:Wetland (PREDICTION 3)
HMMWeatherWetland<- glmer(HMM ~ (scale(Wetland) * (scale(prcp) + scale(tmax) + scale(swe))) + scale(Forest) + scale(Wetland) + scale(Lichen) + scale(Rocky) + scale(Water) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMMWeatherWetland)

HMMWeatherLichen<- glmer(HMM ~ (scale(Lichen) * (scale(prcp) + scale(tmax) + scale(swe))) + scale(Forest) + scale(Lichen) + scale(Wetland) + scale(Rocky) + scale(Water) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMMWeatherLichen)

###rain/snow
HMMSnow<- glmer(HMM ~ scale(swe) * scale(prcp) + scale(tmax) + scale(swe) + scale(prcp) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMMSnow)
boxplot((tmax) ~ factor(HMM)  , data = allNDVIobs, notch = TRUE)

######6th model = Habitat:NDVI (PREDICTION 3)
HMMHab_NDVI <- glmer (HMM ~ scale(Wetland) * scale(NDVI) + scale(Forest) * scale(NDVI) + scale(Lichen) * scale(NDVI) + scale(NDVI) + scale(Wetland) +
                        scale(Forest) + scale(Lichen) + scale(Rocky) + scale(Water) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMMHab_NDVI)

HMM_Weather_NDVI <- glmer (HMM ~ scale(prcp) * scale(NDVI) + scale(tmax) * scale(NDVI) + scale(swe) * scale(NDVI)+
                             scale(prcp) + scale(tmax) + scale(swe) + scale(NDVI) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMM_Weather_NDVI)
####7th model = Full model 
HMMall <- glmer (HMM ~ scale(NDVI) + scale(prcp) + scale(swe) + scale(tmax) + scale(Lichen) + scale(Wetland)
                 + scale(Forest) + scale(Rocky) + scale(Water) + scale(prcp)*scale(swe) + (scale(Wetland)* (scale(tmax) + scale(swe) + scale(prcp)))
                 + (scale(Forest)*(scale(prcp)+scale(tmax)+scale(swe))) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMMall)

HMMall2 <- glmer (HMM ~ scale(NDVI) + scale(prcp) + scale(swe) + scale(tmax) + scale(Lichen) + scale(Wetland)
                  + scale(Forest) + scale(Rocky) + scale(Water) + scale(prcp)*scale(swe) + (scale(Lichen)* (scale(tmax) + scale(swe) + scale(prcp)))
                  + (scale(Forest)*(scale(prcp)+scale(tmax)+scale(swe))) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMMall2)
###Weather and NDVI 
HMMWeather_NDVI <- glmer (HMM ~ scale(NDVI) + scale(prcp) + scale(tmax) + scale(swe) + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
summary(HMMWeather_NDVI)

##Calculate AIC and delta
aics<-AIC(HMMWeather, HMM_Weather_NDVI,HMMHabitat, HMMall, HMMHab_NDVI, HMMSnow, HMMWeatherWetland, HMMWeatherLichen,HMMWeatherForest)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

r.squaredGLMM(HMMall2)
###Calculate interval of confidence
carsprfixef<-fixef(HMMall2)##extract betas
carsprSEs<-c(sqrt(diag(vcov(HMMall2)))) ## Extract SEs
## Make lower and upper 95% CIs
carsprLCI<-carsprfixef-(carsprSEs*1.96)
carsprUCI<-carsprfixef+(carsprSEs*1.96)
## Round the values
carsprfixefR<-round(carsprfixef,3)
## Make the CIs
carsprCI<-paste(carsprfixefR, " [",round(carsprLCI,3), ", ",round(carsprUCI,3),"]",sep="")

stargazer(HMMall, type = "html",
          column.labels = c("Model"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          header = FALSE,
          out = "BestMoveHMMmodel.htm")

names<-names(carsprfixef)
carsprCInamed<-data.frame(names,carsprCI)

ggplot(allNDVIobs,aes(x=tmax,y=HMM)) +
  geom_point() +
  geom_line(data=carsprCI) +
  geom_ribbon(data=carsprCI,aes(ymin=LCI,ymax=UCI),alpha=0.2)

plot_model(HMMall2,sort.est=TRUE, transform = NULL)
plot_model(HMMall2, type = "std", grid = FALSE, sort.est = TRUE, show.values = TRUE, value.offset = .3)
plot_model(HMMall2, type = "std", grid = FALSE, sort.est = TRUE)
plot_model(HMMall2,type = "re")
plot_model(HMMall2,type = "slope")
plot_model(RSFWeather_stop2, type = "re")
plot_model(HMMall2,type = "resid")
plot_model(HMMall2,type = "diag")
ggpredict(HMMall2, "ForestSc",ci.lvl = 0.95)

get_model_data(HMMall2, type ="diag")
dat <- ggpredict(HMMall2, terms = c("ForestSc","prcpSc"))
plot(dat)

ggplot(beta_coefficient, aes(Variables, b)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), 
                colour="black", width = 0,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  ylim(-2,2) +
  xlab("") +
  ylab("Beta coefficient (+/- 95% CI)") +
  coord_flip()+
  theme(legend.position = c(0.85,0.85),
        legend.title = element_blank(),
        legend.background = element_rect(color = "black"),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size=18),
        axis.text = element_text(size=12),
        strip.text  = element_text(size = 16),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))



  

library(Rmisc)
library(jtools)
library(sjPlot)
summ(HMMall)
plot_summs(HMMall)
plot(allEffects(HMMall))
r.squaredGLMM(HMMall)
mod3.prob <- update(HMMall, family = binomial(link = "probit"))

effect_plot(HMMall, pred = Lichen, interval = FALSE, plot.points = FALSE)
?effect_plot


library(car)
library(stargazer)
library(effects)
library(ggplot2)

stargazer(HMMWeather,HMMHabitat, type = "html",
          column.labels = c("Weather","Habitat"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          header = FALSE,
          out = "Weather_Hab.htm")
testcorr = subset(allNDVI, select = c(19:21, 28:33, 36:43))
DT<-subset(allNDVIobs, select=c(28,30:31))
stargazer(DT, type = "html",
          column.labels = c("prcp","swe","tmax"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          header = FALSE,
          out = "Weather_summary.htm")

stargazer(allNDVIobs, type = "text", title="Descriptive statistics", digits=1, out="allNDVIobs.txt", flip=TRUE)


###

dotplot(ranef(HMMWeatherForest,condVar=TRUE))
sjp.glmer(HMMall, type = "fe.cor")


plot(state  ~ tmax, 
     data = allNDVIobs,
     xlab="Continuous", 
     ylab="Factor", 
     pch=19)   
