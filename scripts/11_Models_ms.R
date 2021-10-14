#MAJ 12/04
library(lme4)
library(ggplot2)
library(magrittr)
library(ggeffects)
library(sjmisc)
library(lme4)
library(splines)
library(car)
library(sjPlot)
# install.packages("broom")
library(broom)
library(broom.mixed)
library(performance)

libs <- c('data.table', 'dplyr', 'lubridate', 'lme4', 'broom.mixed', 'performance', 'ggeffects',
          'tidyr', 'ggplot2','survival', 'patchwork', 'AICcmodavg', 'ggthemes', 'see')
lapply(libs, require, character.only = TRUE)

##-
str(allNDVIobs$HMM)

# allNDVIobs$HMM <- as.numeric(as.character(allNDVIobs$HMM))
allNDVIobs$HMM <- as.character(as.numeric(allNDVIobs$HMM))

##Distribution plot SL and TA
ggplot(allNDVIobs, aes(x = step)) +
  geom_density(aes(color = HMM), alpha = 0.5, size = 1.2) + xlim(0,2000)+
  theme_classic()
ggplot(allNDVIobs, aes(x = angle)) +
  geom_density(aes(color = HMM), alpha = 0.5, size = 1.2) +
  theme_classic()

summary(allNDVIobs$swe)
sd(allNDVIobs$swe)

hist(allNDVIobs$prcp, breaks = 20, main="Histogram of precipitation")
hist(allNDVIobs$tmax, breaks = 20, main="Histogram of temperature")
hist(allNDVIobs$swe, breaks = 20, main="Histogram of SWE")

dplyr::count(allNDVIobs, swe <= 0)

ggplot(allNDVIobs, aes(x= HMM, y = Lichen)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()

ggplot(allNDVIobs, aes(x= HMM, y = Wetland)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()

summary(allNDVIobs$Forest) #### mean REALLY LOW 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00000 0.00000 0.01127 0.00000 0.79310

#how many % of data with more than 0.2% of forest in the buffer?
count(allNDVIobs, allNDVIobs$Forest > 0.2 ) ### only 428 rows

##- Models with lichen/wetland/forest

str(allNDVIobs$HMM) ## 0 = mvt and 1 = encamped

##M1 Hab----
allNDVIobs$HMM <- as.numeric(as.character(allNDVIobs$HMM))
M1_hab <- glm(HMM ~ Wetland + Lichen + Forest  , data = allNDVIobs, family = binomial)
Anova(M1_hab)
# LR Chisq Df Pr(>Chisq)    
# Wetland    38.15  1  6.541e-10 ***
#   Lichen    377.77  1  < 2.2e-16 ***
#   Forest     14.16  1  0.0001676 **
summary(M1_hab)
tidy(M1_hab)

##RANDOM 
group.indivs <- tidy(M1_hab, effect = 'ran_vals')

#### GRAPHS ----
pred.hab.wet <- ggpredict(M1_hab, terms = c('Wetland'))
plot(pred.hab.wet)

pred.hab.lic <- ggpredict(M1_hab, terms = c('Lichen'))
plot(pred.hab.lic)

pred.hab.for <- ggpredict(M1_hab, terms = c('Forest'))
plot(pred.hab.for)


print(M1_hab, corr = FALSE)
# library(effects)
# plot(allEffects(M1_hab))

##Plot
plot_model(M1_hab, vline.color = "grey")+theme_sjplot2()

sjPlot::tab_model(M1_hab, 
                  show.re.var= TRUE, 
                  dv.labels= "HMM habitat", 
                  file = 'HMM_mod_hab')


plot_model(M1_hab, type = "pred", terms = c("Lichen")) + 
  labs(x = "Lichen", y = "presence") + 
  ggtitle("")

##M1 meteo----
summary(allNDVIobs$prcp)
summary(allNDVIobs$tmax)
summary(allNDVIobs$swe) ## big variation, need to rescale all var

ggplot(allNDVIobs, aes(x= as.character(HMM), y = prcp)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()

ggplot(allNDVIobs, aes(x= as.character(HMM), y = tmax)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()

ggplot(allNDVIobs, aes(x= as.character(HMM), y = swe)) +
  geom_boxplot(alpha = 0.5) +
  theme_classic()

#mod
M1_meteo <- lmer(HMM ~ scale(prcp) + scale(tmax) + scale(swe)  + (1|Animal_ID), data = allNDVIobs)

Anova(M1_meteo)
# Chisq Df Pr(>Chisq)    
# scale(prcp)    2.5757  1     0.1085    
# scale(tmax)   29.4661  1   5.69e-08 ***
#   scale(swe)  2554.3089  1  < 2.2e-16 ***
summary(M1_meteo)
tidy(M1_meteo)


sjPlot::tab_model(M1_meteo, 
                  show.re.var= TRUE, 
                  dv.labels= "HMM meteo")


#### GRAPHS ----
pred.prcp <- ggpredict(M1_meteo, terms = c('prcp'))
plot(pred.prcp)

pred.temp <- ggpredict(M1_meteo, terms = c('tmax'))
plot(pred.temp)

pred.swe <- ggpredict(M1_meteo, terms = c('swe'))
plot(pred.swe)



se <- sqrt(diag(vcov(M1_meteo))) #get estimates
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(M1_meteo), LL = fixef(M1_meteo) - 1.96 * se, UL = fixef(M1_meteo) + 1.96 *
                se))

###get odd ratios
exp(tab)

### no effect prcp
## positive effects of tmax et swe 

##M1 Hab + meteo ----
##without interaction
M1 <- glmer(HMM ~ scale(Wetland) + scale(Lichen) + scale(Forest) + scale(tmax) +
              scale(swe) + scale(prcp) + (1|Animal_ID), data = allNDVIobs, family = "binomial")
summary(M1)
Anova(M1)

##with interaction ####
M1_inter <-  glmer(HMM ~ scale(Wetland) + scale(Lichen) + scale(Forest) + scale(tmax) +
                     scale(swe) + scale(prcp) + scale(Wetland)*scale(tmax) + scale(Wetland)*scale(swe) +
                     scale(Wetland)*scale(prcp) + scale(Lichen)*scale(tmax) + scale(Lichen)*scale(swe) + scale(Lichen)*scale(prcp) +
                     scale(Forest)*scale(tmax) + scale(Forest)*scale(swe) + scale(Forest)*scale(prcp) + (1|Animal_ID), data = allNDVIobs, family = "binomial")
summary(M1_inter)

fixef(M1_inter)

##summary output
sjPlot::tab_model(M1_inter, 
                  show.re.var= TRUE, 
                  dv.labels= "HMM meteo*hab")



# Extract the prediction data frame
pred.mm <- ggpredict(M1_inter, terms = c("Lichen"))
#do this for the all cov

###comparing AIC all models
anova(M1_meteo, M1_inter, M1_hab,test="Chisq")
# npar   AIC   BIC logLik deviance   Chisq Df Pr(>Chisq)    
# M1_hab      4 38151 38185 -19072    38143                          
# M1_meteo    6 33832 33882 -16910    33820 4323.80  2  < 2.2e-16 ***
#   M1_inter   17 33117 33260 -16541    33083  737.08 11  < 2.2e-16 ***

aics<-AIC(M1_hab,M1_meteo,M1_inter)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics
# df      AIC  deltaaic
# M1_hab    4 38151.41 5034.8735
# M1_meteo  6 33866.72  750.1914
# M1_inter 17 33116.53    0.0000


###summary output model####
library(stargazer)
stargazer(M1_inter, type = "html",
          column.labels = c("Weather","Habitat"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          header = FALSE,
          out = "Weather_Hab.html")

##extract coeff
# library(ggeffects)
# cowplot::plot_grid(plotlist = plot(ggeffect(M1_inter)))
# 
# library(gtsummary)
# theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")
# 
# library(sjPlot)
# sjPlot::plot_model(M1_inter)

###plot effects####
ggpredict(M1_inter, c("Lichen", "tmax")) %>% plot()
M1_inter_Lichen_temp <- ggpredict(M1_inter, terms = c("Lichen" ,"tmax") )
###Figure out why temperature is extracted as group of 3 values!

ggplot(M1_inter_Lichen_temp, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)
ggpredict(M1_inter, c("Lichen", "prcp")) %>% plot()
ggpredict(M1_inter, c("Lichen", "prcp")) %>% plot()
ggpredict(M1_inter, c("Lichen", "swe")) %>% plot()
ggpredict(M1_inter, c("Forest", "swe")) %>% plot()
ggpredict(M1_inter, c("Wetland", "swe")) %>% plot()

##plot model results####

sjPlot::plot_model(M1_inter,  transform = NULL)
plot_model(M1_inter, type = "pred")

p <- sjPlot::plot_model(M1_inter, type = "est", show.values = TRUE, show.p = TRUE,value.offset = .35,
                        axis.lim = c(.5, 2.5),
                   title="Effect of landcover and environmental variation on behavioural states",
                   vline.color = "grey") 

# getwd()
# "C:/Users/emitn/Documents/Internship 2019 Mun/Git/emilie_nlcaribou_2020/ouput"

sjPlot::tab_model(M1_inter, 
                  show.re.var= TRUE, 
                  file = "Interaction model - HMM",
                  pred.labels =c("(Intercept)", "Wetland*Temperature", "Wetland*SWE", "
                                 Wetland*Precipitation", "Lichen*Temperature",
                                 "Lichen*SWE", "Lichen*Precipitation",
                                 "Forest*Temperature", "Forest*SWE", "Forest*Precipitation"),
                  dv.labels= "Effect of landcover and environmental variation on behavioural states")

 


### 2nd part of the analysis ######
###RSF MODELS ####
library(mclogit)
library(lme4)
library(car)
library(Rmisc)
library(ggplot2)
library(jtools)
library(MuMIn)
library(data.table)
library(stargazer)

##read file###
allNDVI<- readRDS("C:/Users/emitn/Documents/Internship 2019 Mun/Git/emilie_nlcaribou/output/allNDVI.RDS")

####Changing name of available and used pts to easily interpret them after
allNDVI$Randoms<-ifelse(allNDVI$Randoms=="1",1,0) #### 0 == available and 1 == used 
head(allNDVI)
summary(allNDVI)

###add column to reorganise Randoms to see if there is some of
### randoms points who not have used pts (removed from water step)
allNDVI[, StrMean := mean(as.numeric(Randoms)),by = .(PtID)]
summary(allNDVI)
nrow(allNDVI)

# state = 2 = movement, state = 1 = stopover
allNDVI_stop<- subset(allNDVI, state == 1)
allNDVI_mvt <- subset(allNDVI, state == 2)

prcprsf <- summarySEwithin(allNDVI_stop, measurevar = "prcp", idvar = "Animal_ID",
                           withinvars = "Randoms", "Year", na.rm = FALSE, conf.interval = .95)

allNDVI_stop$state <- as.factor(allNDVI_stop$state)
allNDVI_stop$Randoms <- as.factor(allNDVI_stop$Randoms)

#covariables distribution##
ggplot(allNDVI_stop, aes(x = swe)) +
  geom_density(aes(color = Randoms), alpha = 0.5) +
  theme_classic()
ggplot(allNDVI_stop, aes(x = prcp)) +
  geom_density(aes(color = Randoms), alpha = 0.5) +
  theme_classic()

ggplot(allNDVI_stop, aes(x = tmax)) +
  geom_density(aes(color = Randoms), alpha = 0.5) +
  theme_classic()


###M1 hab _ stopover ####
# modelhab_RSF_stop<-glmer(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + scale(Water) + scale(Rocky) + (1|Animal_ID), data = allNDVI_stop, family = "binomial")
# summ(modelhab_RSF_stop)
# summary(modelhab_RSF_stop)
# r.squaredGLMM(modelhab_RSF_stop)

modelhab_RSF_stop<-mclogit(cbind(Randoms,PtID)~scale(Lichen) + scale(Wetland) + scale(Forest), data=allNDVI_stop)


sjPlot::tab_model(modelhab_RSF_stop, 
                  show.re.var= TRUE, 
                  dv.labels= "RSF stopover hab")



plot(allEffects(modelhab_RSF_stop))

#### M1 meteo _ stopover##
modelmeteo_RSF_stop<-mclogit(cbind(Randoms,PtID)~scale(swe) + scale(prcp) + scale(tmax), data=allNDVI_stop)
summary(modelmeteo_RSF_stop)

sjPlot::tab_model(modelmeteo_RSF_stop, 
                  show.re.var= TRUE, 
                  dv.labels= "RSF stopover meteo")

##M1 hab + meteo _ stopover####
modelmeteo_hab_RSF_stop<-mclogit(cbind(Randoms,PtID)~scale(swe) + scale(prcp) + scale(tmax) +
                                   scale(Forest) + scale(Lichen) + scale(Wetland), data=allNDVI_stop)
summary(modelmeteo_hab_RSF_stop)

###avec interaction hab + meteo _ stopover #####
model_interact_RSF_stop <- mclogit(cbind(Randoms, PtID)~ scale(swe) + scale(prcp) + scale(tmax) +
                                     scale(Forest) + scale(Lichen) + scale(Wetland) +
                                     (scale(Lichen)*(scale(prcp)+scale(swe)+scale(tmax))) +
                                     (scale(Forest)*(scale(prcp)+scale(swe)+scale(tmax))) +
                                        (scale(Wetland)*(scale(prcp)+scale(swe)+scale(tmax))), data = allNDVI_stop)

summary(model_interact_RSF_stop)

sjPlot::tab_model(model_interact_RSF_stop, 
                  show.re.var= TRUE, 
                  dv.labels= "RSF stopover meteo*hab")

ggpredict(model_interact_RSF_stop, c("Lichen", "tmax")) %>% plot()

##Anova all models ###
##Calculate AIC and delta
aics<-AIC(modelhab_RSF_stop,modelmeteo_RSF_stop,model_interact_RSF_stop)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

#                          df      AIC deltaaic
# modelhab_RSF_stop        3 124839.0 2362.250
# modelmeteo_RSF_stop      3 130516.9 8040.128
# model_interact_RSF_stop 15 122476.7    0.000

##plot _ stopover ####
r <- sjPlot::plot_model(model_interact_RSF_stop, type = "est",
                        title="RSF stopover",
                        vline.color = "grey") 

p + scale_y_log10(limits = c(.8, 2)) + theme_sjplot()

r + theme_sjplot()

##output model
tab_model(model_interact_RSF_stop, dv.labels= "RSF - stopover")



##### RSF Movement####
###M1 hab _ movement #####
# modelhab_RSF_mvt<-glmer(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + (1|Animal_ID), data = allNDVI_mvt, family = "binomial")
# summ(modelhab_RSF_mvt)
# summary(modelhab_RSF_mvt)
# r.squaredGLMM(modelhab_RSF_mvt)
# R2m        R2c
# theoretical 0.06579972 0.06650130
# delta       0.01450472 0.01465938

modelhab_RSF_mvt<-mclogit(cbind(Randoms,PtID)~scale(Lichen) + scale(Wetland) + scale(Forest), data=allNDVI_mvt)
summary(modelhab_RSF_mvt)


qqnorm(modelhab_RSF_mvt)

sjPlot::tab_model(modelhab_RSF_mvt, 
                  show.re.var= TRUE, 
                  dv.labels= "RSF movement hab")

t <- sjPlot::plot_model(modelhab_RSF_mvt, show.values = TRUE, show.p = TRUE,value.offset = .35,
                        title="RSF habitat - movement",
                        vline.color = "grey") 

t + scale_y_log10(limits = c(.5, 2)) + theme_sjplot()



#### M1 meteo _ movement##
modelmeteo_RSF_mvt<-mclogit(cbind(Randoms,PtID)~scale(swe) + scale(prcp) + scale(tmax), data=allNDVI_mvt)
summary(modelmeteo_RSF_mvt)

hist(allNDVI_mvt$swe, col = Randoms)

sjPlot::tab_model(modelmeteo_RSF_mvt, 
                  show.re.var= TRUE, 
                  dv.labels= "RSF movement meteo")

##M1 hab + meteo _ movement####
modelmeteo_hab_RSF_mvt<-mclogit(cbind(Randoms,PtID)~scale(swe) + scale(prcp) + scale(tmax) +
                                   scale(Forest) + scale(Lichen) + scale(Wetland), data=allNDVI_mvt)
summary(modelmeteo_hab_RSF_mvt)


###avec interaction hab + meteo _ movement #####
model_interact_RSF_mvt <- mclogit(cbind(Randoms, PtID)~ scale(swe) + scale(prcp) + scale(tmax) +
                                     scale(Forest) + scale(Lichen) + scale(Wetland) +
                                     (scale(Lichen)*(scale(prcp)+scale(swe)+scale(tmax))) +
                                     (scale(Forest)*(scale(prcp)+scale(swe)+scale(tmax))) +
                                     (scale(Wetland)*(scale(prcp)+scale(swe)+scale(tmax))), data = allNDVI_mvt)

summary(model_interact_RSF_mvt)

sjPlot::tab_model(model_interact_RSF_mvt, 
                  show.re.var= TRUE, 
                  dv.labels= "RSF movement meteo *hab")

##Calculate AIC and delta
aics2<-AIC(modelhab_RSF_mvt,modelmeteo_RSF_mvt,model_interact_RSF_mvt)
aicMin<-min(aics2$AIC)

aics2$deltaaic<-aics2$AIC-aicMin
aics2

###
stargazer(model_interact_RSF_mvt, type = "html",
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          header = FALSE,
          out = "RSF_mvt.html")

##plot _ movement ####
q <- sjPlot::plot_model(model_interact_RSF_mvt, show.values = TRUE, show.p = TRUE,value.offset = .35,
                        title="RSF movement",
                        vline.color = "grey") 

q  + theme_sjplot()

sjPlot::tab_model(model_interact_RSF_mvt, 
                  show.re.var= TRUE, 
                  file = TRUE,
                  pred.labels = c("SWE", "Precipitation", "Temperature_max", 
                                                                    "Forest", "Lichen", "Wetland",
                                                                   "Wetland*Temperature", "Wetland*SWE", "
                                Lichen*Precipitation", "Lichen*SWE",
                                                                    "Lichen*Temperature", "Forest*Precipitation",
                                                                    "Forest*SWE", "Forest*Temperature_max", "Wetland*Precipitation",
                                                                    "Wetland*SWE", "Wetland*Temperature_max"),
                  dv.labels= "RSF movement")



tab_model(model_interact_RSF_mvt, dv.labels= "RSF movement")
