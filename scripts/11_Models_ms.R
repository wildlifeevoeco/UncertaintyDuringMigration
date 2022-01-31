####### ## RUN MODELS ====
# Inputs: Final_MR_migration
# Outputs: Logistic regression and condition logistic models 


libs <- c('data.table', 'magrittr','dplyr', 'lubridate', 'lme4', 'broom.mixed', 'performance', 'ggeffects',
          'tidyr', 'ggplot2','survival', 'patchwork', 'ggthemes', 'data.table',
          'sjmisc', 'car', 'sjPlot', 'broom', 'paletteer', 'MuMIn')
lapply(libs, require, character.only = TRUE)

## ##### First analysis : GLMM MODELS ### 
paletteer::palettes_d_names
paletteer_d("colorblindr::OkabeIto")
# allNDVIobs <- fread("output/allNDVIobs.csv")

Final_MR_migration <- readRDS("~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/Final_MR_migration.RDS")

str(Final_MR_migration$state)

Final_MR_migration$state <- as.character(as.numeric(Final_MR_migration$state))

### Keep only obs data -->  Randoms = 1 
Final_MR_migration_obs <- subset(Final_MR_migration, Randoms == '1')

##Number of data with step lenght > 5000 m 
count(Final_MR_migration_obs, Final_MR_migration_obs$step > 7000) ### 77 and 44 NA 
## removed those data 
Final_MR_migration_obs <- subset(Final_MR_migration_obs, step < 7000)

##Figure 1 ##### Behavioural states distribution ####
###Set color palette
paletteer_d("colorblindr::OkabeIto")
mycolors <- c(paletteer_d("colorblindr::OkabeIto"))
mycolors <- mycolors[c(1,3)] 

colScale <- scale_colour_manual(name = "state",values = mycolors)

###Plot step distribution
a <- ggplot(Final_MR_migration_obs, aes(x = step)) +
  geom_density(aes(color = state), alpha = 0.5, size = 1.2) +
  theme_classic(base_size = 14) + 
   theme(
     legend.position="none") +
  xlab('Step length') + ylab('Density') + colScale

## Plot angle distribution 
b <- ggplot(Final_MR_migration_obs, aes(x = angle)) +
  geom_density(aes(color = state), alpha = 0.5, size = 1.2) +
  theme_classic(base_size = 14) + theme(
  legend.position = c(.98, .97),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
) +
  xlab('Angle') + ylab('Density') + colScale

###Bind plot together and save them
##individually
setwd("C:/Users/emitn/Documents/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/Figures")

save_plot("Step length distribution.png", a)

save_plot("Angle distribution.png", b)

require(gridExtra)
require(cowplot)
plot_Fig1 <- plot_grid(a, b, align = 'hv',
                       labels = c("A", "B"),
                       ncol = 2)

save_plot("plot_Fig1.pdf", plot_Fig1,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1
)

##### Distribution of landcover variable #### Fig S1 ####

c <- ggplot(Final_MR_migration_obs, aes(x= state, y = Lichen_200)) +
  geom_boxplot(aes(color= state),alpha = 0.5, show.legend = FALSE)  +
  theme_classic() + xlab ("State") + ylab("Proportion of lichen") + colScale

d <- ggplot(Final_MR_migration_obs, aes(x= state, y = Open_200)) +
  geom_boxplot(aes(color= state),alpha = 0.5, show.legend = FALSE)  +
  theme_classic() + xlab ("State") + ylab("Proportion of open habitat") + colScale

e <- ggplot(Final_MR_migration_obs, aes(x= state, y = Forest_200)) +
  geom_boxplot(aes(color= state),alpha = 0.5, show.legend = FALSE)+
  theme_classic() + xlab ("State") + ylab("Proportion of forest") + colScale

f <- ggplot(Final_MR_migration_obs, aes(x= state, y = Anthro_200)) +
  geom_boxplot(aes(color= state),alpha = 0.5, show.legend = FALSE) +
  theme_classic() + xlab ("State") + ylab("Proportion of anthropogenic disturbance") + colScale

plot_FigS1 <- plot_grid(c, d ,e ,f, align = 'hv',
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow=2)

save_plot("plot_FigS1.pdf", plot_FigS1,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1
)


##### Distribution of weather variable #### Fig S1 ####

g <- ggplot(Final_MR_migration_obs, aes(x= state, y = swe)) +
  geom_boxplot(aes(color= state),alpha = 0.5, show.legend = FALSE)  +
  theme_classic() + xlab ("State") + ylab("Snow water equivalent") + colScale

h <- ggplot(Final_MR_migration_obs, aes(x= state, y = tmax)) +
  geom_boxplot(aes(color= state),alpha = 0.5, show.legend = FALSE)  +
  theme_classic() + xlab ("State") + ylab("Temperature") + colScale

i <- ggplot(Final_MR_migration_obs, aes(x= state, y = prcp)) +
  geom_boxplot(aes(color= state),alpha = 0.5, show.legend = FALSE)+
  theme_classic() + xlab ("State") + ylab("Precipitation") + colScale

plot_FigS2 <- plot_grid(g, h ,i, align = 'hv',
                        labels = c("A", "B", "C"),
                        ncol = 2, nrow=2)

save_plot("plot_FigS2.pdf", plot_FigS2,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1
)

#### Test for correlation between variables #####
library(GGally)
colnames(Final_MR_migration_obs)
testcorr = subset(Final_MR_migration_obs, select = c(28, 30:32, 45:48))

# Plot the graph
ggcorr(testcorr,method = c("pairwise", "spearman"))
##only tmin/tmax > 0.6

## nothing correlated except tmin/tmax (we use only tmax)
summary(testcorr$tmin)
summary(testcorr$tmax)

######## MODELS GLMM PART 1 ####
####Convert state to run the models 
Final_MR_migration_obs$HMM <- ifelse(Final_MR_migration_obs$state == 2,0,1) 

##- Models with lichen/wetland/forest

str(Final_MR_migration_obs$HMM) ## 1 = encamped and 0 = movement
# Final_MR_migration_obs %>% group_by(HMM) %>%
#   summarize( mean = mean(step))

##M1 Hab----
Final_MR_migration_obs$HMM <- as.numeric(as.character(Final_MR_migration_obs$HMM))
str(Final_MR_migration_obs$HMM)

###Null model
M1_null <- glmer(HMM ~ (1|Animal_ID), data = Final_MR_migration_obs, family = binomial)
summary(M1_null)

##### Habitat model ####
M1_hab <- glmer(HMM ~ Open_200 + Lichen_200 + Forest_200 + (1|Animal_ID), data = Final_MR_migration_obs, family = binomial)
summary(M1_hab)
Anova(M1_hab)
# LR Chisq Df Pr(>Chisq)    
# Wetland    38.15  1  6.541e-10 ***
#   Lichen    377.77  1  < 2.2e-16 ***
#   Forest     14.16  1  0.0001676 **
tidy(M1_hab)
se <- sqrt(diag(vcov(M1_hab)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(M1_hab), LL = fixef(M1_hab) - 1.96 * se, UL = fixef(M1_hab) + 1.96 * se)

print(exp(tab), digits=3)

##RANDOM 
group.indivs <- tidy(M1_hab, effect = 'ran_vals')

#### GRAPHS ----
pred.hab.open <- ggpredict(M1_hab, terms = c('Open_200'))
plot(pred.hab.open)

pred.hab.lic <- ggpredict(M1_hab, terms = c('Lichen_200'))
plot(pred.hab.lic)

pred.hab.for <- ggpredict(M1_hab, terms = c('Forest_200'))
plot(pred.hab.for)


##Plot
plot_model(M1_hab, vline.color = "grey")+theme_sjplot2()

sjPlot::tab_model(M1_hab, 
                  show.re.var= TRUE, 
                  dv.labels= "HMM habitat", 
                  file = 'HMM_mod_hab')


##M1 meteo---- 

#mod
M1_meteo <- glmer(HMM ~ scale(prcp) + scale(tmax) + scale(swe)  + (1|Animal_ID), data = Final_MR_migration_obs, family = 'binomial')
summary(M1_meteo)
Anova(M1_meteo)
# Chisq Df Pr(>Chisq)    
# scale(prcp)    1.9616  1     0.1613    
# scale(tmax)   20.5901  1  5.689e-06 ***
#   scale(swe)  1815.6660  1  < 2.2e-16 ***
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


###Get estimates
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

M1_all <- glmer(HMM ~ scale(Open_200) + scale(Lichen_200) + scale(Forest_200) + scale(tmax) +
              scale(swe) + scale(prcp) + (1|Animal_ID), data = Final_MR_migration_obs, family = "binomial")
summary(M1_all)
Anova(M1_all)


##with interaction ####
M1_all_inter <-  glmer(HMM ~ scale(Open_200) + scale(Lichen_200) + scale(Forest_200) + scale(tmax) +
                     scale(swe) + scale(prcp) + scale(Open_200)*scale(tmax) + scale(Open_200)*scale(swe) +
                     scale(Open_200)*scale(prcp) + scale(Lichen_200)*scale(tmax) + scale(Lichen_200)*scale(swe) + scale(Lichen_200)*scale(prcp) +
                     scale(Forest_200)*scale(tmax) + scale(Forest_200)*scale(swe) + scale(Forest_200)*scale(prcp) + (1|IDYear), data = Final_MR_migration_obs, family = "binomial")
summary(M1_all_inter)

fixef(M1_inter)

##summary output
sjPlot::tab_model(M1_inter, 
                  show.re.var= TRUE, 
                  dv.labels= "HMM meteo*hab")



# Extract the prediction data frame
pred.mm <- ggpredict(M1_all_inter, terms = c("Lichen_200", "tmax"))
#do this for the all cov
plot(pred.mm)

###comparing AIC all models
anova(M1_null, M1_meteo, M1_all_inter, M1_hab,test="Chisq")
# npar   AIC   BIC logLik deviance   Chisq Df Pr(>Chisq)    
# M1_hab      4 38151 38185 -19072    38143                          
# M1_meteo    6 33832 33882 -16910    33820 4323.80  2  < 2.2e-16 ***
#   M1_inter   17 33117 33260 -16541    33083  737.08 11  < 2.2e-16 ***

######Comparing models - HMM ####
aics<-AIC(M1_null, M1_hab,M1_meteo, M1_all_inter)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics
# df      AIC  deltaaic
# M1_null       2 35217.22 2405.8828
# M1_hab        5 34727.39 1916.0540
# M1_meteo      5 33238.96  427.6213
# M1_all_inter 17 32811.34    0.0000

library(AICcmodavg)
#define list of models
models <- list(M1_hab,M1_meteo, M1_all_inter)

#specify model names
mod.names <- c('M1_hab', 'M1_meteo', 'M1_all_inter')
#calculate AIC of each model
aictab(cand.set = models, modnames = mod.names)
#               K     AICc Delta_AICc AICcWt Cum.Wt        LL
# M1_all_inter 17 32811.35       0.00      1      1 -16388.67
# M1_meteo      5 33238.96     427.60      0      1 -16614.48
# M1_hab        5 34727.39    1916.04      0      1 -17358.69

###summary output model####
library(stargazer)
setwd( "C:/Users/emitn/Documents/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/tables")
stargazer(M1_all_inter, type = "html",
          column.labels = c("Weather","Habitat"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          header = FALSE,
          out = "Best_HMM_model.html")

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
ggpredict(M1_all_inter, c("Lichen_200", "tmax")) %>% plot()
# M1_inter_Lichen_temp <- ggpredict(M1_inter, terms = c("Lichen" ,"tmax") )
###Figure out why temperature is extracted as group of 3 values!

ggplot(M1_inter_Lichen_temp, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)
ggpredict(M1_inter, c("Lichen", "prcp")) %>% plot()
ggpredict(M1_inter, c("Lichen", "prcp")) %>% plot()
ggpredict(M1_inter, c("Lichen", "swe")) %>% plot()
ggpredict(M1_inter, c("Forest", "swe")) %>% plot()
ggpredict(M1_inter, c("Wetland", "swe")) %>% plot()


### TEST PLOT INTERACTION
mydf <- ggpredict(M1_all_inter, terms = c("Open_200", "tmax"))
ggplot(mydf, aes(x, predicted, group = group)) +
  geom_line(aes(color = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)



##plot model results####

HMM_mod <- get_model_data(M1_all_inter,
                           type = c("est"), transf = NULL)

HMM_mod$term_rename <- c("Open", "Lichen", "Forest","Temperature","Snow water equivalent",
                          "Precipitation", "Open x Temperature", "Open x Snow water equivalent",
                          "Open x Precipitation", "Lichen x Temperature", "Lichen x Snow water equivalent",
                          "Lichen x Precipitation", "Forest x Temperature", "Forest x Snow water equivalent", 
                          "Forest x Precipitation")

HMM_mod <- HMM_mod %>%
  mutate(term_rename = fct_relevel(term_rename, 
                                   "Forest x Precipitation", "Forest x Snow water equivalent", "Forest x Temperature",
                                   "Lichen x Precipitation","Lichen x Snow water equivalent", "Lichen x Temperature",
                                   "Open x Precipitation", "Open x Snow water equivalent", "Open x Temperature",
                                   "Precipitation", "Snow water equivalent", "Temperature",
                                   "Forest", "Lichen", "Open"))

mycolors2 <- c(paletteer_d("colorblindr::OkabeIto"))
mycolors2 <- mycolors2[c(3,1)] 

colScale2 <- scale_colour_manual(name = "state",values = mycolors2)



a <- ggplot(HMM_mod, aes(estimate, term_rename, color = group)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = 'dashed') +
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size = .7, height = 
                   .2, color = 'gray50') +
  geom_point(size = 2.5,  show.legend = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  # scale_x_continuous(breaks = seq(0,7,1) ) +
  ylab('') +
  xlab('Beta Coefficient') +colScale2


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
library(Rmisc)
library(jtools)


###USING RANDOMS/REAL DATA from Final_MR_migration
##### State 1 = encamped / State 2 = Movement

colnames(Final_MR_migration)
##Subset data by encamped and movement behaviour
ggplot(Final_MR_migration, aes(state, step)) + geom_boxplot()

####Changing name of available and used pts to easily interpret them after
Final_MR_migration_RSF <- Final_MR_migration
Final_MR_migration_RSF$Randoms<-ifelse(Final_MR_migration_RSF$Randoms=="1",1,0) #### 0 == available and 1 == used 


###add column to reorganise Randoms to see if there is some of
### randoms points who not have used pts (removed from water step)
Final_MR_migration_RSF[, StrMean := mean(as.numeric(Randoms)),by = (PtID)]

str(Final_MR_migration_RSF)
sum(is.na(Final_MR_migration_RSF$Lichen_200))

## remove NA's
Final_MR_migration_RSF <- na.omit(Final_MR_migration_RSF) 


# state = 2 = movement, state = 1 = stopover
Final_MR_migration_RSF_stop<- subset(Final_MR_migration_RSF, state == 1)
Final_MR_migration_RSF_mvt <- subset(Final_MR_migration_RSF, state == 2)

# prcprsf <- summarySEwithin(Final_MR_migration_RSF_stop, measurevar = "prcp", idvar = "Animal_ID",
                           # withinvars = "Randoms", "Year", na.rm = FALSE, conf.interval = .95)

#covariables distribution##

ggplot(Final_MR_migration_RSF_stop, aes(x = JDate, y = prcp)) +
  geom_line(alpha = 0.5, show.legend = FALSE)+ 
  theme_classic() + xlab ("State") + ylab("Precipitation") +
  facet_wrap(~Year, scales= 'free')

str(Final_MR_migration_RSF_stop$JDate)

###### Part 2: Models on stopover data #####
summary(Final_MR_migration_RSF_stop$step)
summary(Final_MR_migration_RSF_mvt$step)
###M1 hab _ stopover ####
# modelhab_RSF_stop<-glmer(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + scale(Water) + scale(Rocky) + (1|Animal_ID), data = allNDVI_stop, family = "binomial")
# summ(modelhab_RSF_stop)
# summary(modelhab_RSF_stop)
# r.squaredGLMM(modelhab_RSF_stop)

###Using mclogit function to take into account time variability 
###RSF Stop habitat ####
modelhab_RSF_stop<-mclogit(cbind(Randoms,PtID)~Lichen_200 + Open_200 + Forest_200, data=Final_MR_migration_RSF_stop)

summary(modelhab_RSF_stop)
logLik(modelhab_RSF_stop)


sjPlot::tab_model(modelhab_RSF_stop, 
                  show.re.var= TRUE, 
                  dv.labels= "RSF stopover hab")

ggpredict(modelhab_RSF_stop, terms = c("Open_200")) %>% plot

#### M1 meteo _ stopover##
modelmeteo_RSF_stop<-mclogit(cbind(Randoms,PtID)~scale(swe) + scale(prcp) + scale(tmax), data=Final_MR_migration_RSF_stop)
summary(modelmeteo_RSF_stop)
logLik(modelmeteo_RSF_stop)
# sjPlot::tab_model(modelmeteo_RSF_stop, 
#                   show.re.var= TRUE, 
#                   dv.labels= "RSF stopover meteo")

##M1 hab + meteo _ stopover####
# modelmeteo_hab_RSF_stop<-mclogit(cbind(Randoms,PtID)~scale(swe) + scale(prcp) + scale(tmax) +
#                                    scale(Forest) + scale(Lichen) + scale(Wetland), data=allNDVI_stop)
# summary(modelmeteo_hab_RSF_stop)


###avec interaction hab + meteo _ stopover #####
###create tab with all variable scaled###
RSF_stop_df_scaled <- Final_MR_migration_RSF_stop[c(1,11,24,28,30,31,45:47)]
RSF_stop_df_scaled[c(10:15)] <- c(scale(RSF_stop_df_scaled[c(4:9)]))
RSF_stop_df_scaled$prcp_sc <- c(scale(RSF_stop_df_scaled$prcp))
RSF_stop_df_scaled$swe_sc <- c(scale(RSF_stop_df_scaled$swe))
RSF_stop_df_scaled$tmax_sc <- c(scale(RSF_stop_df_scaled$tmax))
RSF_stop_df_scaled$Lichen_sc <- c(scale(RSF_stop_df_scaled$Lichen_200))
RSF_stop_df_scaled$Open_sc <- c(scale(RSF_stop_df_scaled$Open_200))
RSF_stop_df_scaled$Forest_sc <- c(scale(RSF_stop_df_scaled$Forest_200))

head(scale(RSF_stop_df_scaled$Forest_200, center = TRUE, scale = TRUE))

 # model_interact_RSF_stop <- mclogit(cbind(Randoms, PtID)~ scale(Open_200) + scale(Lichen_200) + scale(Forest_200) + scale(tmax) +
 #                                      scale(swe) + scale(prcp) + (scale(Open_200)*scale(tmax)) + (scale(Open_200)*scale(swe)) +
 #                                      (scale(Open_200)*scale(prcp)) + (scale(Lichen_200)*scale(tmax)) + (scale(Lichen_200)*scale(swe)) + 
 #                                      (scale(Lichen_200)*scale(prcp)) +
 #                                      (scale(Forest_200)*scale(tmax)) + (scale(Forest_200)*scale(swe)) + (scale(Forest_200)*scale(prcp)), data = Final_MR_migration_RSF_stop)

model_interact_RSF_stop <- mclogit(cbind(Randoms, PtID)~ Open_sc + Lichen_sc + Forest_sc + tmax_sc +
                                     swe_sc+ prcp_sc + Open_sc*tmax_sc + Open_sc*swe_sc +
                                     Open_sc*prcp_sc + Lichen_sc*tmax_sc + Lichen_sc*swe_sc + Lichen_sc*prcp_sc +
                                     Forest_sc*tmax_sc + Forest_sc*swe_sc + Forest_sc*prcp_sc, data = RSF_stop_df_scaled)


summary(model_interact_RSF_stop)
logLik(model_interact_RSF_stop)

##Anova all models ###
##Calculate AIC and delta
aics<-AIC(modelhab_RSF_stop,modelmeteo_RSF_stop,model_interact_RSF_stop)
aicMin<-min(aics$AIC)

aics$deltaaic<-aics$AIC-aicMin
aics

# df      AIC deltaaic
# modelhab_RSF_stop        3 126977.5 4022.915
# modelmeteo_RSF_stop      3 130516.9 7562.306
# model_interact_RSF_stop 15 122954.6    0.000


models <- list(modelhab_RSF_stop,modelmeteo_RSF_stop, model_interact_RSF_stop)

#specify model names
mod.names <- c('modelhab_RSF_stop', 'modelmeteo_RSF_stop', 'model_interact_RSF_stop')
#calculate AIC of each model
aictab(cand.set = models, modnames = mod.names) ## doesn't work for this kind of model


##plot odd ratio _ stopover ####
r <- sjPlot::plot_model(model_interact_RSF_stop, type = "est",
                        title="RSF stopover",
                        vline.color = "grey") 

r + scale_y_log10(limits = c(.8, 2)) + theme_sjplot()

r + theme_sjplot()

##### Interaction plot RSF stop ####
### Prediction = effect of forest and temperature####

##test with ggpredict function
predict_df <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Forest_sc", "tmax_sc")
)


# Function
unscale_after_predict <- function(after_predict_data, before_predict_data, column) {
  after_predict_data[[column]] *
    attr(before_predict_data[[column]], 'scaled:scale') +
    attr(before_predict_data[[column]], 'scaled:center')
}


predict_df$Forest_sc <- predict_df$x

str(predict_df$group)
options(digits=2)
predict_df$tmax_sc <- as.numeric(as.character(predict_df$group))
str(predict_df$tmax_sc)

predict_df$Forest_200 <- unscale_after_predict(predict_df, RSF_stop_df_scaled, 'Forest_sc')
print(unscale_after_predict(predict_df, RSF_stop_df_scaled, 'Forest_sc'))
#>  [1] -0.03178898 -0.03178898 -0.03178898  0.16886148  0.16886148  0.16886148
#>  [7]  0.36951194  0.36951194  0.36951194  0.57016240  0.57016240  0.57016240
#> [13]  0.77081286  0.77081286  0.77081286  0.97146331  0.97146331  0.97146331
#> [19]  1.17211377  1.17211377  1.17211377

levels(cut(RSF_stop_df_scaled$Forest_200, 5))
#> [1] "(-0.000995,0.199]" "(0.199,0.398]"     "(0.398,0.597]"
#> [4] "(0.597,0.796]"     "(0.796,0.996]"

range(RSF_stop_df_scaled$Forest_200)

predict_df$tmax <- unscale_after_predict(predict_df, RSF_stop_df_scaled, 'tmax_sc')

range(RSF_stop_df_scaled$tmax)

predict_df$tmax <- as.numeric(predict_df$tmax)
options(digits = 2)

predict_df$tmax <- format(predict_df$tmax, digits = 2)


#### interaction plot between lichen and tmax
predict_df_lichen_temp <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Lichen_sc", "tmax_sc")
)


predict_df_lichen_temp$Lichen_sc <- predict_df_lichen_temp$x


str(predict_df_lichen_temp$group)
options(digits=2)
predict_df_lichen_temp$tmax_sc <- as.numeric(as.character(predict_df_lichen_temp$group))
str(predict_df_lichen_temp$tmax_sc)

predict_df_lichen_temp$Lichen_200 <- unscale_after_predict(predict_df_lichen_temp, RSF_stop_df_scaled, 'Lichen_sc')


###### Interaction plot between open / tmax
predict_df_open_temp <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Open_sc", "tmax_sc")
)


predict_df_open_temp$Open_sc <- predict_df_open_temp$x


# str(predict_df_open_temp$group)
# options(digits=2)
# predict_df_open_temp$tmax_sc <- as.numeric(as.character(predict_df_open_temp$group))
# str(predict_df_open_temp$tmax_sc)

predict_df_open_temp$Open_200 <- unscale_after_predict(predict_df_lichen_temp, RSF_stop_df_scaled, 'Open_sc')

####

##### Interaction plot between Lichen/snow####
predict_df_lichen_swe <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Lichen_sc", "swe_sc")
)


predict_df_lichen_swe$Lichen_sc <- predict_df_lichen_swe$x


# str(predict_df_open_temp$group)
# options(digits=2)
# predict_df_open_temp$tmax_sc <- as.numeric(as.character(predict_df_open_temp$group))
# str(predict_df_open_temp$tmax_sc)

predict_df_lichen_swe$Lichen_200 <- unscale_after_predict(predict_df_lichen_swe, RSF_stop_df_scaled, 'Lichen_sc')

##### Interaction plot between Open/snow####
predict_df_open_swe <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Open_sc", "swe_sc")
)


predict_df_open_swe$Open_sc <- predict_df_open_swe$x


# str(predict_df_open_temp$group)
# options(digits=2)
# predict_df_open_temp$tmax_sc <- as.numeric(as.character(predict_df_open_temp$group))
# str(predict_df_open_temp$tmax_sc)

predict_df_open_swe$Open_200 <- unscale_after_predict(predict_df_open_swe, RSF_stop_df_scaled, 'Lichen_sc')


########## Interaction plot between Forest/snow####
predict_df_forest_swe <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Forest_sc", "swe_sc")
)


predict_df_forest_swe$Forest_sc <- predict_df_forest_swe$x


# str(predict_df_open_temp$group)
# options(digits=2)
# predict_df_open_temp$tmax_sc <- as.numeric(as.character(predict_df_open_temp$group))
# str(predict_df_open_temp$tmax_sc)

predict_df_forest_swe$Forest_200 <- unscale_after_predict(predict_df_forest_swe, RSF_stop_df_scaled, 'Forest_sc')


#### Plot interactions#### 

a <- ggplot(predict_df, aes(x, predicted, group = group)) +
  geom_line(aes(color = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme_bw(base_size = 14) +
  xlab("Proportion of forest habitat") + ylab("Relative probability of selection")+
  labs(col="Temperature")

b <- ggplot(predict_df_open_temp, aes(x, predicted, group = group)) +
  geom_line(aes(color = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme_bw(base_size = 14) +
  xlab("Proportion of open habitat") + ylab("Relative probability of selection")+
  labs(col="Temperature")

c<- ggplot(predict_df_lichen_temp, aes(x, predicted, group = group)) +
  geom_line(aes(color = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme_bw(base_size = 14) +
  xlab("Proportion of lichen habitat") + ylab("Relative probability of selection")+
  labs(col="Temperature")

d <- ggplot(predict_df_lichen_swe, aes(x, predicted, group = group)) +
  geom_line(aes(color = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme_bw(base_size = 14) +
  xlab("Proportion of Lichen habitat") + ylab("Relative probability of selection")+
  labs(col="Snow water equivalent")

e <- ggplot(predict_df_open_swe, aes(x, predicted, group = group)) +
  geom_line(aes(color = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme_bw(base_size = 14) +
  xlab("Proportion of Open habitat") + ylab("Relative probability of selection")+
  labs(col="Snow water equivalent")

f <- ggplot(predict_df_forest_swe, aes(x, predicted, group = group)) +
  geom_line(aes(color = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme_bw(base_size = 14) +
  xlab("Proportion of Forest habitat") + ylab("Relative probability of selection")+
  labs(col="Snow water equivalent")

plot_Fig4 <- plot_grid(a,b,c,d,e,f, align = 'hv',
                        labels = c("A", "B", "C", "D", "E",
                                   "F"),
                        ncol = 3, nrow=3)

save_plot("plot_FigS2.pdf", plot_FigS2,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1
)



###Extract values to plot figure estimates
stop_rsf <- get_model_data(model_interact_RSF_stop,
                          type = c("est"))

stop_rsf$term_rename <- c("Open", "Lichen", "Forest","Temperature","Snow water equivalent",
                         "Precipitation", "Open x Temperature", "Open x Snow water equivalent",
                         "Open x Precipitation", "Lichen x Temperature", "Lichen x Snow water equivalent",
                         "Lichen x Precipitation", "Forest x Temperature", "Forest x Snow water equivalent", 
                         "Forest x Precipitation")

stop_rsf <- stop_rsf %>%
  mutate(term_rename = fct_relevel(term_rename, 
                                   "Forest x Precipitation", "Forest x Snow water equivalent", "Forest x Temperature",
                                   "Lichen x Precipitation","Lichen x Snow water equivalent", "Lichen x Temperature",
                                   "Open x Precipitation", "Open x Snow water equivalent", "Open x Temperature",
                                   "Precipitation", "Snow water equivalent", "Temperature",
                                   "Forest", "Lichen", "Open"))

b <- ggplot(stop_rsf, aes(estimate, term_rename)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = 'dashed') +
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size = .7, height = 
                   .2, color = 'gray50') +
  geom_point(size = 2.5, color = '#E69F00FF') +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank()) +
  # scale_x_continuous(breaks = seq(0,7,1) ) +
  ylab('') +
  xlab('Beta Coefficient') 

##output model
tab_model(model_interact_RSF_stop, dv.labels= "RSF - stopover")


###save dataset
saveRDS(Final_MR_migration_RSF_stop, "~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/Final_MR_migration_RSF_stop.RDS")

##### RSF Movement####
###M1 hab _ movement #####
# modelhab_RSF_mvt<-glmer(Randoms ~ scale(Wetland) + scale(Forest) + scale(Lichen) + (1|Animal_ID), data = allNDVI_mvt, family = "binomial")
# summ(modelhab_RSF_mvt)


modelhab_RSF_mvt<-mclogit(cbind(Randoms,PtID)~ Lichen_200+ Open_200 + Forest_200, data=Final_MR_migration_RSF_mvt)
summary(modelhab_RSF_mvt)
logLik(modelhab_RSF_mvt)

sjPlot::tab_model(modelhab_RSF_mvt, 
                  show.re.var= TRUE, 
                  dv.labels= "RSF movement hab")

t <- sjPlot::plot_model(modelhab_RSF_mvt, show.values = TRUE, show.p = TRUE,value.offset = .35,
                        title="RSF habitat - movement",
                        vline.color = "grey") + theme_sjplot()


#### M1 meteo _ movement##
modelmeteo_RSF_mvt<-mclogit(cbind(Randoms,PtID)~scale(swe) + scale(prcp) + scale(tmax), data=Final_MR_migration_RSF_mvt)
summary(modelmeteo_RSF_mvt)
logLik(modelmeteo_RSF_mvt)

sjPlot::tab_model(modelmeteo_RSF_mvt, 
                  show.re.var= TRUE, 
                  dv.labels= "RSF movement meteo")

# ##M1 hab + meteo _ movement####
# modelmeteo_hab_RSF_mvt<-mclogit(cbind(Randoms,PtID)~scale(swe) + scale(prcp) + scale(tmax) +
#                                    scale(Forest) + scale(Lichen) + scale(Wetland), data=allNDVI_mvt)
# summary(modelmeteo_hab_RSF_mvt)


###avec interaction hab + meteo _ movement #####
model_interact_RSF_mvt <- mclogit(cbind(Randoms, PtID)~ scale(Open_200) + scale(Lichen_200) + scale(Forest_200) + scale(tmax) +
                                    scale(swe) + scale(prcp) + scale(Open_200)*scale(tmax) + scale(Open_200)*scale(swe) +
                                    scale(Open_200)*scale(prcp) + scale(Lichen_200)*scale(tmax) + scale(Lichen_200)*scale(swe) + scale(Lichen_200)*scale(prcp) +
                                    scale(Forest_200)*scale(tmax) + scale(Forest_200)*scale(swe) + scale(Forest_200)*scale(prcp), data = Final_MR_migration_RSF_mvt)

summary(model_interact_RSF_mvt)
logLik(model_interact_RSF_mvt)

sjPlot::tab_model(model_interact_RSF_mvt, 
                  show.re.var= TRUE, 
                  dv.labels= "RSF movement meteo *hab")

##Calculate AIC and delta
aics2<-AIC(modelhab_RSF_mvt,modelmeteo_RSF_mvt,model_interact_RSF_mvt)
aicMin<-min(aics2$AIC)

aics2$deltaaic<-aics2$AIC-aicMin
aics2

# df      AIC deltaaic
# modelhab_RSF_mvt        3 45286.91 667.6012
# modelmeteo_RSF_mvt      3 45577.94 958.6329
# model_interact_RSF_mvt 15 44619.31   0.0000

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

#### PLOT FIGURE C
summary(model_interact_RSF_mvt)
mov_rsf <- get_model_data(model_interact_RSF_mvt,
               type = c("eff"))

mov_rsf$term_rename <- c("Open", "Lichen", "Forest","Temperature","Snow water equivalent",
                          "Precipitation", "Open x Temperature", "Open x Snow water equivalent",
                          "Open x Precipitation", "Lichen x Temperature", "Lichen x Snow water equivalent",
                          "Lichen x Precipitation", "Forest x Temperature", "Forest x Snow water equivalent", 
                          "Forest x Precipitation")


mov_rsf$term <- factor(mov_rsf$term, levels = mov_rsf$term)

library(forcats)
mov_rsf <- mov_rsf %>%
  mutate(term_rename = fct_relevel(term_rename, 
                                   "Open", "Lichen", "Forest","Temperature","Snow water equivalent",
                                   "Precipitation", "Open x Temperature", "Open x Snow water equivalent",
                                   "Open x Precipitation", "Lichen x Temperature", "Lichen x Snow water equivalent",
                                   "Lichen x Precipitation", "Forest x Temperature", "Forest x Snow water equivalent", 
                                   "Forest x Precipitation"))

mov_rsf <- mov_rsf %>%
  mutate(term_rename = fct_relevel(term_rename, 
                                   "Forest x Precipitation", "Forest x Snow water equivalent", "Forest x Temperature",
                                   "Lichen x Precipitation","Lichen x Snow water equivalent", "Lichen x Temperature",
                                   "Open x Precipitation", "Open x Snow water equivalent", "Open x Temperature",
                                   "Precipitation", "Snow water equivalent", "Temperature",
                                   "Forest", "Lichen", "Open"))

c <- ggplot(mov_rsf, aes(estimate, term_rename)) +
  geom_vline(aes(xintercept = 0), size = .25, linetype = 'dashed') +
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), size = .7, height = 
                   .2, color = 'gray50') +
  geom_point(size = 2.5, color = '#009E73FF') +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank()) +
  # scale_x_continuous(breaks = seq(0,7,1) ) +
  ylab('') +
  xlab('Beta Coefficient')  


####Bind models output plot
plot_Fig2 <- plot_grid(a, b, c, align = 'hv',
                        labels = c("A", "B", "C"),
                        ncol = 3, nrow=1)

save_plot("plot_Fig2.pdf", plot_Fig2,
          ncol = 3, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
)

####


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


tab_model(M1_all_inter,  transform = NULL,string.est = "Estimates",pred.labels = c("Intercept", "Open", "Lichen", "Forest","Temperature","Snow water equivalent",
                                                                                                        "Precipitation", "Open x Temperature", "Open x Snow water equivalent",
                                                                                                        "Open x Precipitation", "Lichen x Temperature", "Lichen x Snow water equivalent",
                                                                                                        "Lichen x Precipitation", "Forest x Temperature", "Forest x Snow water equivalent", 
                                                                                                        "Forest x Precipitation"),dv.labels= c("HMM"))

M1_all_inter@beta
tab_model(model_interact_RSF_stop, model_interact_RSF_mvt,pred.labels = c("Open", "Lichen", "Forest","Temperature","Snow water equivalent",
                                                  "Precipitation", "Open x Temperature", "Open x Snow water equivalent",
                                                  "Open x Precipitation", "Lichen x Temperature", "Lichen x Snow water equivalent",
                                                  "Lichen x Precipitation", "Forest x Temperature", "Forest x Snow water equivalent", 
                                                  "Forest x Precipitation"),dv.labels= c("Encamped","Movement"))


###save data
saveRDS(Final_MR_migration_RSF_mvt, "~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/Final_MR_migration_RSF_mvt.RDS")


