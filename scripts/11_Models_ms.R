####### ## RUN MODELS ====
# Inputs: Final_MR_migration
# Outputs: Logistic regression and condition logistic models 

library(data.table)
library(magrittr)
library(dplyr)
library(lubridate)
library(lme4)
library(mclogit)
library(performance)
library(ggeffects)
library(tidyr)
library(ggplot2)
library(survival)
library(patchwork)
library(ggthemes)
library(sjmisc)
library(car)
library(sjPlot)
library(broom)
library(paletteer)
library(MuMIn)
library(scales)

## ##### First analysis : GLMM MODELS ### 
paletteer::palettes_d_names
paletteer_d("colorblindr::OkabeIto")
# allNDVIobs <- fread("output/allNDVIobs.csv")

Final_MR_migration <- readRDS("output/Final_MR_migration.RDS")

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
cols <- c("1"="#E69F00FF","2"="#009E73FF")
###Plot step distribution
a <- ggplot(Final_MR_migration_obs %>% subset (step <= 5000), aes(x = step)) +
  geom_density(aes(color = state), alpha = 0.5, size = 1.2) +
  theme_classic(base_size = 14)  +scale_colour_manual(guide="none", values=cols) +
  xlab('Step length (m)') + ylab('Density') 

## Plot angle distribution 
b <- ggplot(Final_MR_migration_obs, aes(x = angle)) +
  geom_density(aes(color = state), alpha = 0.5, size = 1.2) +
  theme_classic(base_size = 14) +
  scale_colour_manual(name="State", 
                      labels = c("Encamped", "Movement"),values=cols) +
  xlab('Angle (radians)') + ylab('') 

###Bind plot together and save them
##individually
save_plot("graphics/Step length distribution.png", a)

save_plot("graphics/Angle distribution.png", b)


require(gridExtra)
require(cowplot)
plot_Fig1 <- plot_grid(a, b, align = 'hv',
                       labels = c("A", "B"),
                       ncol = 2)

save_plot("plot_Fig1.pdf", plot_Fig1,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
)

save_plot("plot_Fig1.png", plot_Fig1,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
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
###create tab with all variable scaled###
HMM_df_scaled <- Final_MR_migration_obs[c(1:2,11,24,28,30,31,45:47, 49)]
# HMM_df_scaled[c(10:15)] <- scale(HMM_df_scaled[c(4:9)])
HMM_df_scaled$prcp_sc <- scale(HMM_df_scaled$prcp)
HMM_df_scaled$swe_sc <- scale(HMM_df_scaled$swe)
HMM_df_scaled$tmax_sc <- scale(HMM_df_scaled$tmax)
HMM_df_scaled$Lichen_sc <- scale(HMM_df_scaled$Lichen_200)
HMM_df_scaled$Open_sc <- scale(HMM_df_scaled$Open_200)
HMM_df_scaled$Forest_sc <- scale(HMM_df_scaled$Forest_200)

setDT(HMM_df_scaled)

##Min max landcover
min_max_forest <- HMM_df_scaled[, c(min(Forest_200, na.rm = TRUE),
                                        max(Forest_200, na.rm = TRUE))]
scale_attributes_forest <- attributes(HMM_df_scaled$Forest_sc)

min_max_lichen <- HMM_df_scaled[, c(min(Lichen_200, na.rm = TRUE),
                                        max(Lichen_200, na.rm = TRUE))]
scale_attributes_lichen <- attributes(HMM_df_scaled$Lichen_sc)

min_max_open <- HMM_df_scaled[, c(min(Open_200, na.rm = TRUE),
                                      max(Open_200, na.rm = TRUE))]
scale_attributes_open <- attributes(HMM_df_scaled$Open_sc)

##Min max weather
min_max_temp <- HMM_df_scaled[, c(min(tmax, na.rm = TRUE),
                                      max(tmax, na.rm = TRUE))]

min_max_swe <- HMM_df_scaled[, c(min(swe, na.rm = TRUE),
                                     max(swe, na.rm = TRUE))]

min_max_prcp <- HMM_df_scaled[, c(min(prcp, na.rm = TRUE),
                                      max(prcp, na.rm = TRUE))]

####Model 1 - Behavioural states####
M1_all_inter <-  glmer(HMM ~  + Open_sc + Lichen_sc + Forest_sc + tmax_sc +
  swe_sc+ prcp_sc + Open_sc*tmax_sc + Open_sc*swe_sc +
  Open_sc*prcp_sc + Lichen_sc*tmax_sc + Lichen_sc*swe_sc + Lichen_sc*prcp_sc +
  Forest_sc*tmax_sc + Forest_sc*swe_sc + Forest_sc*prcp_sc + (1|IDYear), data = HMM_df_scaled, family = "binomial")

###summary plot
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
stargazer(M1_all_inter, type = "html",
          column.labels = c("Weather","Habitat"),
          intercept.bottom =  FALSE,
          single.row = FALSE,
          notes.append = FALSE,
          header = FALSE,
          out = "output/tables/Best_HMM_model.html")

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
##### Interaction plot between Forest/temp####
predict_df_forest_temp <- ggpredict(
  M1_all_inter,
  terms = c("Forest_sc[all]", "tmax_sc")
)

##### Interaction plot between Lichen/temp####
predict_df_lichen_temp <- ggpredict(
  M1_all_inter,
  terms = c("Lichen_sc[all]", "tmax_sc")
)

##### Interaction plot between Lichen/temp####
predict_df_open_temp <- ggpredict(
  M1_all_inter,
  terms = c("Open_sc[all]", "tmax_sc")
)

##### Interaction plot between Lichen/snow####
predict_df_lichen_snow <- ggpredict(
  M1_all_inter,
  terms = c("Lichen_sc[all]", "swe_sc")
)


##### Interaction plot between Forest/prcp####
predict_df_forest_prcp <- ggpredict(
  M1_all_inter,
  terms = c("Forest_sc[all]", "prcp_sc"))


##### Interaction plot between Open/prcp####
predict_df_open_prcp <- ggpredict(
  M1_all_inter,
  terms = c("Open_sc[all]", "prcp_sc"))



#### Plot interactions#### 
###unscaled labels#
unscaled_proportion_labels <- breaks_pretty()(seq(0, 1))

####
unscaled_labels_temp <- breaks_pretty(2)(seq(min_max_temp[1], min_max_temp[2]))
unscaled_labels_temp <- as.character(unscaled_labels_temp)
unscaled_labels_swe <- breaks_pretty(1)(seq(min_max_swe[1], min_max_swe[2]))
unscaled_labels_swe <- as.character(unscaled_labels_swe)
unscaled_labels_prcp <- breaks_pretty(1)(seq(min_max_prcp[1], min_max_prcp[2]))
unscaled_labels_prcp <- as.character(unscaled_labels_prcp)

# Generate the corresponding scaled breaks, from the saved scaling attributes
scaled_breaks_forest <-  scale(unscaled_proportion_labels, scale_attributes_forest$`scaled:center`, scale_attributes_forest$`scaled:scale`)
scaled_breaks_open <-  scale(unscaled_proportion_labels, scale_attributes_open$`scaled:center`, scale_attributes_open$`scaled:scale`)
scaled_breaks_lichen <-  scale(unscaled_proportion_labels, scale_attributes_lichen$`scaled:center`, scale_attributes_lichen$`scaled:scale`)

####Plot####
###Forest/temp
a <- ggplot(predict_df_forest_temp, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic() +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_forest,
                     limits = c(min(scaled_breaks_forest), max(scaled_breaks_forest))) +
  scale_linetype_discrete(name = "Temperature", labels = unscaled_labels_temp, guide="none")+
  scale_fill_manual(guide="none", name = "Temperature", values=c("blue", "#999999", "red"),labels = unscaled_labels_temp) +
  xlab ("") + ylab("Relative probability of selection")


###Lichen/temp
b <- ggplot(predict_df_lichen_temp, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) + theme_classic() +
  theme(legend.position =  c("none"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_lichen,
                     limits = c(min(scaled_breaks_lichen), max(scaled_breaks_lichen))) +
  scale_linetype_discrete( name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual( name = "Temperature", values=c("blue", "#999999", "red"),labels = unscaled_labels_temp)+
  xlab ("") + ylab("")

ba <- b + theme(legend.position =  c("top"),
                       legend.box = "horizontal",
                legend.title = element_text(size=20),
                legend.text = element_text(size=18))

###Open/temp
c <- ggplot(predict_df_open_temp, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic() +
  theme(
    legend.title.align = 0.5,
    legend.position = c(.98, .98),
    legend.justification = c("right", "top"),
    legend.box.just = "top",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(size=20),
    legend.text = element_text(size=20),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20)
  ) +
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_open,
                     limits = c(min(scaled_breaks_open), max(scaled_breaks_open))) +
  scale_linetype_discrete(guide="none", name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(guide="none", name = "Temperature", values=c("blue", "#999999", "red"),labels = unscaled_labels_temp)+
    xlab ("") + ylab("")


###Forest/prcp
d <- ggplot(predict_df_forest_prcp, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)   + theme_classic() +
  theme(
         legend.title.align = 0.5,
         legend.position = c(.3, 1.07),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6),
         legend.title = element_text(size=20),
         legend.text = element_text(size=20),
         axis.text.y = element_text(size = 20),
         axis.text.x = element_text(size = 20),
         axis.title.y = element_text(size = 20),
         axis.title.x = element_text(size = 20)
       )+ 
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_forest,
                     limits = c(min(scaled_breaks_forest), max(scaled_breaks_forest))) +
  scale_linetype_discrete(name = "Precipitation", labels = unscaled_labels_prcp)+
  scale_fill_manual(name = "Precipitation", values=c("#666666", "#3399FF", "#0000FF"),labels = unscaled_labels_prcp)+
   xlab ("Proportion of forest") + ylab("Relative probability of selection")

###Lichen/snow
e <- ggplot(predict_df_lichen_swe, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic()+
  theme(
    legend.title.align = 0.5,
    legend.position = c(.3, 1.07),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(size=20),
    legend.text = element_text(size=20),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20)
    
  ) +
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_lichen,
                     limits = c(min(scaled_breaks_lichen), max(scaled_breaks_lichen))) +
  scale_linetype_discrete(name = "Snow water \n equivalent", labels = unscaled_labels_swe)+
  scale_fill_manual(name = "Snow water \n equivalent", values=c("blue", "#999999", "red"),labels = unscaled_labels_swe)+
  xlab ("Proportion of lichen") + ylab("")


###Open/prcp
f <- ggplot(predict_df_open_prcp, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic()+
  theme(
    legend.title.align = 0.5,
    legend.position = c(.32, 1.07),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(size=20),
    legend.text = element_text(size=20),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20)
  ) +
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_open,
                     limits = c(min(scaled_breaks_open), max(scaled_breaks_open))) +
  scale_linetype_discrete(name = "Precipitation", labels = unscaled_labels_prcp)+
  scale_fill_manual(name = "Precipitation", values=c("#666666", "#3399FF", "#0000FF"),labels = unscaled_labels_prcp) +
    xlab ("Proportion of open") + ylab("")

####
legend <- get_legend(
  # create some space to the left of the legend
  ba 
)

###save figure
pgrid <- plot_grid(a,b,c,d,e,f, align ='vh', hjust = -1,
                       labels = c("A"), 
                       ncol = 3, nrow=2, label_size = 35)

plot <- pgrid + draw_grob(legend, x =  0.0111, y =  0.480, scale = 0.020)

# plot_Fig4.A <- plot_grid(pgrid, legend)
# title <- ggdraw() + draw_label("Behavioural state", fontface='bold')
# 
# plot_Fig4.A <- plot_grid(plot_Fig4.A, ncol=1) # rel_heights values control title margins

save_plot("graphics/plot_Fig4.A.pdf", plot,
          ncol = 3, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
           base_asp = 1
)

save_plot("graphics/plot_Fig4.A.png", plot,
          ncol = 3, # we're saving a grid plot of 2 columns
          nrow = 3, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          # base_aspect_ratio = 1
          base_asp = 2
          # base_width = 6
)

####Plot####




##plot model odd ratio results####

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
# RSF_stop_df_scaled[c(10:15)] <- scale(RSF_stop_df_scaled[c(4:9)])
RSF_stop_df_scaled$prcp_sc <- scale(RSF_stop_df_scaled$prcp)
RSF_stop_df_scaled$swe_sc <- scale(RSF_stop_df_scaled$swe)
RSF_stop_df_scaled$tmax_sc <- scale(RSF_stop_df_scaled$tmax)
RSF_stop_df_scaled$Lichen_sc <- scale(RSF_stop_df_scaled$Lichen_200)
RSF_stop_df_scaled$Open_sc <- scale(RSF_stop_df_scaled$Open_200)
RSF_stop_df_scaled$Forest_sc <- scale(RSF_stop_df_scaled$Forest_200)

###as numeric
RSF_stop_df_scaled$prcp_sc <- as.numeric(RSF_stop_df_scaled$prcp_sc)
RSF_stop_df_scaled$swe_sc <- as.numeric(RSF_stop_df_scaled$swe_sc)
RSF_stop_df_scaled$tmax_sc <- as.numeric(RSF_stop_df_scaled$tmax_sc)
RSF_stop_df_scaled$Lichen_sc <- as.numeric(RSF_stop_df_scaled$Lichen_sc)
RSF_stop_df_scaled$Open_sc <- as.numeric(RSF_stop_df_scaled$Open_sc)
RSF_stop_df_scaled$Forest_sc <- as.numeric(RSF_stop_df_scaled$Forest_sc)


setDT(RSF_stop_df_scaled)

##Min max landcover
min_max_forest <- RSF_stop_df_scaled[, c(min(Forest_200, na.rm = TRUE),
                  max(Forest_200, na.rm = TRUE))]
scale_attributes_forest <- attributes(RSF_stop_df_scaled$Forest_sc)

min_max_lichen <- RSF_stop_df_scaled[, c(min(Lichen_200, na.rm = TRUE),
                                         max(Lichen_200, na.rm = TRUE))]
scale_attributes_lichen <- attributes(RSF_stop_df_scaled$Lichen_sc)

min_max_open <- RSF_stop_df_scaled[, c(min(Open_200, na.rm = TRUE),
                                         max(Open_200, na.rm = TRUE))]
scale_attributes_open <- attributes(RSF_stop_df_scaled$Open_sc)

##Min max weather
min_max_temp <- RSF_stop_df_scaled[, c(min(tmax, na.rm = TRUE),
                                       max(tmax, na.rm = TRUE))]

min_max_swe <- RSF_stop_df_scaled[, c(min(swe, na.rm = TRUE),
                                       max(swe, na.rm = TRUE))]

min_max_prcp <- RSF_stop_df_scaled[, c(min(prcp, na.rm = TRUE),
                                       max(prcp, na.rm = TRUE))]

####model RSF stop####
 model_interact_RSF_stop <- mclogit(cbind(Randoms, PtID)~ Open_sc + Lichen_sc + Forest_sc + tmax_sc +
                                      swe_sc+ prcp_sc + Open_sc*tmax_sc + Open_sc*swe_sc +
                                      Open_sc*prcp_sc + Lichen_sc*tmax_sc + Lichen_sc*swe_sc + Lichen_sc*prcp_sc +
                                      Forest_sc*tmax_sc + Forest_sc*swe_sc + Forest_sc*prcp_sc, data = RSF_stop_df_scaled)
 

summary(model_interact_RSF_stop)
logLik(model_interact_RSF_stop)

##Anova all models ####
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


##plot odd ratio _ RSF stopover ####
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
  terms = c("Forest_sc[all]", "tmax_sc")
)

#### test conversion scaled - unscaled ####

# predict_df$Forest_sc <- predict_df$x

# str(predict_df$group)
# options(digits=2)
# predict_df$tmax_sc <- as.numeric(as.character(predict_df$group))
# str(predict_df$tmax_sc)

# predict_df$Forest_200 <- unscale_after_predict(predict_df, RSF_stop_df_scaled, 'Forest_sc')
# print(unscale_after_predict(predict_df, RSF_stop_df_scaled, 'Forest_sc'))
# 
# levels(cut(RSF_stop_df_scaled$Forest_200, 5))
# 
# range(RSF_stop_df_scaled$Forest_200)
# 
# predict_df$tmax <- unscale_after_predict(predict_df, RSF_stop_df_scaled, 'tmax_sc')
# 
# range(RSF_stop_df_scaled$tmax)
# 
# predict_df$tmax <- as.numeric(predict_df$tmax)
# options(digits = 2)
# 
# predict_df$tmax <- format(predict_df$tmax, digits = 2)


#### interaction plot between lichen and tmax ####
predict_df_lichen_temp <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Lichen_sc[all]", "tmax_sc")
)


###### Interaction plot between open / tmax ####
predict_df_open_temp <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Open_sc[all]", "tmax_sc")
)

##### Interaction plot between Lichen/snow####
predict_df_lichen_swe <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Lichen_sc[all]", "swe_sc"))


##### Interaction plot between Forest/prcp####
predict_df_forest_prcp <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Forest_sc[all]", "prcp_sc"))


##### Interaction plot between Open/prcp####
predict_df_open_prcp <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Open_sc[all]", "prcp_sc"))


#### Plot interactions#### 
###unscaled labels#
print(min_max_forest)
#> [1] 32.1 59.6
unscaled_proportion_labels <- breaks_pretty()(seq(0, 1))

####
unscaled_labels_temp <- breaks_pretty(2)(seq(min_max_temp[1], min_max_temp[2]))
unscaled_labels_temp <- as.character(unscaled_labels_temp)
unscaled_labels_swe <- breaks_pretty(1)(seq(min_max_swe[1], min_max_swe[2]))
unscaled_labels_swe <- as.character(unscaled_labels_swe)
unscaled_labels_prcp <- breaks_pretty(1)(seq(min_max_prcp[1], min_max_prcp[2]))
unscaled_labels_prcp <- as.character(unscaled_labels_prcp)

# Generate the corresponding scaled breaks, from the saved scaling attributes
scaled_breaks_forest <-  scale(unscaled_proportion_labels, scale_attributes_forest$`scaled:center`, scale_attributes_forest$`scaled:scale`)
scaled_breaks_open <-  scale(unscaled_proportion_labels, scale_attributes_open$`scaled:center`, scale_attributes_open$`scaled:scale`)
scaled_breaks_lichen <-  scale(unscaled_proportion_labels, scale_attributes_lichen$`scaled:center`, scale_attributes_lichen$`scaled:scale`)

####Plot####
###Forest/temp
a <- ggplot(predict_df, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic() +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_forest,
                     limits = c(min(scaled_breaks_forest), max(scaled_breaks_forest))) +
  scale_linetype_discrete(guide = 'none', name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(guide = 'none', name = "Temperature", values=c("blue", "#999999", "red"),labels = unscaled_labels_temp)+
  xlab ("") + ylab("Relative probability of selection")


###Lichen/temp
b <- ggplot(predict_df_lichen_temp, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)  +theme_classic() +
  theme(legend.position = c("none"),
    axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_lichen,
                     limits = c(min(scaled_breaks_lichen), max(scaled_breaks_lichen))) +
  scale_linetype_discrete(name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(name = "Temperature", values=c("blue", "#999999", "red"),labels = unscaled_labels_temp)+
  xlab ("") + ylab("")

ba <- b + theme(legend.position =  c("top"),
                legend.box = "horizontal",
                legend.title = element_text(size=20),
                legend.text = element_text(size=18))


###Open/temp
c <- ggplot(predict_df_open_temp, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic() +
  theme(legend.position = c("none"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_open,
                     limits = c(min(scaled_breaks_open), max(scaled_breaks_open))) +
  scale_linetype_discrete(name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(name = "Temperature", values=c("blue", "#999999", "red"),labels = unscaled_labels_temp)+  
    xlab ("") + ylab("")


###Forest/prcp
d <- ggplot(predict_df_forest_prcp, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic() +
  theme(legend.title.align = 0.5,
        legend.position = c(.98, 1.07),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_forest,
                     limits = c(min(scaled_breaks_forest), max(scaled_breaks_forest))) +
  scale_linetype_discrete(name = "Precipitation", labels = unscaled_labels_prcp)+
  scale_fill_manual(name = "Precipitation", values=c("#666666", "#3399FF", "#0000FF"),labels = unscaled_labels_prcp)+
  xlab ("Proportion of forest") + ylab("Relative probability of selection")


###Lichen/snow
e <- ggplot(predict_df_lichen_swe, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) + theme_classic() +
  theme(legend.title.align = 0.5,
        legend.position = c(.3, 1.07),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_lichen,
                     limits = c(min(scaled_breaks_lichen), max(scaled_breaks_lichen))) +
  scale_linetype_discrete(name = "Snow water \n equivalent", labels = unscaled_labels_swe)+
  scale_fill_manual(name = "Snow water \n equivalent", values=c("blue", "#999999", "red"),labels = unscaled_labels_swe)+  
  xlab ("Proportion of lichen") + ylab("")


###Open/prcp
f <- ggplot(predict_df_open_prcp, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) + theme_classic() +
  theme(legend.title.align = 0.5,
        legend.position = c(.3, 1.07),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) +
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_open,
                     limits = c(min(scaled_breaks_open), max(scaled_breaks_open))) +
  scale_linetype_discrete(name = "Precipitation", labels = unscaled_labels_prcp)+
  scale_fill_manual(name = "Precipitation", values=c("#666666", "#3399FF", "#0000FF"),labels = unscaled_labels_prcp)+  
  xlab ("Proportion of open") + ylab("")


###plot  figure

legend <- get_legend(
  # create some space to the left of the legend
  ba 
)
pgrid2 <- plot_grid(a,b,c,d,e,f, align ='vh', hjust = -1,
                   labels = c("B"), 
                   ncol = 3, nrow=2, label_size = 35)

plot2 <- pgrid2 + draw_grob(legend, x =  0.0111, y =  0.480, scale = 0.020)

###save
save_plot("graphics/plot_Fig4.B.pdf", plot2,
          ncol = 3, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_asp = 1
)

save_plot("graphics/plot_Fig4.B.png", plot2,
          ncol = 3, # we're saving a grid plot of 2 columns
          nrow = 3, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          # base_aspect_ratio = 1
          base_asp = 2
          # base_width = 6
)


###

ggplot(predict_df, aes(x, predicted, group = group)) +
  geom_line(aes(color = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme_bw(base_size = 14) +
  xlab("Proportion of forest habitat") + ylab("Relative probability of selection")+
  labs(col="Temperature")



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
saveRDS(Final_MR_migration_RSF_stop, "output/Final_MR_migration_RSF_stop.RDS")

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

###create tab with all variable scaled###
RSF_mvt_df_scaled <- Final_MR_migration_RSF_mvt[c(1,11,24,28,30,31,45:47)]
# RSF_mvt_df_scaled[c(10:15)] <- scale(RSF_mvt_df_scaled[c(4:9)])
RSF_mvt_df_scaled$prcp_sc <- scale(RSF_mvt_df_scaled$prcp)
RSF_mvt_df_scaled$swe_sc <- scale(RSF_mvt_df_scaled$swe)
RSF_mvt_df_scaled$tmax_sc <- scale(RSF_mvt_df_scaled$tmax)
RSF_mvt_df_scaled$Lichen_sc <- scale(RSF_mvt_df_scaled$Lichen_200)
RSF_mvt_df_scaled$Open_sc <- scale(RSF_mvt_df_scaled$Open_200)
RSF_mvt_df_scaled$Forest_sc <- scale(RSF_mvt_df_scaled$Forest_200)

setDT(RSF_mvt_df_scaled)

##Min max landcover
min_max_forest <- RSF_mvt_df_scaled[, c(min(Forest_200, na.rm = TRUE),
                                         max(Forest_200, na.rm = TRUE))]
scale_attributes_forest <- attributes(RSF_mvt_df_scaled$Forest_sc)

min_max_lichen <- RSF_mvt_df_scaled[, c(min(Lichen_200, na.rm = TRUE),
                                         max(Lichen_200, na.rm = TRUE))]
scale_attributes_lichen <- attributes(RSF_mvt_df_scaled$Lichen_sc)

min_max_open <- RSF_mvt_df_scaled[, c(min(Open_200, na.rm = TRUE),
                                       max(Open_200, na.rm = TRUE))]
scale_attributes_open <- attributes(RSF_mvt_df_scaled$Open_sc)

##Min max weather
min_max_temp <- RSF_mvt_df_scaled[, c(min(tmax, na.rm = TRUE),
                                       max(tmax, na.rm = TRUE))]

min_max_swe <- RSF_mvt_df_scaled[, c(min(swe, na.rm = TRUE),
                                      max(swe, na.rm = TRUE))]

min_max_prcp <- RSF_mvt_df_scaled[, c(min(prcp, na.rm = TRUE),
                                       max(prcp, na.rm = TRUE))]
###with interaction hab + meteo _ movement #####
model_interact_RSF_mvt <- mclogit(cbind(Randoms, PtID)~  Open_sc + Lichen_sc + Forest_sc + tmax_sc +
  swe_sc+ prcp_sc + Open_sc*tmax_sc + Open_sc*swe_sc +
  Open_sc*prcp_sc + Lichen_sc*tmax_sc + Lichen_sc*swe_sc + Lichen_sc*prcp_sc +
  Forest_sc*tmax_sc + Forest_sc*swe_sc + Forest_sc*prcp_sc, data = RSF_mvt_df_scaled)

####summary
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

#### PLOT FIGURE 3.C####
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

save_plot("graphics/plot_Fig2.pdf", plot_Fig2,
          ncol = 3, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
)

######

#### interaction predicted values between lichen and tmax ####
predict_df_lichen_temp_mvt <- ggpredict(
  model_interact_RSF_stop,
  terms = c("Lichen_sc[all]", "tmax_sc")
)

#### interaction predicted values between forest and tmax ####
predict_df_forest_temp_mvt <- ggpredict(
  model_interact_RSF_mvt,
  terms = c("Forest_sc[all]", "tmax_sc")
)

#### interaction predicted values between open and tmax ####
predict_df_open_temp_mvt <- ggpredict(
  model_interact_RSF_mvt,
  terms = c("Open_sc[all]", "tmax_sc")
)


#### interaction predicted values between lichen and prcp ####
predict_df_lichen_prcp_mvt <- ggpredict(
  model_interact_RSF_mvt,
  terms = c("Lichen_sc[all]", "prcp_sc")
)


#### interaction predicted values between forest and prcp ####
predict_df_forest_prcp_mvt <- ggpredict(
  model_interact_RSF_mvt,
  terms = c("Forest_sc[all]", "prcp_sc")
)


#### interaction predicted values between open and prcp ####
predict_df_open_prcp_mvt <- ggpredict(
  model_interact_RSF_mvt,
  terms = c("Open_sc[all]", "prcp_sc")
)

#### interaction predicted values between open and swe ####
predict_df_open_swe_mvt <- ggpredict(
  model_interact_RSF_mvt,
  terms = c("Open_sc[all]", "swe_sc")
)


#### Plot interactions#### 
###unscaled labels#
print(min_max_forest)
#> [1] 32.1 59.6
unscaled_proportion_labels <- breaks_pretty()(seq(0, 1))

####
unscaled_labels_temp <- breaks_pretty(2)(seq(min_max_temp[1], min_max_temp[2]))
unscaled_labels_temp <- as.character(unscaled_labels_temp)
unscaled_labels_swe <- breaks_pretty(1)(seq(min_max_swe[1], min_max_swe[2]))
unscaled_labels_swe <- as.character(unscaled_labels_swe)
unscaled_labels_prcp <- breaks_pretty(1)(seq(min_max_prcp[1], min_max_prcp[2]))
unscaled_labels_prcp <- as.character(unscaled_labels_prcp)

# Generate the corresponding scaled breaks, from the saved scaling attributes
scaled_breaks_forest <-  scale(unscaled_proportion_labels, scale_attributes_forest$`scaled:center`, scale_attributes_forest$`scaled:scale`)
scaled_breaks_open <-  scale(unscaled_proportion_labels, scale_attributes_open$`scaled:center`, scale_attributes_open$`scaled:scale`)
scaled_breaks_lichen <-  scale(unscaled_proportion_labels, scale_attributes_lichen$`scaled:center`, scale_attributes_lichen$`scaled:scale`)


####Plot####
###Forest/temp
a <- ggplot(predict_df_forest_temp_mvt, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic() +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_forest,
                     limits = c(min(scaled_breaks_forest), max(scaled_breaks_forest))) +
  scale_linetype_discrete(guide = "none", name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(guide = "none", name = "Temperature", values=c("blue", "#999999", "red"),labels = unscaled_labels_temp) +
  xlab ("") + ylab("Relative probability of selection")


###Lichen/temp
b <- ggplot(predict_df_lichen_temp_mvt, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic() +
  theme(legend.position = c("none"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_lichen,
                     limits = c(min(scaled_breaks_lichen), max(scaled_breaks_lichen))) +
  scale_linetype_discrete(name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(name = "Temperature", values=c("blue", "#999999", "red"),labels = unscaled_labels_temp) +
  xlab ("") + ylab("")

ba <- b + theme(legend.position =  c("top"),
                legend.box = "horizontal",
                legend.title = element_text(size=20),
                legend.text = element_text(size=18))

###Open/temp
c <- ggplot(predict_df_open_temp_mvt, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic() +
  theme(legend.position = c("none"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_open,
                     limits = c(min(scaled_breaks_open), max(scaled_breaks_open))) +
  scale_linetype_discrete(name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(name = "Temperature", values=c("blue", "#999999", "red"),labels = unscaled_labels_temp)+
  xlab ("") + ylab("")




###Forest/prcp
d <- ggplot(predict_df_forest_prcp_mvt, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) + theme_classic() +
  theme(legend.title.align = 0.5,
        legend.position = c(.98, 1.07),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_forest,
                     limits = c(min(scaled_breaks_forest), max(scaled_breaks_forest))) +
  scale_linetype_discrete(name = "Precipitation", labels = unscaled_labels_prcp)+
  scale_fill_manual(name = "Precipitation", values=c("#666666", "#3399FF", "#0000FF"),labels = unscaled_labels_prcp)+
  xlab ("Proportion of forest") + ylab("Relative probability of selection")


###Lichen/prcp
e <- ggplot(predict_df_lichen_prcp_mvt, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic() +
  theme(legend.title.align = 0.5,
        legend.position = c(.3, 1.07),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_lichen,
                     limits = c(min(scaled_breaks_lichen), max(scaled_breaks_lichen))) +
  scale_linetype_discrete(name = "Precipitation", labels = unscaled_labels_swe)+
  scale_fill_manual(name = "Precipitation", values=c("blue", "#999999", "red"),labels = unscaled_labels_swe) +
  xlab ("Proportion of lichen") + ylab("")

###Open/prcp
# f <- ggplot(predict_df_open_prcp_mvt, aes(x, predicted, fill = group)) +
#   geom_line(aes(linetype = group), size = 0.8) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
#   scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_open,
#                      limits = c(min(scaled_breaks_open), max(scaled_breaks_open))) +
#   scale_linetype_discrete(name = "Precipitation", labels = unscaled_labels_prcp)+
#   scale_fill_manual(name = "Precipitation", values=c("#666666", "#3399FF", "#0000FF"),labels = unscaled_labels_prcp)+
#   theme_classic() +  xlab ("Proportion of open") + ylab("Relative probability of selection")
# 


###Open/swe
f <- ggplot(predict_df_open_swe_mvt, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +theme_classic() +
  theme(legend.title.align = 0.5,
        legend.position = c(.3, 1.07),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) +
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_open,
                     limits = c(min(scaled_breaks_open), max(scaled_breaks_open))) +
  scale_linetype_discrete(name = "Precipitation", labels = unscaled_labels_swe)+
  scale_fill_manual(name = "Precipitation", values=c("#666666", "#3399FF", "#0000FF"),labels = unscaled_labels_swe)+
  xlab ("Proportion of open") + ylab("")


###

###plot  figure

legend <- get_legend(
  # create some space to the left of the legend
  ba 
)
pgrid3 <- plot_grid(a,b,c,d,e,f, align ='vh', hjust = -1,
                    labels = c("C"), 
                    ncol = 3, nrow=2, label_size = 35, label_x = -0.03, label_y = 1.03)

plot3 <- pgrid3 + draw_grob(legend, x =  0.0111, y =  0.480, scale = 0.020)

###save
save_plot("graphics/plot_Fig4.C.pdf", plot3,
          ncol = 3, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_asp = 1
)

save_plot("graphics/plot_Fig4.C.png", plot3,
          ncol = 3, # we're saving a grid plot of 2 columns
          nrow = 3, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          # base_aspect_ratio = 1
          base_asp = 2
          # base_width = 6
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
saveRDS(Final_MR_migration_RSF_mvt, "output/Final_MR_migration_RSF_mvt.RDS")


