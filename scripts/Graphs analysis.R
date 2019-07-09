#############################################
############ MoveHMM model ##################

##load package
library(dplyr)
library(ggplot2)

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
  geom_boxplot() +
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


##Pt ID ~ NDVI model 
logit <- glm(state~scale(NDVI), data=testcorr, family=binomial)
summary(logit)
summ(logit)
library(lme4)
effect_plot(logit, pred = NDVI, interval = TRUE, plot.points = TRUE)
lapply(logit, class)[1:3]
logit$aic
predict <- predict(logit, testcorr, type = 'response')
# confusion matrix
table_mat <- table(testcorr$state, predict > 0.5)
table_mat
#PT ID ~ prcp model 
prcpmodel <- glm(state~prcp, data=testcorr, family=binomial)
summary(prcpmodel)

weathermodel <- glm(state ~ prcp + swe + tmax + tmin , data = testcorr, family = binomial)
summary(weathermodel)

all2011 <- subset(allNDVI, Year == '2011')
ggplot(all2011,aes(JDate,tmax))+stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)
