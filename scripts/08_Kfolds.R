library(mclogit)
library(piecewiseSEM)


formula<-HMM ~ NDVISc + tmaxSc + prcpSc + LichenSc + WetlandSc + 
  ForestSc + RockySc + WaterSc + LichenSc*NDVISc + WetlandSc*NDVISc + RockySc*NDVISc +
  ForestSc*NDVISc + LichenSc*tmaxSc + RockySc*tmaxSc + WetlandSc*prcpSc + ForestSc*prcpSc + 
  NDVISc*tmaxSc + (1|Animal_ID) + (1|Year)


head(allNDVIobs)
data<-allNDVIobs

data$Animal_ID<-droplevels(data$Animal_ID)
data$prcpSc<-scale(data$prcp)
data$tmaxSc<-scale(data$tmax)
data$sweSc<-scale(data$swe)
data$NDVISc<-scale(data$NDVI)
data$WetlandSc<-scale(data$Wetland)
data$ForestSc<-scale(data$Forest)
data$LichenSc<-scale(data$Lichen)
data$RockySc<-scale(data$Rocky)
data$WaterSc<-scale(data$Water)

data <- subset(data, select = -c(13:18,22:23, 25:27))
data <- subset(data, select = -c(4:5,17:30))

## PART 7 - Model evaluation sensu Boyce et al. 2002 - Individual block cross-validation
## -------------------------------------------------------------------------------------
##
## 5-fold cross-validation with data split by randomly assigning all GPS fixes from a single individual to a given fold, resulting in 3 folds with 9 individuals each and 2 folds with 8 individuals each. Home ranges of individuals assigned to different folds may overlap.    

# Remove columns used for evaluations fit earlier
data$rand.vec<- NULL   
data$rand.vec.x <- NULL 
data$rand.vec.y <- NULL 
# split individuals randomly
newdata <- data.frame(Animal_ID = unique(data$Animal_ID))
set.seed(5)
random_sample <- data.frame(Animal_ID = sample(newdata$Animal_ID, 30))
random_sample$rand.vec <- 0
random_sample$rand.vec[1:6] <- 1
random_sample$rand.vec[7:12] <- 2
random_sample$rand.vec[13:18] <- 3
random_sample$rand.vec[19:24] <- 4
random_sample$rand.vec[25:30] <- 5
with(random_sample, tapply(Animal_ID, rand.vec, length))
data <- merge(data, random_sample, by = "Animal_ID", all.x = T )
with(data, tapply(Animal_ID, rand.vec, unique))

# Fit the model in all folds but one.
#################################  SLOW STEP  ##################################
mod_inter_randomIND1 <- glmer(formula, data[data$rand.vec != 1,], family = "binomial")
mod_inter_randomIND2 <- glmer(formula, data[data$rand.vec != 2,], family = "binomial")
mod_inter_randomIND3 <- glmer(formula, data[data$rand.vec != 3,], family = "binomial")
mod_inter_randomIND4 <- glmer(formula, data[data$rand.vec != 4,], family = "binomial")
mod_inter_randomIND5 <- glmer(formula, data[data$rand.vec != 5,], family = "binomial")
#save(mod_inter_randomIND1, file = "mod_inter_randomIND1.RData")
#save(mod_inter_randomIND2, file = "mod_inter_randomIND2.RData")
#save(mod_inter_randomIND3, file = "mod_inter_randomIND3.RData")
#save(mod_inter_randomIND4, file = "mod_inter_randomIND4.RData")
#save(mod_inter_randomIND5, file = "mod_inter_randomIND5.RData")
# load(file = "mod_inter_randomIND1.RData")
# load(file = "mod_inter_randomIND2.RData")
# load(file = "mod_inter_randomIND3.RData")
# load(file = "mod_inter_randomIND4.RData")
# load(file = "mod_inter_randomIND5.RData")

# Remove columns used for evaluations fit earlier
data$lc_30_contribute <- NULL
data$RSFscores <- NULL

# Calculation of the contribution to RSF scores by the levels of the categorical predictor (reference category: coniferous forest) and calculation of RSF scores assuming the exponential form. Repeating it for each withhold fold.

# NOTE THE ATTACH COMMAND HERE - DETACH BELOW
attach(data)

 data$RSFscores[rand.vec == 1] = exp(fixef(mod_inter_randomIND1)[2] * data$NDVISc[rand.vec == 1]+
                                      fixef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 1]+
                                      fixef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 1]+
                                      fixef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 1] + 
                                      fixef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 1] +
                                      fixef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 1] +
                                      fixef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 1] + 
                                      fixef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 1] +
                                      fixef(mod_inter_randomIND1)[10] * data$NDVISc[rand.vec == 1] *
                                      data$LichenSc[rand.vec == 1] +
                                      fixef(mod_inter_randomIND1)[11] * data$NDVISc[rand.vec == 1] *
                                      data$WetlandSc[rand.vec == 1] + 
                                      fixef(mod_inter_randomIND1)[12] * data$NDVISc[rand.vec == 1] *
                                      data$RockySc[rand.vec == 1] +
                                      fixef(mod_inter_randomIND1)[13] * data$NDVISc[rand.vec == 1] *
                                      data$ForestSc[rand.vec == 1] + 
                                      fixef(mod_inter_randomIND1)[14] * data$tmaxSc[rand.vec == 1] *
                                      data$LichenSc[rand.vec == 1] + 
                                      fixef(mod_inter_randomIND1)[15] * data$tmaxSc[rand.vec == 1] *
                                      data$RockySc[rand.vec == 1] + 
                                      fixef(mod_inter_randomIND1)[16] * data$prcpSc[rand.vec == 1] *
                                      data$WetlandSc[rand.vec == 1] + 
                                      fixef(mod_inter_randomIND1)[17] * data$prcpSc[rand.vec == 1] *
                                      data$ForestSc[rand.vec == 1] +
                                      fixef(mod_inter_randomIND1)[18] * data$NDVISc[rand.vec == 1] *
                                      data$tmaxSc[rand.vec == 1]) 
data$RSFscores[rand.vec == 2] = exp(fixef(mod_inter_randomIND1)[2] * data$NDVISc[rand.vec == 2]+
                                      fixef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 2]+
                                      fixef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 2]+
                                      fixef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 2] + 
                                      fixef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 2] +
                                      fixef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 2] +
                                      fixef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 2] + 
                                      fixef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 2] +
                                      fixef(mod_inter_randomIND1)[10] * data$NDVISc[rand.vec == 2] *
                                      data$LichenSc[rand.vec == 2] +
                                      fixef(mod_inter_randomIND1)[11] * data$NDVISc[rand.vec == 2] *
                                      data$WetlandSc[rand.vec == 2] + 
                                      fixef(mod_inter_randomIND1)[12] * data$NDVISc[rand.vec == 2] *
                                      data$RockySc[rand.vec == 2] +
                                      fixef(mod_inter_randomIND1)[13] * data$NDVISc[rand.vec == 2] *
                                      data$ForestSc[rand.vec == 2] + 
                                      fixef(mod_inter_randomIND1)[14] * data$tmaxSc[rand.vec == 2] *
                                      data$LichenSc[rand.vec == 2] + 
                                      fixef(mod_inter_randomIND1)[15] * data$tmaxSc[rand.vec == 2] *
                                      data$RockySc[rand.vec == 2] + 
                                      fixef(mod_inter_randomIND1)[16] * data$prcpSc[rand.vec == 2] *
                                      data$WetlandSc[rand.vec == 2] + 
                                      fixef(mod_inter_randomIND1)[17] * data$prcpSc[rand.vec == 2] *
                                      data$ForestSc[rand.vec == 2] +
                                      fixef(mod_inter_randomIND1)[18] * data$NDVISc[rand.vec == 2] *
                                      data$tmaxSc[rand.vec == 2])
data$RSFscores[rand.vec == 3] = exp(fixef(mod_inter_randomIND1)[2] * data$NDVISc[rand.vec == 3]+
                                      fixef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 3]+
                                      fixef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 3]+
                                      fixef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 3] + 
                                      fixef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 3] +
                                      fixef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 3] +
                                      fixef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 3] + 
                                      fixef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 3] +
                                      fixef(mod_inter_randomIND1)[10] * data$NDVISc[rand.vec == 3] *
                                      data$LichenSc[rand.vec == 3] +
                                      fixef(mod_inter_randomIND1)[11] * data$NDVISc[rand.vec == 3] *
                                      data$WetlandSc[rand.vec == 3] + 
                                      fixef(mod_inter_randomIND1)[12] * data$NDVISc[rand.vec == 3] *
                                      data$RockySc[rand.vec == 3] +
                                      fixef(mod_inter_randomIND1)[13] * data$NDVISc[rand.vec == 3] *
                                      data$ForestSc[rand.vec == 3] + 
                                      fixef(mod_inter_randomIND1)[14] * data$tmaxSc[rand.vec == 3] *
                                      data$LichenSc[rand.vec == 3] + 
                                      fixef(mod_inter_randomIND1)[15] * data$tmaxSc[rand.vec == 3] *
                                      data$RockySc[rand.vec == 3] + 
                                      fixef(mod_inter_randomIND1)[16] * data$prcpSc[rand.vec == 3] *
                                      data$WetlandSc[rand.vec == 3] + 
                                      fixef(mod_inter_randomIND1)[17] * data$prcpSc[rand.vec == 3] *
                                      data$ForestSc[rand.vec == 3] +
                                      fixef(mod_inter_randomIND1)[18] * data$NDVISc[rand.vec == 3] *
                                      data$tmaxSc[rand.vec == 3])
data$RSFscores[rand.vec == 4] = exp(fixef(mod_inter_randomIND1)[2] * data$NDVISc[rand.vec == 4]+
                                      fixef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 4]+
                                      fixef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 4]+
                                      fixef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 4] + 
                                      fixef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 4] +
                                      fixef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 4] +
                                      fixef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 4] + 
                                      fixef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 4] +
                                      fixef(mod_inter_randomIND1)[10] * data$NDVISc[rand.vec == 4] *
                                      data$LichenSc[rand.vec == 4] +
                                      fixef(mod_inter_randomIND1)[11] * data$NDVISc[rand.vec == 4] *
                                      data$WetlandSc[rand.vec == 4] + 
                                      fixef(mod_inter_randomIND1)[12] * data$NDVISc[rand.vec == 4] *
                                      data$RockySc[rand.vec == 4] +
                                      fixef(mod_inter_randomIND1)[13] * data$NDVISc[rand.vec == 4] *
                                      data$ForestSc[rand.vec == 4] + 
                                      fixef(mod_inter_randomIND1)[14] * data$tmaxSc[rand.vec == 4] *
                                      data$LichenSc[rand.vec == 4] + 
                                      fixef(mod_inter_randomIND1)[15] * data$tmaxSc[rand.vec == 4] *
                                      data$RockySc[rand.vec == 4] + 
                                      fixef(mod_inter_randomIND1)[16] * data$prcpSc[rand.vec == 4] *
                                      data$WetlandSc[rand.vec == 4] + 
                                      fixef(mod_inter_randomIND1)[17] * data$prcpSc[rand.vec == 4] *
                                      data$ForestSc[rand.vec == 4] +
                                      fixef(mod_inter_randomIND1)[18] * data$NDVISc[rand.vec == 4] *
                                      data$tmaxSc[rand.vec == 4])
data$RSFscores[rand.vec == 5] = exp(fixef(mod_inter_randomIND1)[2] * data$NDVISc[rand.vec == 5]+
                                      fixef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 5]+
                                      fixef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 5]+
                                      fixef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 5] + 
                                      fixef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 5] +
                                      fixef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 5] +
                                      fixef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 5] + 
                                      fixef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 5] +
                                      fixef(mod_inter_randomIND1)[10] * data$NDVISc[rand.vec == 5] *
                                      data$LichenSc[rand.vec == 5] +
                                      fixef(mod_inter_randomIND1)[11] * data$NDVISc[rand.vec == 5] *
                                      data$WetlandSc[rand.vec == 5] + 
                                      fixef(mod_inter_randomIND1)[12] * data$NDVISc[rand.vec == 5] *
                                      data$RockySc[rand.vec == 5] +
                                      fixef(mod_inter_randomIND1)[13] * data$NDVISc[rand.vec == 5] *
                                      data$ForestSc[rand.vec == 5] + 
                                      fixef(mod_inter_randomIND1)[14] * data$tmaxSc[rand.vec == 5] *
                                      data$LichenSc[rand.vec == 5] + 
                                      fixef(mod_inter_randomIND1)[15] * data$tmaxSc[rand.vec == 5] *
                                      data$RockySc[rand.vec == 5] + 
                                      fixef(mod_inter_randomIND1)[16] * data$prcpSc[rand.vec == 5] *
                                      data$WetlandSc[rand.vec == 5] + 
                                      fixef(mod_inter_randomIND1)[17] * data$prcpSc[rand.vec == 5] *
                                      data$ForestSc[rand.vec == 5] +
                                      fixef(mod_inter_randomIND1)[18] * data$NDVISc[rand.vec == 5] *
                                      data$tmaxSc[rand.vec == 5])

detach(data)

head(data)

# Run the k-fold CV evaluation sensu Boyce et al. 2002
dataset <- data[complete.cases(data[,"RSFscores"]),]
rho_model <- numeric(5) ## it will store Spearman's coefficients

fold <- subset(dataset,rand.vec==unique(dataset$rand.vec)[1]) # run the procedure for the 1st fold - the for-loop below will work on folds 2 to 5
q.pp <- quantile(fold$RSFscores,probs=seq(0,1,.1)) ## computing quantiles of RSF scores
# --------------------------------------------------------
bin <- rep(NA,length(fold$RSFscores))
for (j in 1:10){
  bin[fold$RSFscores>=q.pp[j]& fold$RSFscores<q.pp[j+1]] = j ## binning RSF scores (10 bins)
}
used <- fold$HMM

# --------------------------------------------------------
a <- table(used,bin) ## Occurrence of presence and available data by bin
a <- t(a) #transpose the table
a <- as.data.frame.matrix(a) ## the next few lines compute area-adjusted frequency of categories (bins) of RSF scores 
a$areaadjusted <- rep(NA,length(10))
sum0 <- sum(a[,1])
sum1 <- sum(a[,2])
a$areaadjusted <- (a[,2] / sum1 ) / (a[,1] / sum0)
a$bins <- seq(1,10,by=1);a

# --------------------------------------------------------
rho_model[1] <- with(a,cor.test(bins,areaadjusted,method="spearm"))$estimate ## Spearman correlation coefficient between RSF bin ranks and area-adjusted frequencies
# --------------------------------------------------------

# Run the procedure for the other folds and plot the binned RSF scores from the cross-validation
png("graphics/KfoldmoveHMM3.png", height=360, width=600)

par(oma=c(1,2,1,1)) 
par(mar=c(4.2,4.2,2,2))
with(a,plot(bins,areaadjusted, ylab="Area adjusted frequency", xlab="Binned RSF scores",xlim=c(1,10),ylim=c(0,2.5),type="b",cex=1.4,cex.lab=1.5,las=1,main="K-fold validation"))
abline(h = 1, lty = 3) ## Spearman correlation coefficient between RSF bin ranks and area-adjusted frequencies
plot_random_ind <- data.frame(a[,3], b = NA, c = NA, d = NA, e = NA)
for (i in 2:5){
  fold <- subset(dataset,rand.vec==unique(dataset$rand.vec)[i]) ## run the procedure on folds 2 to 5
  # --------------------------------------------------------
  q.pp <- quantile(fold$RSFscores,probs=seq(0,1,.1)) ## computing quantiles of RSF scores
  # --------------------------------------------------------
  bin <- rep(NA,length(fold$RSFscores))
  for (j in 1:10){
    bin[fold$RSFscores>=q.pp[j]& fold$RSFscores<q.pp[j+1]] = j  ## binning RSF scores (10 bins)
  }
  used <- fold$HMM
  # --------------------------------------------------------
  a <- table(used,bin) ## area adjusted freq in used/available for each bin
  a <- t(a) #transpose the table
  a <- as.data.frame.matrix(a) ## the next few lines compute area-adjusted frequency of categories (bins) of RSF scores 
  a$areaadjusted <- rep(NA,length(10))
  sum0 <- sum(a[,1])
  sum1 <- sum(a[,2])
  a$areaadjusted <- (a[,2] / sum1 ) / (a[,1] / sum0)
  a$bins <- seq(1,10,by=1);a
  # --------------------------------------------------------
  rho_model[i] <- with(a,cor.test(bins,areaadjusted,method="spearm"))$estimate ## store Spearman correlation coefficients between RSF bin ranks and area-adjusted frequencies
  # --------------------------------------------------------
  par(oma=c(1,2,1,1)) 
  par(mar=c(4.2,4.2,2,2))
  with(a,points(bins,areaadjusted,col=i,type="b"))  
  plot_random_ind[,i] = a[,3]
}

dev.off()

## store Spearman correlation coefficients that will be used for final plots below ##
Rho_random_individuals <- rho_model

mean(Rho_random_individuals)
sd(Rho_random_individuals)

######################################################################################################
######################################################################################################
#############K-fold extraction with best model RSF (Global with Habitat/Weather interaction)##########
formula<-cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc + WetlandSc + ForestSc + RockySc + WaterSc + LichenSc:sweSc + WetlandSc:sweSc + RockySc:sweSc + prcpSc:ForestSc  + prcpSc:WetlandSc + tmaxSc:LichenSc + tmaxSc:WetlandSc + tmaxSc:RockySc + NDVISc:tmaxSc


head(allNDVI_stop)
data<-allNDVI_stop

data$Animal_ID<-droplevels(data$Animal_ID)
data$prcpSc<-scale(data$prcp)
data$tmaxSc<-scale(data$tmax)
data$sweSc<-scale(data$swe)
data$NDVISc<-scale(data$NDVI)
data$WetlandSc<-scale(data$Wetland)
data$ForestSc<-scale(data$Forest)
data$LichenSc<-scale(data$Lichen)
data$RockySc<-scale(data$Rocky)
data$WaterSc<-scale(data$Water)

data <- subset(data, select = -c(13:18,22:23, 25:27, 33:35))
data <- subset(data, select = -c(4:5,17:30))

## PART 7 - Model evaluation sensu Boyce et al. 2002 - Individual block cross-validation
## -------------------------------------------------------------------------------------
##
## 5-fold cross-validation with data split by randomly assigning all GPS fixes from a single individual to a given fold, resulting in 3 folds with 9 individuals each and 2 folds with 8 individuals each. Home ranges of individuals assigned to different folds may overlap.    

# Remove columns used for evaluations fit earlier
data$rand.vec<- NULL   
data$rand.vec.x <- NULL 
data$rand.vec.y <- NULL 
# split individuals randomly
newdata <- data.frame(Animal_ID = unique(data$Animal_ID))
set.seed(5)
random_sample <- data.frame(Animal_ID = sample(newdata$Animal_ID, 30))
random_sample$rand.vec <- 0
random_sample$rand.vec[1:6] <- 1
random_sample$rand.vec[7:12] <- 2
random_sample$rand.vec[13:18] <- 3
random_sample$rand.vec[19:24] <- 4
random_sample$rand.vec[25:30] <- 5
with(random_sample, tapply(Animal_ID, rand.vec, length))
data <- merge(data, random_sample, by = "Animal_ID", all.x = T )
with(data, tapply(Animal_ID, rand.vec, unique))

# Fit the model in all folds but one.
#################################  SLOW STEP  ##################################
mod_inter_randomIND1 <- mclogit(formula, data[data$rand.vec != 1,])
mod_inter_randomIND2 <- mclogit(formula, data[data$rand.vec != 2,])
mod_inter_randomIND3 <- mclogit(formula, data[data$rand.vec != 3,])
mod_inter_randomIND4 <- mclogit(formula, data[data$rand.vec != 4,])
mod_inter_randomIND5 <- mclogit(formula, data[data$rand.vec != 5,])
#save(mod_inter_randomIND1, file = "mod_inter_randomIND1.RData")
#save(mod_inter_randomIND2, file = "mod_inter_randomIND2.RData")
#save(mod_inter_randomIND3, file = "mod_inter_randomIND3.RData")
#save(mod_inter_randomIND4, file = "mod_inter_randomIND4.RData")
#save(mod_inter_randomIND5, file = "mod_inter_randomIND5.RData")
# load(file = "mod_inter_randomIND1.RData")
# load(file = "mod_inter_randomIND2.RData")
# load(file = "mod_inter_randomIND3.RData")
# load(file = "mod_inter_randomIND4.RData")
# load(file = "mod_inter_randomIND5.RData")

# Remove columns used for evaluations fit earlier
data$lc_30_contribute <- NULL
data$RSFscores <- NULL

# Calculation of the contribution to RSF scores by the levels of the categorical predictor (reference category: coniferous forest) and calculation of RSF scores assuming the exponential form. Repeating it for each withhold fold.

# NOTE THE ATTACH COMMAND HERE - DETACH BELOW
attach(data)

data$RSFscores[rand.vec == 1] = exp(coef(mod_inter_randomIND1)[1] * data$NDVISc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[2] * data$sweSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 1] + 
                                      coef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[10] * data$LichenSc[rand.vec == 1] * 
                                      data$sweSc[rand.vec == 1] +
                                      coef(mod_inter_randomIND1)[11] * data$WetlandSc[rand.vec == 1] * 
                                      data$sweSc[rand.vec == 1] +
                                      coef(mod_inter_randomIND1)[12] * data$RockySc[rand.vec == 1] *
                                      data$sweSc[rand.vec == 1] +
                                      coef(mod_inter_randomIND1)[13] * data$prcpSc[rand.vec == 1] * 
                                      data$ForestSc[rand.vec == 1] +
                                      coef(mod_inter_randomIND1)[14] * data$prcpSc[rand.vec == 1] *
                                      data$WetlandSc[rand.vec == 1] +
                                      coef(mod_inter_randomIND1)[15] * data$tmaxSc[rand.vec == 1] *
                                      data$LichenSc[rand.vec == 1] + 
                                      coef(mod_inter_randomIND1)[16] * data$tmaxSc[rand.vec == 1] *
                                      data$WetlandSc[rand.vec == 1] + 
                                      coef(mod_inter_randomIND1)[17] * data$tmaxSc[rand.vec == 1] *
                                      data$RockySc[rand.vec == 1] + 
                                      coef(mod_inter_randomIND1)[18] * data$NDVISc[rand.vec == 1] *
                                      data$tmaxSc[rand.vec == 1])
data$RSFscores[rand.vec == 2] = exp(coef(mod_inter_randomIND1)[1] * data$NDVISc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[2] * data$sweSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 2] + 
                                      coef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[10] * data$LichenSc[rand.vec == 2] * 
                                      data$sweSc[rand.vec == 2] +
                                      coef(mod_inter_randomIND1)[11] * data$WetlandSc[rand.vec == 2] * 
                                      data$sweSc[rand.vec == 2] +
                                      coef(mod_inter_randomIND1)[12] * data$RockySc[rand.vec == 2] *
                                      data$sweSc[rand.vec == 2] +
                                      coef(mod_inter_randomIND1)[13] * data$prcpSc[rand.vec == 2] * 
                                      data$ForestSc[rand.vec == 2] +
                                      coef(mod_inter_randomIND1)[14] * data$prcpSc[rand.vec == 2] *
                                      data$WetlandSc[rand.vec == 2] +
                                      coef(mod_inter_randomIND1)[15] * data$tmaxSc[rand.vec == 2] *
                                      data$LichenSc[rand.vec == 2] + 
                                      coef(mod_inter_randomIND1)[16] * data$tmaxSc[rand.vec == 2] *
                                      data$WetlandSc[rand.vec == 2] + 
                                      coef(mod_inter_randomIND1)[17] * data$tmaxSc[rand.vec == 2] *
                                      data$RockySc[rand.vec == 2] + 
                                      coef(mod_inter_randomIND1)[18] * data$NDVISc[rand.vec == 2] *
                                      data$tmaxSc[rand.vec == 2])
data$RSFscores[rand.vec == 3] = exp(coef(mod_inter_randomIND1)[1] * data$NDVISc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[2] * data$sweSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 3] + 
                                      coef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[10] * data$LichenSc[rand.vec == 3] * 
                                      data$sweSc[rand.vec == 3] +
                                      coef(mod_inter_randomIND1)[11] * data$WetlandSc[rand.vec == 3] * 
                                      data$sweSc[rand.vec == 3] +
                                      coef(mod_inter_randomIND1)[12] * data$RockySc[rand.vec == 3] *
                                      data$sweSc[rand.vec == 3] +
                                      coef(mod_inter_randomIND1)[13] * data$prcpSc[rand.vec == 3] * 
                                      data$ForestSc[rand.vec == 3] +
                                      coef(mod_inter_randomIND1)[14] * data$prcpSc[rand.vec == 3] *
                                      data$WetlandSc[rand.vec == 3] +
                                      coef(mod_inter_randomIND1)[15] * data$tmaxSc[rand.vec == 3] *
                                      data$LichenSc[rand.vec == 3] + 
                                      coef(mod_inter_randomIND1)[16] * data$tmaxSc[rand.vec == 3] *
                                      data$WetlandSc[rand.vec == 3] + 
                                      coef(mod_inter_randomIND1)[17] * data$tmaxSc[rand.vec == 3] *
                                      data$RockySc[rand.vec == 3] + 
                                      coef(mod_inter_randomIND1)[18] * data$NDVISc[rand.vec == 3] *
                                      data$tmaxSc[rand.vec == 3])
data$RSFscores[rand.vec == 4] = exp(coef(mod_inter_randomIND1)[1] * data$NDVISc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[2] * data$sweSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 4] + 
                                      coef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[10] * data$LichenSc[rand.vec == 4] * 
                                      data$sweSc[rand.vec == 4] +
                                      coef(mod_inter_randomIND1)[11] * data$WetlandSc[rand.vec == 4] * 
                                      data$sweSc[rand.vec == 4] +
                                      coef(mod_inter_randomIND1)[12] * data$RockySc[rand.vec == 4] *
                                      data$sweSc[rand.vec == 4] +
                                      coef(mod_inter_randomIND1)[13] * data$prcpSc[rand.vec == 4] * 
                                      data$ForestSc[rand.vec == 4] +
                                      coef(mod_inter_randomIND1)[14] * data$prcpSc[rand.vec == 4] *
                                      data$WetlandSc[rand.vec == 4] +
                                      coef(mod_inter_randomIND1)[15] * data$tmaxSc[rand.vec == 4] *
                                      data$LichenSc[rand.vec == 4] + 
                                      coef(mod_inter_randomIND1)[16] * data$tmaxSc[rand.vec == 4] *
                                      data$WetlandSc[rand.vec == 4] + 
                                      coef(mod_inter_randomIND1)[17] * data$tmaxSc[rand.vec == 4] *
                                      data$RockySc[rand.vec == 4] + 
                                      coef(mod_inter_randomIND1)[18] * data$NDVISc[rand.vec == 4] *
                                      data$tmaxSc[rand.vec == 4])
data$RSFscores[rand.vec == 5] = exp(coef(mod_inter_randomIND1)[1] * data$NDVISc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[2] * data$sweSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 5] + 
                                      coef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[10] * data$LichenSc[rand.vec == 5] * 
                                      data$sweSc[rand.vec == 5] +
                                      coef(mod_inter_randomIND1)[11] * data$WetlandSc[rand.vec == 5] * 
                                      data$sweSc[rand.vec == 5] +
                                      coef(mod_inter_randomIND1)[12] * data$RockySc[rand.vec == 5] *
                                      data$sweSc[rand.vec == 5] +
                                      coef(mod_inter_randomIND1)[13] * data$prcpSc[rand.vec == 5] * 
                                      data$ForestSc[rand.vec == 5] +
                                      coef(mod_inter_randomIND1)[14] * data$prcpSc[rand.vec == 5] *
                                      data$WetlandSc[rand.vec == 5] +
                                      coef(mod_inter_randomIND1)[15] * data$tmaxSc[rand.vec == 5] *
                                      data$LichenSc[rand.vec == 5] + 
                                      coef(mod_inter_randomIND1)[16] * data$tmaxSc[rand.vec == 5] *
                                      data$WetlandSc[rand.vec == 5] + 
                                      coef(mod_inter_randomIND1)[17] * data$tmaxSc[rand.vec == 5] *
                                      data$RockySc[rand.vec == 5] + 
                                      coef(mod_inter_randomIND1)[18] * data$NDVISc[rand.vec == 5] *
                                      data$tmaxSc[rand.vec == 5])




detach(data)

head(data)

# Run the k-fold CV evaluation sensu Boyce et al. 2002
dataset <- data[complete.cases(data[,"RSFscores"]),]
rho_model <- numeric(5) ## it will store Spearman's coefficients

fold <- subset(dataset,rand.vec==unique(dataset$rand.vec)[1]) # run the procedure for the 1st fold - the for-loop below will work on folds 2 to 5
q.pp <- quantile(fold$RSFscores,probs=seq(0,1,.1)) ## computing quantiles of RSF scores
# --------------------------------------------------------
bin <- rep(NA,length(fold$RSFscores))
for (j in 1:10){
  bin[fold$RSFscores>=q.pp[j]& fold$RSFscores<q.pp[j+1]] = j ## binning RSF scores (10 bins)
}
used <- fold$Randoms
# --------------------------------------------------------
a <- table(used,bin) ## Occurrence of presence and available data by bin
a <- t(a) #transpose the table
a <- as.data.frame.matrix(a) ## the next few lines compute area-adjusted frequency of categories (bins) of RSF scores 
a$areaadjusted <- rep(NA,length(10))
sum0 <- sum(a[,1])
sum1 <- sum(a[,2])
a$areaadjusted <- (a[,2] / sum1 ) / (a[,1] / sum0)
a$bins <- seq(1,10,by=1);a

# --------------------------------------------------------
rho_model[1] <- with(a,cor.test(bins,areaadjusted,method="spearm"))$estimate ## Spearman correlation coefficient between RSF bin ranks and area-adjusted frequencies
# --------------------------------------------------------

# Run the procedure for the other folds and plot the binned RSF scores from the cross-validation
png("graphics/KfoldRSFstop.png", height=360, width=600)

par(oma=c(1,2,1,1)) 
par(mar=c(4.2,4.2,2,2))
with(a,plot(bins,areaadjusted, ylab="Area adjusted frequency", xlab="Binned RSF scores",xlim=c(1,10),ylim=c(0,2.5),type="b",cex=1.4,cex.lab=1.5,las=1,main="K-fold validation"))
abline(h = 1, lty = 3) ## Spearman correlation coefficient between RSF bin ranks and area-adjusted frequencies
plot_random_ind <- data.frame(a[,3], b = NA, c = NA, d = NA, e = NA)
for (i in 2:5){
  fold <- subset(dataset,rand.vec==unique(dataset$rand.vec)[i]) ## run the procedure on folds 2 to 5
  # --------------------------------------------------------
  q.pp <- quantile(fold$RSFscores,probs=seq(0,1,.1)) ## computing quantiles of RSF scores
  # --------------------------------------------------------
  bin <- rep(NA,length(fold$RSFscores))
  for (j in 1:10){
    bin[fold$RSFscores>=q.pp[j]& fold$RSFscores<q.pp[j+1]] = j  ## binning RSF scores (10 bins)
  }
  used <- fold$Randoms
  # --------------------------------------------------------
  a <- table(used,bin) ## area adjusted freq in used/available for each bin
  a <- t(a) #transpose the table
  a <- as.data.frame.matrix(a) ## the next few lines compute area-adjusted frequency of categories (bins) of RSF scores 
  a$areaadjusted <- rep(NA,length(10))
  sum0 <- sum(a[,1])
  sum1 <- sum(a[,2])
  a$areaadjusted <- (a[,2] / sum1 ) / (a[,1] / sum0)
  a$bins <- seq(1,10,by=1);a
  # --------------------------------------------------------
  rho_model[i] <- with(a,cor.test(bins,areaadjusted,method="spearm"))$estimate ## store Spearman correlation coefficients between RSF bin ranks and area-adjusted frequencies
  # --------------------------------------------------------
  par(oma=c(1,2,1,1)) 
  par(mar=c(4.2,4.2,2,2))
  with(a,points(bins,areaadjusted,col=i,type="b"))  
  plot_random_ind[,i] = a[,3]
}

dev.off()

## store Spearman correlation coefficients that will be used for final plots below ##
Rho_random_individuals <- rho_model

mean(Rho_random_individuals)
sd(Rho_random_individuals)

######################################################################################################
######################################################################################################
#############K-fold extraction with best model RSF MOVEMENT (Global with Habitat/Weather interaction)##########
formula<-cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc +
  WetlandSc + ForestSc + RockySc + WaterSc + sweSc:WetlandSc +
  prcpSc:RockySc + tmaxSc:ForestSc + tmaxSc:LichenSc + tmaxSc:WetlandSc +
  prcpSc:sweSc + NDVISc:tmaxSc


head(allNDVI_mvt)
data<-allNDVI_mvt

data$Animal_ID<-droplevels(data$Animal_ID)
data$prcpSc<-scale(data$prcp)
data$tmaxSc<-scale(data$tmax)
data$sweSc<-scale(data$swe)
data$NDVISc<-scale(data$NDVI)
data$WetlandSc<-scale(data$Wetland)
data$ForestSc<-scale(data$Forest)
data$LichenSc<-scale(data$Lichen)
data$RockySc<-scale(data$Rocky)
data$WaterSc<-scale(data$Water)

data <- subset(data, select = -c(13:18,22:23, 25:27, 33:35))
data <- subset(data, select = -c(4:5,17:30))

## PART 7 - Model evaluation sensu Boyce et al. 2002 - Individual block cross-validation
## -------------------------------------------------------------------------------------
##
## 5-fold cross-validation with data split by randomly assigning all GPS fixes from a single individual to a given fold, resulting in 3 folds with 9 individuals each and 2 folds with 8 individuals each. Home ranges of individuals assigned to different folds may overlap.    

# Remove columns used for evaluations fit earlier
data$rand.vec<- NULL   
data$rand.vec.x <- NULL 
data$rand.vec.y <- NULL 
# split individuals randomly
newdata <- data.frame(Animal_ID = unique(data$Animal_ID))
set.seed(5)
random_sample <- data.frame(Animal_ID = sample(newdata$Animal_ID, 30))
random_sample$rand.vec <- 0
random_sample$rand.vec[1:6] <- 1
random_sample$rand.vec[7:12] <- 2
random_sample$rand.vec[13:18] <- 3
random_sample$rand.vec[19:24] <- 4
random_sample$rand.vec[25:30] <- 5
with(random_sample, tapply(Animal_ID, rand.vec, length))
data <- merge(data, random_sample, by = "Animal_ID", all.x = T )
with(data, tapply(Animal_ID, rand.vec, unique))

# Fit the model in all folds but one.
#################################  SLOW STEP  ##################################
mod_inter_randomIND1 <- mclogit(formula, data[data$rand.vec != 1,])
mod_inter_randomIND2 <- mclogit(formula, data[data$rand.vec != 2,])
mod_inter_randomIND3 <- mclogit(formula, data[data$rand.vec != 3,])
mod_inter_randomIND4 <- mclogit(formula, data[data$rand.vec != 4,])
mod_inter_randomIND5 <- mclogit(formula, data[data$rand.vec != 5,])
#save(mod_inter_randomIND1, file = "mod_inter_randomIND1.RData")
#save(mod_inter_randomIND2, file = "mod_inter_randomIND2.RData")
#save(mod_inter_randomIND3, file = "mod_inter_randomIND3.RData")
#save(mod_inter_randomIND4, file = "mod_inter_randomIND4.RData")
#save(mod_inter_randomIND5, file = "mod_inter_randomIND5.RData")
# load(file = "mod_inter_randomIND1.RData")
# load(file = "mod_inter_randomIND2.RData")
# load(file = "mod_inter_randomIND3.RData")
# load(file = "mod_inter_randomIND4.RData")
# load(file = "mod_inter_randomIND5.RData")

# Remove columns used for evaluations fit earlier
data$lc_30_contribute <- NULL
data$RSFscores <- NULL

# Calculation of the contribution to RSF scores by the levels of the categorical predictor (reference category: coniferous forest) and calculation of RSF scores assuming the exponential form. Repeating it for each withhold fold.

# NOTE THE ATTACH COMMAND HERE - DETACH BELOW
attach(data)

data$RSFscores[rand.vec == 1] = exp(coef(mod_inter_randomIND1)[1] * data$NDVISc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[2] * data$sweSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 1] + 
                                      coef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 1]+
                                      coef(mod_inter_randomIND1)[10] * data$WetlandSc[rand.vec == 1] * 
                                      data$sweSc[rand.vec == 1] +
                                      coef(mod_inter_randomIND1)[11] * data$prcpSc[rand.vec == 1] *
                                      data$RockySc[rand.vec == 1] +
                                      coef(mod_inter_randomIND1)[12] * data$tmaxSc[rand.vec == 1] * 
                                      data$ForestSc[rand.vec == 1] +
                                      coef(mod_inter_randomIND1)[13] * data$tmaxSc[rand.vec == 1] *
                                      data$LichenSc[rand.vec == 1] +
                                      coef(mod_inter_randomIND1)[14] * data$tmaxSc[rand.vec == 1] *
                                      data$WetlandSc[rand.vec == 1] + 
                                      coef(mod_inter_randomIND1)[15] * data$sweSc[rand.vec == 1] *
                                      data$prcpSc[rand.vec == 1] + 
                                      coef(mod_inter_randomIND1)[16] * data$NDVISc[rand.vec == 1] *
                                      data$tmaxSc[rand.vec == 1])
data$RSFscores[rand.vec == 2] = exp(coef(mod_inter_randomIND1)[1] * data$NDVISc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[2] * data$sweSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 2] + 
                                      coef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 2]+
                                      coef(mod_inter_randomIND1)[10] * data$WetlandSc[rand.vec == 2] * 
                                      data$sweSc[rand.vec == 2] +
                                      coef(mod_inter_randomIND1)[11] * data$prcpSc[rand.vec == 2] *
                                      data$RockySc[rand.vec == 2] +
                                      coef(mod_inter_randomIND1)[12] * data$tmaxSc[rand.vec == 2] * 
                                      data$ForestSc[rand.vec == 2] +
                                      coef(mod_inter_randomIND1)[13] * data$tmaxSc[rand.vec == 2] *
                                      data$LichenSc[rand.vec == 2] +
                                      coef(mod_inter_randomIND1)[14] * data$tmaxSc[rand.vec == 2] *
                                      data$WetlandSc[rand.vec == 2] + 
                                      coef(mod_inter_randomIND1)[15] * data$sweSc[rand.vec == 2] *
                                      data$prcpSc[rand.vec == 2] + 
                                      coef(mod_inter_randomIND1)[16] * data$NDVISc[rand.vec == 2] *
                                      data$tmaxSc[rand.vec == 2])
data$RSFscores[rand.vec == 3] = exp(coef(mod_inter_randomIND1)[1] * data$NDVISc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[2] * data$sweSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 3] + 
                                      coef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 3]+
                                      coef(mod_inter_randomIND1)[10] * data$WetlandSc[rand.vec == 3] * 
                                      data$sweSc[rand.vec == 3] +
                                      coef(mod_inter_randomIND1)[11] * data$prcpSc[rand.vec == 3] *
                                      data$RockySc[rand.vec == 3] +
                                      coef(mod_inter_randomIND1)[12] * data$tmaxSc[rand.vec == 3] * 
                                      data$ForestSc[rand.vec == 3] +
                                      coef(mod_inter_randomIND1)[13] * data$tmaxSc[rand.vec == 3] *
                                      data$LichenSc[rand.vec == 3] +
                                      coef(mod_inter_randomIND1)[14] * data$tmaxSc[rand.vec == 3] *
                                      data$WetlandSc[rand.vec == 3] + 
                                      coef(mod_inter_randomIND1)[15] * data$sweSc[rand.vec == 3] *
                                      data$prcpSc[rand.vec == 3] + 
                                      coef(mod_inter_randomIND1)[16] * data$NDVISc[rand.vec == 3] *
                                      data$tmaxSc[rand.vec == 3])
data$RSFscores[rand.vec == 4] = exp(coef(mod_inter_randomIND1)[1] * data$NDVISc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[2] * data$sweSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 4] + 
                                      coef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 4]+
                                      coef(mod_inter_randomIND1)[10] * data$WetlandSc[rand.vec == 4] * 
                                      data$sweSc[rand.vec == 4] +
                                      coef(mod_inter_randomIND1)[11] * data$prcpSc[rand.vec == 4] *
                                      data$RockySc[rand.vec == 4] +
                                      coef(mod_inter_randomIND1)[12] * data$tmaxSc[rand.vec == 4] * 
                                      data$ForestSc[rand.vec == 4] +
                                      coef(mod_inter_randomIND1)[13] * data$tmaxSc[rand.vec == 4] *
                                      data$LichenSc[rand.vec == 4] +
                                      coef(mod_inter_randomIND1)[14] * data$tmaxSc[rand.vec == 4] *
                                      data$WetlandSc[rand.vec == 4] + 
                                      coef(mod_inter_randomIND1)[15] * data$sweSc[rand.vec == 4] *
                                      data$prcpSc[rand.vec == 4] + 
                                      coef(mod_inter_randomIND1)[16] * data$NDVISc[rand.vec == 4] *
                                      data$tmaxSc[rand.vec == 4])
data$RSFscores[rand.vec == 5] = exp(coef(mod_inter_randomIND1)[1] * data$NDVISc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[2] * data$sweSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[3] * data$tmaxSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[4] * data$prcpSc[rand.vec == 5] + 
                                      coef(mod_inter_randomIND1)[5] * data$LichenSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[6] * data$WetlandSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[7] * data$ForestSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[8] * data$RockySc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[9] * data$WaterSc[rand.vec == 5]+
                                      coef(mod_inter_randomIND1)[10] * data$WetlandSc[rand.vec == 5] * 
                                      data$sweSc[rand.vec == 5] +
                                      coef(mod_inter_randomIND1)[11] * data$prcpSc[rand.vec == 5] *
                                      data$RockySc[rand.vec == 5] +
                                      coef(mod_inter_randomIND1)[12] * data$tmaxSc[rand.vec == 5] * 
                                      data$ForestSc[rand.vec == 5] +
                                      coef(mod_inter_randomIND1)[13] * data$tmaxSc[rand.vec == 5] *
                                      data$LichenSc[rand.vec == 5] +
                                      coef(mod_inter_randomIND1)[14] * data$tmaxSc[rand.vec == 5] *
                                      data$WetlandSc[rand.vec == 5] + 
                                      coef(mod_inter_randomIND1)[15] * data$sweSc[rand.vec == 5] *
                                      data$prcpSc[rand.vec == 5] + 
                                      coef(mod_inter_randomIND1)[16] * data$NDVISc[rand.vec == 5] *
                                      data$tmaxSc[rand.vec == 5])




detach(data)

head(data)

# Run the k-fold CV evaluation sensu Boyce et al. 2002
dataset <- data[complete.cases(data[,"RSFscores"]),]
rho_model <- numeric(5) ## it will store Spearman's coefficients

fold <- subset(dataset,rand.vec==unique(dataset$rand.vec)[1]) # run the procedure for the 1st fold - the for-loop below will work on folds 2 to 5
q.pp <- quantile(fold$RSFscores,probs=seq(0,1,.1)) ## computing quantiles of RSF scores
# --------------------------------------------------------
bin <- rep(NA,length(fold$RSFscores))
for (j in 1:10){
  bin[fold$RSFscores>=q.pp[j]& fold$RSFscores<q.pp[j+1]] = j ## binning RSF scores (10 bins)
}
used <- fold$Randoms
# --------------------------------------------------------
a <- table(used,bin) ## Occurrence of presence and available data by bin
a <- t(a) #transpose the table
a <- as.data.frame.matrix(a) ## the next few lines compute area-adjusted frequency of categories (bins) of RSF scores 
a$areaadjusted <- rep(NA,length(10))
sum0 <- sum(a[,1])
sum1 <- sum(a[,2])
a$areaadjusted <- (a[,2] / sum1 ) / (a[,1] / sum0)
a$bins <- seq(1,10,by=1);a

# --------------------------------------------------------
rho_model[1] <- with(a,cor.test(bins,areaadjusted,method="spearm"))$estimate ## Spearman correlation coefficient between RSF bin ranks and area-adjusted frequencies
# --------------------------------------------------------

# Run the procedure for the other folds and plot the binned RSF scores from the cross-validation
png("graphics/KfoldRSFmvt.png", height=360, width=600)

par(oma=c(1,2,1,1)) 
par(mar=c(4.2,4.2,2,2))
with(a,plot(bins,areaadjusted, ylab="Area adjusted frequency", xlab="Binned RSF scores",xlim=c(1,10),ylim=c(0,2.5),type="b",cex=1.4,cex.lab=1.5,las=1,main="K-fold validation"))
abline(h = 1, lty = 3) ## Spearman correlation coefficient between RSF bin ranks and area-adjusted frequencies
plot_random_ind <- data.frame(a[,3], b = NA, c = NA, d = NA, e = NA)
for (i in 2:5){
  fold <- subset(dataset,rand.vec==unique(dataset$rand.vec)[i]) ## run the procedure on folds 2 to 5
  # --------------------------------------------------------
  q.pp <- quantile(fold$RSFscores,probs=seq(0,1,.1)) ## computing quantiles of RSF scores
  # --------------------------------------------------------
  bin <- rep(NA,length(fold$RSFscores))
  for (j in 1:10){
    bin[fold$RSFscores>=q.pp[j]& fold$RSFscores<q.pp[j+1]] = j  ## binning RSF scores (10 bins)
  }
  used <- fold$Randoms
  # --------------------------------------------------------
  a <- table(used,bin) ## area adjusted freq in used/available for each bin
  a <- t(a) #transpose the table
  a <- as.data.frame.matrix(a) ## the next few lines compute area-adjusted frequency of categories (bins) of RSF scores 
  a$areaadjusted <- rep(NA,length(10))
  sum0 <- sum(a[,1])
  sum1 <- sum(a[,2])
  a$areaadjusted <- (a[,2] / sum1 ) / (a[,1] / sum0)
  a$bins <- seq(1,10,by=1);a
  # --------------------------------------------------------
  rho_model[i] <- with(a,cor.test(bins,areaadjusted,method="spearm"))$estimate ## store Spearman correlation coefficients between RSF bin ranks and area-adjusted frequencies
  # --------------------------------------------------------
  par(oma=c(1,2,1,1)) 
  par(mar=c(4.2,4.2,2,2))
  with(a,points(bins,areaadjusted,col=i,type="b"))  
  plot_random_ind[,i] = a[,3]
}

dev.off()

## store Spearman correlation coefficients that will be used for final plots below ##
Rho_random_individuals <- rho_model

mean(Rho_random_individuals)
sd(Rho_random_individuals)
