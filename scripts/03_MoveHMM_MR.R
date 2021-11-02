#### MoveHMM extraction###
rm(list=ls())

### Packages ---
libs <- c('data.table', 'moveHMM', 'tseries')
lapply(libs, require, character.only = TRUE)

## load data
locs <-readRDS("~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/migration_MR.RDS")


## Put the dataframe in order --> check year in first, then animal and then the date
locs <- locs[order(locs$Year, locs$Animal_ID, locs$JDateTime),]
head(locs)

##Function 
data<-data.frame(locs$Easting,locs$Northing,locs$IDYear)
colnames(data)<-c("x","y","ID")

# Prep data to moveHMM
MRPrep<-prepData(data, type="UTM")

# Check if there is step = 0, if yes -->  add zero mass parameter 
whichzero <- which(MRPrep$step == 0)
length (whichzero/nrow(MRPrep))

# Check step and angle graphs
hist(MRPrep$step) ### max = 12000
hist(MRPrep$angle)
head(MRPrep)

##Fit HMM ---> set parameters
mid.fit<-fitHMM(MRPrep, nbStates=2, stepPar0=c(20,200,100,100,0.01,0.001), 
                anglePar0=c(pi,0,1,3), verbose=2)

mid.fit

# Step length parameters:
#   ----------------------
#   state 1      state 2
# mean      1.190991e+02 7.147807e+02
# sd        1.589131e+02 7.058030e+02
# zero-mass 3.968411e-04 5.813628e-08

# Turning angle parameters:
#   ------------------------
#   state 1    state 2
# mean          0.2702059 -0.0414552
# concentration 0.1037351  1.5004377

# Transition probability matrix:
#   -----------------------------
#   [,1]       [,2]
# [1,] 0.9141342 0.08586577
# [2,] 0.2053745 0.79462546


saveRDS(mid.fit, '~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/mid.fit.RDS')



### Wrap up states + residuals ----
# Decode the most probable state sequence - 1 = "encamped" 2 = "exploratory" (viterbi)
#  + pseudo residuals
DecodeAndPseudo <- function(x){
  cbind(state = viterbi(x),
        x$data,
        pseudoRes(x))
}

ls.fits <- list(mid.fit)

collect <- lapply(ls.fits, FUN = function(fit){
  DecodeAndPseudo(fit)
})

##Check results on graphs (steplength, turnangle and example of indiv)
plot(mid.fit,ask=TRUE,animals=NULL,breaks=20)

locs.w.states <- rbindlist(collect)

##number of movement and encamp values
aggregate(locs.w.states$state, locs.w.states[,c("state")], length)
#state     x
# 1     26384
# 2      9406

summary(locs.w.states)

# Merge the states back onto the locs
outMR <- merge(locs, locs.w.states, 
             by.x = c('Easting','Northing','IDYear'),
             by.y = c('x', 'y', 'ID'))
outMR$ptID<-c(1:nrow(outMR))

outMR$ptID[duplicated(outMR$ptID)]
outMR<-outMR[!duplicated(outMR$ptID), ]

saveRDS(outMR, '~/Internship 2019 Mun/Git/emilie_nlcaribou_2020/output/outMR.Rds')

message('=== PREP COMPLETE ===')

