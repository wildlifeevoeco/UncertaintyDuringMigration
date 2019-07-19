#### MoveHMM analysis ###

library(moveHMM)
library(data.table)

## load data
caribouclean<-readRDS("~/Emilie_project/Git/emilie_nlcaribou/output/caribouclean.RDS")

# check than all years for only MIDRIDGE is subset and drop others level
MigrMidR<-subset(caribouclean, Herd=="MIDRIDGE")
unique(MigrMidR$Year)
levels(MigrMidR$Herd)
MigrMidR$Herd <- levels(droplevels(MigrMidR$Herd))
unique(MigrMidR$Herd)

# Check number indiv == 34
unique(MigrMidR$Animal_ID)

# Check if there is step = 0, if yes -->  add zero mass parameter 
whichzero <- which(MRPrep$step == 0)
length (whichzero/nrow(MRPrep))

## Put the dataframe in order --> check year in first, then animal and then the date
MigrOrdered <- MigrMidR[order(MigrMidR$Year, MigrMidR$Animal_ID, MigrMidR$JDateTime),]
head(MigrMidR)

##Function 
data<-data.frame(MigrOrdered$Easting,MigrOrdered$Northing,MigrOrdered$IDYear)
colnames(data)<-c("x","y","ID")

# Prep data to moveHMM
MRPrep<-prepData(data, type="UTM")

# Check step and angle graphs
hist(MRPrep$step)
hist(MRPrep$angle)

head(MRPrep)

##Fit HMM
fitHMM1<-fitHMM(MRPrep, nbStates=2, stepPar0=c(20,200,100,100,0.01,0.001), 
                anglePar0=c(pi,0,1,3), verbose=2)

fitHMM1
saveRDS(fitHMM1, '~/Emilie_project/Git/emilie_nlcaribou/output/fitHMM1.Rds')

### Wrap up states + residuals ----
# Decode the most probable state sequence - 1 = "encamped" 2 = "exploratory" (viterbi)
#  + pseudo residuals
DecodeAndPseudo <- function(x){
  cbind(state = viterbi(x),
        x$data,
        pseudoRes(x))
}

ls.fits <- list(fitHMM1)

collect <- lapply(ls.fits, FUN = function(fit){
  DecodeAndPseudo(fit)
})

##Check results on graphs (steplength, turnangle and example of indiv)
plot(fitHMM1,ask=TRUE,animals=NULL,breaks=20)

locs.w.states <- rbindlist(collect)

##number of movement and encamp values
aggregate(locs.w.states$state, locs.w.states[,c("state")], length)
#state     x
# 1     28282
# 2      9930

summary(locs.w.states)

# Merge the states back onto the locs
out <- merge(MigrMidR, locs.w.states, 
             by.x = c('Easting','Northing','IDYear'),
             by.y = c('x', 'y', 'ID'))
MigrMidR$ptID<-c(1:nrow(MigrMidR))

out$ptID[duplicated(out$ptID)]
out<-out[!duplicated(out$ptID), ]

saveRDS(out, '~/Emilie_project/Git/emilie_nlcaribou/output/MoveHMM/out.Rds')
