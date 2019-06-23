### Packages ---
libs <- c('data.table', 'moveHMM', 'tseries')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
locs <- readRDS('~/Git/emilie_nlcaribou/output/caribouclean.Rds')
locs <- caribouclean
###remove Pothill from locs tab (This herd not migrate enough)
#locs<- subset(locs, HERD != "POTHILL")

### Functions ----
PrepHerd <- function(in.dt){
  prepData(in.dt[, .(Easting, Northing, ID = Animal_ID, Herd)],
           type = 'UTM', coordNames = c('Easting', 'Northing'))
}

### Prep Data By Herd ----
# BUCHANS
buch <- PrepHerd(locs[HERD == 'BUCHANS'])

# GREY
grey <- PrepHerd(locs[HERD == 'GREY'])

# LAPOILE
lap <- PrepHerd(locs[HERD == 'LAPOILE'])

# MIDDLE RIDGE
mid <- PrepHerd(locs[Herd == 'MIDRIDGE'])

# POTHILL
pot <- PrepHerd(locs[HERD == 'POTHILL'])     ####not need to run

# TOPSAILS
top <- PrepHerd(locs[HERD == 'TOPSAILS'])

### Set HMM Parameters ----
# TODO: check with CH & MB about stationary (see readme as well)
buch.params <- list(
  data = buch,
  nbStates = 2,
  stepPar0 = c(100, 1000, 100, 1000, 0.1, 0.005),
  anglePar0 = c(pi, 0, 1, 1.5), 
  verbose = 2,
  stationary = TRUE,
  fit = TRUE
)
grey.params <- list(
  data = grey,
  nbStates = 2,
  stepPar0 = c(20, 300, 100, 100, 0.01, 0.001),
  anglePar0 = c(pi, 0, 1, 3), 
  verbose = 2,
  # stationary = TRUE,
  fit = TRUE
)
lap.params <- list(
  data = lap,
  nbStates = 2,
  stepPar0 = c(100, 1000, 100, 1000, 0.01, 0.001),  
  anglePar0 = c(pi, 0, 0.1, 2),
  verbose = 2,
  stationary = TRUE,
  fit = TRUE
)
mid.params <- list(
  data = mid,
  nbStates = 2, 
  stepPar0 = c(20, 300, 100, 100, 0.01, 0.001),
  anglePar0 = c(pi, 0, 1, 3), 
  verbose = 2,
  # stationary = TRUE,
  fit = TRUE
)
pot.params <- list(             ###not need to run
  data = pot,
  nbStates = 2,
  stepPar0 = c(100, 1000, 100, 300),
  anglePar0 = c(0, 0, 1, 1),
  beta0 = matrix(c(-3, -1), 1, 2), 
  stationary = TRUE,
  fit = TRUE,
  verbose = 2
)

top.params <- list(
  data = top,
  nbStates = 2,
  stepPar0 = c(100, 1000, 100, 1000, 0.25, 0.25),
  anglePar0 = c(pi, 0, 1, 0.5),
  stationary = TRUE,
  fit = TRUE,
  verbose = 2
)

### Fit HMM By Herd ----
top.fit <- do.call(fitHMM, top.params)
pot.fit <- do.call(fitHMM, pot.params)  #### not need to run 
lap.fit <- do.call(fitHMM, lap.params)
mid.fit <- do.call(fitHMM, mid.params)
grey.fit <- do.call(fitHMM, grey.params)
buch.fit <- do.call(fitHMM, buch.params)

### Wrap up states + residuals ----
# Decode the most probable state sequence - 1 = "encamped" 2 = "exploratory" (viterbi)
#  + pseudo residuals
DecodeAndPseudo <- function(x){
  cbind(state = viterbi(x),
        x$data,
        pseudoRes(x))
}

ls.fits <- list(top.fit, lap.fit, mid.fit, grey.fit, buch.fit)  
collect <- lapply(ls.fits, FUN = function(fit){
  DecodeAndPseudo(fit)
})

plot(top.fit,ask=TRUE,animals=NULL,breaks=20)


# TODO(MB, CH): check error below
# Note: Some angles are equal to pi, and the corresponding pseudo-residuals are not included
# and     In qnorm(t(a) %*% (gamma/sum(a)) %*% pStepMat[i, ]) : NaNs produced
locs.w.states <- rbindlist(collect)

##number of movement and encamp values
aggregate(locs.w.states$state, locs.w.states[,c("state")], length)

# Merge the states back onto the locs
out <- merge(locs, locs.w.states, 
             by.x = c('EASTING', 'NORTHING', 'ANIMAL_ID'),
             by.y = c('x', 'y', 'ID'))

###remove duplicates
out$V1[duplicated(out$V1)]
out<-out[!duplicated(out$V1), ]

### Output ----
# Locs with states
saveRDS(out, '~/Git/emilie_nlcaribou/output/Data extraction/MoveHMM/out.Rds')
saveRDS(out, '~/Git/emilie_nlcaribou/output/Data extraction/MoveHMM/out.csv')

# Save the fitted HMM objects
saveRDS(top.fit, '~/Git/emilie_nlcaribou/output/Data extraction/MoveHMM/top-hmmobj')
saveRDS(pot.fit, '~/Git/emilie_nlcaribou/output/Data extraction/MoveHMM/pot-hmmobj')
saveRDS(lap.fit, '~/Git/emilie_nlcaribou/output/Data extraction/MoveHMM/lap-hmmobj')
saveRDS(mid.fit, '~/Git/emilie_nlcaribou/output/Data extraction/MoveHMM/mid-hmmobj.Rds')
saveRDS(grey.fit, '~/Git/emilie_nlcaribou/output/Data extraction/MoveHMM/grey-hmmobj')
saveRDS(buch.fit, '~/Git/emilie_nlcaribou/output/Data extraction/MoveHMM/buch-hmmobj')

### Extract Estimates ----

top.fit[2]$mle
CI(top.fit)

pot.fit[2]$mle
CI(pot.fit)

lap.fit[2]$mle
CI(lap.fit)

mid.fit[2]$mle
CI(mid.fit)

grey.fit[2]$mle
CI(grey.fit)

buch.fit[2]$mle
CI(buch.fit)

### Extract Number of Fixes per state*herd combination ---

out[, .N, by = .(state)][, .(N,state,herd_total = sum(.SD))]

##Extract number of ind by year for Midridge herd
out[, uniqueN(ANIMAL_ID), by = .(HERD,year)]

out[, .N, by = .(HERD, year, ANIMAL_ID)][order(N)]

