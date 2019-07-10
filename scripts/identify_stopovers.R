# Load packages
library(data.table)

# Read data
DT <- readRDS('input/allNDVI2011.Rds')

# Set order
setorder(DT, FixDate, FixTime)

# Save stopover ID (note: not unique across animals, each will have stopover 1, 2, 3... )
DT[, stopover := rleid(state), by = .(Animal_ID, Year)]

# Count number of locs in each stopover (for each individual)
DT[, nByStopover := .N, .(stopover, .(Animal_ID, Year))]
