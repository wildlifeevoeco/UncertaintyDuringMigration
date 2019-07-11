# Load packages
library(data.table)

# Read data
DT <- readRDS('input/allNDVI2011.Rds')

# Set order
setorder(DT, FixDate, FixTime)

# Save stateSeries ID (note: not unique across animals, each will have stateSeries 1, 2, 3... )
DT[, stateSeries := rleid(state), by = .(Animal_ID, Year)]

# Count number of locs in each stopover (for each individual)
DT[, nByStateSeries := .N, .(stateSeries, Animal_ID, Year)]


DT[, idStateSeries := seq_len(.N), .(stateSeries, Animal_ID, Year)]
