
library(shellpipes)
rpcall("IBM_base_pars.Rout IBM_base_pars.R")
# get event timings: here is when each individual contact occurs
# Initialization
tMax <- 2e3
# epidemic parameters

beta <- 3
gamma <- 1

popSize <- 1e1

# daily per-person interactions
seed <- 240
saveEnvironment()

