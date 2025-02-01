library(shellpipes)
rpcall("IBM_base_pars.Rout IBM_base_pars.R")
# get event timings: here is when each individual contact occurs
# Initialization
tMax <- 2e3
# epidemic parameters

setBeta <- 1
setGamma <- 1/3
tProb <- 1
popSize <- 1e4

# daily per-person interactions
dailyRate <- setBeta/tProb
seed <- 240
saveEnvironment()
