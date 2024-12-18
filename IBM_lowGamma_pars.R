library(shellpipes)
rpcall("IBM_lowGamma_pars.Rout IBM_lowGamma_pars.R")
# get event timings: here is when each individual contact occurs
# Initialization
tMax <- 2e3
# epidemic parameters

setBeta <- 1
setGamma <- 1/30
tProb <- 0.075

# daily per-person interactions
dailyRate <- setBeta/tProb
seed <- floor(runif(1, 0, 1e4))
saveEnvironment()