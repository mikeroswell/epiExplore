library(shellpipes)
rpcall("IBM_highGamma_pars.Rout IBM_highGamma_pars.R")
# get event timings: here is when each individual contact occurs
# Initialization
tMax <- 2e3
# epidemic parameters

setBeta <- 0.05
setGamma <- 10
tProb <- 0.075

# daily per-person interactions
dailyRate <- setBeta/tProb
seed <- floor(runif(1, 0, 1e4))
saveEnvironment()