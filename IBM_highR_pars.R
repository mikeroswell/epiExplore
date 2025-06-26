library(shellpipes)
rpcall("IBM_highR_pars.Rout IBM_highR_pars.R")
# get event timings: here is when each individual contact occurs
# Initialization
tMax <- 2e3
# epidemic parameters

setBeta <- 8
setGamma <- 1/3
tProb <- 0.075
popSize <- 1e4
rPar <- "exp"

# daily per-person interactions
dailyRate <- setBeta/tProb
seed <- floor(runif(1, 0, 1e4))
saveEnvironment()