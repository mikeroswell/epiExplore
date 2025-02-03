library(shellpipes)
rpcall("IBM_base_pars.Rout IBM_base_pars.R")
# get event timings: here is when each individual contact occurs
# Initialization
tMax <- 2e3
# epidemic parameters

## Probably should have gamma=1 as a default
setBeta <- 1
setGamma <- 1/3
tProb <- 1 ## This seems like a really good default
popSize <- 1e4 ## popSize is a parameter, and belongs here

# daily per-person interactions 
dailyRate <- setBeta/tProb ## this is code, and does not belong here.
seed <- 240
saveEnvironment()
