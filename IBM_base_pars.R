library(shellpipes)
rpcall("IBM_base_pars.Rout IBM_base_pars.R")

# how long to run
tMax <- 2e3
# epidemic parameters

setBeta <- 3
setGamma <- 1 # because we can rescale time if needed
tProb <- 1 ## could fiddle with transmission probabilities, but that might be equivalent to anothe rtime rescaling
popSize <- 1e4 ## popSize is a parameter, and belongs here
seed <- 240
rPar <- "exp" # sets duration distribution as exponential (base model)
saveEnvironment()
