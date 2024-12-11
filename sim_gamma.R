library(shellpipes)
rpcall("sim_gamma.Rout sim_gamma.R")
manageConflicts()
simDist <- "gamma"
saveEnvironment()
