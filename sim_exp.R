library(shellpipes)
rpcall("sim_exp.Rout sim_exp.R")
manageConflicts()
simDist <- "rexp"
saveEnvironment()