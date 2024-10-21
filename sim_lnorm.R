library(shellpipes)
rpcall("sim_lnorm.Rout sim_lnorm.R")
manageConflicts()
simDist <- "lnorm"
saveEnvironment()