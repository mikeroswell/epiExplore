library(shellpipes)
rpcall("IBM_change_12_pars.Rout change_12.R IBM_base_pars.rda")
loadEnvironments()

setBeta <- 12
dailyRate <- setBeta
saveEnvironment()