library(shellpipes)
rpcall("IBM_change_6_pars.Rout change_6.R IBM_base_pars.rda")
loadEnvironments()

setBeta <- 6
dailyRate <- setBeta
saveEnvironment()