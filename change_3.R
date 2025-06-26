library(shellpipes)
rpcall("IBM_change_3_pars.Rout change_3.R IBM_base_pars.rda")
loadEnvironments()

setBeta <- 3
dailyRate <- setBeta
saveEnvironment()