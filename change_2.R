library(shellpipes)
rpcall("IBM_change_2_pars.Rout change_2.R IBM_base_pars.rda")
loadEnvironments()

setBeta <- 2
dailyRate <- setBeta
saveEnvironment()